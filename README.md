# Zero downtime upgrades of stateful systems

*Work in progress, please don't share, but do feel free to get involved!*

## Motivation

Most deployed programs need to be upgraded at some point. The reasons vary from
adding new features to patching a bug and potentially fixing a broken state.

Even though upgrades are an essential part of software maintenance, programming
languages tend to not help the programmer deal with them in any way.

Erlang OTP perhaps being one exception, although from what I understand, even
there hot-code swapping is not really recommended in production.

OTP application and release are library constructs to help with deployment and
upgrades.

What would good support for upgrades look like?

* No downtime
* Seamless, don't interrupt existing client connections / sessions
* Typed state migrations
* Backwards and forwards compatbility?

## Constraints

* Upgrades of from arbitrary program to arbitrary program is too messy

* Abstract state machine, model of computation that allows us to express the
  problem at the right level of abstration

* Refinement of state machines gives us a model of updates, perhaps even clearer
  when looking at indexed containers and their morphisms?

* SMs and actors, messy call graph vs dag (["pipeline-oriented
  programming"](https://youtu.be/ipceTuJlw-M?t=493))

## Plan

* Linear pipelines, to keep things simple

* Each stage of the pipeline runs in parallel with all other stages, thus giving
  us pipelining parallelism a la assembly lines

* The transformation at each stage is done via a state machine (transducer)

* Both pipelines and state machines can be seralised and sent over the wire,
  this enables remote deployments and upgrades

* The remote end will need to deserialise and typecheck the receiving code in
  order to assure that it's compatible with the already deployed code

## How it works

### State machines

```haskell
data T s a b where
  -- Identity and composition.
  Id      :: T s a a
  Compose :: T s b c -> T s a b -> T s a c

  -- Introducing and incrementing integers.
  Int     :: Int -> T s () Int
  Incr    :: T s Int Int

  -- Mapping over sum types.
  Case    :: T s a c -> T s b d -> T s (Either a b) (Either c d)

  -- Read and update the state.
  Get     :: T s () s
  Put     :: T s s ()

  -- Converting values from and to strings.
  Read    :: Read a => T s String a
  Show    :: Show a => T s a String

-- Forward composition.
(>>>) :: T s a b -> T s b c -> T s a c
f >>> g = g `Compose` f
```

### Example

```haskell
counterV1 :: T Int String String
counterV1 =
  Read >>> Get `Case` (Get >>> Incr >>> Put) >>> Show
```

### Semantics

```haskell
runT :: T s a b -> a -> s -> (b, s)
runT f x s = runState (eval f x) s

eval :: T s a b -> a -> State s b
eval Id            = return
eval (Compose g f) = eval g <=< eval f
eval (Int i)       = return . const i
eval Incr          = return . (+ 1)
eval (Case f g)    = either (fmap Left . eval f) (fmap Right . eval g)
eval Get           = const get
eval Put           = put
eval Read          = return . read
eval Show          = return . show
```

```haskell
> runT counterV1 "Left ()" 0
("Left 0",0)
> runT counterV1 "Left ()" 1
("Left 1",1)
> runT counterV1 "Right ()" 0
("Right ()",1)
```

### Pipelines

```haskell
data P a b where
  IdP    :: P a a
  (:>>>) :: Typeable b => P a b -> P b c -> P a c
  SM     :: Typeable s => Name -> s -> T s a b -> P a b

type Name = String
```

### Deployment

```haskell
deploy :: forall a b. (Typeable a, Typeable b)
       => P a b -> Queue (Msg a) -> IO (Queue (Msg b))
```

```haskell
data Msg a where
  Item    :: Maybe Socket -> a -> Msg a
  Upgrade :: Maybe Socket -> Name -> UpgradeData_-> Msg a
  ...
```

### Upgrades

```haskell
data UpgradeData_ = UpgradeData_
  { oldState       :: Ty_
  , newState       :: Ty_
  , newInput       :: Ty_
  , newOutput      :: Ty_
  , newSM          :: U
  , stateMigration :: U
  }
```


### Untyped state machines

```haskell
data U
  = IdU
  | ComposeU U U
  | IntU Int
  | CaseU U U
  | IncrU
  | GetU
  | PutU
  | ReadU Ty_
  | ShowU Ty_
```

```haskell
data Ty_
  = UTUnit
  | UTInt
  | UTBool
  | UTString
  | UTPair Ty_ Ty_
  | UTEither Ty_ Ty_
```

### Type checking and inference

```haskell
data Ty a where
  TUnit   :: Ty ()
  TInt    :: Ty Int
  TBool   :: Ty Bool
  TString :: Ty String
  TPair   :: Ty a -> Ty b -> Ty (a, b)
  TEither :: Ty a -> Ty b -> Ty (Either a b)
  TDon'tCare :: Ty a
```


```haskell
data ETy where
  ETy :: Typeable a => Ty a -> ETy

inferTy :: Ty_ -> ETy

typeCheck :: U -> Ty s -> Ty a -> Ty b -> Either TypeError (T s a b)
```

### Sources and sinks

```haskell
run :: (Typeable a, Typeable b) => Source a -> Codec (Msg a) (Msg b) -> P a b -> Sink b r -> IO r
```

### Remote upgrades

```haskell
run (FromTCP "127.0.0.1" 3000) readShowCodec (SM "counter" 0 counterV1) ToTCP
```

```bash
# Get the current state of the counter.
$ echo 'Item "Left ()"' | nc 127.0.0.1 3000
Item "Left 0"

# Increment the counter.
$ echo 'Item "Right ()"' | nc 127.0.0.1 3000
Item "Right ()"

# Read the counter again.
$ echo 'Item "Left ()"' | nc 127.0.0.1 3000
Item "Left 1"
```

```haskell
nc "127.0.0.1" 3000 (Item_ (show ReadCountV1))
nc "127.0.0.1" 3000 (Item_ (show IncrCountV1))
nc "127.0.0.1" 3000 (Item_ (show IncrCountV1))
nc "127.0.0.1" 3000 (Item_ (show ReadCountV1))

let msg :: Msg ()
    msg = Upgrade_ "counter"
            (UpgradeData_ UTInt UTInt UTString UTString counterV2U IdU)
nc "127.0.0.1" 3000 msg

nc "127.0.0.1" 3000 (Item_ (show ReadCountV2))
nc "127.0.0.1" 3000 (Item_ (show ResetCountV2))
nc "127.0.0.1" 3000 (Item_ (show ReadCountV2))
```

```
Item "Left 0"               -- The initial value of the counter is 0.
Item "Right ()"             -- Two increments.
Item "Right ()"
Item "Left 2"               -- The value is now 2
UpgradeSucceeded "counter"
Item "Left 2"               -- The counter's state is preserved by the upgrade
Item "Right (Right ())"     -- Reset the counter.
Item "Left 0"               -- The value is back to 0.
```


## Future work

- [ ] Upgrade pipelines, rather than state machines running in the pipelines;
- [ ] Better language for describing state machines?
- [ ] Content-addressed hashes?
- [ ] Combining multiple sources?
- [ ] Multiple sinks? `Tee :: P a b -> Sink a () -> P a b`?
- [ ] Blocking file I/O, let it block and rely on pipelining parallelism and sharding
- [ ] How to we build something like a REST API on top of the TCP stuff?


## Contributing

```bash
git clone https://github.com/stevana/arrow-loop-state-machines.git
cd arrow-loop-state-machines
nix-shell
cabal repl
```

## See also

* [Parallel stream processing with zero-copy fan-out and
  sharding](https://stevana.github.io/parallel_stream_processing_with_zero-copy_fan-out_and_sharding.html);

* [Hot-swapping state
  machines](https://stevana.github.io/hot-code_swapping_a_la_erlang_with_arrow-based_state_machines.html);

* [Application architecture as
  code](https://www.youtube.com/watch?v=vasvpFRPx9c)
* [IxC: Infrastructure as Code, from Code, with
  Code](https://architectelevator.com/cloud/iac-ifc-trends/)
* [Benthos: fancy stream processing made operationally
  mundane](https://www.benthos.dev/)
