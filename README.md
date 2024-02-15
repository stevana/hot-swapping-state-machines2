# Zero downtime upgrades of stateful systems

*Work in progress, please don't share, but do feel free to get involved!*

## Motivation

Most deployed programs need to be upgraded at some point. The reasons vary from
adding new features to patching a bug and potentially fixing a broken state.

Even though upgrades are an essential part of software maintenance, programming
languages tend to not help the programmer deal with them in any way.

Some languages are easier to deploy than others. For example due to static and
cross compilation, targeting an infrequently changing VM, or by being
interpreted. The ease of deployment is only one part of upgrades though.

In stateful systems we want to preserve the state...

Lisp, Smalltalk, fix-and-continue when debugging? REPL into production?

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

What exactly are upgrades?

What are programs?

* Upgrades of from arbitrary program to arbitrary program is too messy

* Abstract state machine, model of computation that allows us to express the
  problem at the right level of abstration
  - gen_server, important building block from OTP

* Refinement of state machines gives us a model of updates, perhaps even clearer
  when looking at indexed containers and their morphisms?

* SMs and actors, messy call graph vs dag (["pipeline-oriented
  programming"](https://youtu.be/ipceTuJlw-M?t=493))

* A simple way adding parallelism: pipelines

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

Typed state machines are represented using a datatyped parametrised by the
state, `s`, and indexed by its input type, `a`, and output type `b`.

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

(The initiated might recognise that this is an instance of `Category` and
partially an instance of `Cocartesian`, plus some extras.)

### Example

To keep things concrete let's have an example. Here's how we can represent a
counter with two operations: read the current value of the counter and increment
the counter by one.

```haskell
type InputV1  = Either () ()
type OutputV1 = Either Int ()

pattern ReadCountV1 :: InputV1
pattern ReadCountV1 = Left ()

pattern IncrCountV1 :: InputV1
pattern IncrCountV1 = Right ()

counterV1 :: T Int String String
counterV1 =
  Read >>>
  Get `Case` (Get >>> Incr >>> Put) >>>
  Show
```

Notice how the two operations' inputs and outputs are represented with an
`Either` over which the `Case` operates. The counter does its own
deserialisation and serialisation via `Read` and `Show`, we'll come back to why
in a bit.

### Semantics

We can intepret our typed state machines in terms of the `State` monad as
follows.

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

Using the above interpreter we can run our example from before.

```haskell
> runT counterV1 (show ReadCountV1) 0
("Left 0",0)
> runT counterV1 (show IncrCountV1) 0
("Right ()",1)
> runT counterV1 (show ReadCountV1) 1
("Left 1",1)
```

### Pipelines

Pipelines are represented by a type similar to that for typed state machines,
it's also indexed by the input and output types.

We can picture a pipeline as a conveyor belt with state machines operating on
the items passing through.

```haskell
data P a b where
  IdP    :: P a a
  (:>>>) :: Typeable b => P a b -> P b c -> P a c
  SM     :: Typeable s => Name -> s -> T s a b -> P a b

type Name = String
```
Notice how the state type of the state machines is existentially quantified,
meaning each state machine can have it's own state.

### Deployment

Pipelines can be deployed. Each state machine will be spawned on its own thread,
meaning that all state machines run in parallel, and they will be connected via
queues.

Given a pipeline `P a b` and an input `Queue (Msg a)` we get an output `Queue
(Msg b)`, where `Msg` is defined as follows.

```haskell
data Msg a
  = Item (Maybe Socket) a
  | Upgrade (Maybe Socket) Name UpgradeData_
  ...
```

This little wrapper allows us to perform upgrades of state machines, assuming
they are compatible with the running state machine.

```haskell
deploy :: forall a b. (Typeable a, Typeable b)
       => P a b -> Queue (Msg a) -> IO (Queue (Msg b))
deploy IdP             q = return q
deploy (f :>>> g)      q = deploy g =<< deploy f q
deploy (SM name s0 f0) q = do
  q' <- newQueue
  let go :: Typeable s => s -> T s a b -> IO ()
      go s f = do
        m <- readQueue q
        case m of
          Item msock i -> do
            let (o, s') = runT f i s
            writeQueue q' (Item msock o)
            go s' f
          Upgrade msock name' ud
            | name /= name' -> do
                writeQueue q' (Upgrade msock name' ud)
                go s f
            | otherwise ->
                case typeCheckUpgrade s f ud of
                  Just (UpgradeData (f' :: T s a b) (g :: T () s s)) -> do
                    let (s', ()) = runT g s ()
                    go s' f'
                  Nothing ->
                    go s f
          ...
  _pid <- forkIO (go s0 f0)
  return q'
```

XXX: types don't change?!

### Upgrades

Upgrades are sent over the wire in a serialised format and deserialised at the
other end, so they need to be plain first-order data.

This means we can't merely send over our typed state machine type `t :: T s a
b`, or rather the receiver will have to reconstruct the type information. If
this would strange, the perhaps easiest way to convince yourself is to imagine
you receive `show t` and now you want to reconstruct `t`. When you call `read
(show t)` you need to annotate it with what type to read into, and that's the
problem: at this point you don't have `T s a b`.

So the plan around this is to introduce a plain first-order datatype for
upgrades, which can easily be serialised and deserialised, and then use
*typechecking* to reconstruct the type information.

```haskell
data UpgradeData_ = UpgradeData_
  { oldState        :: Ty_
  , newState        :: Ty_
  , newInput        :: Ty_
  , newOutput       :: Ty_
  , newStateMachine :: U
  , stateMigration  :: U
  }
  deriving (Show, Read)
```

We can to typecheck the above untyped upgrade into the following typed version.

```haskell
data UpgradeData s a b = UpgradeData (T s a b) (T () s s)
```

The way typechecking for upgrades work is basically the user needs to provide
the untyped types of the state, input and output types of the new state machine
as well as the state migration function, from the untyped types we can infer the
typed types which we then typecheck the new state machine and migration function
against.

```haskell
typeCheckUpgrade :: forall s s' a b. (Typeable s, Typeable a, Typeable b)
                 => s -> T s a b -> UpgradeData_ -> Maybe (UpgradeData s a b)
typeCheckUpgrade _s _f (UpgradeData_ t_ t'_ a'_ b'_ f_ g_) =
  case (inferTy t_, inferTy t'_, inferTy a'_, inferTy b'_) of
    (ETy (t :: Ty t), ETy (t' :: Ty t'), ETy (a' :: Ty a'), ETy (b' :: Ty b')) -> do
      Refl <- eqT @a @a'
      Refl <- eqT @b @b'
      Refl <- eqT @s @t
      f <- typeCheck f_
      g <- typeCheck g_ TUnit t t
      return (UpgradeData f g)
```
Where untyped types are defined as follows:

```haskell
data Ty_
  = UTUnit
  | UTInt
  | UTBool
  | UTString
  | UTPair Ty_ Ty_
  | UTEither Ty_ Ty_
```

and typed types as follows:

```haskell
data Ty a where
  TUnit   :: Ty ()
  TInt    :: Ty Int
  TBool   :: Ty Bool
  TString :: Ty String
  TPair   :: Ty a -> Ty b -> Ty (a, b)
  TEither :: Ty a -> Ty b -> Ty (Either a b)
  ...
```

and the way we infer typed types from the untyped ones is done as follows:

```haskell
data ETy where
  ETy :: Typeable a => Ty a -> ETy

inferTy :: Ty_ -> ETy
inferTy UTUnit = ETy TUnit
inferTy UTInt  = ETy TInt
inferTy UTBool = ETy TBool
inferTy UTString = ETy TString
inferTy (UTPair ua ub) = case (inferTy ua, inferTy ub) of
  (ETy a, ETy b) -> ETy (TPair a b)
inferTy (UTEither ua ub) = case (inferTy ua, inferTy ub) of
  (ETy a, ETy b) -> ETy (TEither a b)
```

Now the only piece missing is untyped state machines and how to typecheck those
into typed ones.

```haskell
typeCheck :: U -> Ty s -> Ty a -> Ty b -> Maybe (T s a b)

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

I'll spare you from the details, but the main ingredient is to use the
`Data.Typeable` instances to check if the types match up, similarly to how it
was done above in `typeCheckUpgrade`.

### Sources and sinks

Almost there. When we deploy a pipeline `P a b` we need to provide a `Queue (Msg
a)` and get a `Queue (Msg b)`, what are we supposed to do with those queues? We
could manually feed them with items, but for convenience it's nice to have some
basic reusable adaptors that we can connect these "garden hoses" to.

We call something that provides an input queue a `Source` and something that
consumes an output queue a `Sink`. Useful sources and sinks include
stdin/stdout, files, and TCP streams.

We can then implement a run function with the following type:

```haskell
run :: (Typeable a, Typeable b) => Source a -> Codec (Msg a) (Msg b) -> P a b -> Sink b r -> IO r
```

Where `Codec a b` contains a deserialiser from `ByteString` to `Maybe a` and a
serialiser from `b` to `ByteString`. We need this because our sources and sinks
produce and consume `ByteString`s.

### Remote upgrades

Putting it all together we can now create a TCP server for our counter:

```haskell
run (FromTCP "127.0.0.1" 3000) readShowCodec (SM "counter" 0 counterV1) ToTCP
```

If we run the above in a REPL, then from another terminal we can interact with
the server as follows.

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

In order to make life a bit easier for ourselves, we can implement a simple TCP
client in Haskell and use from another REPL to achieve the same result.

```haskell
nc "127.0.0.1" 3000 (Item Nothing (show ReadCountV1))
nc "127.0.0.1" 3000 (Item Nothing (show IncrCountV1))
nc "127.0.0.1" 3000 (Item Nothing (show IncrCountV1))
nc "127.0.0.1" 3000 (Item Nothing (show ReadCountV1))
```
```
Item "Left 0"               -- The initial value of the counter is 0.
Item "Right ()"             -- Two increments.
Item "Right ()"
Item "Left 2"               -- The value is now 2
```


```haskell
type InputV2  = Either () InputV1
type OutputV2 = Either () OutputV1

counterV2 :: T Int String String
counterV2 =
  Read >>> (Get `Case` (Get >>> Incr >>> Put) `Case` (Int 0 >>> Put)) >>> Show

pattern ReadCountV2 :: InputV2
pattern ReadCountV2  = Left ()

pattern IncrCountV2 :: InputV2
pattern IncrCountV2  = Right (Left ())

pattern ResetCountV2 :: InputV2
pattern ResetCountV2 = Right (Right ())
```


```haskell

let msg :: Msg ()
    msg = Upgrade Nothing "counter"
            (UpgradeData_ UTInt UTInt UTString UTString (erase counterV2) IdU)
nc "127.0.0.1" 3000 msg

nc "127.0.0.1" 3000 (Item Nothing (show ReadCountV2))
nc "127.0.0.1" 3000 (Item Nothing (show ResetCountV2))
nc "127.0.0.1" 3000 (Item Nothing (show ReadCountV2))
```

```
UpgradeSucceeded "counter"
Item "Left 2"               -- The counter's state is preserved by the upgrade
Item "Right (Right ())"     -- Reset the counter.
Item "Left 0"               -- The value is back to 0.
```


## Future work

- [ ] Upgrades that change the state type
- [ ] Upgrade pipelines, rather than state machines running in the pipelines;
- [ ] Upgrade the platform/language/VM without downtime?
- [ ] Compiled rather than interpreted?
- [ ] Better language for describing state machines?
- [ ] Content-addressed hashes?
- [ ] Combining multiple sources?
- [ ] Multiple sinks? `Tee :: P a b -> Sink a () -> P a b`?
- [ ] Blocking file I/O, let it block and rely on pipelining parallelism and sharding
- [ ] How to we build something like a HTTP-based API on top of the TCP stuff?


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

* Hot-swapping in
  [Elm](https://web.archive.org/web/20131006235603/http://elm-lang.org/blog/Interactive-Programming.elm)

* https://github.com/nmattia/haskell-hot-swap
* https://github.com/fbsamples/ghc-hotswap/
