# Zero-downtime upgrades of stateful systems

*Work in progress, please don't share, but do feel free to get involved!*

## Motivation

Most deployed programs need to be upgraded at some point. The reasons vary from
adding new features to patching a bug and potentially fixing a broken state.

Even though upgrades are an essential part of software maintenance, programming
languages tend to not help the programmer deal with them in any way.

There's one exception, that I know of, Erlang/OTP. In OTP there are library
constructs for *applications* and *releases*, which can be used to hotswap code
with zero-downtime.

If you haven't seen Erlang's hotswapping feature before, then you might want to
have a look at [Erlang the movie](https://www.youtube.com/watch?v=xrIjfIjssLE),
which contains an example of this.

If you prefer reading over watching, then I've written an earlier
[post](https://stevana.github.io/hot-code_swapping_a_la_erlang_with_arrow-based_state_machines.html)
which starts off by explaining a REPL session which does an upgrade.

Lisp and Smalltalk
[fix-and-continue](https://lispcookbook.github.io/cl-cookbook/debugging.html#resume-a-program-execution-from-anywhere-in-the-stack)
when debugging? REPL into production? Late binding helps?

Let's take a step back and ask ourselves: what would good support for upgrades
look like?

* No downtime
* Seamless, don't interrupt existing client connections / sessions
  - In stateful systems we want to preserve the state...
* Typed state migrations
* Backwards and forwards compatbility?
* Atomic (succeed or fail and rollback)
* Downgrades?
* (Signed, periodic... c.f. TUF)
* Fix-and-continue debugging?
* Live coding?

In the rest of this post I'd like to explore how we can achieve some of this.

## State machines and upgrades

Having defined some desirable characteristics of upgrades, let's move on to
defining what we mean by upgrades.

The first question is upgrades of *what* exactly?

There's different kinds of software systems one might want to upgrade.

  1. Client-only, e.g. a compiler, editor, or some command line utility which
     runs locally on your computer and doesn't interact with any server.
     Downtime is typically not a problem, and the state of the program is
     typically saved to disk. The operating system's package mangager typically
     takes care of the upgrades, with minimal user involvement. However there
     are situations where one might like to perform an upgrade without first
     terminating the old version of a client-only application, e.g. [live-coding
     music](https://en.wikipedia.org/wiki/TidalCycles) or when working with
     large data sets, e.g. in
     [bioinformatics](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1386713/);

  2. Client-server applications where the target of the upgrade is a *stateless*
     component of the server, e.g. a front-end or a REST API. The stateless
     components typically retrive the state they need to service a request from
     a stateful component, e.g. a database, but they don't maintain any state of
     their own, which makes stateless components easier to upgrade. A common
     strategy is to stick a load balancer in-front of the stateless
     component(s), spin up the new version of the component while keeping the
     old version around, and then (slowly) migrate traffic over to the new
     version. Notice that this wouldn't necessarily work if there was state in
     the components, as then the state of the old and new versions of the
     components might diverge and potentially have unexpected results;

  3. Client-server applications where the target of the upgrade is a *stateful*
     component of the server, e.g. a database or a service with a stateful
     protocol like FTP. Databases were designed for supporting upgrades, with
     features like schema migrations and replication. The high-level idea would
     be to spin up the new version, take a snapshot of the old database, start
     logically replicating all new requests from the old to the new database
     while also restoring the snapshot to the new database, once the new
     database has caught up, we can switch over and tear down the old database.
     Depending on the volume and traffic this can still be a difficult operation.

     A service like FTP, where once the user is connected they can "move around"
     by e.g. changing the working directory and list the contents of the current
     working directory, are typically not possible to upgrade without downtime.
     The problem is that the response of one command depends on the history of
     previous commands in that user sessions, and this state is transient;

  4. Distributed stateful systems, e.g. a distributed key-value database. This
     is similar to the above, but the replication of data is performed all the
     time rather than only at the moment an upgrade is performed. The
     disadvantage is that we need more hardware and bandwidth, but on the other
     hand it makes upgrades much easier. Distributed systems can typically
     tolerate and repair some amount of faulty replicas, which allows for
     rolling upgrades where we replace one of the server components at the time;

  5. There's also [local-first](https://www.inkandswitch.com/local-first/)
     systems, which are different than all above. I've not had a chance to think
     about upgrades in that context, so I won't talk about them any further.

* Let's focus on stateful systems that are not distributed
  - sweet spot for this technique, even though the technique might simplify
    upgrades in other settings or potentially enable possibilities (debugging /
    live coding, etc)
  - do people avoid stateful systems because of the upgrade problem?
      + ORM mismatch also has its problems, Thompson

* Within a system, how do we represent the client and potential server *programs*?

* Specific programming language? Lambda calculus? Turing machines?
  - Too clumsy, too low-level
  - perfect machine model would step-for-step simulation of any algorithm
  - for any algorithm, there's an asm with the same states and the same
    transition function.

* Abstract state machine, model of computation that allows us to express the
  problem at the right level of abstration

  - Lamport's [Computation and State
    Machines](https://www.microsoft.com/en-us/research/publication/computation-state-machines/)
  - gen_server, important building block from OTP

- what are states?
- What are inputs and outputs?

* Second question: in this model of computation, what are upgrades exactly?

* Containers and their morphisms?

* Refinement of state machines gives us a model of updates, perhaps even clearer
  when looking at indexed containers and their morphisms?

* SMs and actors, messy call graph vs dag (["pipeline-oriented
  programming"](https://youtu.be/ipceTuJlw-M?t=493))

* A simple way adding parallelism: pipelines

## Plan

* I hope that the abstract theory helps explain where my ideas are coming from,
  now let's make things concrete with some code

* Linear pipelines, to keep things simple

* Each stage of the pipeline runs in parallel with all other stages, thus giving
  us pipelining parallelism a la assembly lines

* The transformation at each stage is done via a state machine (transducer)

* Both pipelines and state machines can be seralised and sent over the wire,
  this enables remote deployments and upgrades

* The remote end will need to deserialise and typecheck the receiving code in
  order to assure that it's compatible with the already deployed code

## Implementation

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
partially an instance of `Cocartesian`, plus some extras. In a "real"
implementation we would want this datatype to be an instance of `Cocartesian`
instance as well as `Cartesian`.)

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
  { newState        :: Ty_
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
typeCheckUpgrade :: forall s a b. (Typeable s, Typeable a, Typeable b)
                 => s -> T s a b -> UpgradeData_ -> Maybe (UpgradeData s a b)
typeCheckUpgrade _s _f (UpgradeData_ s'_ a'_ b'_ f_ g_) =
  case (inferTy s'_, inferTy a'_, inferTy b'_) of
    (ETy (s' :: Ty s'), ETy (a' :: Ty a'), ETy (b' :: Ty b')) -> do
      Refl <- eqT @a @a'
      Refl <- eqT @b @b'
      Refl <- eqT @s @s'
      f <- typeCheck f_
      g <- typeCheck g_ TUnit s' s'
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
run :: (Typeable a, Typeable b)
    => Source a -> Codec (Msg a) (Msg b) -> P a b -> Sink b r -> IO r
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

At this point, let's imagine we want to add a reset feature to our counter.
Reset takes no argument and returns nothing, so we use the unit type in both the
input and output types.

```haskell
type InputV2  = Either () InputV1
type OutputV2 = Either () OutputV1

pattern ReadCountV2 :: InputV2
pattern ReadCountV2  = Left ()

pattern IncrCountV2 :: InputV2
pattern IncrCountV2  = Right (Left ())

pattern ResetCountV2 :: InputV2
pattern ResetCountV2 = Right (Right ())
```

The state machine looks the same, except for the last `Case` where we update the
state to be `0`, thus resetting the counter.

```haskell
counterV2 :: T Int String String
counterV2 =
  Read >>> (Get `Case` (Get >>> Incr >>> Put) `Case` (Int 0 >>> Put)) >>> Show
```

Back in our REPL we can now do the upgrade, by sending over a type `erase`d
version of `counterV2`.

```haskell
let msg :: Msg ()
    msg = Upgrade Nothing "counter"
            (UpgradeData_ UTInt UTInt UTString UTString (erase counterV2) IdU)
nc "127.0.0.1" 3000 msg

nc "127.0.0.1" 3000 (Item Nothing (show ReadCountV2))
nc "127.0.0.1" 3000 (Item Nothing (show ResetCountV2))
nc "127.0.0.1" 3000 (Item Nothing (show ReadCountV2))
```

Which yields the following annotated output.

```
UpgradeSucceeded "counter"
Item "Left 2"              -- The counter's state is preserved by the upgrade.
Item "Right (Right ())"    -- Reset the counter.
Item "Left 0"              -- The value is back to 0.
```

## Discussion and future work

XXX: revisit list from motivation...

Here are a bunch of things I've thought of but not done yet:

1. Notice how the type of `counterV1` and `counterV2` is the same. I think the
   input and output types perhaps need to stay the same, otherwise we wouldn't
   be able to perform the upgrade in the `deploy` function because the types of
   the input and output queues cannot change (that's why we set them both to be
   `String` and made deserialisation and serialisation part of the upgrade, thus
   allowing for changes in the inputs and outputs). I think that the state type
   is different though, and we should be able to change that during an upgrade;
2. To support backwards compatibility we'd need to deploy the new state machine
   next to the old one and choose which to use as part of parsing the client
   request (i.e. if client is on an old version redirect it to the old state
   machine). We'd probably want to also tell the old clients that they should
   upgrade and in some subsequent upgrade we'd want to remove support for the
   old API;
XXX: the above doesn't work, the states of the two state machines will be disjoint...

3. We've seen upgrades of state machines running on top of pipelines, but what
   if we wanted to change the pipelines themselves? This seems tricker. Perhaps
   can start by thinking about what kind of changes one would like to allow,
   e.g. prepending or appending something to a pipeline seems easier than
   changing some part in the middle?
4. The state machine are represented by first-order datatypes, that get
   typechecked and then interpreted. What would upgrades look like if we wanted
   to state machines to be compiled rather than interpreted?
5. Writing state machines and pipelines using combinators is not fun, can we
   have something like Haskell's arrow syntax at the very least? C.f. Conal
   Elliott's [*Compiling to
   categories*](http://conal.net/papers/compiling-to-categories/) and Oleg
   Grenrus'
   [*Overloaded.Categories*](https://hackage.haskell.org/package/overloaded-0.3.1/docs/Overloaded-Categories.html);
6. One advantage with the combinators is that they don't contain variables, so
   it should be easier to do something like Unison does with content-addressed
   hashes?
7. On the pipeline level we might want to support multiple sources
   (`Alternative` instance?), multiple sinks (perhaps via something like `Tee ::
   P a b -> Sink a () -> P a b`?), fanout and sharding as well as making
   everything efficient (as I've written about
   [earlier](https://stevana.github.io/parallel_stream_processing_with_zero-copy_fan-out_and_sharding.html));
8. Finally we also need to figure out how to we build something more
   complicated, like a HTTP-based API, on top of the TCP stuff.

If any of the above sounds interesting, please do feel free to get involved!

## Running the code

The easiest way to get the code running is probably using the [Nix package
manager](https://nixos.org/download).

```bash
git clone https://github.com/stevana/arrow-loop-state-machines.git
cd arrow-loop-state-machines
nix-shell
cabal repl
```

Although using [GHCup](https://www.haskell.org/ghcup/) should work too, if you
replace `nix-shell` with `ghcup install ghc 9.8.1`.

## See also

* [Parallel stream processing with zero-copy fan-out and
  sharding](https://stevana.github.io/parallel_stream_processing_with_zero-copy_fan-out_and_sharding.html);
* [Hot-swapping state
  machines](https://stevana.github.io/hot-code_swapping_a_la_erlang_with_arrow-based_state_machines.html);
* [Application architecture as
  code](https://www.youtube.com/watch?v=vasvpFRPx9c);
* [IxC: Infrastructure as Code, from Code, with
  Code](https://architectelevator.com/cloud/iac-ifc-trends/);
* [Benthos: fancy stream processing made operationally
  mundane](https://www.benthos.dev/);
* Hot-swapping in
  [Elm](https://web.archive.org/web/20131006235603/http://elm-lang.org/blog/Interactive-Programming.elm);
* https://github.com/nmattia/haskell-hot-swap;
* https://github.com/fbsamples/ghc-hotswap/.
