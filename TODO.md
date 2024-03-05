1. Notice how the type of the inputs and outputs of our counters is always `String`.
`counterV1` and `counterV2` is the same. I think the


The counter does its own
deserialisation and serialisation via `Read` and `Show`, we'll come back to why
in a bit.

XXX:

   input and output types perhaps need to stay the same, otherwise we wouldn't
   be able to perform the upgrade in the `deploy` function because the types of
   the input and output queues cannot change (that's why we set them both to be
   `String` and made deserialisation and serialisation part of the upgrade, thus
   allowing for changes in the inputs and outputs). I think that the state type
   is different though, and we should be able to change that during an upgrade;

2. To support backwards compatibility we'd need to extend the notion of upgrade
   with an input upgrade function (upgrading old inputs to new inputs) and an
   output downgrade function (taking new outputs to old outputs), like we
   discussed in the introduction;
3. For forward compatibility we'd need a way for an old server to ignore the new
   stuff that was added to an input. One way to achieve this could be to define
   a function on types, which annotates the input with extra constructors or
   parameters to existing constructors, etc, then the server could ignore these
   extra annotations. We'd also need default values for anything that is added
   to the outputs, so that the servers old output can be upgraded to the new
   output that the new client expects.

   Alternatively clients can be made to support multiple versions and establish
   which version to use in the initial handshake with the server, this is
   arguably not as satisfying of a solution though;
4. We've seen upgrades of state machines running on top of pipelines, but what
   if we wanted to change the pipelines themselves? This seems trickier. Perhaps
   can start by thinking about what kind of changes one would like to allow,
   e.g. prepending or appending something to a pipeline seems easier than
   changing some part in the middle?
5. The state machine are represented by first-order datatypes, that get
   typechecked and then interpreted. What would upgrades look like if we wanted
   to state machines to be compiled rather than interpreted? For some prior work
   in Haskell see the repos
   [`haskell-hot-swap`](https://github.com/nmattia/haskell-hot-swap) and
   [`ghc-hotswap`](https://github.com/fbsamples/ghc-hotswap/);
6. Writing state machines and pipelines using combinators is not fun, can we
   have something like Haskell's arrow syntax at the very least? C.f. Conal
   Elliott's [*Compiling to
   categories*](http://conal.net/papers/compiling-to-categories/) and Oleg
   Grenrus'
   [*Overloaded.Categories*](https://hackage.haskell.org/package/overloaded-0.3.1/docs/Overloaded-Categories.html);
7. One advantage with the combinators is that they don't contain variables, so
   it should be easier to do something like Unison does with content-addressed
   hashes?
8. On the pipeline level we might want to support multiple sources
   (`Alternative` instance?), multiple sinks (perhaps via something like `Tee ::
   P a b -> Sink a () -> P a b`?), fanout and sharding as well as making
   everything efficient (as I've written about
   [earlier](https://stevana.github.io/parallel_stream_processing_with_zero-copy_fan-out_and_sharding.html));
9. Finally we also need to figure out how to we build something more
   complicated, like a HTTP-based API, on top of the TCP stuff. Scott Wlaschin's
   ["pipeline-oriented
   programming"](https://www.youtube.com/watch?v=ipceTuJlw-M) approach can be
   useful here.
