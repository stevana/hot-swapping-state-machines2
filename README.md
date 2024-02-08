# State machines as `ArrowLoop`s and hot code upgrades with zero downtime

*Work in progress, please don't share, but do feel free to get involved!*

In this post I'd like to show how one can represent state machines as
`ArrowLoop` instances and what consequences that has with respect to performing
hot code upgrades of said state machines withou without any downtime.

This post builds upon ideas form two previous posts, but can be read on its own.

* https://stevana.github.io/parallel_stream_processing_with_zero-copy_fan-out_and_sharding.html
* https://stevana.github.io/hot-code_swapping_a_la_erlang_with_arrow-based_state_machines.html

## Motivation

* Deployed software needs to be upgraded
* No downtime
* Typed state migrations
* Backwards and forwards compatbility?

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

### Arrow loop

### Pipelines

### Local (typed) upgrades

### Untyped state machines

### Type checking and inference

### Remote upgrades

## A bit of theory

* Upgrades of from arbitrary program to arbitrary program is too messy

* Abstract state machine, model of computation that allows us to express the
  problem at the right level of abstration

* Refinement of state machines gives us a model of updates, perhaps even clearer
  when looking at indexed containers and their morphisms?

## Future work

- [ ] Upgrade pipelines, rather than state machines running in the pipelines;
- [ ] Better language for describing state machines?


## Contributing

```bash
git clone https://github.com/stevana/arrow-loop-state-machines.git
cd arrow-loop-state-machines
nix-shell
cabal repl
```

## See also

* [Application architecture as
  code](https://www.youtube.com/watch?v=vasvpFRPx9c)
* [IxC: Infrastructure as Code, from Code, with
  Code](https://architectelevator.com/cloud/iac-ifc-trends/)
* [Benthos: fancy stream processing made operationally
  mundane](https://www.benthos.dev/)
