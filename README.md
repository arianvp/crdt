# CRDT

This is an implementation of common state-based CRDTS [?]

We also provide implementations of delta-mutator state based CRDTS for Counters and AWORSs.  As found based on the paper in [?]

Also provided, is a basic anti-entropy algorithm for delta-mutator CRDTs that has eventual-consistency guarentess
but no causal consistency guarentees.

Implementing an algorithm with causal consistency guarentees is out of scope for now, but is explained in the paper [?]

## What is a CRDT?
A CRDT (short for Conflict-free replicated datatype) is a datastructure that supports an operation `join :: a -> a -> a` where `join` is associative, commutative and idempotent. These three attributes together are usually referred to as a _semilattice_ in literature. CRDTs have monotonically increasing state, where clients never observe state rollback. The set of states is partially ordered.

CRDTs can be shared over a network between multiple nodes whilst guarenteeing eventual consistency.
Every node has a local copy of the CRDT, and receives full copies of CRDTs on a regular basis from other nodes and `joins` this with the local copy.  We of course also broadcast our own copy every once in a while. 

Because of the properties of the semilattice, missing a message every once in a while is not a problem. Neither is it a problem if messages are received out of order or duplicated.  As long as the same message gets broadcasted to all other nodes, the nodes will eventually reach a consistent view of the shared state.

Note that we don't have to use locks anywhere. We only have to `join` messages into our own local state and that's it.

One property of a CRDT is that it's monotonically increasing. This means that over time, a CRDT will only grow larger and larger. The problem with this is, that we have to send the _entire_ state in the message to allow for sharing of state. This means that over time, messages will get bigger and bigger and congest the network.

## Delta-mutators
Luckily there is a way to reduce bandwidth in safe manner. And it's based on the observation that 


## Not Implemented

We have not implemented ORSets. They allow multiple removals of elements. (Whilst 2psets don't) but implementing them with delta-mutators whilst maintaining causality is a lot more involved and I simply did not have the time. I might implement this later
