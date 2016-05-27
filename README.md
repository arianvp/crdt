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
Luckily there is a way to reduce bandwidth in safe manner. And it's based on the observation that the delta's of some CRDTs are CRDTs themselves.     Say I have  a Counter `c`.  Which has to keep track of every nodes internal counter value. So basically a `Map Node Int` (See `IncrementOnlyCounter.hs`).   Instead of sending the entire Map, I can send a subset of the Map with only the nodes for which the counter actually changed...  Note that this is also a `Map Node Int`, and hence also a Counter CRDT.  We can join this delta with CRDTs on other systems to propagate the updates, or join it with other deltas on other systems or deltas within the own system.  This way you can send smaller fragments over the network with the same result.

One thing that we do lose is causality.  This is not really a problem with simple counter or an add-only set, but once you're going to have races between adds and removes, causality is really important.    But even this can be mitigated by the means of vector clocks.  I do not have an implementation of this mechanism though. I only implemented delta-mutators for CRDTs where causality is not a problem.  Namely  counters  and Sets where you can only remove an element _once_.


