# CRDT

This is an implementation of common state-based CRDTS [?]

We also provide implementations of delta-mutator state based CRDTS for Counters and AWORSs.  As found based on the paper in [?]

Also provided, is a basic anti-entropy algorithm for delta-mutator CRDTs that has eventual-consistency guarentess
but no causal consistency guarentees.

Implementing an algorithm with causal consistency guarentees is out of scope for now, but is explained in the paper [?]
