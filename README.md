Off-chain channel networks are one of the most
promising technologies for dealing with blockchain scalability
and delayed finality issues. Parties that are connected within
such networks can send coins to each other without interacting
with the blockchain. Moreover, these payments can be “routed”
over the network. Thanks to this, even the parties that do not
have a channel in common can perform payments between each
other with the help of intermediaries.
In this paper, we introduce a new notion that we call
Non-Atomic Payment Splitting (NAPS) protocols that allow the
intermediaries in the network to split the payments recursively
into several sub-payments in such a way that the payment can
be successful “partially” (i.e. not all the requested amount may
be transferred). This is in contrast with the existing splitting
techniques that are “atomic” in the sense that they did not
allow such partial payments (we compare the “atomic” and “nonatomic”
approach in the paper). We define NAPS formally, and
then present a protocol, that we call “ETHNA”, that satisfies
this definition. ETHNA is based on very simple and efficient
cryptographic tools, and in particular does not use any expensive
cryptographic primitives. We implement a simple variant of
ETHNA in Solidity and provide some benchmarks. We also report
on some experiments with routing using ETHNA.
