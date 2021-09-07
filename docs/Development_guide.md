CompaREST
=========
The tool works by simultaneously looking at a pair of nodes in OpenAPI schema
trees. At every place we try to understand whether one side (the producer) can
produce something that the other side (the consumer) cannot consume. This often
involves recursively running the same check on child sub-nodes. The necessary
criteria are implemented manually for all types of nodes that appear in the
schema. The `Subtree` class in `Data.OpenApi.Compare.Subtree` designates types
of nodes for which this compatibility checking is defined. The implementations
for various types reside in `Data.OpenApi.Compare.Validate.*`.

Producer/Consumer
-----------------
Rather than looking at which schema is the server's and which is the client's,
in terms of compatibility checking it is more useful to track which side is the
"producer" and which is the "consumer". At the root of the schema the producer
is the client, but as we descend into an HTTP response, the direction flips and
the producer becomes the server.

We deal with a lot of things in pairs, where one thing belongs to the producer
and another belongs to the consumer. The `ProdCons` datatype (in
`Data.OpenApi.Compare.Subtree`) is just a pair type that provides an Applicative
abstraction for working with two things simultaneously.

Trees
-----
We directly use the datatypes defined in the `openapi3` library. For reasons of
identifiability and memoization we tag all nodes in the trees with a path from
the root. The tree is heterogeneous (nodes have different types), and every type
of node has a different arrangement of children. There is a `Step` data family
that defines all the possible children (of a specific type) of a node (of a
specific type).

A `Trace` is a sequence of steps, except in a path the types of adjacent steps
have to agree. The `Paths` datatype achieves this. Together all paths form a
`Category`, and we often concatenate them using `>>>`. This is defined in
`Data.OpenApi.Compare.Paths`.

We often keep a node and a path to it next to each other using an environment
comonad. `Traced` is a type alias around `Env` which ensures that the type of
the path matches the type of the node.

Issues
------
The output of the tool is a list of compatibility issues that came up during
checking. The `Issue` data family describes the kinds of issues that can occur
at each type of node. An issue is supposed to characterize a set of interactions
(requests or responses) that demonstrate that the schemas are incompatible.

Each issue is tagged with a `Behavior` that describes the path to reproducing
the issue. While a `Trace` describes a syntactic path down the schema tree, a
`Behavior` describes which part of an interaction needs to be focused on for the
issue to manifest itself.

In most places where we keep multiple issues (e.g. the output), to store them we
use a prefix tree map, indexed by both types and values of `Behavior`s. See
`Data.OpenApi.Compare.PathsPrefixTree`.

Report
------
The checker ultimately outputs a list of `Issue`s, together with `Behavior`s
that cause them. We run the checker twice: forwards and backwards -- to detect
non-breaking changes. All of this is then compiled into a report in
`Data.OpenApi.Compare.Report`. The tree structure of headings is computed from
`Behavior`s, using `Jet`s to collapse particular sequences of behavior steps
into a single human-readable header item.  The text of each paragraph comes from
`describeIssue`.

We then use the `pandoc` library to render the report in a variety of formats.

Formulas
--------
The schema has references, so it can end up being recursive. Similarly the
process for establishing compatibility can end up being recursive. To keep track
of this, the checker actually manipulates *formulas* which can have variables in
them, defined in (`Data.OpenApi.Compare.Formula`).

A formula represents a set of issues, with the empty set corresponding to a
successful compatibility check. A formula can have conjunctions, where we only
report success if all parts succeeded, and if not we take the union of all
issues. A formula can also have disjunctions, but there it's impossible to
guarantee that the issues for the parts also make sense as issues for the whole
thing. Due to this if all disjuncted parts have issues we instead return a
different issue (usually non-descriptive).

The `FormulaF` datatype implements a functor that carries formulaic calculations
as well as a result value (a co-Yoneda extension of formulas for the most part).
The Applicative interface provides conjunction, and the Alternative interface
provides disjunction.

When we detect recursion we introduce a variable, compute the compatibility
check as a formula with that variable, and then we solve a fixed point equation
(`maxFixpoint`) with the variable to obtain the answer.

This `FomulaF` is further wrapped in a couple monad transformers
(`SemanticCompatFormula`, `StructuralCompatFormula`), and the whole checker is
then implemented in the resulting Applicative using ApplicativeDo.

Memoization
-----------
The `Data.OpenApi.Compare.Memo` module contains utilities for stateful
memoization and recursion detection. The entire checking process is memoized,
and can detect, propagate, and resolve recursion.

Structural/Semantic Compatibility
---------------------------------
When we encounter two nodes we first try to optimistically establish whether
they're structurally equal, i.e. the trees are exactly the same modulo reference
inlining. This check is also memoized and recursion-aware. If the check succeeds
we conclude that the schemas are compatible. Otherwise we fall back to the
"normal" semantic compatibility.


JSON Schema
-----------
The language that describes JSON payloads is probably the richest part of
OpenAPI schemas, and the checker for it also comes with a lot of moving parts.

JSON schema allows arbitrary set arithmetic using `allOf`, `anyOf`, `not` and
`oneOf`. To fully check `not` and `oneOf` we would need to compare set
subtraction, which involves negating a comparison result. If recursion is
involved we would need to be able to solve equations with negation, which we
cannot do. So `not` is unsupported altogether, and `oneOf` is only supported
when it has disjoint branches and behaves like `anyOf`.

With that in mind a JSON schema is first pre-processed into a Disjunctive Normal
Form (`DNF` in `Data.OpenApi.Compare.Validate.Schema.DNF`). This pre-processing
happens in `Data.OpenApi.Compare.Validate.Schema.Process`.

The DNF contains elementary clauses that match on a specific aspect of a JSON
object (`Condition`). Most clauses only affect objects of a single type, so JSON
values and clauses are segregated by type (`JsonType`, `TypedValue` in
`Data.OpenApi.Compare.Validate.Schema.JsonFormula`)

Then we reduce the problem of comparing two DNF's to the comparison of a
conjunction of clauses in the producer with a single clause in the consumer
(`checkFormulas` in `Data.OpenApi.Compare.Validate.Schema`).

When we do disjunction in the formula we lose information, so we try to avoid
having disjunctions in the consumer DNF. This is done by the means of
partitioning. We look for factors that would let us partition the set of all
objects into several bins, in a way that hopefully corresponds only a single
consumer to each producer. This is implemented in
`Data.OpenApi.Compare.Validate.Schema.Partition`. Currently we can only
partition by an `enum` value that is accessible via a chain of `required`
fields.

The same partitioning mechanism is used to test whether the branches of a
`oneOf` are disjoint, and emit a warning otherwise.
