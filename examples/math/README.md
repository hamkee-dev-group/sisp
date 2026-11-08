# SISP Program Collection

**79 programs · 7,750 lines · All executable**

A curated collection of programs written in [SISP](../../README.md) — a symbolic Lisp
with first-class set theory and exact rational arithmetic.  Every program runs
without errors.  Many of them demonstrate computations that are **impossible or
impractical in conventional languages**: operating on infinite sets, performing
lossless rational arithmetic, and treating powersets and Cartesian products as
primitive values.

---

## What Makes These Programs Special

SISP provides three capabilities that no mainstream language offers together:

| Capability | What it means | Which programs show it |
|---|---|---|
| **Infinite sets** | `{tau : (primep tau)}` defines the set of *all* primes — you can test membership, intersect it with other infinite sets, take its complement | 03, 05, 12, 48, 53 |
| **Exact rational arithmetic** | `(+ 1/2 1/3)` → `5/6`, never `0.833333…` — no rounding, no drift, no epsilon comparisons | 21–30, 49, 54 |
| **Native powerset & set algebra** | `(powerset {1 2 3})` → all 8 subsets as a first-class set; `(cartesian-product A B)` → Cartesian product; `(complement S)` → complement of an infinite set | 01, 08, 09, 10, 17, 19, 31, 51 |

---

## Program Index

### Set Theory Fundamentals (01–10)

#### 01 · Powerset Lattice
**File:** `set-theory/powerset-lattice.lsp`

Generates the full powerset P(S) of a set, displays it as a Hasse diagram
grouped by subset cardinality (∅ at the bottom, S at the top), identifies all
cover relations between adjacent levels, and verifies the identity |P(S)| = 2^|S|.

**Key features:** `pow` (powerset), `length` (cardinality), level-wise filtering
of a set-of-sets.

**Why SISP:** The powerset is a single built-in call.  In Python you'd need
`itertools.combinations` for each size; in C++ you'd bit-mask.  Here it's just
`(powerset S)`.

---

#### 02 · Relation Properties
**File:** `set-theory/relation-properties.lsp`

Given a binary relation R on a set S (represented as a set of ordered pairs
like `{(1 . 1) (1 . 2) (2 . 2)}`), checks whether R is reflexive, symmetric,
antisymmetric, and transitive.  Classifies R as an equivalence relation (RST)
or a partial order (RAT).

**Key features:** Relations as native sets of dotted pairs, membership testing
with `in`, Cartesian product for exhaustive pair checking.

**Why SISP:** A relation literally *is* a set of pairs — no wrapper class, no
`HashMap<Pair, Boolean>`.

---

#### 03 · Equivalence Classes
**File:** `set-theory/equivalence-classes.lsp`

Partitions {1,…,9} into congruence classes mod 3, proves that the classes are
pairwise disjoint and their union recovers S, then defines the *infinite*
equivalence classes `{tau : (equal (mod tau 3) 0)}` and tests membership for
arbitrary integers like 99 and 100.

**Key features:** Infinite set comprehension, `cap` to prove disjointness,
`union` to verify coverage, membership on infinite sets.

**Why SISP:** The equivalence class [0]₃ = {0, 3, 6, 9, …} is a *value* you
can bind to a variable and query — no lazy-stream library needed.

> **Impossible elsewhere:** Defining an infinite equivalence class and testing
> `(in 99 class0)` directly.

---

#### 04 · Function Properties
**File:** `set-theory/function-properties.lsp`

Models functions f: A → B as sets of ordered pairs `{(1 . 5) (2 . 4) (3 . 6)}`.
Checks injectivity (distinct outputs), surjectivity (all of B is hit), and
bijectivity.  Computes the inverse of a bijection by swapping pairs.

**Key features:** Functions-as-sets, Cartesian product `prod`, range/domain
cardinality comparison.

**Why SISP:** A function *is* a set of pairs — injectivity means
|range| = |domain|, surjectivity means range = codomain, both checked with
`length` and `equal`.

---

#### 05 · De Morgan's Laws for Sets
**File:** `set-theory/demorgan-sets.lsp`

Proves both De Morgan's Laws — (A ∪ B)ᶜ = Aᶜ ∩ Bᶜ and (A ∩ B)ᶜ = Aᶜ ∪ Bᶜ
— first on finite sets, then on **infinite** comprehension sets (evens,
multiples of 3).  Verifies by testing membership for specific integers.

**Key features:** `complement` (complement of infinite sets), `cap`, `union`, `diff`,
infinite set operations.

**Why SISP:** `(complement evens)` returns the infinite set of odd numbers. You
can intersect it with `(complement mult3)` and test `(in 5 result)`.

> **Impossible elsewhere:** Taking the complement of an infinite set and
> intersecting two complements.

---

#### 06 · Inclusion-Exclusion Principle
**File:** `set-theory/inclusion-exclusion.lsp`

Demonstrates |A ∪ B ∪ C| = |A| + |B| + |C| − |A∩B| − |A∩C| − |B∩C| + |A∩B∩C|
using concrete finite sets.  Verifies the two-set case and the symmetric
difference identity |A △ B| = |A| + |B| − 2|A∩B|.

**Key features:** `length` for cardinality, `cap` for intersection, `symdiff`
(symmetric difference — a built-in).

**Why SISP:** `symdiff` is a primitive set operation.  In most languages you'd
have to implement it manually.

---

#### 07 · Relation Composition & Transitive Closure
**File:** `set-theory/relation-composition.lsp`

Composes two relations R∘S = {(a,c) : ∃b, (a,b)∈R ∧ (b,c)∈S} and then
computes the transitive closure of a directed graph's edge relation by
iterating composition until a fixed point.

**Key features:** Pair-wise composition via `labels` recursion, `merge-unique`
for set union of pairs, fixed-point detection.

**Why SISP:** Relations are lists of native dotted pairs; composition is a
natural recursive operation on them.

---

#### 08 · Set Partitions & Bell Numbers
**File:** `set-theory/set-partition.lsp`

Enumerates partitions of {1,2,3}, verifies partition properties (disjointness
and coverage) using set operations, and computes Bell numbers B(0)–B(5) via
the recurrence B(n+1) = Σ C(n,k)·B(k).

**Key features:** Partition verification with `cap` and `union`, binomial
coefficients with exact arithmetic, powerset cardinality comparison.

**Why SISP:** B(3) = 5 while |P({1,2,3})| = 8 — the powerset is much larger
than the set of valid partitions, and both are computed natively.

---

#### 09 · Boolean Algebra on Powerset
**File:** `set-theory/boolean-algebra.lsp`

Proves that (P(S), ∪, ∩, ᶜ, ∅, S) satisfies all Boolean algebra axioms:
identity, complement, idempotent, commutative, associative, distributive,
absorption, and De Morgan — each verified by computing both sides and checking
`equal`.

**Key features:** Every Boolean law expressed as one-line set operation
equalities.

**Why SISP:** Nine axiom families, each verified in a single `(equal lhs rhs)`
call.  The powerset lattice is a built-in structure.

---

#### 10 · Topological Spaces
**File:** `geometry-topology/topological-space.lsp`

Checks whether a collection of subsets forms a topology on X (closed under
finite ∩ and arbitrary ∪, contains ∅ and X).  Demonstrates the discrete
topology, a valid chain topology, and an invalid topology (missing
{1} ∪ {2} = {1,2}).  Computes the interior of a set.

**Key features:** `subset` for containment, `cap`/`union` for closure checks,
interior as the largest open subset.

**Why SISP:** Topology verification is pure set-theoretic — no special
framework needed.

---

### Infinite Sets & Applied Set Theory (11–20)

#### 11 · Venn Diagram Regions
**File:** `set-theory/venn-diagram.lsp`

Given three finite sets A, B, C, computes all 7 non-empty Venn diagram regions
using `diff` and `cap`:  A-only, B-only, C-only, A∩B-only, A∩C-only,
B∩C-only, and A∩B∩C.  Verifies that the regions partition A∪B∪C.

**Key features:** `diff` chains to isolate exclusive regions, cardinality sums.

---

#### 12 · Infinite Set Operations
**File:** `set-theory/infinite-set-ops.lsp`

The flagship demonstration of SISP's unique power.  Defines infinite sets
(evens, odds, multiples of 3, multiples of 5) and performs union, intersection,
complement, difference, and symmetric difference on them.  Implements
**FizzBuzz via pure set theory**: `(cap mult3 mult5)` is the infinite set of
FizzBuzz numbers.  Also tests Cartesian product of a finite set with an
infinite set.

**Key features:** All six set operations on infinite comprehension sets,
`prod` with infinite operand, membership testing.

> **Impossible elsewhere:** `(in '(1 . 4) (cartesian-product {1 2} evens))` tests whether
> (1,4) is in the infinite Cartesian product {1,2} × Evens.

---

#### 13 · Fixed Point Computation
**File:** `logic/fixed-point.lsp`

Applies the Knaster-Tarski theorem: iterates a monotone set operator F until
F(X) = X.  Two examples: reachability in a directed graph (start from {1},
add neighbors until fixed point) and closure under +2 mod 5 (generates all of
Z₅).

**Key features:** `subset` to verify monotonicity, `equal` for fixed-point
detection, iterated `union`.

---

#### 14 · Formal Concept Analysis (Galois Connection)
**File:** `logic/galois-connection.lsp`

Models a formal context (objects × attributes) for animal classification.
Computes derivation operators σ (objects → common attributes) and τ (attributes
→ objects sharing all of them).  Identifies formal concepts and verifies the
Galois connection property.

**Key features:** `cap` for attribute derivation, `subset` for concept ordering.

---

#### 15 · Greedy Set Cover
**File:** `logic/set-cover.lsp`

Given a universe U and a collection of subsets, finds a near-minimal cover
using the greedy algorithm (pick the subset covering the most uncovered
elements, repeat).  Verifies the final cover using `(equal (cap cover U) U)`.

**Key features:** `diff` to track remaining elements, `cap` + `length` for
coverage measurement.

---

#### 16 · Voting Theory & Condorcet Paradox
**File:** `misc/voting-theory.lsp`

Models voter preferences as sets of ordered pairs.  With 3 voters and 3
candidates in a cyclic preference, demonstrates the Condorcet paradox: no
candidate beats all others pairwise.  Shows that plurality and Borda count also
produce ties.  Connects to Arrow's impossibility theorem.

**Key features:** Majority relation as a set of pairs, `in` for pairwise
comparisons.

---

#### 17 · Probability via Set Theory
**File:** `probability/probability-sets.lsp`

Models dice sample spaces as Cartesian products: Ω = {1,…,6} × {1,…,6}
(36 outcomes).  Defines events as subsets (sum=7, at least one 1, both even).
Computes P(A) = |A|/|Ω| as an **exact rational**.  Verifies the addition rule
P(A∪B) = P(A) + P(B) − P(A∩B) and conditional probability P(A|B).

**Key features:** `prod` for sample space, exact rational probabilities,
set operations on events.

> **Impossible with floats:** P(sum=7) = 6/36 = 1/6 exactly.  Conditional
> probability P(sum=7 | at least one 1) = 2/11 exactly.  No rounding.

---

#### 18 · Relational Database Algebra
**File:** `set-theory/relational-algebra.lsp`

Implements core relational algebra operations — SELECT (filter rows), PROJECT
(extract column), UNION, INTERSECT, DIFFERENCE, JOIN — on tables represented
as lists of tuples.  Joins an employees table with a departments table on
the dept column.

**Key features:** Set operations for UNION/INTERSECT/DIFFERENCE, list
processing for SELECT/PROJECT/JOIN.

**Why SISP:** Relational algebra *is* set theory.  SISP makes this transparent.

---

#### 19 · Finite Automata
**File:** `automata-machines/automata.lsp`

Builds a DFA that accepts strings with an even number of 1's: states as a set,
transition function, acceptance via `(in state accept-states)`.  Simulates on
multiple inputs.  Shows the NFA-to-DFA subset construction: DFA states =
P(NFA states), computed by `(powerset nfa-states)`.

**Key features:** `pow` for subset construction, set membership for acceptance.

**Why SISP:** The subset construction — the core of NFA→DFA conversion — is a
single `(powerset Q)` call.

---

#### 20 · Group Theory (Z₄)
**File:** `algebra/group-theory.lsp`

Studies Z₄ under addition mod 4.  Builds the Cayley table, verifies all four
group axioms (closure, identity, inverses, associativity), identifies the
subgroup H = {0,2}, verifies Lagrange's theorem (|H| divides |G|), computes
cosets 0+H and 1+H, and identifies generators.

**Key features:** Group elements as sets, cosets via set operations, Lagrange's
theorem via cardinality division.

---

### Exact Rational Arithmetic (21–30)

*Every program in this section computes results as exact fractions — not
floating point approximations.  This is impossible in C, Python (without
`fractions`), JavaScript, Java, Go, Rust, etc.*

#### 21 · Egyptian Fractions
**File:** `number-theory/egyptian-fractions.lsp`

Decomposes any fraction p/q into a sum of distinct unit fractions using the
greedy Fibonacci-Sylvester algorithm.  Example: 5/7 = 1/2 + 1/5 + 1/70.
Verifies each decomposition by summing the unit fractions back — the sum is
**exactly** equal to the original.

---

#### 22 · Farey Sequences
**File:** `number-theory/farey-sequence.lsp`

Generates the Farey sequence F_n — all reduced fractions 0 ≤ p/q ≤ 1 with
q ≤ n, in ascending order.  Verifies the mediant property: for consecutive
terms a/b, c/d, we have |bc − ad| = 1.

---

#### 23 · Continued Fractions
**File:** `number-theory/continued-fractions.lsp`

Expands rationals into continued fraction form [a₀; a₁, a₂, …] and
reconstructs the original fraction.  Computes convergents (best rational
approximations).  Shows π ≈ 355/113 and Fibonacci ratios as CF convergents.

---

#### 24 · Harmonic Numbers
**File:** `number-theory/harmonic-numbers.lsp`

Computes exact harmonic numbers H_n = 1 + 1/2 + 1/3 + ⋯ + 1/n.
Example: H₁₀ = 7381/2520.  No rounding at any step.

---

#### 25 · Stern-Brocot Tree
**File:** `number-theory/stern-brocot.lsp`

Builds the Stern-Brocot tree where every positive rational appears exactly
once.  Finds the L/R path to any target fraction.  Mediants are computed with
exact arithmetic.

---

#### 26 · Bernoulli Numbers
**File:** `number-theory/bernoulli-numbers.lsp`

Computes Bernoulli numbers B₀ through B₈ via the recursive formula with
binomial coefficients.  Results: B₀ = 1, B₁ = −1/2, B₂ = 1/6, B₄ = −1/30,
B₆ = 1/42, B₈ = −1/30.  Verifies that odd Bernoulli numbers (except B₁)
are zero.

---

#### 27 · Rational Matrix Arithmetic
**File:** `numerical/rational-matrix.lsp`

Performs 2×2 matrix operations — multiplication, determinant, inverse — with
exact rational entries.  Solves 2×2 linear systems; the solution is an exact
rational vector.

---

#### 28 · Pascal's Triangle
**File:** `combinatorics/pascal-triangle.lsp`

Generates rows of Pascal's triangle.  Verifies Σ C(n,k) = 2ⁿ.  Connects
binomial coefficients to powerset subset counts: |{S ∈ P(X) : |S| = k}| =
C(n,k).

---

#### 29 · π Approximation (Leibniz Series)
**File:** `numerical/pi-approximation.lsp`

Sums the Leibniz series π/4 = 1 − 1/3 + 1/5 − 1/7 + ⋯ as **exact rational
partial sums** (never floating point).  Compares with 355/113 and 22/7.
Demonstrates convergence pattern: odd terms overshoot, even terms undershoot.

---

#### 30 · Catalan Numbers
**File:** `combinatorics/catalan-numbers.lsp`

Computes Catalan numbers C_n = (2n)! / ((n+1)! · n!) using exact division of
large factorials.  Verifies C₀ = 1, C₁ = 1, C₂ = 2, C₃ = 5, C₄ = 14,
C₅ = 42.  The intermediate division produces rationals that simplify to
integers — exact arithmetic ensures no precision loss.

---

### Combinatorics & Logic (31–40)

#### 31 · Combinations via Powerset
**File:** `combinatorics/combinations.lsp`

Generates all k-element subsets of a set by filtering `(powerset S)` by cardinality.
Verifies C(n,k) = |{S ∈ P(X) : |S| = k}|.  The combinatorial definition IS
the algorithm.

---

#### 32 · Derangements
**File:** `combinatorics/derangements.lsp`

Counts derangements (permutations with no fixed points) using the
inclusion-exclusion formula D(n) = n! Σ (−1)ᵏ/k!.  Verifies D(1)=0, D(2)=1,
D(3)=2, D(4)=9, D(5)=44.

---

#### 33 · Bell Numbers
**File:** `combinatorics/bell-numbers.lsp`

Computes Bell numbers via the Bell triangle method.  B(n) counts the number of
partitions of an n-element set.  Verifies B(0)=1 through B(6)=203.

---

#### 34 · Truth Tables
**File:** `logic/truth-tables.lsp`

Generates truth tables for all six logical connectives: AND, OR, NOT, XOR,
⟹ (implication), ⟺ (biconditional).  Proves three tautologies by exhaustive
evaluation: (p ⟹ q) ⟺ (¬p ∨ q), De Morgan's Law, and the Contrapositive.

**Why SISP:** All six connectives are built-in operators (`and`, `or`, `not`,
`xor`, `=>`, `<=>`).

---

#### 35 · Aristotelian Syllogisms
**File:** `logic/syllogisms.lsp`

Translates four classic syllogistic forms into set theory:
- **Barbara:** All M⊆P, All S⊆M ⟹ All S⊆P (subset transitivity)
- **Celarent:** No M∩P, All S⊆M ⟹ No S∩P
- **Darii:** All M⊆P, Some S∩M ⟹ Some S∩P
- **Ferio:** No M∩P, Some S∩M ⟹ Some S∩̄P

---

#### 36 · Permutation Group (S₃)
**File:** `algebra/permutation-group.lsp`

Generates all 6 permutations of {1,2,3}, implements composition, finds
permutation orders (cycle lengths), verifies closure and inverses for the
symmetric group S₃.

---

#### 37 · Sieve of Eratosthenes
**File:** `37-sieve-eratosthenes.lsp`

Implements the sieve using set difference: start with {2,…,n}, remove
multiples via `diff`.  The set-difference approach is natural in SISP.
Also implements trial-division primality and verifies prime counting.

---

#### 38 · Graph Theory
**File:** `38-graph-theory.lsp`

Represents graphs as sets of edges (pairs).  Computes vertex degrees, finds
neighbors, verifies the Handshaking Lemma (sum of degrees = 2|E|), and
implements BFS for path existence.

---

#### 39 · Abstract Algebra (Rings & Fields)
**File:** `algebra/abstract-algebra.lsp`

Studies Z₆ (ring with zero divisors: 2·3 ≡ 0) vs Z₅ (field: every nonzero
element has an inverse).  Identifies zero divisors and units as sets, verifies
they're disjoint.  Connects to Euler's totient.

---

#### 40 · Pigeonhole Principle
**File:** `combinatorics/pigeonhole.lsp`

Assigns 5 pigeons to 3 holes; finds collisions via cardinality.  Verifies the
partition property (holes are disjoint, union = all pigeons).  Uses
`(cartesian-product pigeons holes)` to show the size of all possible assignments.

---

### Advanced Programs (41–55)

#### 41 · Closure-Based Objects
**File:** `lambda/closure-objects.lsp`

Implements mutable objects using closures: a counter (increment/read), a stack
(push/pop), and an accumulator (add/read).  Demonstrates message-passing
OOP style with captured state.

---

#### 42 · Church Numerals
**File:** `lambda/church-numerals.lsp`

Encodes natural numbers as nested cons structures (0 = nil, n = (nil . n−1)).
Implements successor, addition, multiplication, predecessor, subtraction, and
isZero from first principles.  Verifies the distributive law a·(b+c) = a·b + a·c
on Church-encoded values.

---

#### 43 · Y Combinator
**File:** `lambda/y-combinator.lsp`

Demonstrates recursion without named definitions by passing functions to
themselves.  Implements factorial, Fibonacci, and GCD via self-application and
fixed-point drivers — SISP's `labels` is equivalent to the Y combinator.

---

#### 44 · Symbolic Differentiation
**File:** `misc/symbolic-diff.lsp`

Differentiates symbolic expressions algebraically: handles `+`, `*`, `^`,
constants, and variables.  Applies the product rule and power rule.  Includes
simplification (x+0 → x, x·1 → x).  Input and output are S-expressions — no
parsing needed.

---

#### 45 · Functional Sorting
**File:** `sorting/set-based-sorting.lsp`

Implements insertion sort and merge sort in purely functional style — no
mutation, no arrays.  Merge sort splits lists via odd/even alternation.
Verifies sorted output.

---

#### 46 · Graph Coloring
**File:** `46-graph-coloring.lsp`

Analyzes chromatic numbers: K₃ needs 3 colors, bipartite K₂,₂ needs 2, odd
cycle C₅ needs 3, complete K₄ needs 4.  Colors are a set; adjacency is a set
of edges; the constraint is checked via set membership.

---

#### 47 · Matrix Chain Multiplication (DP)
**File:** `dp/matrix-chain.lsp`

Solves the matrix chain multiplication problem using dynamic programming with
memoization via association lists.  Finds the optimal parenthesization cost.

---

#### 48 · Number Theory
**File:** `number-theory/number-theory.lsp`

Computes GCD (Euclid), LCM, Euler's totient φ(n), sum-of-divisors, and
primality.  Defines the **infinite set of numbers coprime to 12**:
`{tau : (and (> tau 0) (= (gcd tau 12) 1))}`.  Defines the **infinite set of
twin primes**: `{tau : (and (primep tau) (primep (+ tau 2)))}`.

> **Impossible elsewhere:** `(in 11 twin-candidates)` → `t`.

---

#### 49 · Cantor Set (Exact Rationals)
**File:** `geometry-topology/cantor-set.lsp`

Constructs the Cantor set by iteratively removing middle thirds of intervals.
After 4 levels: 16 intervals, total length = 16/81.  Every endpoint is an
exact rational — no floating-point rounding at any step.

> **Impossible with floats:** Level 4 length is verified to equal (2/3)⁴ = 16/81
> exactly.  Floating-point arithmetic would introduce rounding errors by level 3.

---

#### 50 · Towers of Hanoi
**File:** `misc/towers-of-hanoi.lsp`

Solves Towers of Hanoi for 3 and 4 disks, printing every move.  Verifies the
total move count equals 2ⁿ − 1.

---

#### 51 · Powerset Counting & P(P(S))
**File:** `set-theory/powerset-counting.lsp`

Verifies |P(S)| = 2^|S|, checks membership of specific subsets in P(S),
explores the subset lattice (meet = ∩, join = ∪), and computes the
**powerset of the powerset**: |P(P({1,2}))| = 2^(2^2) = 16.

---

#### 52 · Peano Arithmetic
**File:** `logic/peano-arithmetic.lsp`

Constructs natural numbers from scratch: 0 = nil, S(n) = (nil . n).
Implements addition, multiplication, predecessor, and comparison purely from
the Peano axioms.  Verifies commutativity, associativity, and distributivity on
Peano-encoded values.

---

#### 53 · Infinite Number-Theoretic Sets
**File:** `set-theory/infinite-number-sets.lsp`

Defines infinite sets by comprehension: **primes**, **perfect squares**,
**triangular numbers**.  Performs set operations on them: Primes ∩ {4k+1}
(the primes ≡ 1 mod 4), Composites = ℕ>1 \ Primes, Squares ∩ Triangulars
(square-triangular numbers).

> **Impossible elsewhere:** `(in 36 (cap squares triangulars))` tests whether
> 36 is both a perfect square and a triangular number — via set intersection
> of two infinite sets.

---

#### 54 · Markov Chain (Exact Probabilities)
**File:** `probability/markov-chain.lsp`

Models a weather Markov chain with three states and rational transition
probabilities (1/2, 1/3, 1/4, 2/5).  Simulates 4 steps from two different
initial states.  At every step, the probability vector sums to **exactly 1** —
no accumulated drift.

> **Impossible with floats:** After 4 matrix-vector multiplications with
> fractions like 1/3 and 2/5, floating-point probabilities would drift from 1.
> Here: `(+ p_sunny p_cloudy p_rainy)` = 1 at every single step.

---

#### 55 · Lambda Calculus Evaluator
**File:** `lambda/lambda-calculus.lsp`

Implements a lambda calculus evaluator in SISP: free-variable analysis,
capture-safe substitution, and beta-reduction.  Demonstrates Church booleans
(TRUE = λx.λy.x, FALSE = λx.λy.y) and the K combinator.  This is
meta-circular — a language writing an evaluator for its own foundations.

#### 56 · Boolean Algebra on {t, nil}
**File:** `set-theory/boolean-algebra-values.lsp`

The value-level companion to 09: proves that ({t, nil}, and, or, not, nil, t)
is the two-element Boolean algebra 2.  Every law — commutative, associative,
identity, domination, idempotent, complement, double negation, distributive,
absorption, and De Morgan — is verified **exhaustively** over all truth
assignments via higher-order `funcall`, so each `t` is a proof by finite
enumeration, not a spot check.

**Why SISP:** strict `and`/`or`/`not` (operands must be `t`/`nil`) make the
connectives a genuine closed algebra, so every law holds as an identity.

#### 57 · Relation Algebra and Closure Operators
**File:** `set-theory/relation-algebra.lsp`

Relations as sets of pairs: domain, range, inverse, composition, and image,
then reachability as a least fixpoint (`lfp`) and full equivalence closure
(reflexive + symmetric + transitive) built from the same operator. Function
and injectivity properties are decided by enumeration, and comprehension sets
are materialized against finite carriers with `enumerate`.

**Why SISP:** the prelude's relation algebra composes with native sets, so
`(transitive-closure r)` is a one-line fixpoint over `union` and
`rel-compose`, and every claim ends in a machine-checked `pass`.

#### 58 · Finite Category Theory
**File:** `algebra/finite-category.lsp`

The divisor poset of 12 as a thin category: identity and composition laws,
then **universal properties decided by exhaustive search** — products are
`gcd`, coproducts are `lcm`, terminal and initial objects found by
quantifying over all objects. Functors appear as monotone maps, a genuine
adjunction (inclusion ⊣ `gcd`) is verified pointwise, left adjoints are shown
to preserve joins, and (ℤ/3, +) is checked as a one-object category with
negation as an endofunctor.

**Why SISP:** in a finite category every categorical statement is decidable,
so `every` over `pairs-such-that` turns definitions from category theory
directly into executable proofs.

#### 59 · The Bourbaki Structure Tower
**File:** `algebra/bourbaki-structures.lsp`

Universal algebra as executable Bourbaki: one generic axiom checker
(closure, associativity, identity, inverses, commutativity) classifies any
finite (carrier, operation) pair up the tower magma → semigroup → monoid →
group → abelian group. ℤ/4 lands differently under +, −, and ×; S₃ is
verified non-abelian; ℤ/6 is a ring but not a field (zero divisors found);
ℤ/5 is a field; kernels of homomorphisms are proven subgroups.

**Why SISP:** a mathematical *structure* is just data plus predicates, so
Bourbaki's structuralist program becomes literally executable.

#### 60 · Finite Topology = Preorders = Modal Logic S4
**File:** `geometry-topology/finite-topology.lsp`

The Alexandrov correspondence computed in all three directions: a preorder
generates its up-set topology (axioms verified over the full powerset), the
specialization order recovers the preorder exactly, and the modal operator □
of the S4 Kripke frame **equals the interior operator** on every one of the
16 propositions. Kuratowski axioms, axioms K/T/4, ◇ = closure, and
continuity = monotonicity, all machine-checked.

**Why SISP:** three fields' definitions run on one relation, and their
equivalence is a finite computation instead of a theorem cited.

#### 61 · Heyting Algebras and Intuitionistic Logic
**File:** `logic/heyting-intuitionistic.lsp`

The opens of a three-point chain form a Heyting algebra whose implication is
found by its defining adjunction (W∧U ≤ V ⟺ W ≤ U⇒V, verified over all
triples). The law of excluded middle **fails on a concrete open** — U ∨ ¬U
computed and shown ≠ ⊤ — double-negation elimination fails, ¬¬¬ collapses
to ¬, and the discrete space is shown to restore Boolean logic.

**Why SISP:** a counterexample to classical logic is *computed*, making
intuitionism a runnable observation rather than a philosophical stance.

#### 62 · Spec(ℤ/8), the Zariski Topology, and Baby EGA
**File:** `geometry-topology/spec-zariski.lsp`

Commutative algebra by brute enumeration: all ideals of ℤ/8 found by
filtering the powerset (they form the divisor chain), prime ideals by
definition, Spec computed, and the **nilradical theorem** — nilpotents =
intersection of all primes — verified. The Chinese Remainder isomorphism
ℤ/6 ≅ ℤ/2 × ℤ/3 is checked element by element, and Spec(ℤ/6) has exactly
the two expected points.

**Why SISP:** Grothendieck's Spec is uncomputable in general but decidable
for finite rings — an honest, machine-checked shadow of EGA.

#### 63 · The Fano Plane PG(2,2)
**File:** `geometry-topology/fano-plane.lsp`

The projective plane over GF(2) built from the seven nonzero vectors of
GF(2)³, its lines generated by vector addition. Every projective axiom is
enumerated: two points determine one line, two lines meet in one point,
3 points per line and 3 lines per point, a quadrilateral in general
position, self-duality, and the Steiner system S(2,3,7). The {1,2,4}
perfect difference set mod 7 reconstructs the same geometry.

**Why SISP:** incidence geometry is sets of sets; the smallest projective
plane fits in seven 3-element sets and every axiom is an `every`.

#### 64 · Simplicial Homology over GF(2)
**File:** `geometry-topology/simplicial-homology.lsp`

Real algebraic topology: boundary matrices over GF(2) with exact integer
Gaussian elimination, ∂∘∂ = 0 verified by matrix multiplication, and Betti
numbers computed for the circle (1,1,0), the tetrahedral sphere (1,0,1),
and the 7-vertex Császár torus on K₇ (1,2,1). The Euler–Poincaré theorem
χ = b₀ − b₁ + b₂ is confirmed on all three, and homology separates the
sphere from the torus.

**Why SISP:** exact mod-2 arithmetic means ranks are theorems, not
floating-point estimates — a hole is detected by a computation.

#### 65 · GF(8) and the Frobenius: Baby Galois Theory
**File:** `algebra/galois-field.lsp`

The field with eight elements constructed as GF(2)[x]/(x³+x+1): field
axioms enumerated, the multiplicative group shown cyclic of order 7 with x
as generator, a⁸ = a for all a. The Frobenius a ↦ a² is verified additive
(the freshman's dream is a theorem in characteristic 2), multiplicative,
and bijective; its **fixed field is exactly GF(2)** and it generates the
cyclic Galois group of order 3 = [GF(8):GF(2)].

**Why SISP:** the fundamental theorem of Galois theory at its smallest
nontrivial instance, with every claim decided by finite enumeration.

#### 66 · An Elliptic Curve over F₁₃ and the Hasse Bound
**File:** `algebra/elliptic-curve.lsp`

E: y² = x³ + x + 1 over F₁₃ has 18 rational points; the Hasse inequality
|N − (p+1)| ≤ 2√p is verified in exact integers as (N−p−1)² ≤ 4p. The
chord-and-tangent law (with Fermat inverses) is then **proven to be a group
structure by exhausting every axiom — associativity over all 5,832
triples** — and Lagrange annihilation N·P = O holds for every point.

**Why SISP:** the group law's associativity, normally a page of algebra or
a leap of faith, becomes a 0.3-second exhaustive computation.

#### 67 · Turing Machines and Busy Beaver BB(2), by Exhaustion
**File:** `automata-machines/turing-machines.lsp`

A Turing machine simulator (sparse assoc-list tape, tail-recursive stepper),
illustrative machines, replays of the BB(2) and BB(3) champions — then the
main event: **all 20,736 two-state two-symbol machines are enumerated and
run**. Result, established by complete search: Σ(2) = 4 ones, S(2) = 6
steps, 9,784 halting machines, and *not one* halts between steps 7 and 20.
An actual busy-beaver value proven, not cited — in under two seconds.

**Why SISP:** genuine tail-call elimination lets 20,736 machines spin in
constant stack, and the enumeration is twelve top-level batches so the
collector reclaims each batch's tapes.

#### 68 · A Model Checker: Peterson's Algorithm, Verified
**File:** `logic/model-checker.lsp`

Exhaustive verification of concurrent protocols: the state space is a set,
the protocol a successor function, reachability a least fixpoint (`lfp`).
Peterson's mutual-exclusion algorithm is **proven** safe over its 20
reachable states — mutual exclusion, deadlock freedom, and the CTL property
AG EF critical (no scheduling ever locks a process out permanently, by
backward reachability). The same checker then *finds real bugs*: a
flags-only lock deadlocks and a naive check-then-set lock puts both
processes in the critical section, each with a concrete counterexample
state.

**Why SISP:** this is TLA⁺-in-miniature built from the relation-algebra
prelude — states are set elements, model checking is `filter` over a
fixpoint, and a race condition becomes a computed object.

#### 69 · Universality: Gödel Numbers, a Universal Simulator, the Diagonal
**File:** `automata-machines/universal-machine.lsp`

Every Turing machine becomes a **single integer** (mixed-radix Gödel
numbering, exact encode/decode round-trip verified), and one fixed function
`utm` runs any machine from its number alone — checked equal to direct
execution across a battery including both busy-beaver champions. The
**padding lemma** (different numbers, same behavior) and the **diagonal
argument** are then constructive: the k-bounded value function is computable,
its diagonal-plus-one differs from every machine on its own code across 400+
indices, and the comment says precisely which step no computation can take —
removing the bound is the halting problem.

**Why SISP:** machines-as-numbers is arithmetic, and exact integers make the
encoding honest — universality without a single hand-wave.

#### 70 · The Metacircular Evaluator Tower
**File:** `lambda/evaluator-tower.lsp`

A McCarthy-style evaluator for a SISP subset, written **twice**: as ordinary
SISP, and as *pure quoted data in the very subset it interprets* — no
`define`, recursion by self-passing, environment extension by
immediately-applied lambdas, `cond` walking itself by rebuilding a smaller
`cond`. The same programs then run at four levels — native, interpreted,
interpreter-inside-the-interpreter, and three deep — and **every level
returns the same answer** (factorial, higher-order application, lexical
capture). Measured cost of each floor printed: ~45 µs → ~0.3 ms → ~14 ms.

**Why SISP:** homoiconicity is the whole trick — the evaluator's source is a
value it can consume, so "a tower of interpreters" is not a metaphor but a
nested list, and each floor is one assertion.

#### 71 · Sorting Networks and the 0–1 Principle (TAOCP Vol 3)
**File:** `sorting/sorting-networks.lsp`

Batcher's odd-even merge networks **generated by the recursive construction**
(sizes 1/5/19 for n = 2/4/8, no memorized constants), proven correct via the
zero-one principle — sorting all 2ⁿ binary inputs suffices — and
cross-checked on all 24 real permutations. Then a theorem by exhaustion:
**S(4) = 5**, because all 1,296 four-comparator networks fail on some binary
input. Knuth's inversion statistic closes it: the average over all 120
permutations of five elements is *exactly* n(n−1)/4 = 5, as an exact
rational.

**Why SISP:** the 0–1 principle turns "sorts everything" into a finite
check, and exact rationals make average-case analysis a computation.

#### 72 · Combinatorial Generation (TAOCP Vol 4A, 7.2.1)
**File:** `combinatorics/combinatorial-generation.lsp`

Three classic generation orders with their defining invariants verified over
the complete output: reflected binary **Gray code** (16 distinct codes, one
bit per step, cyclic closure, and rank recovery 0…15 by pure arithmetic —
no bitwise operators), **plain changes** for permutations (all 24, starting
at the identity, every consecutive pair one adjacent transposition), and
**lexicographic combinations** (C(6,3) = 20, in order, matching the
`k-subsets` set machinery exactly).

**Why SISP:** generation orders are recursive list definitions here — the
reflection in "reflected Gray code" is one `append` of a `reverse`.

#### 73 · Testing Randomness, and the Fall of RANDU (TAOCP Vol 2, 3.3)
**File:** `73-rng-tests.lsp`

Knuth's test battery pointed at SISP's own generator: reproducible seeding,
chi-square frequency (χ² as an exact rational against the 27.88 quantile),
serial pairs on a 4×4 grid, and a runs test. Then the famous demolition:
IBM's RANDU **passes the weak frequency test** yet satisfies the exact
identity x₍ₖ₊₂₎ = 6x₍ₖ₊₁₎ − 9xₖ (mod 2³¹) on **every one of its 998
triples**, while SISP's xoshiro satisfies it zero times — badness proven
as an algebraic identity, not suggested by statistics. The final check is
Knuth's moral itself: weak tests certify nothing.

**Why SISP:** exact integers make the plane structure of RANDU an identity
you can assert, and exact rationals keep χ² free of floating-point excuses.

#### 74 · Huffman Codes, Optimal by Exhaustion (TAOCP Vol 2, 2.3.4.5)
**File:** `74-huffman-optimal.lsp`

The greedy Huffman construction on five weighted symbols, with the prefix
property checked pairwise and the **Kraft equality Σ2^(−ℓ) = 1 verified in
exact rationals**. Then optimality done honestly: all 14 full binary tree
shapes on five leaves (Catalan(4), itself checked) times all 120 leaf
assignments — 1,680 prefix codes enumerated, none cheaper than greedy's 104.

**Why SISP:** "greedy is optimal" is usually an exchange-argument proof;
here the entire code space is small enough to simply *look at all of it*.

#### 75 · Euclid's Algorithm and Lamé's Theorem (TAOCP Vol 2, 4.5.3)
**File:** `number-theory/euclid-lame.lsp`

The oldest nontrivial algorithm, analyzed Knuth's way: over **all 26,796
input pairs up to 233**, the maximum step count is achieved *uniquely* at
the consecutive-Fibonacci pair (233, 144), whose quotient sequence is all
1s — the slowest possible descent. Lamé's five-times-the-digits bound holds
on every pair, extended-Euclid Bézout certificates verify a·x + b·y = gcd
across a 60×60 grid (the Fibonacci pair's coefficients are −55 and 89 —
Fibonaccis themselves), and gcd(F(m), F(n)) = F(gcd(m,n)) over 17×17.

**Why SISP:** worst-case analysis becomes an exhaustive measurement, and
exact integers keep every certificate honest.

#### 76 · Exact Cover and Algorithm X (TAOCP Vol 4B, 7.2.2.1)
**File:** `76-exact-cover.lsp`

Knuth's Algorithm X where the data structure *is* the mathematics: rows are
native sets, covering is disjoint union, secondary columns fall out of the
disjointness filter. The three problems of the Dancing Links paper at small
scale, every count textbook-exact: **Langford pairs** L(3) (2 mirror
solutions, witness verified as a true partition), **domino tilings** of 2×3
(3 tilings from 7 placements), and **N-queens** via primary files/ranks and
secondary diagonals — 2, 10, and 4 solutions for n = 4, 5, 6, with the
witness checked non-attacking on all four constraint families.

**Why SISP:** no dancing links needed — sets give Algorithm X in fifteen
lines, and the search tree is the recursion.

#### 80 · Linear Probing and Parking Functions (TAOCP Vol 3, 6.4)
**File:** `random/linear-probing-parking.lsp`

Linear probing is rendered as cars scanning right from preferred spaces. All
4⁴ preference vectors are generated: simulation agrees exactly with the sorted
criterion b_i ≤ i, precisely 125 vectors succeed, and their average extra
travel is the exact rational 222/125. Exhaustive counts for one through five
cars are 1, 3, 16, 125, 1,296, verifying the classical formula
(n+1)^(n−1) at every size.

**Why SISP:** preference vectors are recursive lists, exhaustive finite proof is
`every`, and average probe displacement stays an exact rational.

#### 81 · Multiple-Precision Arithmetic (TAOCP Vol 2, 4.3)
**File:** `systems/multiprecision-arithmetic.lsp`

Little-endian decimal digit lists implement normalization, comparison, addition
with carry, subtraction with borrow, schoolbook multiplication, and long
division. The digit algorithms reproduce 12,345×6,789 = 83,810,205, survive
long carry/borrow chains, and are cross-checked against native exact arithmetic
on all 64 pairs from an eight-number battery for both multiplication and
quotient/remainder.

**Why SISP:** arbitrary recursive digit structures make the textbook algorithms
literal, while native exact integers remain an independent verification oracle.

#### 82 · Buddy-System Storage Allocation (TAOCP Vol 1, 2.5)
**File:** `systems/buddy-storage-allocation.lsp`

A 64-word heap is modeled as ordered `(start size state id)` blocks. Requests
round to powers of two and recursively split left buddies; release repeatedly
coalesces aligned free pairs. Machine-checked invariants prove complete,
gap-free coverage, power-of-two alignment, correct 5/9/7-word allocation as
8/16/8-word blocks, blocked coalescing while a buddy is occupied, and exact
reconstruction of the original 64-word heap after all releases.

**Why SISP:** the allocator is a persistent state transformation, so every heap
generation remains inspectable and comparable without raw-pointer hazards.

---

## Statistics

| Metric | Value |
|---|---|
| Total programs | 79 |
| Total lines of code | 7,750 |
| Programs using infinite sets | 11 |
| Programs using exact rationals | 16 |
| Programs using powerset | 9 |
| Programs using Cartesian product | 6 |
| Programs with features impossible in C/Python/JS | 19 |

---

## Running the Programs

```bash
# Build SISP (if not already built)
make

# Run any program
echo '(quit 0)' | ./src/sisp examples/math/set-theory/infinite-set-ops.lsp

# Run all programs and verify they work
for f in examples/math/*/*.lsp; do
  echo -n "$(basename $f): "
  echo '(quit 0)' | timeout 15 ./src/sisp "$f" 2>/dev/null | wc -c
done
```

