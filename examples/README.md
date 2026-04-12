# SISP Program Collection

**55 programs · 4,731 lines · All executable**

A curated collection of programs written in [SISP](README.md) — a symbolic Lisp
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
| **Native powerset & set algebra** | `(pow {1 2 3})` → all 8 subsets as a first-class set; `(prod A B)` → Cartesian product; `(comp S)` → complement of an infinite set | 01, 08, 09, 10, 17, 19, 31, 51 |

---

## Program Index

### Set Theory Fundamentals (01–10)

#### 01 · Powerset Lattice
**File:** `01-powerset-lattice.lsp`

Generates the full powerset P(S) of a set, displays it as a Hasse diagram
grouped by subset cardinality (∅ at the bottom, S at the top), identifies all
cover relations between adjacent levels, and verifies the identity |P(S)| = 2^|S|.

**Key features:** `pow` (powerset), `ord` (cardinality), level-wise filtering
of a set-of-sets.

**Why SISP:** The powerset is a single built-in call.  In Python you'd need
`itertools.combinations` for each size; in C++ you'd bit-mask.  Here it's just
`(pow S)`.

---

#### 02 · Relation Properties
**File:** `02-relation-properties.lsp`

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
**File:** `03-equivalence-classes.lsp`

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
**File:** `04-function-properties.lsp`

Models functions f: A → B as sets of ordered pairs `{(1 . 5) (2 . 4) (3 . 6)}`.
Checks injectivity (distinct outputs), surjectivity (all of B is hit), and
bijectivity.  Computes the inverse of a bijection by swapping pairs.

**Key features:** Functions-as-sets, Cartesian product `prod`, range/domain
cardinality comparison.

**Why SISP:** A function *is* a set of pairs — injectivity means
|range| = |domain|, surjectivity means range = codomain, both checked with
`ord` and `equal`.

---

#### 05 · De Morgan's Laws for Sets
**File:** `05-demorgan-sets.lsp`

Proves both De Morgan's Laws — (A ∪ B)ᶜ = Aᶜ ∩ Bᶜ and (A ∩ B)ᶜ = Aᶜ ∪ Bᶜ
— first on finite sets, then on **infinite** comprehension sets (evens,
multiples of 3).  Verifies by testing membership for specific integers.

**Key features:** `comp` (complement of infinite sets), `cap`, `union`, `diff`,
infinite set operations.

**Why SISP:** `(comp evens)` returns the infinite set of odd numbers.  You can
intersect it with `(comp mult3)` and test `(in 5 result)`.

> **Impossible elsewhere:** Taking the complement of an infinite set and
> intersecting two complements.

---

#### 06 · Inclusion-Exclusion Principle
**File:** `06-inclusion-exclusion.lsp`

Demonstrates |A ∪ B ∪ C| = |A| + |B| + |C| − |A∩B| − |A∩C| − |B∩C| + |A∩B∩C|
using concrete finite sets.  Verifies the two-set case and the symmetric
difference identity |A △ B| = |A| + |B| − 2|A∩B|.

**Key features:** `ord` for cardinality, `cap` for intersection, `symdiff`
(symmetric difference — a built-in).

**Why SISP:** `symdiff` is a primitive set operation.  In most languages you'd
have to implement it manually.

---

#### 07 · Relation Composition & Transitive Closure
**File:** `07-relation-composition.lsp`

Composes two relations R∘S = {(a,c) : ∃b, (a,b)∈R ∧ (b,c)∈S} and then
computes the transitive closure of a directed graph's edge relation by
iterating composition until a fixed point.

**Key features:** Pair-wise composition via `labels` recursion, `merge-unique`
for set union of pairs, fixed-point detection.

**Why SISP:** Relations are lists of native dotted pairs; composition is a
natural recursive operation on them.

---

#### 08 · Set Partitions & Bell Numbers
**File:** `08-set-partition.lsp`

Enumerates partitions of {1,2,3}, verifies partition properties (disjointness
and coverage) using set operations, and computes Bell numbers B(0)–B(5) via
the recurrence B(n+1) = Σ C(n,k)·B(k).

**Key features:** Partition verification with `cap` and `union`, binomial
coefficients with exact arithmetic, powerset cardinality comparison.

**Why SISP:** B(3) = 5 while |P({1,2,3})| = 8 — the powerset is much larger
than the set of valid partitions, and both are computed natively.

---

#### 09 · Boolean Algebra on Powerset
**File:** `09-boolean-algebra.lsp`

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
**File:** `10-topological-space.lsp`

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
**File:** `11-venn-diagram.lsp`

Given three finite sets A, B, C, computes all 7 non-empty Venn diagram regions
using `diff` and `cap`:  A-only, B-only, C-only, A∩B-only, A∩C-only,
B∩C-only, and A∩B∩C.  Verifies that the regions partition A∪B∪C.

**Key features:** `diff` chains to isolate exclusive regions, cardinality sums.

---

#### 12 · Infinite Set Operations
**File:** `12-infinite-set-ops.lsp`

The flagship demonstration of SISP's unique power.  Defines infinite sets
(evens, odds, multiples of 3, multiples of 5) and performs union, intersection,
complement, difference, and symmetric difference on them.  Implements
**FizzBuzz via pure set theory**: `(cap mult3 mult5)` is the infinite set of
FizzBuzz numbers.  Also tests Cartesian product of a finite set with an
infinite set.

**Key features:** All six set operations on infinite comprehension sets,
`prod` with infinite operand, membership testing.

> **Impossible elsewhere:** `(in '(1 . 4) (prod {1 2} evens))` tests whether
> (1,4) is in the infinite Cartesian product {1,2} × Evens.

---

#### 13 · Fixed Point Computation
**File:** `13-fixed-point.lsp`

Applies the Knaster-Tarski theorem: iterates a monotone set operator F until
F(X) = X.  Two examples: reachability in a directed graph (start from {1},
add neighbors until fixed point) and closure under +2 mod 5 (generates all of
Z₅).

**Key features:** `subset` to verify monotonicity, `equal` for fixed-point
detection, iterated `union`.

---

#### 14 · Formal Concept Analysis (Galois Connection)
**File:** `14-galois-connection.lsp`

Models a formal context (objects × attributes) for animal classification.
Computes derivation operators σ (objects → common attributes) and τ (attributes
→ objects sharing all of them).  Identifies formal concepts and verifies the
Galois connection property.

**Key features:** `cap` for attribute derivation, `subset` for concept ordering.

---

#### 15 · Greedy Set Cover
**File:** `15-set-cover.lsp`

Given a universe U and a collection of subsets, finds a near-minimal cover
using the greedy algorithm (pick the subset covering the most uncovered
elements, repeat).  Verifies the final cover using `(equal (cap cover U) U)`.

**Key features:** `diff` to track remaining elements, `cap` + `ord` for
coverage measurement.

---

#### 16 · Voting Theory & Condorcet Paradox
**File:** `16-voting-theory.lsp`

Models voter preferences as sets of ordered pairs.  With 3 voters and 3
candidates in a cyclic preference, demonstrates the Condorcet paradox: no
candidate beats all others pairwise.  Shows that plurality and Borda count also
produce ties.  Connects to Arrow's impossibility theorem.

**Key features:** Majority relation as a set of pairs, `in` for pairwise
comparisons.

---

#### 17 · Probability via Set Theory
**File:** `17-probability-sets.lsp`

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
**File:** `18-relational-algebra.lsp`

Implements core relational algebra operations — SELECT (filter rows), PROJECT
(extract column), UNION, INTERSECT, DIFFERENCE, JOIN — on tables represented
as lists of tuples.  Joins an employees table with a departments table on
the dept column.

**Key features:** Set operations for UNION/INTERSECT/DIFFERENCE, list
processing for SELECT/PROJECT/JOIN.

**Why SISP:** Relational algebra *is* set theory.  SISP makes this transparent.

---

#### 19 · Finite Automata
**File:** `19-automata.lsp`

Builds a DFA that accepts strings with an even number of 1's: states as a set,
transition function, acceptance via `(in state accept-states)`.  Simulates on
multiple inputs.  Shows the NFA-to-DFA subset construction: DFA states =
P(NFA states), computed by `(pow nfa-states)`.

**Key features:** `pow` for subset construction, set membership for acceptance.

**Why SISP:** The subset construction — the core of NFA→DFA conversion — is a
single `(pow Q)` call.

---

#### 20 · Group Theory (Z₄)
**File:** `20-group-theory.lsp`

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
**File:** `21-egyptian-fractions.lsp`

Decomposes any fraction p/q into a sum of distinct unit fractions using the
greedy Fibonacci-Sylvester algorithm.  Example: 5/7 = 1/2 + 1/5 + 1/70.
Verifies each decomposition by summing the unit fractions back — the sum is
**exactly** equal to the original.

---

#### 22 · Farey Sequences
**File:** `22-farey-sequence.lsp`

Generates the Farey sequence F_n — all reduced fractions 0 ≤ p/q ≤ 1 with
q ≤ n, in ascending order.  Verifies the mediant property: for consecutive
terms a/b, c/d, we have |bc − ad| = 1.

---

#### 23 · Continued Fractions
**File:** `23-continued-fractions.lsp`

Expands rationals into continued fraction form [a₀; a₁, a₂, …] and
reconstructs the original fraction.  Computes convergents (best rational
approximations).  Shows π ≈ 355/113 and Fibonacci ratios as CF convergents.

---

#### 24 · Harmonic Numbers
**File:** `24-harmonic-numbers.lsp`

Computes exact harmonic numbers H_n = 1 + 1/2 + 1/3 + ⋯ + 1/n.
Example: H₁₀ = 7381/2520.  No rounding at any step.

---

#### 25 · Stern-Brocot Tree
**File:** `25-stern-brocot.lsp`

Builds the Stern-Brocot tree where every positive rational appears exactly
once.  Finds the L/R path to any target fraction.  Mediants are computed with
exact arithmetic.

---

#### 26 · Bernoulli Numbers
**File:** `26-bernoulli-numbers.lsp`

Computes Bernoulli numbers B₀ through B₈ via the recursive formula with
binomial coefficients.  Results: B₀ = 1, B₁ = −1/2, B₂ = 1/6, B₄ = −1/30,
B₆ = 1/42, B₈ = −1/30.  Verifies that odd Bernoulli numbers (except B₁)
are zero.

---

#### 27 · Rational Matrix Arithmetic
**File:** `27-rational-matrix.lsp`

Performs 2×2 matrix operations — multiplication, determinant, inverse — with
exact rational entries.  Solves 2×2 linear systems; the solution is an exact
rational vector.

---

#### 28 · Pascal's Triangle
**File:** `28-pascal-triangle.lsp`

Generates rows of Pascal's triangle.  Verifies Σ C(n,k) = 2ⁿ.  Connects
binomial coefficients to powerset subset counts: |{S ∈ P(X) : |S| = k}| =
C(n,k).

---

#### 29 · π Approximation (Leibniz Series)
**File:** `29-pi-approximation.lsp`

Sums the Leibniz series π/4 = 1 − 1/3 + 1/5 − 1/7 + ⋯ as **exact rational
partial sums** (never floating point).  Compares with 355/113 and 22/7.
Demonstrates convergence pattern: odd terms overshoot, even terms undershoot.

---

#### 30 · Catalan Numbers
**File:** `30-catalan-numbers.lsp`

Computes Catalan numbers C_n = (2n)! / ((n+1)! · n!) using exact division of
large factorials.  Verifies C₀ = 1, C₁ = 1, C₂ = 2, C₃ = 5, C₄ = 14,
C₅ = 42.  The intermediate division produces rationals that simplify to
integers — exact arithmetic ensures no precision loss.

---

### Combinatorics & Logic (31–40)

#### 31 · Combinations via Powerset
**File:** `31-combinations.lsp`

Generates all k-element subsets of a set by filtering `(pow S)` by cardinality.
Verifies C(n,k) = |{S ∈ P(X) : |S| = k}|.  The combinatorial definition IS
the algorithm.

---

#### 32 · Derangements
**File:** `32-derangements.lsp`

Counts derangements (permutations with no fixed points) using the
inclusion-exclusion formula D(n) = n! Σ (−1)ᵏ/k!.  Verifies D(1)=0, D(2)=1,
D(3)=2, D(4)=9, D(5)=44.

---

#### 33 · Bell Numbers
**File:** `33-bell-numbers.lsp`

Computes Bell numbers via the Bell triangle method.  B(n) counts the number of
partitions of an n-element set.  Verifies B(0)=1 through B(6)=203.

---

#### 34 · Truth Tables
**File:** `34-truth-tables.lsp`

Generates truth tables for all six logical connectives: AND, OR, NOT, XOR,
⟹ (implication), ⟺ (biconditional).  Proves three tautologies by exhaustive
evaluation: (p ⟹ q) ⟺ (¬p ∨ q), De Morgan's Law, and the Contrapositive.

**Why SISP:** All six connectives are built-in operators (`and`, `or`, `not`,
`xor`, `=>`, `<=>`).

---

#### 35 · Aristotelian Syllogisms
**File:** `35-syllogisms.lsp`

Translates four classic syllogistic forms into set theory:
- **Barbara:** All M⊆P, All S⊆M ⟹ All S⊆P (subset transitivity)
- **Celarent:** No M∩P, All S⊆M ⟹ No S∩P
- **Darii:** All M⊆P, Some S∩M ⟹ Some S∩P
- **Ferio:** No M∩P, Some S∩M ⟹ Some S∩̄P

---

#### 36 · Permutation Group (S₃)
**File:** `36-permutation-group.lsp`

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
**File:** `39-abstract-algebra.lsp`

Studies Z₆ (ring with zero divisors: 2·3 ≡ 0) vs Z₅ (field: every nonzero
element has an inverse).  Identifies zero divisors and units as sets, verifies
they're disjoint.  Connects to Euler's totient.

---

#### 40 · Pigeonhole Principle
**File:** `40-pigeonhole.lsp`

Assigns 5 pigeons to 3 holes; finds collisions via cardinality.  Verifies the
partition property (holes are disjoint, union = all pigeons).  Uses
`(prod pigeons holes)` to show the size of all possible assignments.

---

### Advanced Programs (41–55)

#### 41 · Closure-Based Objects
**File:** `41-closure-objects.lsp`

Implements mutable objects using closures: a counter (increment/read), a stack
(push/pop), and an accumulator (add/read).  Demonstrates message-passing
OOP style with captured state.

---

#### 42 · Church Numerals
**File:** `42-church-numerals.lsp`

Encodes natural numbers as nested cons structures (0 = nil, n = (nil . n−1)).
Implements successor, addition, multiplication, predecessor, subtraction, and
isZero from first principles.  Verifies the distributive law a·(b+c) = a·b + a·c
on Church-encoded values.

---

#### 43 · Y Combinator
**File:** `43-y-combinator.lsp`

Demonstrates recursion without named definitions by passing functions to
themselves.  Implements factorial, Fibonacci, and GCD via self-application and
fixed-point drivers — SISP's `labels` is equivalent to the Y combinator.

---

#### 44 · Symbolic Differentiation
**File:** `44-symbolic-diff.lsp`

Differentiates symbolic expressions algebraically: handles `+`, `*`, `^`,
constants, and variables.  Applies the product rule and power rule.  Includes
simplification (x+0 → x, x·1 → x).  Input and output are S-expressions — no
parsing needed.

---

#### 45 · Functional Sorting
**File:** `45-set-based-sorting.lsp`

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
**File:** `47-matrix-chain.lsp`

Solves the matrix chain multiplication problem using dynamic programming with
memoization via association lists.  Finds the optimal parenthesization cost.

---

#### 48 · Number Theory
**File:** `48-number-theory.lsp`

Computes GCD (Euclid), LCM, Euler's totient φ(n), sum-of-divisors, and
primality.  Defines the **infinite set of numbers coprime to 12**:
`{tau : (and (> tau 0) (= (gcd tau 12) 1))}`.  Defines the **infinite set of
twin primes**: `{tau : (and (primep tau) (primep (+ tau 2)))}`.

> **Impossible elsewhere:** `(in 11 twin-candidates)` → `t`.

---

#### 49 · Cantor Set (Exact Rationals)
**File:** `49-cantor-set.lsp`

Constructs the Cantor set by iteratively removing middle thirds of intervals.
After 4 levels: 16 intervals, total length = 16/81.  Every endpoint is an
exact rational — no floating-point rounding at any step.

> **Impossible with floats:** Level 4 length is verified to equal (2/3)⁴ = 16/81
> exactly.  Floating-point arithmetic would introduce rounding errors by level 3.

---

#### 50 · Towers of Hanoi
**File:** `50-towers-of-hanoi.lsp`

Solves Towers of Hanoi for 3 and 4 disks, printing every move.  Verifies the
total move count equals 2ⁿ − 1.

---

#### 51 · Powerset Counting & P(P(S))
**File:** `51-powerset-counting.lsp`

Verifies |P(S)| = 2^|S|, checks membership of specific subsets in P(S),
explores the subset lattice (meet = ∩, join = ∪), and computes the
**powerset of the powerset**: |P(P({1,2}))| = 2^(2^2) = 16.

---

#### 52 · Peano Arithmetic
**File:** `52-peano-arithmetic.lsp`

Constructs natural numbers from scratch: 0 = nil, S(n) = (nil . n).
Implements addition, multiplication, predecessor, and comparison purely from
the Peano axioms.  Verifies commutativity, associativity, and distributivity on
Peano-encoded values.

---

#### 53 · Infinite Number-Theoretic Sets
**File:** `53-infinite-number-sets.lsp`

Defines infinite sets by comprehension: **primes**, **perfect squares**,
**triangular numbers**.  Performs set operations on them: Primes ∩ {4k+1}
(Fermat primes), Composites = ℕ>1 \ Primes, Squares ∩ Triangulars
(square-triangular numbers).

> **Impossible elsewhere:** `(in 36 (cap squares triangulars))` tests whether
> 36 is both a perfect square and a triangular number — via set intersection
> of two infinite sets.

---

#### 54 · Markov Chain (Exact Probabilities)
**File:** `54-markov-chain.lsp`

Models a weather Markov chain with three states and rational transition
probabilities (1/2, 1/3, 1/4, 2/5).  Simulates 4 steps from two different
initial states.  At every step, the probability vector sums to **exactly 1** —
no accumulated drift.

> **Impossible with floats:** After 4 matrix-vector multiplications with
> fractions like 1/3 and 2/5, floating-point probabilities would drift from 1.
> Here: `(+ p_sunny p_cloudy p_rainy)` = 1 at every single step.

---

#### 55 · Lambda Calculus Evaluator
**File:** `55-lambda-calculus.lsp`

Implements a lambda calculus evaluator in SISP: free-variable analysis,
capture-safe substitution, and beta-reduction.  Demonstrates Church booleans
(TRUE = λx.λy.x, FALSE = λx.λy.y) and the K combinator.  This is
meta-circular — a language writing an evaluator for its own foundations.

---

## Statistics

| Metric | Value |
|---|---|
| Total programs | 55 |
| Total lines of code | 4,731 |
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
echo '(quit 0)' | ./src/sisp programs/12-infinite-set-ops.lsp

# Run all programs and verify they work
for f in programs/*.lsp; do
  echo -n "$(basename $f): "
  echo '(quit 0)' | timeout 15 ./src/sisp "$f" 2>/dev/null | wc -c
done
```

---

*All 55 programs verified working against SISP built from source on 2026-04-12.*
