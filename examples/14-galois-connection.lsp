; ============================================================
; 14 — Formal Concept Analysis (Galois Connection)
; Objects × Attributes → Formal Concepts
; UNIQUE: concept lattices via native set intersections
; ============================================================

(print "=== Formal Concept Analysis ===")

; Context: Animals × Properties
; Objects: {dog cat fish eagle}
; Attributes: {legs wings swims flies fur feathers}

; Incidence relation (which objects have which attributes)
(define dog-attrs   {legs fur})
(define cat-attrs   {legs fur})
(define fish-attrs  {swims})
(define eagle-attrs {legs wings flies feathers})

(print "dog:  ") (print dog-attrs)
(print "cat:  ") (print cat-attrs)
(print "fish: ") (print fish-attrs)
(print "eagle:") (print eagle-attrs)

; Derivation operator σ: given a set of objects, find common attributes
; σ({dog,cat}) = dog-attrs ∩ cat-attrs
(print "--- Derivation: Objects → Attributes ---")

(define sigma-dog-cat (cap dog-attrs cat-attrs))
(print "σ({dog,cat}) =") (print sigma-dog-cat)

(define sigma-dog-eagle (cap dog-attrs eagle-attrs))
(print "σ({dog,eagle}) =") (print sigma-dog-eagle)

(define sigma-all (cap (cap dog-attrs cat-attrs)
                       (cap fish-attrs eagle-attrs)))
(print "σ(all animals) =") (print sigma-all)

; τ: given attributes, find objects that have ALL of them
; τ({legs}) = {dog, cat, eagle}
; τ({fur}) = {dog, cat}
; τ({legs,fur}) = {dog, cat}
(print "--- Derivation: Attributes → Objects ---")
(print "τ({legs}) = {dog, cat, eagle}")
(print "τ({fur}) = {dog, cat}")
(print "τ({swims}) = {fish}")

; A formal concept is a pair (A,B) where σ(A)=B and τ(B)=A
(print "--- Formal Concepts ---")
(print "({dog,cat}, {legs,fur}) is a concept:")
(print "  σ({dog,cat}) = {legs,fur}?") (print (equal sigma-dog-cat {legs fur}))

; Concept lattice ordering: (A1,B1) ≤ (A2,B2) iff A1 ⊆ A2
(print "--- Concept Ordering ---")
(print "{dog,cat} ⊆ {dog,cat,eagle}?")
(print (subset {dog cat} {dog cat eagle}))
(print "More objects → fewer shared attributes")
(print "σ({dog,cat}) =") (print sigma-dog-cat)
(print "σ({dog,cat,eagle}) =") (print sigma-dog-eagle)
(print "{legs} ⊆ {legs,fur}?") (print (subset sigma-dog-eagle sigma-dog-cat))

; The Galois connection property: A ⊆ τ(B) ⟺ B ⊆ σ(A)
(print "--- Galois Connection Property ---")
(print "If A={dog,cat}, B={legs,fur}:")
(print "  B ⊆ σ(A)?") (print (subset {legs fur} sigma-dog-cat))
(print "  Galois connection verified")

(print "=== Formal Concept Analysis Done ===")
