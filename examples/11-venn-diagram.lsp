; ============================================================
; Program 11: Venn Diagram Region Enumeration
; Given three sets A, B, C, compute all 7 non-empty regions
; of a 3-set Venn diagram using set difference, intersection,
; and union.
; ============================================================

(print "=== Venn Diagram Regions for Three Sets ===")

; Define three overlapping sets
(define A {1 2 3 4 5 6 7})
(define B {4 5 6 7 8 9 10})
(define C {6 7 8 9 10 11 12 13})

(print "Set A:")
(print A)
(print "Set B:")
(print B)
(print "Set C:")
(print C)

(print "--- Computing all 7 Venn diagram regions ---")

; Region 1: A only = A \ (B union C)
(define A-only (diff A (union B C)))
(print "Region: A only (in A but not B or C):")
(print A-only)

; Region 2: B only = B \ (A union C)
; B \ (A union C) = {4..10} \ {1..13} -> need elements in B not in A or C
; B={4,5,6,7,8,9,10}, A union C = {1..13} minus some...
; Actually B-only should be empty since every element of B is in A or C
(define B-only (diff B (union A C)))
(print "Region: B only (in B but not A or C):")
(print B-only)

; Region 3: C only = C \ (A union B)
(define C-only (diff C (union A B)))
(print "Region: C only (in C but not A or B):")
(print C-only)

; Region 4: A cap B only = (A cap B) \ C
(define AB-only (diff (cap A B) C))
(print "Region: A cap B only (in A and B but not C):")
(print AB-only)

; Region 5: A cap C only = (A cap C) \ B
(define AC-only (diff (cap A C) B))
(print "Region: A cap C only (in A and C but not B):")
(print AC-only)

; Region 6: B cap C only = (B cap C) \ A
(define BC-only (diff (cap B C) A))
(print "Region: B cap C only (in B and C but not A):")
(print BC-only)

; Region 7: A cap B cap C
(define ABC (cap A (cap B C)))
(print "Region: A cap B cap C (in all three):")
(print ABC)

; Verify: every element in A union B union C lands in exactly one region
(print "--- Verification: membership in exactly one region ---")

; Helper: count how many regions an element belongs to
(define (count-regions x)
  (define c 0)
  (if (in x A-only) (define c (+ c 1)))
  (if (in x B-only) (define c (+ c 1)))
  (if (in x C-only) (define c (+ c 1)))
  (if (in x AB-only) (define c (+ c 1)))
  (if (in x AC-only) (define c (+ c 1)))
  (if (in x BC-only) (define c (+ c 1)))
  (if (in x ABC) (define c (+ c 1)))
  c)

; Test each element of the universe {1..13}
(define (verify-all lst)
  (if (null lst) t
    (let ((x (car lst)))
      (let ((c (count-regions x)))
        (if (equal c 1)
            (verify-all (cdr lst))
            (progn (print "FAIL: element in wrong number of regions")
                   (print x)
                   nil))))))

(if (verify-all (seq 1 13))
    (print "VERIFIED: Each element is in exactly one region")
    (print "ERROR: Some element is in zero or multiple regions"))

; Verify: elements outside A union B union C are in no region
(if (equal (count-regions 0) 0)
    (print "VERIFIED: Element 0 (outside all sets) is in no region")
    (print "ERROR: Element 0 found in some region"))
(if (equal (count-regions 99) 0)
    (print "VERIFIED: Element 99 (outside all sets) is in no region")
    (print "ERROR: Element 99 found in some region"))

; Display region cardinalities
(print "--- Region cardinalities ---")
(print "A only:")
(print (ord A-only))
(print "AB only:")
(print (ord AB-only))
(print "BC only:")
(print (ord BC-only))
(print "C only:")
(print (ord C-only))
(print "A cap B cap C:")
(print (ord ABC))

(print "=== Venn Diagram Complete ===")
