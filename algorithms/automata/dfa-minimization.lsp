; ============================================================
; name: dfa-minimization
; file: algorithms/automata/dfa-minimization.lsp
; subject: automata
; capability: minimize automata
; aliases: "hopcroft" "moore" "dfa minimization" "state minimization"
; signature: (dfa-minimize states alphabet delta accepting) -> blocks
; complexity: Moore refinement, at most n rounds of O(k n) signatures
;             (Hopcroft's splitter variant is O(k n log n))
; see-also: (none)
; derived-from: Moore 1956; Hopcroft & Ullman 1979, sec. 3.4
; status: example (tested)
; checks: 19 - partition refinement; a 4-state DFA that shrinks to 3
; ============================================================
; DFA minimization by Moore's partition refinement.
; Partition refinement: start with accepting/non-accepting split.
; Refine: states p,q in same block must have delta(p,a),delta(q,a) in same block.
; Repeat until stable.
; Minimal DFA for recognizing binary strings divisible by 3.

; DFA for binary numbers divisible by 3. States: 0 (rem 0, accept), 1 (rem 1), 2 (rem 2).
; Alphabet: "0" "1". Transition: reading from MSB.
(define dfa-states '(0 1 2))
(define dfa-alphabet '("0" "1"))
(define dfa-start 0)
(define dfa-accepting '(0))

(define (dfa-delta state sym)
  (cond ((and (= state 0) (equal sym "0")) 0)
        ((and (= state 0) (equal sym "1")) 1)
        ((and (= state 1) (equal sym "0")) 2)
        ((and (= state 1) (equal sym "1")) 0)
        ((and (= state 2) (equal sym "0")) 1)
        ((and (= state 2) (equal sym "1")) 2)
        (t state)))

(define (dfa-accepts state)
  (contains state dfa-accepting))

(define (dfa-simulate states input-chars)
  (let ((final (foldl (lambda (cur sym) (dfa-delta cur sym))
               dfa-start input-chars)))
    (contains final dfa-accepting)))

; Partition refinement
; The block (as a list of states) that contains STATE, or nil.
(define (partition-find blocks state)
  (find (lambda (b) (contains state b)) blocks))

; Index of the block containing STATE, or nil when STATE is in no block.
; Block indices are the "names" of the merged states, so two states are
; still equivalent only if every symbol sends them to the same index.
(define (block-id blocks state)
  (cond ((eq blocks nil) nil)
        ((contains state (car blocks)) 0)
        (t (let ((r (block-id (cdr blocks) state)))
             (cond ((eq r nil) nil) (t (+ r 1)))))))

(define (partition-init states accepting)
  (let ((acc-states (filter (lambda (s) (contains s accepting)) states))
        (rej-states (filter (lambda (s) (not (contains s accepting))) states)))
    (cond ((eq rej-states nil) (list acc-states))
          ((eq acc-states nil) (list rej-states))
          (t (list acc-states rej-states)))))

; Signature of a state under the current partition: the list of destination
; block indices, one per input symbol.
(define (state-signature blocks alphabet delta state)
  (map (lambda (a) (block-id blocks (delta state a))) alphabet))

; Split one block into the sub-blocks of states sharing a signature.
; Every state of the block lands in exactly one sub-block (nothing is dropped).
(define (split-block blocks alphabet delta block)
  (let ((sigs (dedup (map (lambda (s) (state-signature blocks alphabet delta s)) block))))
    (map (lambda (sig)
           (filter (lambda (s) (equal (state-signature blocks alphabet delta s) sig)) block))
         sigs)))

; One round of Moore refinement: split every block, concatenate the results.
; foldr/append concatenates one level; flatmap would deep-flatten the blocks.
(define (partition-refine blocks alphabet delta)
  (foldr (lambda (block acc)
           (append (split-block blocks alphabet delta block) acc))
         nil blocks))

(define (dfa-minimize states alphabet delta accepting)
  (dfa-minimize-iterate (partition-init states accepting) alphabet delta accepting))

(define (dfa-minimize-iterate blocks alphabet delta accepting)
  (let ((refined (partition-refine blocks alphabet delta)))
    (cond ((= (length refined) (length blocks)) blocks)
          (t (dfa-minimize-iterate refined alphabet delta accepting)))))

(define (min-dfa-state-count states alphabet delta accepting)
  (length (dfa-minimize-iterate (partition-init states accepting) alphabet delta accepting)))

(print "=== DFA Minimization (Hopcroft) ===")
(define dfa-states (list 0 1 2))

(print "Original DFA: 3 states, binary strings divisible by 3")
(print "States: 0 (accept), 1, 2")

(print "--- Test DFA ---")
(print "Accept 0 (0 div 3):") (print (dfa-simulate dfa-states (str-to-list "0")))
(print "Accept 11 (3 div 3):") (print (dfa-simulate dfa-states (str-to-list "11")))
(print "Accept 110 (6 div 3):") (print (dfa-simulate dfa-states (str-to-list "110")))
(print "Reject 1:") (print (dfa-simulate dfa-states (str-to-list "1")))
(print "Reject 10 (2):") (print (dfa-simulate dfa-states (str-to-list "10")))
(print "Reject 100 (4):") (print (dfa-simulate dfa-states (str-to-list "100")))

(print "--- Minimization ---")
(define init-blocks (partition-init dfa-states dfa-accepting))
(print "Initial partition:")
(print init-blocks)
(define refined-blocks (partition-refine init-blocks dfa-alphabet dfa-delta))
(print "After one refinement:")
(print refined-blocks)
(define final-blocks (dfa-minimize-iterate init-blocks dfa-alphabet dfa-delta dfa-accepting))
(print "Final minimal partition (stable):")
(print final-blocks)
(print "Minimal DFA states:")
(print (length final-blocks))
(print "Expected: 3 (cannot reduce further; 3 equivalence classes mod 3)")

; ---- checks: the mod-3 DFA recognises exactly the multiples of 3 ----
(check "accepts 0"    (dfa-simulate dfa-states (str-to-list "0")))
(check "accepts 11"   (dfa-simulate dfa-states (str-to-list "11")))
(check "accepts 110"  (dfa-simulate dfa-states (str-to-list "110")))
(check "accepts 1001" (dfa-simulate dfa-states (str-to-list "1001")))
(check "rejects 1"    (not (dfa-simulate dfa-states (str-to-list "1"))))
(check "rejects 10"   (not (dfa-simulate dfa-states (str-to-list "10"))))
(check "rejects 100"  (not (dfa-simulate dfa-states (str-to-list "100"))))

; ---- checks: partition refinement on the mod-3 DFA ----
(check "initial partition splits accepting from the rest"
       (equal init-blocks '((0) (1 2))))
; State 1 goes to block 1 on "0" and block 0 on "1"; state 2 goes to
; block 1 on both.  Different signatures, so {1 2} splits.
(check "one refinement fully separates 0, 1 and 2"
       (equal refined-blocks '((0) (1) (2))))
(check "refinement is a fixpoint on the mod-3 DFA"
       (equal final-blocks '((0) (1) (2))))
(check "mod-3 DFA is already minimal (3 states)"
       (= (min-dfa-state-count dfa-states dfa-alphabet dfa-delta dfa-accepting) 3))
(check "refinement never loses a state"
       (equal (sort < (flatten final-blocks)) '(0 1 2)))
(check "partition-find locates the block of a state"
       (equal (partition-find final-blocks 2) '(2)))
(check "partition-find returns nil for an unknown state"
       (eq (partition-find final-blocks 9) nil))
(check "block-id agrees with partition-find"
       (equal (block-id init-blocks 2) 1))

; ---- a DFA that really does shrink: two equivalent accepting sinks ----
; States 0,1 non-accepting; 2,3 accepting sinks that behave identically.
(define dfa2-states (list 0 1 2 3))
(define dfa2-accepting '(2 3))
(define (dfa2-delta state sym)
  (cond ((= state 0) 1)
        ((and (= state 1) (equal sym "0")) 2)
        ((and (= state 1) (equal sym "1")) 3)
        (t state)))

(print "--- Second DFA: 4 states, two equivalent sinks ---")
(define dfa2-init (partition-init dfa2-states dfa2-accepting))
(define dfa2-final (dfa-minimize dfa2-states dfa-alphabet dfa2-delta dfa2-accepting))
(print "Initial partition:") (print dfa2-init)
(print "Minimal partition:") (print dfa2-final)

(check "second DFA starts from {2 3} vs {0 1}"
       (equal dfa2-init '((2 3) (0 1))))
; 0 and 1 are distinguishable (0 stays non-accepting, 1 reaches a sink),
; but 2 and 3 are both accepting sinks and stay merged.
(check "second DFA minimises to 3 blocks"
       (equal dfa2-final '((2 3) (0) (1))))
(check "second DFA drops exactly one state"
       (= (- (length dfa2-states)
             (min-dfa-state-count dfa2-states dfa-alphabet dfa2-delta dfa2-accepting))
          1))
(check "second DFA refinement never loses a state"
       (equal (sort < (flatten dfa2-final)) '(0 1 2 3)))

(print "=== Minimization Done ===")
