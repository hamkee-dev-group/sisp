; Rice's Theorem: Every non-trivial semantic property of computable
; functions is undecidable.

; ---- computable example programs ----
(define (prog-halts n) (+ n 1))
; Same partial function as prog-halts, different text: a semantic property
; must give both the same answer.
(define (prog-halts-again n) (+ 1 n))
(define (prog-returns-42 n)
  (cond ((= n 7) 42) (t (+ n 1))))
; Always odd, so it never returns 42 on ANY input.  prog-halts is not a
; witness for "lacks the property": prog-halts(41) = 42.
(define (prog-odd n) (+ (* 2 n) 1))
; Undefined at 0: our stand-in for a program that diverges there.
(define (prog-stuck-at-0 n)
  (cond ((= n 0) (error "no value at 0" n)) (t n)))

; ---- semantic properties (modeled as predicates) ----
; Catch needs funcall to call stored function values.

(define (safe-call f x)
  (catch (funcall f x) (lambda (e) 'diverges)))

(define (halts-on-0-p prog)
  (not (equal (safe-call prog 0) 'diverges)))

; Bounded search: the local helper is named SCAN, not CHECK, so it does not
; shadow the prelude's assertion helper.
(define (returns-42-some-p prog bound)
  (labels (((scan n)
    (cond ((> n bound) nil)
          (t (let ((r (safe-call prog n)))
               (cond ((equal r 42) t)
                     (t (scan (+ n 1)))))))))
    (scan 0)))

(print "=== Rice's Theorem ===")
(print "Every non-trivial semantic property of partial")
(print "computable functions is undecidable.")
(print "")

; --- Demonstrate non-trivial semantic property ---
(print "--- Property: halts on input 0 ---")
(print "prog-halts(0) =" (prog-halts 0) "=> has property")
(print "prog-returns-42(0) =" (prog-returns-42 0) "=> has property")
(print "A divergent-on-0 program => lacks property")
(print "=> This is a non-trivial semantic property.")
(print "")

(print "--- Property: returns 42 for some input ---")
(print "prog-returns-42 on [0,5]: no 42 yet:" (returns-42-some-p prog-returns-42 5))
(print "prog-returns-42 on [0,8]: found 42:" (returns-42-some-p prog-returns-42 8))
(print "prog-halts on [0,5]: no 42 yet:" (returns-42-some-p prog-halts 5))
(print "prog-halts on [0,50]: found 42 at n=41:" (returns-42-some-p prog-halts 50))
(print "prog-odd on [0,50]: never 42 (always odd):" (returns-42-some-p prog-odd 50))
(print "=> Non-trivial: prog-returns-42 has it, prog-odd lacks it.")
(print "=> A bounded search only ever answers 'not yet': the unbounded")
(print "   question is exactly the undecidable one.")
(print "")

; --- Formal statement ---
(print "--- Formal Rice's Theorem ---")
(print "Let P: Programs -> {t, nil} be a semantic property")
(print "(i.e., if programs p,q compute the same partial function,")
(print " then P(p) = P(q)).")
(print "If P is non-trivial (???p: P(p)=t and ???q: P(q)=nil),")
(print "then P is undecidable.")
(print "")

; --- Proof sketch ---
(print "--- Proof (by reduction from Halting Problem) ---")
(print "Assume decider D exists: D(p) = t iff P(p) = t.")
(print "Let A,B be programs with P(A)=t, P(B)=nil.")
(print "For any (M,w), construct N(x):")
(print "  run M(w); if halts then compute A(x) else diverge")
(print "Then P(N)=t iff M(w) halts.")
(print "D decides P => D decides halting => contradiction.")
(print "")

; --- Examples of undecidable semantic properties ---
(print "--- Undecidable properties (all non-trivial) ---")
(print "1. does-program-halt-on-0?")
(print "2. does-program-return-42-for-some-input?")
(print "3. is-program-equivalent-to-f(x)=x^2?")
(print "4. does-program-have-a-bug? (semantic variant)")
(print "5. is-program-correct? (relative to spec)")
(print "")

; --- Only trivial properties are decidable ---
(print "--- Decidable properties: only trivial ones ---")
(print "  P(p) = t for all p  (always true)")
(print "  P(p) = nil for all p (always false)")
(print "These are the ONLY decidable semantic properties.")
(print "All others (any non-trivial semantic property) are undecidable.")

; ---- checks ----

; The programs compute what the argument claims they compute.
(check "prog-halts is n+1" (= (prog-halts 5) 6))
(check "prog-returns-42 returns 42 exactly at 7"
       (and (= (prog-returns-42 7) 42) (= (prog-returns-42 6) 7)))
(check "prog-odd is always odd" (every (lambda (n) (= (mod (prog-odd n) 2) 1)) (seq 0 20)))
(check "prog-odd never returns 42 on [0,50]" (not (returns-42-some-p prog-odd 50)))

; safe-call turns an undefined result into the marker, so the property
; predicates are total even though the programs are not.
(check "safe-call reports an undefined point"
       (equal (safe-call prog-stuck-at-0 0) 'diverges))
(check "safe-call is transparent elsewhere"
       (= (safe-call prog-stuck-at-0 3) 3))

; Property 1: "halts on input 0" is NON-TRIVIAL -- some programs have it,
; some do not.  That is precisely Rice's hypothesis.
(check "prog-halts halts on 0" (halts-on-0-p prog-halts))
(check "prog-returns-42 halts on 0" (halts-on-0-p prog-returns-42))
(check "prog-stuck-at-0 does not halt on 0" (not (halts-on-0-p prog-stuck-at-0)))
(check "'halts on 0' is non-trivial"
       (and (halts-on-0-p prog-halts) (not (halts-on-0-p prog-stuck-at-0))))

; Property 2: "returns 42 for some input" is also non-trivial.
(check "prog-returns-42 has the 42 property" (returns-42-some-p prog-returns-42 8))
(check "'returns 42' is non-trivial"
       (and (returns-42-some-p prog-returns-42 8)
            (not (returns-42-some-p prog-odd 50))))

; The property is SEMANTIC (extensional): programs computing the same
; partial function must get the same answer.  prog-halts and
; prog-halts-again are different text for the same function.
(check "prog-halts and prog-halts-again agree pointwise"
       (every (lambda (n) (= (prog-halts n) (prog-halts-again n))) (seq 0 20)))
(check "a semantic property cannot separate them (halts on 0)"
       (equal (halts-on-0-p prog-halts) (halts-on-0-p prog-halts-again)))
(check "a semantic property cannot separate them (returns 42)"
       (equal (returns-42-some-p prog-halts 50) (returns-42-some-p prog-halts-again 50)))

; The escape hatch of Rice's theorem: the TRIVIAL properties are decidable,
; and they are exactly the two constant ones.
(define (always-true prog) t)
(define (always-false prog) nil)
(define sample-programs (list prog-halts prog-halts-again prog-returns-42 prog-odd prog-stuck-at-0))
(check "the always-true property holds of every program"
       (every always-true sample-programs))
(check "the always-false property holds of no program"
       (not (any always-false sample-programs)))
(check "a trivial property never separates two programs"
       (every (lambda (p) (equal (always-true p) (always-true prog-halts))) sample-programs))

; The bounded searcher is sound but not complete: a t answer is a real
; witness, but nil only means "not found within the bound", which is why
; it is not a decider.
(check "bounded search misses 42 for prog-halts at bound 5"
       (not (returns-42-some-p prog-halts 5)))
(check "bounded search finds 42 for prog-halts at bound 50"
       (returns-42-some-p prog-halts 50))
(check "so the bounded answer depends on the bound, not on the program"
       (not (equal (returns-42-some-p prog-halts 5) (returns-42-some-p prog-halts 50))))

(print "=== Rice done ===")
t
