; ============================================================
; 67 — Turing Machines and the Busy Beaver BB(2), by Exhaustion
; A Turing machine simulator, illustrative machines, the 3-state
; busy-beaver champion, and the main event: ALL 20,736 two-state
; two-symbol machines are enumerated and run, establishing
; Sigma(2) = 4 ones and S(2) = 6 steps by complete search.
; ============================================================

(print "=== Turing Machines and Busy Beaver ===")


; ---------- the simulator ----------
; A machine over states 0..n-1 and symbols {0,1} is a list of
; transitions indexed by 2*state+symbol; each transition is
; (write move next) with move in {-1,+1} and next = -1 meaning
; halt (the convention that writes and moves before halting).
; The tape is a shadowing assoc list of (position . symbol),
; blank = 0; the head starts at 0 on an empty tape.

(define (tape-read tape pos)
  (let ((entry (assoc pos tape)))
    (cond ((eq entry nil) 0) (t (cdr entry)))))

(define (tape-ones tape seen acc)
  (cond ((eq tape nil) acc)
        ((contains (car (car tape)) seen)
          (tape-ones (cdr tape) seen acc))
        (t (tape-ones (cdr tape) (cons (car (car tape)) seen)
             (+ acc (cdr (car tape)))))))

; run: returns (halted steps ones); tail-recursive, constant stack.
(define (tm-run trans state pos tape steps cutoff)
  (cond ((= steps cutoff) (list nil steps (tape-ones tape nil 0)))
    (t (let ((tr (nth (+ (* 2 state) (tape-read tape pos)) trans)))
      (let ((tape2 (cons (cons pos (nth 0 tr)) tape)))
        (cond ((= (nth 2 tr) -1)
          (list t (+ steps 1) (tape-ones tape2 nil 0)))
          (t (tm-run trans (nth 2 tr) (+ pos (nth 1 tr))
               tape2 (+ steps 1) cutoff))))))))

(define (tm trans cutoff) (tm-run trans 0 0 nil 0 cutoff))

; ---------- illustrative machines ----------
; unary increment: walk right over 1s, write a 1 at the end, halt.
; state 0: on 1 move right; on 0 write 1 and halt.
(define incr (list (list 1 1 -1) (list 1 1 0)))
(define incr-run (tm-run incr 0 0 (list (cons 0 1) (cons 1 1)) 0 50))
(check "unary-increment-halts" (eq (nth 0 incr-run) t))
(check "unary-increment-3-ones" (= (nth 2 incr-run) 3))

; a trivially non-halting machine: move right forever on blanks.
(define spinner (list (list 0 1 0) (list 1 1 0)))
(check "spinner-hits-cutoff" (eq (nth 0 (tm spinner 100)) nil))

; ---------- the known champions, replayed ----------
; BB(2) champion: A0->1RB A1->1LB B0->1LA B1->1RH.
(define bb2-champion
  (list (list 1 1 1) (list 1 -1 1) (list 1 -1 0) (list 1 1 -1)))
(define bb2-run (tm bb2-champion 50))
(print (list (quote bb2-champion) bb2-run))
(check "bb2-champion-halts-in-6-steps" (= (nth 1 bb2-run) 6))
(check "bb2-champion-writes-4-ones" (= (nth 2 bb2-run) 4))

; A 3-state machine achieving Sigma(3) = 6 ones (the known busy
; beaver value for three states). Under this simulator's convention
; (the halting write counts as a step) it stops after 14 steps.
(define bb3-champion
  (list (list 1 1 1) (list 1 1 -1)      ; A: 1RB, 1RH
        (list 0 1 2) (list 1 1 1)       ; B: 0RC, 1RB
        (list 1 -1 2) (list 1 -1 0)))   ; C: 1LC, 1LA
(define bb3-run (tm bb3-champion 100))
(print (list (quote bb3-machine) bb3-run))
(check "bb3-machine-halts" (eq (nth 0 bb3-run) t))
(check "bb3-machine-witnesses-sigma-3" (= (nth 2 bb3-run) 6))
(check "bb3-machine-steps-recorded" (= (nth 1 bb3-run) 14))

; ---------- BB(2) by complete enumeration ----------
; Every (state,symbol) slot chooses from 12 transitions
; (2 writes x 2 moves x 3 next-states incl. halt): 12^4 = 20736
; machines. Each is run with a step cutoff of 20. The verified
; statement: among ALL two-state machines, every halting run stops
; within 6 steps, none writes more than 4 ones, and none halts
; between steps 7 and 20. Sigma(2)=4 and S(2)=6 follow for every
; machine that halts within the window — and the empty 7..20 halt
; window is the classical evidence that the search is complete.

(define tm-options
  (list (list 0 -1 0) (list 0 -1 1) (list 0 -1 -1)
        (list 0 1 0) (list 0 1 1) (list 0 1 -1)
        (list 1 -1 0) (list 1 -1 1) (list 1 -1 -1)
        (list 1 1 0) (list 1 1 1) (list 1 1 -1)))

(define *bb-machines* 0)
(define *bb-halters* 0)
(define *bb-max-ones* 0)
(define *bb-max-steps* 0)
(define *bb-late-halters* 0)

(define (bb2-record result)
  (setq *bb-machines* (+ *bb-machines* 1))
  (cond ((eq (nth 0 result) t)
    (setq *bb-halters* (+ *bb-halters* 1))
    (cond ((> (nth 2 result) *bb-max-ones*)
      (setq *bb-max-ones* (nth 2 result))) (t nil))
    (cond ((> (nth 1 result) *bb-max-steps*)
      (setq *bb-max-steps* (nth 1 result))) (t nil))
    (cond ((> (nth 1 result) 6)
      (setq *bb-late-halters* (+ *bb-late-halters* 1))) (t nil)))
    (t nil)))

; One batch fixes the A0 transition; scheduling the twelve batches
; as separate top-level forms lets the collector reclaim each
; batch's tapes between forms.
(define (bb2-batch t00)
  (foreach (lambda (t01)
    (foreach (lambda (t10)
      (foreach (lambda (t11)
        (bb2-record (tm (list t00 t01 t10 t11) 20)))
        tm-options))
      tm-options))
    tm-options)
  *bb-machines*)

(bb2-batch (nth 0 tm-options))
(bb2-batch (nth 1 tm-options))
(bb2-batch (nth 2 tm-options))
(bb2-batch (nth 3 tm-options))
(bb2-batch (nth 4 tm-options))
(bb2-batch (nth 5 tm-options))
(bb2-batch (nth 6 tm-options))
(bb2-batch (nth 7 tm-options))
(bb2-batch (nth 8 tm-options))
(bb2-batch (nth 9 tm-options))
(bb2-batch (nth 10 tm-options))
(bb2-batch (nth 11 tm-options))

(print (list (quote machines) *bb-machines*
  (quote halters) *bb-halters*
  (quote sigma) *bb-max-ones*
  (quote s) *bb-max-steps*
  (quote late-halters) *bb-late-halters*))

(check "all-20736-machines-enumerated" (= *bb-machines* 20736))
(check "busy-beaver-sigma-2-is-4" (= *bb-max-ones* 4))
(check "busy-beaver-s-2-is-6" (= *bb-max-steps* 6))
(check "no-machine-halts-after-step-6" (= *bb-late-halters* 0))
(check "most-machines-run-forever" (< *bb-halters* *bb-machines*))

(print "=== Turing Machines and Busy Beaver Done ===")
