; ============================================================
; name: stack
; file: algorithms/data-structures/stack.lsp
; subject: data-structure
; capability: lifo push pop
; aliases: "lifo" "pushdown stack"
; signature: (stack-new) (stack-push s x) (stack-pop s) (stack-top s)
;            (stack-empty-p s) (stack-size s)
; complexity: O(1) per operation
; see-also: queue deque
; status: example (tested)
; checks: 8 checks at bottom; run: src/sisp <this file> from repo root
; ============================================================
; A stack is a list used as a pushdown store: cons is push, car is top,
; cdr is pop.  Empty stack is nil; stack-top/stack-pop on an empty stack
; abort the calling form (error).

(define (stack-new) nil)

(define (stack-push s x) (cons x s))

(define (stack-pop s)
  (cond ((null s) (error "STACK-POP: EMPTY STACK" s))
        (t (cdr s))))

(define (stack-top s)
  (cond ((null s) (error "STACK-TOP: EMPTY STACK" s))
        (t (car s))))

(define (stack-empty-p s) (null s))

(define (stack-size s) (length s))

; ---- checks ----

(check "stack: new is empty"
       (stack-empty-p (stack-new)))

(check "stack: push then top"
       (= (stack-top (stack-push (stack-new) 1)) 1))

(check "stack: lifo order"
       (equal (stack-top (stack-pop (stack-push (stack-push (stack-new) 1) 2))) 1))

(check "stack: three pushes pop in reverse"
       (let ((s (stack-push (stack-push (stack-push (stack-new) 1) 2) 3)))
         (and (= (stack-top s) 3)
              (= (stack-top (stack-pop s)) 2)
              (= (stack-top (stack-pop (stack-pop s))) 1))))

(check "stack: size tracks pushes"
       (= (stack-size (stack-push (stack-push (stack-new) 1) 2)) 2))

(check "stack: empty after pops"
       (stack-empty-p (stack-pop (stack-pop (stack-push (stack-push (stack-new) 1) 2)))))

(check "stack: different types mix"
       (equal (stack-top (stack-push (stack-new) "top")) "top"))

(check "stack: error on empty top is catchable"
       (equal (car (catch (stack-top (stack-new)) (lambda (e) e))) (quote error)))
