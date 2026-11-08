; ============================================================
; DATAFLOW MACHINE — no program counter, fires on operand arrival
;
; Instructions execute when ALL operands are available.
; Results flow to destination instructions via tokens.
; Natural parallelism — independent instructions fire simultaneously.
;
; Example:  (a + b) * (c - d)
; Node 1: + waits for a,b, sends result to node 3
; Node 2: - waits for c,d, sends result to node 3
; Node 3: * waits for results from 1 and 2, outputs final
; ============================================================

; Instruction template: (op src1 src2 dst)
; A token is (value . dst-port)
(define token-queue nil)

(define (send-token value dst port)
  (setq token-queue (cons (list value dst port) token-queue)))

(define (recv-tokens node port)
  (let ((matches (filter (lambda (token) (and (= (cadr token) node) (= (caddr token) port)))
                         token-queue)))
    (setq token-queue (remove-if (lambda (token) (and (= (cadr token) node) (= (caddr token) port)))
                                 token-queue))
    (map car matches)))

; Instruction graph: list of (op left-node left-port right-node right-port dst-node)
(define (instr-op graph i) (car (nth i graph)))
(define (instr-ln graph i) (cadr (nth i graph)))
(define (instr-lp graph i) (caddr (nth i graph)))
(define (instr-rn graph i) (cadddr (nth i graph)))
(define (instr-rp graph i) (car (cddddr (nth i graph))))
(define (instr-dst graph i) (cadr (cddddr (nth i graph))))

(define (destination-port graph source destination)
  (cond ((= source (instr-ln graph destination)) (instr-lp graph destination))
        ((= source (instr-rn graph destination)) (instr-rp graph destination))
        (t (error "DATAFLOW: SOURCE IS NOT CONNECTED TO DESTINATION" source))))

; Fire an instruction if both operands are available
(define (dataflow-fire graph i)
  (let ((lefts (recv-tokens i 0))
        (rights (recv-tokens i 1)))
    (cond ((and (not (null lefts)) (not (null rights)))
           (let ((lv (car lefts)) (rv (car rights)))
             (let ((result (cond ((eq (instr-op graph i) '+) (+ lv rv))
                                 ((eq (instr-op graph i) '-) (- lv rv))
                                 ((eq (instr-op graph i) '*) (* lv rv))
                                 ((eq (instr-op graph i) '/) (truncate (/ lv rv)))
                                 (t 0))))
               (cond ((instr-dst graph i)
                      (send-token result (instr-dst graph i)
                                  (destination-port graph i (instr-dst graph i))))
                     (t (print (str-join (list "OUTPUT: " (num-to-str result)) "")))))))
          (t nil))))

(define (dataflow-run graph inputs)
  ; Inject initial tokens
  (labels (((inject in)
      (cond ((null in) nil)
            (t (send-token (caar in) (cadar in) (caddar in))
               (inject (cdr in))))))
    (inject inputs))
  ; Keep firing until quiescence
  (dataflow-loop graph 0 50))

(define (dataflow-loop graph step max)
  (cond ((>= step max) nil)
        (t (dataflow-step graph 0)
           (dataflow-loop graph (+ step 1) (- max 1)))))

(define (dataflow-step graph i)
  (cond ((>= i (length graph)) nil)
        (t (dataflow-fire graph i)
           (dataflow-step graph (+ i 1)))))

(define (remove-if pred lst)
  (cond ((null lst) nil)
        ((pred (car lst)) (remove-if pred (cdr lst)))
        (t (cons (car lst) (remove-if pred (cdr lst))))))

(define (str-join parts sep)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (cat (car parts) sep) (str-join (cdr parts) sep)))))

; ============================================================
; DEMO
; ============================================================
(print "=============================================")
(print "  DATAFLOW MACHINE — Token-Driven Execution")
(print "=============================================")
(print "")

; Graph: (a+b)*(c-d)
; Node 0: + a b -> node 2
; Node 1: - c d -> node 2
; Node 2: * (from 0) (from 1) -> output
; Inputs: a=10 -> node 0/port 0, b=3 -> node 0/port 1, c=8 -> node 1/port 0, d=2 -> node 1/port 1
(define graph
  (list (list '+ 0 0 0 1 2)
        (list '- 1 0 1 1 2)
        (list '* 0 0 1 1 nil)))

(define inputs (list (list 10 0 0) (list 3 0 1) (list 8 1 0) (list 2 1 1)))

(print "Graph: (a+b)*(c-d) with a=10, b=3, c=8, d=2")
(print "Nodes fire when both operands arrive.")
(dataflow-run graph inputs)
(print "Expected: (10+3)*(8-2) = 13*6 = 78")
(print "")

; Graph: fibonacci dataflow
(print "--- Fibonacci dataflow ---")
(setq token-queue nil)
(define fib-graph
  (list (list '+ 0 0 0 1 nil)))  ; just add a+b
(define fib-inputs (list (list 8 0 0) (list 5 0 1)))
(dataflow-run fib-graph fib-inputs)
(print "Expected: 8+5 = 13")
(print "")

(print "=============================================")
(print "  Dataflow Machine — token-driven, no PC")
(print "=============================================")
