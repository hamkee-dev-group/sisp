; A native macro compiles declarative transition rows into reusable state-step functions.
; Rows are (current-state event next-state); missing transitions preserve the current state.
(define (machine-transition transitions state event)
  (find (lambda (transition)
    (and (eq (car transition) state)
         (eq (cadr transition) event))) transitions))

(define (machine-next transitions state event)
  (let ((transition (machine-transition transitions state event)))
    (cond ((eq transition nil) state) (t (caddr transition)))))

(define (machine-definition name transitions)
  (list 'define (list name 'state 'event)
    (list 'machine-next (list 'quote transitions) 'state 'event)))

(defmacro (define-state-machine name transitions)
  (machine-definition name transitions))

(define-state-machine door-step
  ((closed open opened)
   (opened close closed)
   (closed lock locked)
   (locked unlock closed)))

(define-state-machine job-step
  ((queued start running)
   (running finish complete)
   (running fail failed)
   (failed retry queued)))

(door-step 'closed 'open) ; => opened
(door-step 'locked 'open) ; => locked
(foldl (lambda (state event) (job-step state event)) 'queued '(start fail retry start finish)) ; => complete
