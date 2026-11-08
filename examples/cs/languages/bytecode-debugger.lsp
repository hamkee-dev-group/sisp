; Bytecode debugger executes one instruction at a time, records traces, and stops at breakpoints.
; State is (program-counter stack environment halted-p).
(define (bd-env-get environment name)
  (cadr (find (lambda (binding) (equal (car binding) name)) environment)))
(define (bd-env-put environment name value)
  (cons (list name value) (remove (lambda (binding) (equal (car binding) name)) environment)))
(define (bd-step program state)
  (cond ((nth 3 state) state)
        (t (let ((pc (car state)) (stack (cadr state)) (environment (caddr state)))
          (let ((instruction (nth pc program)))
            (cond ((eq (car instruction) 'const)
                   (list (+ pc 1) (cons (cadr instruction) stack) environment nil))
                  ((eq (car instruction) 'load)
                   (list (+ pc 1) (cons (bd-env-get environment (cadr instruction)) stack) environment nil))
                  ((eq (car instruction) 'store)
                   (list (+ pc 1) (cdr stack) (bd-env-put environment (cadr instruction) (car stack)) nil))
                  ((eq (car instruction) 'add)
                   (list (+ pc 1) (cons (+ (cadr stack) (car stack)) (cddr stack)) environment nil))
                  ((eq (car instruction) 'halt) (list pc stack environment t))))))))
(define (bd-run program state breakpoints trace budget)
  (cond ((or (nth 3 state) (= budget 0) (in (car state) breakpoints))
         (list state (reverse trace)))
        (t (bd-run program (bd-step program state) breakpoints (cons state trace) (- budget 1)))))
(define bd-program '((const 2) (const 3) (add) (store x) (load x) (halt)))
(car (car (bd-run bd-program '(0 nil nil nil) '(3) nil 20))) ; => 3
(cadr (car (bd-run bd-program '(0 nil nil nil) '(3) nil 20))) ; => (5)
(bd-env-get (caddr (car (bd-run bd-program '(0 nil nil nil) nil nil 20))) 'x) ; => 5

