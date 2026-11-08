; Native SISP macros implement guarded evaluation, local binding, threading, and compile-time repetition.
; The helpers receive unevaluated forms and return ordinary SISP code.
(defmacro (meta-when condition body)
  `(if ,condition ,body nil))

(defmacro (meta-unless condition body)
  `(if (not ,condition) ,body nil))

(defmacro (meta-let1 name value body)
  (list 'let (list (list name value)) body))

(define (meta-thread-step value step)
  (cond ((atom step) (list step value))
        (t (cons (car step) (cons value (cdr step))))))

(define (meta-thread-build value steps)
  (cond ((eq steps nil) value)
        (t (meta-thread-build
             (meta-thread-step value (car steps)) (cdr steps)))))

(defmacro (meta-thread value steps)
  (meta-thread-build value steps))

(define (meta-repeat-build count body)
  (cond ((<= count 0) nil)
        ((= count 1) body)
        (t (list 'progn body (meta-repeat-build (- count 1) body)))))

(defmacro (meta-repeat count body)
  (meta-repeat-build count body))

(define (meta-add value amount) (+ value amount))
(define (meta-square value) (* value value))
(define meta-counter 0)
(meta-repeat 4 (setq meta-counter (+ meta-counter 3)))

(meta-thread 5 ((meta-add 3) meta-square)) ; => 64
(meta-let1 value (+ 2 3) (meta-thread value ((meta-add 1) meta-square))) ; => 36
(list meta-counter (macroexpand '(meta-unless ready action))) ; => (12 (if (not ready) action nil))
