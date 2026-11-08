; A native macro compiles a declarative query pipeline into map, filter, sort, and take calls.
; Steps are code, not interpreted commands, so predicates retain lexical access to surrounding values.
(define (query-step source step)
  (cond ((eq (car step) 'where)
         (list 'filter (list 'lambda (list (cadr step)) (caddr step)) source))
        ((eq (car step) 'select)
         (list 'map (list 'lambda (list (cadr step)) (caddr step)) source))
        ((eq (car step) 'order)
         (list 'sort
           (list 'lambda (list (cadr step) (caddr step)) (cadddr step)) source))
        ((eq (car step) 'take) (list 'take source (cadr step)))
        (t source)))

(define (query-build source steps)
  (cond ((eq steps nil) source)
        (t (query-build (query-step source (car steps)) (cdr steps)))))

(defmacro (query source steps)
  (query-build source steps))

; Employees are (name department level salary).
(define query-employees
  '((ada engineering 4 92000)
    (ben sales 3 71000)
    (cy engineering 2 68000)
    (dia engineering 3 81000)
    (eli support 4 76000)))

(define query-minimum 70000)
(define query-result
  (query query-employees
    ((where employee (and (eq (cadr employee) 'engineering)
                       (>= (cadddr employee) query-minimum)))
     (order left right (> (cadddr left) (cadddr right)))
     (select employee (list (car employee) (cadddr employee)))
     (take 2))))

(list query-result) ; => (((ada 92000) (dia 81000)))
(query query-employees ((where employee (= (caddr employee) 4)) (select employee (car employee)))) ; => (ada eli)
(macroexpand '(query rows ((where row (> (cadr row) 0))))) ; => (filter (lambda (row) (> (cadr row) 0)) rows)
