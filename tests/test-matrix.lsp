; Tests for the OBJ_MATRIX dense double-matrix type and its kernels.
(load "lib/math/matrix.lsp")

(print "=== MATRIX TESTS ===")

(define a (matrix-from-list 2 3 (list 1 2 3 4 5 6)))
(if (equal (typeof a) "matrix") (print "PASS: typeof-matrix") (print "FAIL: typeof-matrix"))
(if (equal (mat-rows a) 2) (print "PASS: mat-rows") (print "FAIL: mat-rows"))
(if (equal (mat-cols a) 3) (print "PASS: mat-cols") (print "FAIL: mat-cols"))
(if (= (mat-ref a 1 2) 6) (print "PASS: mat-ref") (print "FAIL: mat-ref"))
(mat-set a 0 0 99)
(if (= (mat-ref a 0 0) 99) (print "PASS: mat-set") (print "FAIL: mat-set"))

; matmul
(define m (matrix-from-list 2 2 (list 1 2 3 4)))
(define i2 (matrix-from-list 2 2 (list 1 0 0 1)))
(if (= (mat-ref (mat* m i2) 1 1) 4) (print "PASS: matmul-identity") (print "FAIL: matmul-identity"))
(define c (mat* (matrix-from-list 2 3 (list 1 2 3 4 5 6)) (matrix-from-list 3 2 (list 1 0 0 1 1 1))))
(if (= (mat-ref c 0 0) 4) (print "PASS: matmul-00") (print "FAIL: matmul-00"))
(if (= (mat-ref c 1 1) 11) (print "PASS: matmul-11") (print "FAIL: matmul-11"))

; elementwise
(define x (matrix-from-list 1 3 (list 1 2 3)))
(define y (matrix-from-list 1 3 (list 10 20 30)))
(if (= (mat-ref (+ x y) 0 2) 33) (print "PASS: generic-matrix-add") (print "FAIL: generic-matrix-add"))
(if (= (mat-ref (- y x) 0 0) 9) (print "PASS: generic-matrix-sub") (print "FAIL: generic-matrix-sub"))
(if (= (mat-ref (mat-mul x y) 0 1) 40) (print "PASS: mat-mul") (print "FAIL: mat-mul"))
(if (= (mat-ref (* x 5) 0 2) 15) (print "PASS: generic-matrix-scale") (print "FAIL: generic-matrix-scale"))
(if (= (mat-sum x) 6) (print "PASS: mat-sum") (print "FAIL: mat-sum"))

; transpose
(define tp (mat-transpose (matrix-from-list 2 3 (list 1 2 3 4 5 6))))
(if (equal (mat-rows tp) 3) (print "PASS: transpose-shape") (print "FAIL: transpose-shape"))
(if (= (mat-ref tp 2 1) 6) (print "PASS: transpose-val") (print "FAIL: transpose-val"))

; activations
(define r (relu (matrix-from-list 1 2 (list -5 7))))
(if (= (mat-ref r 0 0) 0) (print "PASS: relu-neg") (print "FAIL: relu-neg"))
(if (= (mat-ref r 0 1) 7) (print "PASS: relu-pos") (print "FAIL: relu-pos"))
(if (= (mat-ref (tanh (make-matrix 1 1 0)) 0 0) 0) (print "PASS: tanh-0") (print "FAIL: tanh-0"))
(if (= (mat-ref (exp (make-matrix 1 1 0)) 0 0) 1) (print "PASS: exp-0") (print "FAIL: exp-0"))

; make-matrix
(if (= (mat-ref (make-matrix 2 2 7) 1 1) 7) (print "PASS: make-fill") (print "FAIL: make-fill"))
(if (= (mat-ref (make-matrix 2 2) 0 0) 0) (print "PASS: make-zeros") (print "FAIL: make-zeros"))

; structural equality (same shape + same entries)
(if (equal (matrix-from-list 1 2 (list 1 2)) (matrix-from-list 1 2 (list 1 2))) (print "PASS: equal-same") (print "FAIL: equal-same"))
(if (equal (matrix-from-list 1 2 (list 1 2)) (matrix-from-list 1 2 (list 1 3))) (print "FAIL: equal-diff-content") (print "PASS: equal-diff-content"))
(if (equal (matrix-from-list 1 2 (list 1 2)) (matrix-from-list 2 1 (list 1 2))) (print "FAIL: equal-diff-shape") (print "PASS: equal-diff-shape"))

; broadcast row-add and axis reductions
(if (= (mat-ref (mat-add-row (matrix-from-list 2 2 (list 1 2 3 4)) (matrix-from-list 1 2 (list 10 20))) 1 1) 24) (print "PASS: mat-add-row") (print "FAIL: mat-add-row"))
(if (= (mat-ref (mat-sum-axis (matrix-from-list 2 2 (list 1 2 3 4)) 0) 0 1) 6) (print "PASS: sum-axis-0") (print "FAIL: sum-axis-0"))
(if (= (mat-ref (mat-sum-axis (matrix-from-list 2 2 (list 1 2 3 4)) 1) 1 0) 7) (print "PASS: sum-axis-1") (print "FAIL: sum-axis-1"))
(if (= (mat-ref (mat-max-axis (matrix-from-list 1 3 (list 3 9 5)) 1) 0 0) 9) (print "PASS: max-axis") (print "FAIL: max-axis"))

; stable softmax (rows sum to 1, and no overflow at large logits)
(if (< (abs (- (mat-ref (mat-softmax (matrix-from-list 1 3 (list 0 0 0))) 0 0) 0.3333333333333333)) 0.0001) (print "PASS: softmax-uniform") (print "FAIL: softmax-uniform"))
(if (< (abs (- (mat-sum (mat-softmax (matrix-from-list 1 3 (list 1000 1001 999)))) 1)) 0.0001) (print "PASS: softmax-stable") (print "FAIL: softmax-stable"))

; gather / scatter-add (embeddings)
(if (= (mat-ref (mat-gather-rows (matrix-from-list 3 2 (list 1 2 3 4 5 6)) (list 2 0)) 0 1) 6) (print "PASS: gather") (print "FAIL: gather"))
(if (eq (catch (mat-gather-rows [[1][2]] (cons 0 1)) (lambda (e) 'raised)) 'raised) (print "PASS: gather-improper-indices-error") (print "FAIL: gather-improper-indices-error"))
(if (eq (catch (mat-gather-rows [[1][2]] '(0 2)) (lambda (e) 'raised)) 'raised) (print "PASS: gather-index-error") (print "FAIL: gather-index-error"))
(define g (make-matrix 2 2))
(mat-scatter-add g (list 0 0) (matrix-from-list 2 2 (list 1 1 1 1)))
(if (= (mat-ref g 0 0) 2) (print "PASS: scatter-add") (print "FAIL: scatter-add"))

; elementwise div/sqrt/add-scalar
(if (= (mat-ref (mat-div (matrix-from-list 1 2 (list 10 20)) (matrix-from-list 1 2 (list 2 4))) 0 1) 5) (print "PASS: mat-div") (print "FAIL: mat-div"))
(if (= (mat-ref (sqrt (matrix-from-list 1 2 (list 9 16))) 0 1) 4) (print "PASS: generic-matrix-sqrt") (print "FAIL: generic-matrix-sqrt"))
(if (= (mat-ref (+ (matrix-from-list 1 2 (list 1 2)) 100) 0 0) 101) (print "PASS: generic-matrix-add-scalar") (print "FAIL: generic-matrix-add-scalar"))
(if (eq (catch (mat-div [1 2] [1 0]) (lambda (e) 'raised)) 'raised) (print "PASS: mat-div-zero-error") (print "FAIL: mat-div-zero-error"))
(if (eq (catch (sqrt [4 -1]) (lambda (e) 'raised)) 'raised) (print "PASS: generic-matrix-sqrt-negative-error") (print "FAIL: generic-matrix-sqrt-negative-error"))
(if (eq (catch (matrix-from-list 1 1 (cons 1 2)) (lambda (e) 'raised)) 'raised) (print "PASS: matrix-from-list-improper-error") (print "FAIL: matrix-from-list-improper-error"))
(if (equal (catch (mat-ref [[1]] 2 0) (lambda (e) (caddr e))) 2) (print "PASS: mat-ref-row-error-culprit") (print "FAIL: mat-ref-row-error-culprit"))
(if (equal (catch (mat-ref [[1]] 0 "x") (lambda (e) (cadr e))) "MAT-REF: NOT INTEGER") (print "PASS: mat-ref-type-error-message") (print "FAIL: mat-ref-type-error-message"))
(if (equal (catch (mat* [[1 2]] [[1 2]]) (lambda (e) (cadr e))) "MAT*: SHAPE MISMATCH") (print "PASS: matmul-shape-error-message") (print "FAIL: matmul-shape-error-message"))

; scatter validation is transactional: rejected calls do not partly mutate G
(define scatter-guard [[0][0]])
(if (eq (catch (mat-scatter-add scatter-guard '(0) [[1][2]]) (lambda (e) 'raised)) 'raised) (print "PASS: scatter-row-count-error") (print "FAIL: scatter-row-count-error"))
(if (equal scatter-guard [[0][0]]) (print "PASS: scatter-row-count-no-mutation") (print "FAIL: scatter-row-count-no-mutation"))
(if (eq (catch (mat-scatter-add scatter-guard '(0 1) [[1]])
               (lambda (e) 'raised))
        'raised)
    (print "PASS: scatter-too-few-rows-error")
    (print "FAIL: scatter-too-few-rows-error"))
(if (equal scatter-guard [[0][0]])
    (print "PASS: scatter-too-few-rows-no-mutation")
    (print "FAIL: scatter-too-few-rows-no-mutation"))
(if (eq (catch (mat-scatter-add scatter-guard '(0 2) [[1][2]]) (lambda (e) 'raised)) 'raised) (print "PASS: scatter-index-error") (print "FAIL: scatter-index-error"))
(if (equal scatter-guard [[0][0]]) (print "PASS: scatter-index-no-mutation") (print "FAIL: scatter-index-no-mutation"))
(if (eq (catch (mat-scatter-add scatter-guard '(0 bad) [[1][2]])
               (lambda (e) 'raised))
        'raised)
    (print "PASS: scatter-index-type-error")
    (print "FAIL: scatter-index-type-error"))
(if (equal scatter-guard [[0][0]])
    (print "PASS: scatter-index-type-no-mutation")
    (print "FAIL: scatter-index-type-no-mutation"))
(if (eq (catch (mat-scatter-add scatter-guard (cons 0 1) [[1]]) (lambda (e) 'raised)) 'raised) (print "PASS: scatter-improper-indices-error") (print "FAIL: scatter-improper-indices-error"))

; in-place: axpy (SGD update), scale-into, zero
(define w (matrix-from-list 1 2 (list 5.0 5.0)))
(mat-axpy w -1 (matrix-from-list 1 2 (list 2 3)))
(if (= (mat-ref w 0 1) 2) (print "PASS: mat-axpy") (print "FAIL: mat-axpy"))
(mat-scale-into w 10)
(if (= (mat-ref w 0 0) 30) (print "PASS: mat-scale-into") (print "FAIL: mat-scale-into"))
(mat-zero w)
(if (= (mat-ref w 0 0) 0) (print "PASS: mat-zero") (print "FAIL: mat-zero"))

; norm and random init
(if (= (mat-norm (matrix-from-list 1 2 (list 3 4))) 5) (print "PASS: mat-norm") (print "FAIL: mat-norm"))
(if (equal (mat-rows (matrix-randn 4 8 0.02)) 4) (print "PASS: matrix-randn-shape") (print "FAIL: matrix-randn-shape"))
(if (equal (mat-cols (mat-softmax (make-matrix 2 0))) 0) (print "PASS: softmax-zero-columns") (print "FAIL: softmax-zero-columns"))
(if (equal (mat-rows (mat-softmax (make-matrix 0 2))) 0) (print "PASS: softmax-zero-rows") (print "FAIL: softmax-zero-rows"))
(if (eq (catch (mat-max-axis (make-matrix 0 2) 0)
               (lambda (e) 'raised))
        'raised)
    (print "PASS: max-axis-zero-rows-error")
    (print "FAIL: max-axis-zero-rows-error"))
(if (eq (catch (mat-max-axis (make-matrix 2 0) 1)
               (lambda (e) 'raised))
        'raised)
    (print "PASS: max-axis-zero-columns-error")
    (print "FAIL: max-axis-zero-columns-error"))

; [ ] matrix literals (numbers only, like { } sets: literal, not evaluated)
(if (equal [[1 2 3][4 5 6]] (matrix-from-list 2 3 (list 1 2 3 4 5 6))) (print "PASS: lit-2x3") (print "FAIL: lit-2x3"))
(if (equal (mat-rows [10 20 30]) 1) (print "PASS: lit-rowvec-rows") (print "FAIL: lit-rowvec-rows"))
(if (equal (mat-cols [10 20 30]) 3) (print "PASS: lit-rowvec-cols") (print "FAIL: lit-rowvec-cols"))
(if (= (mat-ref [10 20 30] 0 2) 30) (print "PASS: lit-rowvec-ref") (print "FAIL: lit-rowvec-ref"))
(if (equal (mat-rows [[1][2][3]]) 3) (print "PASS: lit-colvec") (print "FAIL: lit-colvec"))
(if (= (mat-ref [[1.5 2.5]] 0 1) 2.5) (print "PASS: lit-float") (print "FAIL: lit-float"))
(if (= (mat-ref [-1 -2 -3] 0 1) -2) (print "PASS: lit-negative") (print "FAIL: lit-negative"))
(if (= (mat-ref (mat* [[1 2][3 4]] [[1 0][0 1]]) 1 0) 3) (print "PASS: lit-matmul") (print "FAIL: lit-matmul"))

; (matrix rows) constructor: nested list -> matrix (rows may be quoted, list, or computed)
(if (= (mat-ref (matrix (quote ((1 2) (3 4)))) 1 0) 3) (print "PASS: ctor-quoted") (print "FAIL: ctor-quoted"))
(if (= (mat-ref (matrix (list (list (+ 1 2) 9) (list 0 0))) 0 0) 3) (print "PASS: ctor-computed") (print "FAIL: ctor-computed"))
(if (equal [[1 2][3 4]] (matrix (quote ((1 2) (3 4))))) (print "PASS: lit-eq-ctor") (print "FAIL: lit-eq-ctor"))

; a matrix prints as the reader's literal, in full: read(print(m)) == m
(if (equal [[1 2 3][4 5 6]] (car (read-from-string "[[1 2 3][4 5 6]]"))) (print "PASS: print-roundtrip-mxn") (print "FAIL: print-roundtrip-mxn"))
(if (equal [1.5 2.25 3] (car (read-from-string "[1.5 2.25 3]"))) (print "PASS: print-roundtrip-row") (print "FAIL: print-roundtrip-row"))

(print "=== MATRIX TESTS DONE ===")
