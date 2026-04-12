; ============================================================
; 33-bell-numbers.lsp
; Compute Bell numbers using the Bell triangle.
; B(n) = number of partitions of an n-element set.
; Bell triangle:
;   Row 0: [1]
;   Each new row starts with last element of previous row,
;   then each entry = previous entry in this row + entry above that.
; Verify B(0)=1, B(1)=1, B(2)=2, B(3)=5, B(4)=15, B(5)=52, B(6)=203.
; ============================================================

(print "=== Bell Numbers via Bell Triangle ===")

; --- List helper: get last element ---
(define (last-elem lst)
  (if (equal (cdr lst) nil) (car lst)
    (last-elem (cdr lst))))

; --- Build next row of Bell triangle from previous row ---
; new_row[0] = last element of prev_row
; new_row[i] = new_row[i-1] + prev_row[i-1]
(define (build-next-row prev-row new-row-so-far idx)
  (if (= idx (ord prev-row)) new-row-so-far
    (let ((next-val (+ (last-elem new-row-so-far)
                       (nth (+ idx 1) prev-row))))
      (build-next-row prev-row
                      (my-append new-row-so-far (list next-val))
                      (+ idx 1)))))

; --- Custom append (avoids built-in append issues with nil) ---
(define (my-append a b)
  (if (equal a nil) b
    (cons (car a) (my-append (cdr a) b))))

(define (next-bell-row prev-row)
  (let ((start (list (last-elem prev-row))))
    (build-next-row prev-row start 0)))

; --- Compute Bell triangle up to row n ---
; Returns list of rows. B(n) = first element of row n.
(define (bell-rows n)
  (labels (((build row-num current-row rows)
    (if (> row-num n) rows
      (let ((next (next-bell-row current-row)))
        (build (+ row-num 1) next (my-append rows (list next)))))))
    (build 1 '(1) (list '(1)))))

; --- Extract Bell number: first element of nth row ---
(define (bell n)
  (if (= n 0) 1
    (let ((rows (bell-rows n)))
      (car (nth (+ n 1) rows)))))

; ============================================================
; Compute and verify B(0) through B(6)
; ============================================================
(print "--- Bell Triangle ---")

(define rows (bell-rows 6))

; Print each row of the triangle
(define (print-rows rs idx)
  (if (equal rs nil) nil
    (progn
      (print (car rs))
      (print-rows (cdr rs) (+ idx 1)))))
(print-rows rows 0)

; Extract Bell numbers (first element of each row)
(print "--- Bell Numbers ---")

(define (check-bell n expected)
  (let ((bn (car (nth (+ n 1) rows))))
    (progn
      (print bn)
      (if (= bn expected)
          (print (cat "VERIFIED: B(" (cat (cond ((= n 0) "0") ((= n 1) "1") ((= n 2) "2") ((= n 3) "3") ((= n 4) "4") ((= n 5) "5") ((= n 6) "6")) (cat ") = " (cond ((= expected 1) "1") ((= expected 2) "2") ((= expected 5) "5") ((= expected 15) "15") ((= expected 52) "52") ((= expected 203) "203"))))))
          (print "FAILED")))))

(check-bell 0 1)
(check-bell 1 1)
(check-bell 2 2)
(check-bell 3 5)
(check-bell 4 15)
(check-bell 5 52)
(check-bell 6 203)

(print "=== Bell Numbers Complete ===")
