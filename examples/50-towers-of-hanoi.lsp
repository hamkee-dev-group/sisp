; ============================================================
; 50 — Towers of Hanoi
; Classic recursion, counting moves
; ============================================================

(print "=== Towers of Hanoi ===")

(define move-count 0)

(define (hanoi n src tgt aux)
  (cond ((= n 0) nil)
        (t (progn
             (hanoi (+ n -1) src aux tgt)
             (define move-count (+ move-count 1))
             (print (cat "Move " (cat (nth (+ move-count -1) '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15"))
                    (cat ": disk from " (cat (cond ((eq src 'a) "A")((eq src 'b) "B")(t "C"))
                    (cat " to " (cond ((eq tgt 'a) "A")((eq tgt 'b) "B")(t "C"))))))))
             (hanoi (+ n -1) aux tgt src)))))

; --- 3 disks ---
(print "--- 3 Disks ---")
(define move-count 0)
(hanoi 3 'a 'c 'b)
(print "Total moves:") (print move-count)
(print "Optimal = 2^3 - 1 =") (print (+ (^ 2 3) -1))
(print "Match?") (print (= move-count (+ (^ 2 3) -1)))

; --- Formula verification ---
(print "--- Move counts 2^n - 1 ---")
(print "H(1) =") (print (+ (^ 2 1) -1))
(print "H(2) =") (print (+ (^ 2 2) -1))
(print "H(3) =") (print (+ (^ 2 3) -1))
(print "H(4) =") (print (+ (^ 2 4) -1))
(print "H(5) =") (print (+ (^ 2 5) -1))
(print "H(10) =") (print (+ (^ 2 10) -1))
(print "H(20) =") (print (+ (^ 2 20) -1))

(print "=== Towers of Hanoi Done ===")
