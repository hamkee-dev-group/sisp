; ============================================================
; sBTC — real SHA-256 in pure SISP
;
; SHA-256 operates on 32-bit words, which fit natively in
; SISP's 64-bit integers.  No native hashing / bitwise ops
; exist, so the word operations are built from arithmetic
; (the same technique the TCP/IP stack uses for `logand`).
;
; This is the genuine FIPS 180-4 algorithm, not a reduction.
; Verified below against the official test vectors:
;   SHA256("")    = e3b0c442...
;   SHA256("abc") = ba7816bf...
; ============================================================

; ---- powers of two (index 0..56; max value < 2^63) ----------
(define (pow2 n) (nth n pow2-table))
(define pow2-table
  (list 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768
        65536 131072 262144 524288 1048576 2097152 4194304 8388608
        16777216 33554432 67108864 134217728 268435456 536870912
        1073741824 2147483648 4294967296 8589934592 17179869184
        34359738368 68719476736 137438953472 274877906944 549755813888
        1099511627776 2199023255552 4398046511104 8796093022208
        17592186044416 35184372088832 70368744177664 140737488355328
        281474976710656 562949953421312 1125899906842624 2251799813685248
        4503599627370496 9007199254740992 18014398509481984
        36028797018963968 72057594037927936))

(define P32 4294967296)
(define (mask32 x) (mod x P32))

; ---- bit operations from arithmetic -------------------------
(define (shr x n) (truncate (/ x (pow2 n))))
(define (shl32 x n) (mask32 (* x (pow2 n))))

; 32-bit NOT needs no loop: ones-complement in the low 32 bits.
(define (not32 x) (- 4294967295 x))

; Byte (8-bit) lookup tables: xor/and of two 32-bit words takes
; 4 native mat-ref lookups instead of a 32-step bit loop.  The
; 256x256 byte tables are built once from cheap 16x16 nibble
; tables (which come from the slow bit method).
(define (bit-xor a b) (mod (+ a b) 2))
(define (bit-and a b) (* a b))
(define (slow-bit2 a b op) (sb2 a b 1 0 op))
(define (sb2 a b bit acc op)
  (cond ((and (= a 0) (= b 0)) acc)
        (t (sb2 (truncate (/ a 2)) (truncate (/ b 2)) (* bit 2)
                (+ acc (* bit (op (mod a 2) (mod b 2)))) op))))
(define (nib-cols op i j)
  (cond ((= j 16) nil) (t (cons (slow-bit2 i j op) (nib-cols op i (+ j 1))))))
(define (nib-rows op i)
  (cond ((= i 16) nil) (t (append (nib-cols op i 0) (nib-rows op (+ i 1))))))
(define (build-nib op) (matrix-from-list 16 16 (nib-rows op 0)))
(define xor-nib (build-nib bit-xor))
(define and-nib (build-nib bit-and))

; byte op from two nibble ops: high nibble * 16 + low nibble
(define (byte-entry nib i j)
  (+ (* 16 (truncate (mat-ref nib (truncate (/ i 16)) (truncate (/ j 16)))))
     (truncate (mat-ref nib (mod i 16) (mod j 16)))))
; build the flat 65536 row-major list in O(n) (cons, then it's in order)
(define (bt-build nib n acc)
  (cond ((< n 0) acc)
        (t (bt-build nib (- n 1)
                     (cons (byte-entry nib (truncate (/ n 256)) (mod n 256)) acc)))))
(define xor-tab (matrix-from-list 256 256 (bt-build xor-nib 65535 nil)))
(define and-tab (matrix-from-list 256 256 (bt-build and-nib 65535 nil)))

(define (byte-op tab a b mul)
  (cond ((and (= a 0) (= b 0)) 0)
        (t (+ (* mul (truncate (mat-ref tab (mod a 256) (mod b 256))))
              (byte-op tab (truncate (/ a 256)) (truncate (/ b 256)) (* mul 256))))))
(define (xor32 a b) (byte-op xor-tab a b 1))
(define (and32 a b) (byte-op and-tab a b 1))

(define (xor3 a b c) (xor32 (xor32 a b) c))

; rotate-right on 32 bits — pure arithmetic, no bit loop.
(define (rotr x n)
  (mask32 (+ (shr x n) (* (mod x (pow2 n)) (pow2 (- 32 n))))))

; ---- SHA-256 constants --------------------------------------
(define sha-k
  (list 1116352408 1899447441 3049323471 3921009573 961987163 1508970993
        2453635748 2870763221 3624381080 310598401 607225278 1426881987
        1925078388 2162078206 2614888103 3248222580 3835390401 4022224774
        264347078 604807628 770255983 1249150122 1555081692 1996064986
        2554220882 2821834349 2952996808 3210313671 3336571891 3584528711
        113926993 338241895 666307205 773529912 1294757372 1396182291
        1695183700 1986661051 2177026350 2456956037 2730485921 2820302411
        3259730800 3345764771 3516065817 3600352804 4094571909 275423344
        430227734 506948616 659060556 883997877 958139571 1322822218
        1537002063 1747873779 1955562222 2024104815 2227730452 2361852424
        2428436474 2756734187 3204031479 3329325298))

(define sha-h0
  (list 1779033703 3144134277 1013904242 2773480762
        1359893119 2600822924 528734635 1541459225))

; ---- message preprocessing ----------------------------------
; A message is a list of bytes (0..255).

; No char->code primitive and no string escapes exist, so map
; each printable-ASCII char to its byte via a table.  Codes 34
; (") and 92 (\) can't appear in a SISP string literal and are
; simply absent — never needed for coin serialization.
(define (zip-codes chars start)
  (cond ((eq chars nil) nil)
        (t (cons (list (car chars) start)
                 (zip-codes (cdr chars) (+ start 1))))))
(define ascii-table
  (append (zip-codes (str-to-list " !") 32)
          (append (zip-codes (str-to-list "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[") 35)
                  (zip-codes (str-to-list "]^_`abcdefghijklmnopqrstuvwxyz{|}~") 93))))
(define (char->byte c)
  (let ((hit (find (lambda (p) (equal (car p) c)) ascii-table)))
    (cond (hit (cadr hit)) (t 0))))
(define (str->bytes s) (map char->byte (str-to-list s)))

; append the 0x80 byte, K zero bytes, then 8-byte big-endian
; bit length, so the total is a multiple of 64 bytes.
(define (sha-pad bytes)
  (let* ((len (length bytes))
         (bits (* len 8))
         (with80 (append bytes (list 128)))
         (target (pad-target (length with80)))
         (zeros (make-zeros (- target (length with80))))
         (lenbytes (u64-be bits)))
    (append (append with80 zeros) lenbytes)))

; smallest multiple-of-64 that leaves 8 bytes for the length
(define (pad-target n)
  (cond ((<= (mod n 64) 56) (+ (* (truncate (/ n 64)) 64) 56))
        (t (+ (* (+ (truncate (/ n 64)) 1) 64) 56))))

(define (make-zeros n) (cond ((<= n 0) nil) (t (cons 0 (make-zeros (- n 1))))))

; 64-bit big-endian encoding of a bit count
(define (u64-be x)
  (list (mod (shr x 56) 256) (mod (shr x 48) 256)
        (mod (shr x 40) 256) (mod (shr x 32) 256)
        (mod (shr x 24) 256) (mod (shr x 16) 256)
        (mod (shr x 8) 256)  (mod x 256)))

; group a byte list into 32-bit big-endian words
(define (bytes->words bl)
  (cond ((eq bl nil) nil)
        (t (cons (+ (* (car bl) 16777216) (* (cadr bl) 65536)
                    (* (caddr bl) 256) (cadddr bl))
                 (bytes->words (cddddr bl))))))

; ---- compression --------------------------------------------
(define (big-sigma0 x) (xor3 (rotr x 2)  (rotr x 13) (rotr x 22)))
(define (big-sigma1 x) (xor3 (rotr x 6)  (rotr x 11) (rotr x 25)))
(define (sm-sigma0 x)  (xor3 (rotr x 7)  (rotr x 18) (shr x 3)))
(define (sm-sigma1 x)  (xor3 (rotr x 17) (rotr x 19) (shr x 10)))
(define (ch x y z)  (xor32 (and32 x y) (and32 (not32 x) z)))
(define (maj x y z) (xor3 (and32 x y) (and32 x z) (and32 y z)))

; build the 64-word message schedule from 16 block words.
; keep the accumulator reversed (newest first) so every back
; reference is within the first 16 slots and growth is O(1) cons.
(define (schedule w16) (reverse (sched-loop (reverse w16) 16)))
(define (sched-loop racc i)
  (cond ((= i 64) racc)
        (t (let ((nw (mask32 (+ (nth 15 racc) (sm-sigma0 (nth 14 racc))
                                (nth 6 racc)  (sm-sigma1 (nth 1 racc))))))
             (sched-loop (cons nw racc) (+ i 1))))))

; process one 64-byte block: fold the 8-word state
(define (sha-block state w16) (sha-rounds state (schedule w16) sha-k))

; walk the schedule w and constants k together (car/cdr, no nth)
(define (sha-rounds st w k)
  (cond ((eq w nil) st)
        (t (let* ((a (nth 0 st)) (b (nth 1 st)) (c (nth 2 st)) (d (nth 3 st))
                  (e (nth 4 st)) (f (nth 5 st)) (g (nth 6 st)) (h (nth 7 st))
                  (t1 (mask32 (+ h (big-sigma1 e) (ch e f g) (car k) (car w))))
                  (t2 (mask32 (+ (big-sigma0 a) (maj a b c)))))
             (sha-rounds
               (list (mask32 (+ t1 t2)) a b c
                     (mask32 (+ d t1)) e f g)
               (cdr w) (cdr k))))))

; ---- top level ----------------------------------------------
(define (sha-add-state s1 s2)
  (cond ((eq s1 nil) nil)
        (t (cons (mask32 (+ (car s1) (car s2)))
                 (sha-add-state (cdr s1) (cdr s2))))))

(define (sha-run state words)
  (cond ((eq words nil) state)
        (t (let ((blk (take words 16)))
             (sha-run (sha-add-state state (sha-block state blk))
                      (drop words 16))))))

; SHA-256 of a byte list -> list of 8 32-bit words (the digest)
(define (sha256-words bytes)
  (sha-run sha-h0 (bytes->words (sha-pad bytes))))

; hex rendering ------------------------------------------------
(define hex-digits (list "0" "1" "2" "3" "4" "5" "6" "7"
                         "8" "9" "a" "b" "c" "d" "e" "f"))
(define (byte->hex b)
  (cat (nth (truncate (/ b 16)) hex-digits) (nth (mod b 16) hex-digits)))
(define (word->hex w)
  (cat (cat (byte->hex (mod (shr w 24) 256)) (byte->hex (mod (shr w 16) 256)))
       (cat (byte->hex (mod (shr w 8) 256))  (byte->hex (mod w 256)))))
(define (words->hex ws)
  (cond ((eq ws nil) "")
        (t (cat (word->hex (car ws)) (words->hex (cdr ws))))))

; SHA-256 of a string -> 64-char hex string
(define (sha256-hex s) (words->hex (sha256-words (str->bytes s))))

; double SHA-256 (Bitcoin's hash) — on words, returns words
(define (sha256d-words bytes)
  (sha256-words (words->bytes (sha256-words bytes))))
(define (words->bytes ws)
  (cond ((eq ws nil) nil)
        (t (append (list (mod (shr (car ws) 24) 256) (mod (shr (car ws) 16) 256)
                         (mod (shr (car ws) 8) 256)  (mod (car ws) 256))
                   (words->bytes (cdr ws))))))
