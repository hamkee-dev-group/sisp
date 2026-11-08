; ============================================================
; sBTC — persistence (the ledger on disk)
;
; SISP has no object writer and no way to put a quote char in a
; string literal, so we serialize by hand to a quote-free
; S-expression: strings become (q c0 c1 ...) byte-code lists,
; everything else is numbers/nil/lists.  read-from-string parses
; it back and `deser` rebuilds the original structure.
;
; The saved state is (blocks mempool).  Blocks are replayed
; through chain-add on load, so a corrupted ledger fails to
; validate rather than loading silently.
; ============================================================

(define (stringp x) (equal (typeof x) "string"))

; byte <-> char via hash.lsp's ascii-table
(define (byte->char b)
  (let ((hit (find (lambda (p) (= (cadr p) b)) ascii-table)))
    (cond (hit (car hit)) (t "?"))))

(define (join-nums ns)
  (cond ((eq ns nil) "")
        ((eq (cdr ns) nil) (num-to-str (car ns)))
        (t (cat (cat (num-to-str (car ns)) " ") (join-nums (cdr ns))))))

; ---- serialize object -> string -----------------------------
(define (ser x)
  (cond ((eq x nil) "nil")
        ((numberp x) (num-to-str x))
        ((stringp x) (cat (cat "(q " (join-nums (map char->byte (str-to-list x)))) ")"))
        ((consp x) (cat (cat "(" (ser-elems x)) ")"))
        (t "nil")))
(define (ser-elems xs)
  (cond ((eq xs nil) "")
        ((eq (cdr xs) nil) (ser (car xs)))
        (t (cat (cat (ser (car xs)) " ") (ser-elems (cdr xs))))))

; ---- deserialize parsed form -> object ----------------------
(define sym-q (quote q))
(define (codes->string codes)
  (cond ((eq codes nil) "") (t (list-to-str (map byte->char codes)))))
(define (deser x)
  (cond ((numberp x) x)
        ((consp x) (cond ((eq (car x) sym-q) (codes->string (cdr x)))
                         (t (map deser x))))
        (t x)))

; ---- state (blocks mempool) on disk -------------------------
(define (save-obj path obj) (write-file path (ser obj)))
(define (load-obj path) (deser (car (read-from-string (read-file path)))))

(define (state-save path store pool)
  (save-obj path (list (chain-blocks store) pool)))
(define (load-state path) (load-obj path))
; rebuild the block store by replaying (revalidating) saved blocks
(define (state-store st) (foldl chain-add (chain-empty) (car st)))
(define (state-pool st) (cadr st))
