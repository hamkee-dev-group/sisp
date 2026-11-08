; ============================================================
; name: dancing-links
; file: algorithms/searching/dancing-links.lsp
; subject: searching
; capability: search backtracking exact-cover
; aliases: "dlx" "dancing links" "algorithm x" "knuth dlx"
; signature: (search k) counts the exact covers of the matrix at *hdr*
;            into *solutions*
; complexity: exponential worst case; efficient exact-cover via O(1) cover/uncover
; see-also: exact-cover
; derived-from: Knuth 2000 ("Dancing Links"); TAOCP 4B 7.2.2.1
; status: example (tested)
; checks: 4 - cover/uncover inverse, solution count cross-checked vs Algorithm X
; ============================================================
; Dancing Links (TAOCP Vol 4B, 7.2.2.1)
; Four-way sparse links (L,R,U,D pointers on nodes), cover/uncover
; as exact inverse operations, solve a domino 2x3 exact-cover
; instance. Cross-check solution COUNT = 3 against the set-based
; Algorithm X in exact-cover.lsp on the same instance.
; ============================================================

(print "=== Dancing Links ===")

(define (test name ok)
  (cond (ok (print (list (quote pass) name)))
        (t (print (list (quote FAIL) name)))))

; ---------- node: (id L R U D col-hdr) ----------
(define (nd-id nd) (car nd))
(define (nd-L nd) (cadr nd))
(define (nd-R nd) (caddr nd))
(define (nd-U nd) (cadddr nd))
(define (nd-D nd) (car (cddddr nd)))
(define (nd-col nd) (cadr (cddddr nd)))
(define (nd-set-L nd v) (set-car (cdr nd) v))
(define (nd-set-R nd v) (set-car (cddr nd) v))
(define (nd-set-U nd v) (set-car (cdddr nd) v))
(define (nd-set-D nd v) (set-car (cddddr nd) v))
(define (make-node id col) (list id nil nil nil nil col))

; ---------- cover/uncover as exact inverse operations ----------

(define (cover-col fst)
  (nd-set-R (nd-L fst) (nd-R fst))
  (nd-set-L (nd-R fst) (nd-L fst))
  (cover-down (nd-D fst) fst)
  nil)

(define (cover-down r col)
  (cond ((eq r col) nil)
        (t (cover-right (nd-R r) r)
           (cover-down (nd-D r) col))))

(define (cover-right j start)
  (cond ((eq j start) nil)
        (t (nd-set-D (nd-U j) (nd-D j))
           (nd-set-U (nd-D j) (nd-U j))
           (cover-right (nd-R j) start))))

(define (uncover-col fst)
  (uncover-up (nd-U fst) fst)
  (nd-set-R (nd-L fst) fst)
  (nd-set-L (nd-R fst) fst))

(define (uncover-up r col)
  (cond ((eq r col) nil)
        (t (uncover-left (nd-L r) r)
           (uncover-up (nd-U r) col))))

(define (uncover-left j start)
  (cond ((eq j start) nil)
        (t (nd-set-D (nd-U j) j)
           (nd-set-U (nd-D j) j)
           (uncover-left (nd-L j) start))))

; ---------- column size (tail-recursive, no labels) ----------
(define (col-size col) (col-walk (nd-D col) col 0))
(define (col-walk nd col cnt)
  (cond ((eq nd col) cnt)
        (t (col-walk (nd-D nd) col (+ cnt 1)))))

; ---------- choose column with fewest entries ----------
(define (best-col hdr) (best-walk (nd-R hdr) hdr (nd-R hdr) 100000))
(define (best-walk c hdr best bsz)
  (cond ((eq c hdr) best)
        (t (let ((sz (col-size c)))
          (cond ((< sz bsz) (best-walk (nd-R c) hdr c sz))
                (t (best-walk (nd-R c) hdr best bsz)))))))

; ---------- search (Algorithm X, depth-first) ----------
(setq *hdr* nil)
(setq *solutions* 0)

(define (search k)
  (cond ((eq (nd-R *hdr*) *hdr*)
    (setq *solutions* (+ *solutions* 1))
    nil)
        (t (let ((col (best-col *hdr*)))
          (cond ((= (col-size col) 0) nil)
                (t (cover-col col)
                   (try-rows (nd-D col) col k)
                   (uncover-col col)
                   nil))))))

(define (try-rows r col k)
  (cond ((eq r col) nil)
        (t (cover-row-others (nd-R r) r)
           (search (+ k 1))
           (uncover-row-others (nd-L r) r)
           (try-rows (nd-D r) col k))))

(define (cover-row-others j start)
  (cond ((eq j start) nil)
        (t (cover-col (nd-col j))
           (cover-row-others (nd-R j) start))))

(define (uncover-row-others j start)
  (cond ((eq j start) nil)
        (t (uncover-col (nd-col j))
           (uncover-row-others (nd-L j) start))))

; ---------- build DLX structure ----------
(define (init-col-header nd)
  (nd-set-U nd nd)
  (nd-set-D nd nd))

(define (build-row-nodes colhdrs)
  (cond ((eq colhdrs nil) nil)
        (t (let ((ch (car colhdrs)))
          (let ((nd (make-node (quote row) ch)))
            (nd-set-U nd (nd-U ch))
            (nd-set-D nd ch)
            (nd-set-D (nd-U ch) nd)
            (nd-set-U ch nd)
            (cons nd (build-row-nodes (cdr colhdrs))))))))

(define (link-row-nodes nodes)
  (cond ((eq nodes nil) nil)
        (t (let ((fst (car nodes))
                 (lst (last nodes)))
          (nd-set-R lst fst)
          (nd-set-L fst lst)
          (link-pairs nodes)))))

(define (link-pairs nodes)
  (cond ((eq (cdr nodes) nil) nil)
        (t (let ((a (car nodes))
                 (b (cadr nodes)))
          (nd-set-R a b)
          (nd-set-L b a)
          (link-pairs (cdr nodes))))))

(define (add-row-dlx colhdrs)
  (let ((nodes (build-row-nodes colhdrs)))
    (link-row-nodes nodes)
    nil))

(define (build-domino-dlx)
  (let* ((c0 (make-node (quote c0) nil))
         (c1 (make-node (quote c1) nil))
         (c2 (make-node (quote c2) nil))
         (c3 (make-node (quote c3) nil))
         (c4 (make-node (quote c4) nil))
         (c5 (make-node (quote c5) nil))
         (root (make-node (quote root) nil))
         (chs (list c0 c1 c2 c3 c4 c5)))
    (foreach (lambda (h) (progn (set-car (cddddr h) h) (init-col-header h) nil)) chs)
    (progn (set-car (cddddr root) root) (init-col-header root) nil)
    (nd-set-R root c0) (nd-set-L c0 root)
    (nd-set-R c0 c1) (nd-set-L c1 c0)
    (nd-set-R c1 c2) (nd-set-L c2 c1)
    (nd-set-R c2 c3) (nd-set-L c3 c2)
    (nd-set-R c3 c4) (nd-set-L c4 c3)
    (nd-set-R c4 c5) (nd-set-L c5 c4)
    (nd-set-R c5 root) (nd-set-L root c5)
    (add-row-dlx (list c0 c1))
    (add-row-dlx (list c1 c2))
    (add-row-dlx (list c3 c4))
    (add-row-dlx (list c4 c5))
    (add-row-dlx (list c0 c3))
    (add-row-dlx (list c1 c4))
    (add-row-dlx (list c2 c5))
    (setq *hdr* root)
    t))

(build-domino-dlx)

; ---------- cover/uncover really are exact inverses ----------
; Signature of the live matrix: (column-id . column-size) for every
; column still linked into the header ring, in ring order.
(define (ring-sig c h)
  (cond ((eq c h) nil)
        (t (cons (list (nd-id c) (col-size c)) (ring-sig (nd-R c) h)))))
(define (matrix-sig h) (ring-sig (nd-R h) h))

(setq *sig-before* (matrix-sig *hdr*))
; 2x3 domino instance: c1 and c4 hold 3 rows each, the other columns 2.
(test "dlx-initial-column-sizes"
  (equal *sig-before*
         (list (list (quote c0) 2) (list (quote c1) 3) (list (quote c2) 2)
               (list (quote c3) 2) (list (quote c4) 3) (list (quote c5) 2))))

(setq *victim* (nd-R *hdr*))
(cover-col *victim*)
(setq *sig-covered* (matrix-sig *hdr*))
; covering c0 unlinks it and shrinks c1 (row c0c1) and c3 (row c0c3) by one
(test "dlx-cover-removes-column-and-its-rows"
  (equal *sig-covered*
         (list (list (quote c1) 2) (list (quote c2) 2) (list (quote c3) 1)
               (list (quote c4) 3) (list (quote c5) 2))))

(uncover-col *victim*)
(test "dlx-uncover-is-exact-inverse-of-cover"
  (equal (matrix-sig *hdr*) *sig-before*))

(setq *solutions* 0)
(search 0)
(print (list (quote dlx-domino-2x3-solutions) *solutions*))

(test "dlx-domino-2x3-matches-algx-3-solutions" (= *solutions* 3))

(print "=== Dancing Links Done ===")
