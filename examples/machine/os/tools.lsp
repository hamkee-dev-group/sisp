; ============================================================
; TOOLS — editor, cron, package manager, sysmon, build, debugger
; All operate on the real kernel / VFS, not simulations.
; ============================================================

; ============================================================
; ED — line editor on VFS files
;   (ed-open path)      -> session
;   (ed-print s [n])    -> line n (default: all, numbered)
;   (ed-ins s n text)   -> insert text after line n
;   (ed-del s n)        -> delete line n
;   (ed-sub s old new)  -> substitute first match on every line
;   (ed-write s)        -> save to the VFS
; ============================================================
(define (ed-open path)
  (let ((s (kf-read path)))
    (cond ((null s)
           (cond ((null (fs-resolve-ino path FS-ROOT)) (kf-create path)))
           (list path nil))
          (t (list path (split-lines s))))))
(define (ed-lines s) (cadr s))
(define (ed-set-lines s l) (set-car (cdr s) l))
(define (ed-print s n)
  (cond ((null n) (ed-print-all s 1))
        (t (nth (- n 1) (ed-lines s)))))
(define (ed-print-all s i)
  (cond ((> i (length (ed-lines s))) "")
        (t (cat (cat (num-to-str i) ": ")
                (cat (nth (- i 1) (ed-lines s))
                     (cat nl (ed-print-all s (+ i 1))))))))
(define (ed-ins s n text)
  (let ((l (ed-lines s)))
    (cond ((= n 0) (ed-set-lines s (cons text l)))
          ((>= n (length l)) (ed-set-lines s (append l (list text))))
          (t (ed-set-lines s
                (append (take l n) (cons text (drop l n)))))))
  s)
(define (ed-del s n)
  (let ((l (ed-lines s)))
    (cond ((or (< n 1) (> n (length l))) s)
          (t (ed-set-lines s
                (append (take l (- n 1)) (drop l n))))))
  s)
(define (str-replace line old new)
  (cond ((> (length old) (length line)) line)
        ((str-contains line old)
         (cat (substr 0 (str-find line old) line)
              (cat new
                   (substr (+ (str-find line old) (length old))
                           (- (length line) (+ (str-find line old) (length old)))
                           line))))
        (t line)))
(define (ed-sub s old new)
  (ed-set-lines s (map (lambda (l) (str-replace l old new)) (ed-lines s)))
  s)
(define (ed-write s)
  (kf-write (car s) (str-join (ed-lines s) nl))
  (str-join (list "wrote " (car s)) ""))

; ============================================================
; CRON — run programs on a schedule (driven by the kernel's
; scheduling loop through *k-tick-hook*)
;   (cron-add image-name interval)   -> job id
;   (cron-remove id)
;   (cron-jobs)
; ============================================================
(define *cron-jobs* nil) (define *cron-now* 0) (define *cron-next-id* 1)
(define (cron-add image-name interval)
  (let ((job (list *cron-next-id* image-name interval 0)))
    (setq *cron-next-id* (+ *cron-next-id* 1))
    (setq *cron-jobs* (append *cron-jobs* (list job)))
    (car job)))
(define (cron-remove id)
  (setq *cron-jobs* (remove (lambda (j) (= (car j) id)) *cron-jobs*)))
(define (cron-jobs) *cron-jobs*)
(define (cron-tick)
  (setq *cron-now* (+ *cron-now* 1))
  (map (lambda (j)
         (cond ((>= (- *cron-now* (nth 3 j)) (nth 2 j))
                (set-car (drop j 3) *cron-now*)
                (k-spawn-image (cadr j) 1))))
       *cron-jobs*))
(define (cron-start) (setq *k-tick-hook* cron-tick))
(define (cron-stop) (setq *k-tick-hook* nil))

; ============================================================
; PACKAGE MANAGER — install/remove packages into the VFS.
; A registry entry:
;   (name version (dep ...) ((path content) ...) ((img src) ...))
; files are written to the VFS; img sources are assembled into
; runnable programs (see BUILD).
; ============================================================
(define *pm-registry* nil)
(define *pm-installed* nil)
(define (pm-register entry)
  (setq *pm-registry* (append *pm-registry* (list entry))))
(define (pm-find name) (assoc name *pm-registry* (quote equal)))
(define (pm-deps-ok name)
  (cond ((null name) t)
        (t (let ((ok (or (not (null (member (car name) *pm-installed*)))
                         (pm-install (car name)))))
             (cond ((not (null ok)) (pm-deps-ok (cdr name)))
                   (t nil))))))
(define (member x l)
  (cond ((null l) nil)
        ((equal x (car l)) l)
        (t (member x (cdr l)))))
; mkdir -p for the PARENT directories of a file path
(define (ensure-parents path)
  (let ((comps (cdr (split-path path))))
    (labels (((go prefix cs)
        (cond ((null (cdr cs)) t)
              (t (let ((p (cat prefix (car cs))))
                   (cond ((null (fs-resolve-ino p FS-ROOT))
                          (fs-mkdir p FS-ROOT)))
                   (go (cat p "/") (cdr cs)))))))
      (go "/" comps))))
(define (pm-install name)
  (let ((e (pm-find name)))
    (cond ((null e) (print (str-join (list "pm: " name ": unknown package") "")) nil)
          ((member name *pm-installed*) t)
          ((not (pm-deps-ok (nth 2 e))) (print (str-join (list "pm: " name ": dependency failed") "")) nil)
          (t (map (lambda (f)
                    (ensure-parents (car f))
                    (cond ((null (fs-resolve-ino (car f) FS-ROOT))
                           (kf-create (car f))))
                    (kf-write (car f) (cadr f)))
                  (nth 3 e))
             (map (lambda (img)
                    (pm-install-image (car img) (cadr img)))
                  (nth 4 e))
             (setq *pm-installed* (cons name *pm-installed*))
             (print (str-join (list "pm: installed " name " " (num-to-str (nth 1 e))) ""))
             t))))
(define (pm-remove name)
  (let ((e (pm-find name)))
    (cond ((null e) (str-join (list "pm: " name ": unknown") ""))
          (t (map (lambda (f) (fs-delete (car f) FS-ROOT)) (nth 3 e))
             (setq *pm-installed* (remove (lambda (x) (equal x name)) *pm-installed*))
             (str-join (list "pm: removed " name) "")))))
(define (pm-list) *pm-installed*)
(define (pm-available) (map car *pm-registry*))

; ============================================================
; BUILD — assemble V2.1 source text into a runnable program
;   (build src-text name)  -> installs name in *images*
;   (build-file path name) -> same, reading source from the VFS
; Source is sexp data: a list of assembler statements.
; ============================================================
(define (build src-text name)
  (let ((forms (read-from-string src-text)))
    (cond ((null forms) nil)
          (t (let ((words (assemble (car forms))))
               (setq *images* (cons (list name words)
                                    (remove (lambda (i) (equal (car i) name)) *images*)))
               (str-join (list "built " name " ("
                               (num-to-str (length words)) " words)") ""))))))
(define (build-file path name)
  (let ((src (kf-read path)))
    (cond ((null src) (str-join (list "build: " path ": not found") ""))
          (t (build src name)))))
(define (pm-install-image name src-text) (build src-text name))

; ============================================================
; SYSMON — live system state report
; ============================================================
(define *kern-ticks* 0)
(define (sysmon-tick) (setq *kern-ticks* (+ *kern-ticks* 1)))
(define *fs-ui* 0) (define *fs-ub* 0)
(define (fs-usage)
  (setq *fs-ui* 0) (setq *fs-ub* 0)
  (map (lambda (byte)
         (map (lambda (bit)
                (cond ((= (mod (truncate (/ byte (^ 2 bit))) 2) 1)
                       (setq *fs-ui* (+ *fs-ui* 1)))))
              (iota 8)))
       *fs-ibm*)
  (map (lambda (byte)
         (map (lambda (bit)
                (cond ((= (mod (truncate (/ byte (^ 2 bit))) 2) 1)
                       (setq *fs-ub* (+ *fs-ub* 1)))))
              (iota 8)))
       *fs-dbm*)
  (list *fs-ui* *fs-ub*))
(define (sysmon)
  (let ((u (fs-usage)))
    (str-join
      (list "up " (num-to-str *kern-ticks*) " ticks"
            " | procs " (num-to-str (length *procs*))
            " | mem windows " (num-to-str (length (filter (lambda (s) s) *slots*)))
            "/" (num-to-str NSLOTS)
            " | inodes " (num-to-str (car u)) "/" (num-to-str FS-NINODE)
            " | blocks " (num-to-str (cadr u)) "/" (num-to-str FS-NDATA)
            nl
            (sh-ps))
      "")))

; ============================================================
; DEBUGGER — step/trace machine-code programs on a scratch CPU
;   (dbg-trace name budget)  -> prints a per-instruction trace
;   (dbg-dis name)           -> disassemble an installed image
; ============================================================
(define *dbg-opnames*
  (list "halt" "loadi" "load" "store" "add" "sub" "mul" "div" "mov"
        "jmp" "jz" "jnz" "jmpr" "sys" "call" "ret" "outb" "inb" "ei"
        "di" "tas" "cmp" "jl" "jg" "je" "jne" "shl" "shr" "and" "or"
        "xor" "not" "iret" "push" "pop"))
(define (dbg-opname op)
  (cond ((< op (length *dbg-opnames*)) (nth op *dbg-opnames*))
        (t "???")))
(define (dbg-dis-line w)
  (str-join (list (dbg-opname (hi w)) " "
                  (num-to-str (mid w)) " "
                  (num-to-str (lo w))) ""))
(define (dbg-cpu words)
  (let ((m (list (make-matrix 1 1024) (make-matrix 1 NREG))))
    (load-words m 32 words)
    (rw m FLG 2) (rw m SPI 512) (rw m PCI 32)
    m))
(define (dbg-dis name)
  (let ((img (assoc name *images* (quote equal))))
    (cond ((null img) (str-join (list "dbg: " name ": not found") ""))
          (t (dbg-dis-lines (cadr img) 32)))))
(define (dbg-dis-lines words addr)
  (cond ((null words) "")
        (t (cat (cat (num-to-str addr) ": ")
                (cat (dbg-dis-line (car words))
                     (cat nl (dbg-dis-lines (cdr words) (+ addr 1))))))))
(define (dbg-trace name budget)
  (let ((img (assoc name *images* (quote equal))))
    (cond ((null img) (str-join (list "dbg: " name ": not found") ""))
          (t (let ((m (dbg-cpu (cadr img))))
               (print (str-join (list "trace of " name ":") ""))
               (dbg-step m budget 0)
               (str-join (list "halted=" (num-to-str (halt-p m))
                               " R0=" (num-to-str (rr m 0))) ""))))))
(define (dbg-step m budget n)
  (cond ((= n budget) (print "...budget out..."))
        ((halt-p m) (print "halted"))
        (t (let ((p (rr m PCI)))
             (print (str-join
                      (list (num-to-str p) ": "
                            (dbg-dis-line (rd m p))
                            "   | R0=" (num-to-str (rr m 0))
                            " R1=" (num-to-str (rr m 1))
                            " R2=" (num-to-str (rr m 2))
                            " R3=" (num-to-str (rr m 3))) ""))
             (run1 m (+ n 1) n)
             (dbg-step m budget (+ n 1))))))
