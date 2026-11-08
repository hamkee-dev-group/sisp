; ============================================================
; lib/collections/alist.lsp -- association-list operations
;
; Public:   (alist-get al key) -> value | nil
;           (alist-set al key val) -> alist
; Private:  none
; Depends:  none (uses builtin equal, null, car, cdr, caar, cdar, cons, cond, list, quote)
; Assumes:  keys are compared with equal (deep structural equality)
; ============================================================

(define (alist-get al key)
  "value bound to key in association list al, or nil if absent (structural keys)"
  (cond ((null al) nil)
        ((equal (caar al) key) (cdar al))
        (t (alist-get (cdr al) key))))

(define (alist-set al key val)
  "association list with key bound to val, shadowing any prior binding"
  (cons (cons key val) al))
