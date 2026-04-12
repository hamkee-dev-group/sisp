; ============================================================
; 35-syllogisms.lsp
; Model Aristotelian syllogisms using set theory.
; "All A are B"  = subset(A, B)
; "No A are B"   = cap(A, B) is empty
; "Some A are B" = cap(A, B) is not empty
; Validate Barbara, Celarent, Darii, Ferio.
; ============================================================

(print "=== Aristotelian Syllogisms via Set Theory ===")

; --- Helper: check if intersection of two finite sets is non-empty ---
(define (has-common a b)
  (if (equal a nil) nil
    (if (in (car a) b) t
        (has-common (cdr a) b))))

; --- Helper: check if intersection is empty ---
(define (no-common a b) (not (has-common a b)))

; ============================================================
; Define our universe of entities
; ============================================================
(define Humans    {socrates plato aristotle cicero})
(define Mortals   {socrates plato aristotle cicero rover felix})
(define Animals   {rover felix socrates plato aristotle cicero})
(define Greeks    {socrates plato aristotle})
(define Romans    {cicero})
(define Dogs      {rover})
(define Cats      {felix})
(define Stones    {rock1 rock2})

; ============================================================
; BARBARA (AAA-1): All M are P, All S are M => All S are P
; "All humans are mortal, All Greeks are human => All Greeks are mortal"
; ============================================================
(print "--- BARBARA (AAA-1) ---")
(print "All Humans are Mortals:")
(print (subset Humans Mortals))
(print "All Greeks are Humans:")
(print (subset Greeks Humans))

; Conclusion: All Greeks are Mortals
(define barbara-valid
  (if (and (subset Humans Mortals) (subset Greeks Humans))
      (subset Greeks Mortals)
      nil))
(print "Conclusion - All Greeks are Mortals:")
(print barbara-valid)
(if barbara-valid
    (print "VERIFIED: Barbara is VALID")
    (print "FAILED"))

; ============================================================
; CELARENT (EAE-1): No M are P, All S are M => No S are P
; "No dogs are cats, All Rovers are dogs => No Rovers are cats"
; ============================================================
(print "--- CELARENT (EAE-1) ---")
(print "No Dogs are Cats:")
(print (no-common Dogs Cats))
(print "All Dogs are Dogs (trivial):")
(print (subset Dogs Dogs))

; Conclusion: No Dogs are Cats
(define celarent-valid
  (if (and (no-common Dogs Cats) (subset Dogs Dogs))
      (no-common Dogs Cats)
      nil))
(print "Conclusion - No Dogs are Cats:")
(print celarent-valid)
(if celarent-valid
    (print "VERIFIED: Celarent is VALID")
    (print "FAILED"))

; ============================================================
; DARII (AII-1): All M are P, Some S are M => Some S are P
; "All Humans are Animals, Some Greeks are Humans => Some Greeks are Animals"
; ============================================================
(print "--- DARII (AII-1) ---")
(print "All Humans are Animals:")
(print (subset Humans Animals))
(print "Some Greeks are Humans:")
(print (has-common Greeks Humans))

; Conclusion: Some Greeks are Animals
(define darii-valid
  (if (and (subset Humans Animals) (has-common Greeks Humans))
      (has-common Greeks Animals)
      nil))
(print "Conclusion - Some Greeks are Animals:")
(print darii-valid)
(if darii-valid
    (print "VERIFIED: Darii is VALID")
    (print "FAILED"))

; ============================================================
; FERIO (EIO-1): No M are P, Some S are M => Some S are not P
; "No Stones are Animals, Some things are Stones =>
;  Some things (Stones) are not Animals"
; ============================================================
(print "--- FERIO (EIO-1) ---")
(define Things (union Stones Animals))
(print "No Stones are Animals:")
(print (no-common Stones Animals))
(print "Some Things are Stones:")
(print (has-common Things Stones))

; "Some S are not P" means cap(S, comp(P)) is non-empty
; i.e., diff(S, P) is non-empty -- some things are not animals
(define things-not-animals (\ Things Animals))
(define ferio-valid
  (if (and (no-common Stones Animals) (has-common Things Stones))
      (has-common Things things-not-animals)
      nil))
(print "Conclusion - Some Things are not Animals:")
(print ferio-valid)
(if ferio-valid
    (print "VERIFIED: Ferio is VALID")
    (print "FAILED"))

(print "=== Syllogisms Complete ===")
