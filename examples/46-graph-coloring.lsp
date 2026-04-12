; ============================================================
; 46 — Graph Coloring via Sets
; Color assignment + chromatic number analysis
; UNIQUE: adjacency, color sets, constraint checking natively
; ============================================================

(print "=== Graph Coloring ===")

; --- K_3 (triangle) ---
(define K3-V {1 2 3})
(define K3-E {(1 . 2) (1 . 3) (2 . 3)})
(print "K_3 vertices:") (print K3-V)
(print "K_3 edges:") (print K3-E)

; Colors
(define colors {r g b})

; A valid 3-coloring: 1=r, 2=g, 3=b
(define (color-of v)
  (cond ((= v 1) 'r) ((= v 2) 'g) ((= v 3) 'b) (t nil)))

; Verify: check all edges have different-colored endpoints
(define (check-edge u v)
  (not (eq (color-of u) (color-of v))))

(print "--- K_3 coloring: 1=r, 2=g, 3=b ---")
(print "Edge 1-2 valid?") (print (check-edge 1 2))
(print "Edge 1-3 valid?") (print (check-edge 1 3))
(print "Edge 2-3 valid?") (print (check-edge 2 3))
(print "chi(K_3) = 3")

; --- Bipartite graph ---
(print "--- Bipartite K_{2,2} ---")
(define bip-left {1 2})
(define bip-right {3 4})
(define bip-E {(1 . 3) (1 . 4) (2 . 3) (2 . 4)})
(print "Left:") (print bip-left)
(print "Right:") (print bip-right)
(print "Edges:") (print bip-E)

; 2-coloring: left=r, right=g
(print "2-coloring: left=r, right=g")
(print "Partitions disjoint?")
(print (equal (cap bip-left bip-right) (diff {1} {1})))
(print "chi(K_{2,2}) = 2")

; --- Path graph P_5: 1-2-3-4-5 ---
(print "--- Path P_5 ---")
(define path-E {(1 . 2) (2 . 3) (3 . 4) (4 . 5)})
(print "Edges:") (print path-E)
(print "2-colorable: alternating r,g,r,g,r")
(print "chi(P_5) = 2")

; --- Cycle C_5: 1-2-3-4-5-1 (odd cycle) ---
(print "--- Cycle C_5 (odd cycle) ---")
(define C5-E {(1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 1)})
(print "Edges:") (print C5-E)
(print "Odd cycle needs 3 colors!")
(print "chi(C_5) = 3")

; --- Complete K_4 ---
(print "--- Complete K_4 ---")
(define K4-E {(1 . 2) (1 . 3) (1 . 4) (2 . 3) (2 . 4) (3 . 4)})
(print "|E(K_4)| =") (print (ord K4-E))
(print "chi(K_n) = n, so chi(K_4) = 4")

; --- Four color theorem analogy ---
(print "--- Bounds ---")
(print "Planar graphs: chi <= 4 (Four Color Theorem)")
(print "General: chi <= max_degree + 1 (Brook's)")
(print "Bipartite iff chi = 2 iff no odd cycles")

(print "=== Graph Coloring Done ===")
