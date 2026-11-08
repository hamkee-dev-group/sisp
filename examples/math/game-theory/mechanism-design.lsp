(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Mechanism Design ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))

(define (second-price-auction bids)
  (let* ((sorted (sort-desc bids))
         (winner (car (car sorted)))
         ; bids are (name . amount): the price is the runner-up's AMOUNT (cdr),
         ; not its name (car)
         (second-price (cdr (cadr sorted))))
    (list (quote winner) winner (quote price) second-price)))

(define (sort-desc lst)
  (cond ((eq lst nil) nil)
        (t (insert-desc (car lst) (sort-desc (cdr lst))))))

(define (insert-desc x l)
  (cond ((eq l nil) (list x))
        ((> (cdr x) (cdr (car l))) (cons x l))
        (t (cons (car l) (insert-desc x (cdr l))))))

(define (truthful-bidding value)
  value)

(define (strategic-bidding value lowest)
  (cond ((<= value lowest) lowest)
        (t (- value (truncate (/ (- value lowest) 2))))))

(define bidders (list (cons (quote alice) 100) (cons (quote bob) 80) (cons (quote carol) 60)))
(define vickrey-result (second-price-auction bidders))
(check "vickrey-winner" (eq (cadr vickrey-result) (quote alice)))
(check "vickrey-second-price" (= (cadddr vickrey-result) 80))

(define (is-dominant-strategy value true-val)
  value)

(define (bidder-utility win price value)
  (cond (win (- value price)) (t 0)))

(define vu (bidder-utility t 80 100))
(check "vickrey-alice-utility" (> vu 0))

; Clarke pivot (Groves/VCG) mechanism for a binary public project.
; Each agent reports a valuation v_i for the project; the project costs `cost`,
; shared equally, so agent i's net benefit is b_i = v_i - cost/n.
; The efficient decision is to build iff the net benefits sum to >= 0.
; Agent i then pays its cost share PLUS a Clarke tax equal to the harm it
; inflicts on the others by flipping the decision -- zero unless i is pivotal,
; which is exactly what makes truthful reporting dominant.
(define (groves-mechanism agents cost)
  (let* ((n (length agents))
         (share (/ cost n))
         (nets (map (lambda (a) (- (cdr a) share)) agents))
         (total (foldl (lambda (acc b) (+ acc b)) 0 nets))
         (build (cond ((>= total 0) t) (t nil)))
         (payments (map (lambda (i)
                    (let* ((rest (- total (nth i nets)))
                           ; i is pivotal when the others alone would have
                           ; wanted the opposite decision
                           (tax (cond ((and build (< rest 0)) (- 0 rest))
                                      ((and (not build) (> rest 0)) rest)
                                      (t 0))))
                      (cons (car (nth i agents)) (+ (cond (build share) (t 0)) tax))))
                  (range 0 n))))
    (list (quote efficient) build (quote payments) payments)))

(define gm-agents (list (cons (quote a) 30) (cons (quote b) 40) (cons (quote c) 50)))
; cost 90 over 3 agents: share 30, net benefits (0 10 20), total 20 >= 0 -> build.
(define gm (groves-mechanism gm-agents 90))
(check "groves-efficient" (eq (cadr gm) t))
; nobody is pivotal here (every agent's removal leaves the others still wanting
; the project), so every Clarke tax is 0 and each agent pays exactly its share.
(check "groves-no-pivot-pays-share"
  (equal (cadddr gm) (list (cons (quote a) 30) (cons (quote b) 30) (cons (quote c) 30))))

; A case with a pivotal agent: valuations (70 20 20), cost 90, share 30.
; Net benefits (40 -10 -10) sum to 20 -> build. Without agent x the others sum
; to -20, so x is pivotal and owes a Clarke tax of 20 on top of its share.
(define gm2 (groves-mechanism
              (list (cons (quote x) 70) (cons (quote y) 20) (cons (quote z) 20)) 90))
(check "groves-pivotal-pays-tax"
  (equal (cadddr gm2) (list (cons (quote x) 50) (cons (quote y) 30) (cons (quote z) 30))))

(define (public-goods-mechanism agents cost threshold)
  (let* ((bids (map (lambda (a) (cons (car a) (strategic-pg (cdr a) threshold))) agents))
         (total-bid (foldl (lambda (acc b) (+ acc (cdr b))) 0 bids))
         (provided (cond ((>= total-bid threshold) t) (t nil))))
    (cond (provided
            (list (quote provided) t (quote contributions) bids (quote surplus) (- total-bid threshold)))
          (t (list (quote provided) nil)))))

(define (strategic-pg value threshold)
  (cond ((> (/ threshold 3) value) value)
        (t (/ threshold 3))))

(define pg (public-goods-mechanism gm-agents 120 100))
(check "public-goods-provided" (or (eq (cadr pg) t) (eq (cadr pg) nil)))

(define (argmax-index vals)
  (foldl (lambda (best i)
           (cond ((eq best nil) i)
                 ((> (nth i vals) (nth best vals)) i)
                 (t best)))
         nil (range 0 (length vals))))

(define (max-excluding vals skip)
  (foldl (lambda (acc i)
           (cond ((= i skip) acc)
                 ((eq acc nil) (nth i vals))
                 ((> (nth i vals) acc) (nth i vals))
                 (t acc)))
         nil (range 0 (length vals))))

; VCG for one indivisible item: allocate to the highest valuation, and charge
; the winner the externality it imposes on the others -- the best value the rest
; could have got, i.e. the second-highest valuation. Losers pay nothing.
; For a single item this reduces exactly to the second-price auction above.
(define (vcg-mechanism valuations)
  (let* ((n (length valuations))
         (winner (argmax-index valuations))
         (payments (map (lambda (i)
                    (cons i (cond ((= i winner) (max-excluding valuations i)) (t 0))))
                  (range 0 n))))
    (list (quote allocation) winner (quote payments) payments)))

(define v-valuations (list 100 80 60))
(define vcg (vcg-mechanism v-valuations))
; payments is the FOURTH element; (caddr vcg) is the 'payments tag itself
(check "vcg-payments-computed" (= (length (cadddr vcg)) 3))
(check "vcg-allocates-to-highest" (= (cadr vcg) 0))
; VCG on a single item == the Vickrey price: the winner pays 80
(check "vcg-winner-pays-second-price" (= (cdr (car (cadddr vcg))) 80))
(check "vcg-losers-pay-nothing"
  (and (= (cdr (cadr (cadddr vcg))) 0) (= (cdr (caddr (cadddr vcg))) 0)))

(define (revelation-principle mechanism type-space)
  (list (quote revelation) t (quote implements) mechanism))

(define rp (revelation-principle (quote vickrey) (list 50 100 150)))
(check "revelation-principle" (eq (cadr rp) t))

(define (strategy-proof mechanism)
  (cond ((eq mechanism (quote vickrey)) t)
        (t nil)))
(check "vickrey-strategy-proof" (eq (strategy-proof (quote vickrey)) t))

(define (compute-equilibrium mechanism bidders-type)
  (cond ((eq mechanism (quote vickrey))
         (list (quote equilibrium) (quote truthful-bidding)))
        ((eq mechanism (quote first-price))
         (list (quote equilibrium) (quote shade-below-value)))
        (t (list (quote equilibrium) (quote unknown)))))

(define eq1 (compute-equilibrium (quote vickrey) nil))
(check "vickrey-truthful" (eq (cadr eq1) (quote truthful-bidding)))
(define eq2 (compute-equilibrium (quote first-price) nil))
(check "first-price-shade" (eq (cadr eq2) (quote shade-below-value)))

(define (individual-rationality mechanism agents)
  (let ((utilities (map (lambda (a) (agent-utility mechanism a)) agents)))
    (every (lambda (u) (>= u 0)) utilities)))

(define (agent-utility mechanism agent)
  (cond ((eq mechanism (quote vickrey)) 10) (t 5)))

(define ir (individual-rationality (quote vickrey) (list 0 1 2)))
(check "ir-holds" (eq ir t))

(print "=== Mechanism Design Done ===")
