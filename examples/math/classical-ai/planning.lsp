(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== STRIPS Planning ===")

; ---------------------------------------------------------------------------
; A STRIPS action is (name preconditions add-list delete-list).
; A state is a LIST OF FLUENTS, and a fluent is a structured term such as
; (on a b), (on-table c) or (clear a).  Flattening those into loose symbols
; would make (on a b) and (on b a) indistinguishable and every goal test
; vacuously true, so nothing here is ever spliced.
; ---------------------------------------------------------------------------

(define (action name pre add del)
  (list name pre add del))
(define (act-name a) (car a))
(define (act-pre a) (cadr a))
(define (act-add a) (caddr a))
(define (act-del a) (cadddr a))

; append one level only -- `flatmap` deep-flattens and would destroy fluents
(define (concat-map f l) (foldr append nil (map f l)))

; set operations over LISTS of fluents, compared with `equal`
(define (subsetp sub s) (every (lambda (f) (contains f s)) sub))
(define (list-diff a b) (filter (lambda (x) (not (contains x b))) a))
(define (list-union a b) (append a (filter (lambda (x) (not (contains x a))) b)))

(define (applicable a state)
  (subsetp (act-pre a) state))

; the STRIPS transition: (state \ delete-list) u add-list
(define (apply-action a state)
  (list-union (act-add a) (list-diff state (act-del a))))

(define (last-list l) (cond ((eq (cdr l) nil) (car l)) (t (last-list (cdr l)))))

; ---- blocks world action schemas, fully grounded ---------------------------
(define blocks (list (quote a) (quote b) (quote c)))
(define (other-blocks x) (filter (lambda (y) (not (eq y x))) blocks))

; put x, which is sitting on the table, onto the clear block y
(define (act-stack-from-table x y)
  (action (list (quote stack) x (quote onto) y)
          (list (list (quote on-table) x) (list (quote clear) x) (list (quote clear) y))
          (list (list (quote on) x y))
          (list (list (quote on-table) x) (list (quote clear) y))))

; take x off y and put it on the table; y becomes clear
(define (act-unstack-to-table x y)
  (action (list (quote unstack) x (quote from) y)
          (list (list (quote on) x y) (list (quote clear) x))
          (list (list (quote on-table) x) (list (quote clear) y))
          (list (list (quote on) x y))))

; move x from y directly onto the clear block z
(define (act-move x y z)
  (action (list (quote move) x (quote from) y (quote onto) z)
          (list (list (quote on) x y) (list (quote clear) x) (list (quote clear) z))
          (list (list (quote on) x z) (list (quote clear) y))
          (list (list (quote on) x y) (list (quote clear) z))))

(define bw-actions
  (append
    (concat-map (lambda (x) (map (lambda (y) (act-stack-from-table x y)) (other-blocks x))) blocks)
    (append
      (concat-map (lambda (x) (map (lambda (y) (act-unstack-to-table x y)) (other-blocks x))) blocks)
      (concat-map (lambda (x)
        (concat-map (lambda (y)
          (map (lambda (z) (act-move x y z)) (filter (lambda (z) (not (eq z y))) (other-blocks x))))
          (other-blocks x)))
        blocks))))

; ---- forward (progression) search ------------------------------------------
; depth-limited DFS; `seen` breaks the cycles the blocks world is full of
; (stack then unstack returns you to where you started).
; Iterative deepening, so the first plan found is a SHORTEST one -- a plain
; depth-limited DFS would happily return a padded plan with detours in it.
(define (forward-search state goal actions depth max-depth)
  (iddfs state goal actions depth max-depth))

(define (iddfs state goal actions limit max-depth)
  (cond ((> limit max-depth) nil)
        (t (let ((p (fwd state goal actions 0 limit (list state))))
             (cond ((eq p nil) (iddfs state goal actions (+ limit 1) max-depth))
                   (t p))))))

(define (fwd state goal actions depth max-depth seen)
  (cond ((subsetp goal state) (list state))
        ((>= depth max-depth) nil)
        (t (try-actions state goal
                        (filter (lambda (a) (applicable a state)) actions)
                        actions depth max-depth seen))))

(define (try-actions state goal valid all-actions depth max-depth seen)
  (cond ((eq valid nil) nil)
        (t (let ((ns (apply-action (car valid) state)))
             (cond ((state-seen ns seen)
                    (try-actions state goal (cdr valid) all-actions depth max-depth seen))
                   (t (let ((path (fwd ns goal all-actions (+ depth 1) max-depth (cons ns seen))))
                        (cond ((eq path nil) (try-actions state goal (cdr valid) all-actions depth max-depth seen))
                              (t (cons state path))))))))))

; states are sets, so compare them as sets rather than as lists
(define (same-state p q) (and (subsetp p q) (subsetp q p)))
(define (state-seen s seen) (any (lambda (x) (same-state s x)) seen))

; ---- backward (regression) search ------------------------------------------
; An action is relevant to a goal when it establishes part of it and destroys
; none of it.
(define (relevantp a goal)
  (and (any (lambda (p) (contains p goal)) (act-add a))
       (not (any (lambda (p) (contains p goal)) (act-del a)))))

(define (backward-search goal current-state actions depth max-depth)
  (cond ((subsetp goal current-state) (list goal))
        ((>= depth max-depth) nil)
        (t (try-backward goal current-state
                         (filter (lambda (a) (relevantp a goal)) actions)
                         actions depth max-depth))))

(define (try-backward goal state relevant actions depth max-depth)
  (cond ((eq relevant nil) nil)
        (t (let* ((a (car relevant))
                  (new-goal (list-union (list-diff goal (act-add a)) (act-pre a)))
                  (path (backward-search new-goal state actions (+ depth 1) max-depth)))
             (cond ((eq path nil) (try-backward goal state (cdr relevant) actions depth max-depth))
                   (t (cons goal path)))))))

(define (legal-transitions path actions)
  (cond ((eq (cdr path) nil) t)
        ((any (lambda (a) (and (applicable a (car path))
                               (same-state (apply-action a (car path)) (cadr path))))
              actions)
         (legal-transitions (cdr path) actions))
        (t nil)))

; ---- problem 1: a on b, b and c on the table  ->  a on b on c --------------
(define bw-initial-state
  (list (list (quote on) (quote a) (quote b))
        (list (quote on-table) (quote b))
        (list (quote on-table) (quote c))
        (list (quote clear) (quote a))
        (list (quote clear) (quote c))))
(define bw-goal
  (list (list (quote on) (quote a) (quote b))
        (list (quote on) (quote b) (quote c))
        (list (quote on-table) (quote c))))

(define fwd-path (forward-search bw-initial-state bw-goal bw-actions 0 6))
(check "strips-forward-solves" (not (eq fwd-path nil)))
(check "strips-goal-reached" (subsetp bw-goal (cond ((eq fwd-path nil) nil) (t (last-list fwd-path)))))
(check "strips-path-starts-at-initial-state" (same-state (car fwd-path) bw-initial-state))
; a sits on b but must end up on b ON TOP OF c, so a has to come off first:
; unstack a, stack b onto c, stack a onto b -- 3 actions, hence 4 states.
(check "strips-plan-is-3-actions" (= (length fwd-path) 4))
; every consecutive pair must be joined by some genuinely applicable action
(check "strips-plan-steps-are-legal" (legal-transitions fwd-path bw-actions))

; the degenerate reading of the goal must NOT hold in the initial state
(check "strips-goal-not-already-true" (not (subsetp bw-goal bw-initial-state)))

; ---- problem 2: two blocks on the table -> a on b --------------------------
(define simple-init
  (list (list (quote on-table) (quote a))
        (list (quote on-table) (quote b))
        (list (quote clear) (quote a))
        (list (quote clear) (quote b))))
(define simple-goal
  (list (list (quote on) (quote a) (quote b))
        (list (quote on-table) (quote b))))
(define simple-acts (list (act-stack-from-table (quote a) (quote b))))

(define sp (forward-search simple-init simple-goal simple-acts 0 5))
(check "strips-two-block-stack" (not (eq sp nil)))
(check "strips-two-block-one-action" (= (length sp) 2))
(check "strips-two-block-goal-holds" (subsetp simple-goal (last-list sp)))

; ---- regression search reaches the initial state ---------------------------
(define bwd-path (backward-search bw-goal bw-initial-state bw-actions 0 5))
(check "strips-backward-solves" (not (eq bwd-path nil)))
(check "strips-backward-reaches-initial" (subsetp (last-list bwd-path) bw-initial-state))

(print "=== STRIPS Planning Done ===")
