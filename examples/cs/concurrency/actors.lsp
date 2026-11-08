; Actor Model — Hewitt's asynchronous agents.
; Each actor is an entry (id mailbox behavior) in *actors*, updated in place.

(define *actors* nil)

(define (actor-lookup id)
  (assoc id *actors*))

(define (actor-update id mailbox behavior)
  (let ((old-entry (actor-lookup id)))
    (cond (old-entry (set-car (cdr old-entry) mailbox)
                     (set-car (cdr (cdr old-entry)) behavior))
          (t (setq *actors*
               (cons (list id mailbox behavior) *actors*))))))

(define (actor-spawn id initial-behavior)
  (actor-update id nil initial-behavior)
  id)

(define (actor-send target-id msg)
  (let ((entry (actor-lookup target-id)))
    (cond (entry (let ((mailbox-cell (cdr entry)))
                   (set-car mailbox-cell (append (car mailbox-cell) (list msg)))))
          (t nil))))

(define (actor-become actor-id new-behavior)
  (let ((entry (actor-lookup actor-id)))
    (cond (entry (set-car (cdr (cdr entry)) new-behavior))
          (t nil))))

(define (actor-step actor-id)
  (let ((entry (actor-lookup actor-id)))
    (cond ((eq entry nil) nil)
          (t (let ((mailbox-cell (cdr entry)))
               (let ((mailbox (car mailbox-cell))
                     (behavior (cadr mailbox-cell)))
                 (cond ((eq mailbox nil) nil)
                       (t (let ((msg (car mailbox)))
                            (set-car mailbox-cell (cdr mailbox))
                            (let ((result (behavior actor-id msg)))
                              (cond (result (actor-become actor-id result)))))))))))))

(define (actor-run ids max-steps)
  (cond ((= max-steps 0) nil)
        ((eq ids nil) nil)
        (t (actor-step (car ids))
           (actor-run (append (cdr ids) (list (car ids))) (- max-steps 1)))))

; ========== Ping-Pong ==========
(define (ping-beh self msg)
  (cond ((equal msg 'start) (actor-send 'pong 'ping) ping-beh)
        ((equal msg 'pong) (print "ping received pong") (actor-send 'pong 'ping) ping-beh)
        ((equal msg 'stop) (print "ping stopping") nil)
        (t ping-beh)))

(define (pong-beh self msg)
  (cond ((equal msg 'ping) (print "pong received ping") (actor-send 'ping 'pong) pong-beh)
        ((equal msg 'stop) (print "pong stopping") nil)
        (t pong-beh)))

(print "=== Ping-Pong ===")
(setq *actors* nil)
(actor-spawn 'ping ping-beh)
(actor-spawn 'pong pong-beh)
(actor-send 'ping 'start)
(actor-run '(ping pong) 6)

; ========== Counter ==========
(define (counter-beh count)
  (lambda (self msg)
    (cond ((equal (car msg) 'inc) (counter-beh (+ count (cadr msg))))
          ((equal (car msg) 'get) (print (list 'counter count)) (counter-beh count))
          (t (counter-beh count)))))

(print "=== Counter ===")
(setq *actors* nil)
(actor-spawn 'counter (counter-beh 0))
(actor-send 'counter (list 'inc 5))
(actor-send 'counter (list 'inc 3))
(actor-send 'counter (list 'get 'v1))
(actor-send 'counter (list 'inc 2))
(actor-send 'counter (list 'get 'v2))
(actor-run '(counter) 10)

; ========== Forwarder chain (with become) ==========
(define (fwd-beh next-id)
  (lambda (self msg)
    (cond ((equal (car msg) 'forward)
           (print (list self 'forwards (cadr msg) 'to next-id))
           (actor-send next-id (list 'forward (cadr msg)))
           (fwd-beh next-id))
          ((equal (car msg) 'reroute)
           (print (list self 'reroutes 'to (cadr msg)))
           (fwd-beh (cadr msg)))
          (t (fwd-beh next-id)))))

(define (sink-beh self msg)
  (print (list 'sink 'delivered (cadr msg)))
  sink-beh)

(print "=== Forwarder chain ===")
(setq *actors* nil)
(actor-spawn 'f1 (fwd-beh 'f2))
(actor-spawn 'f2 (fwd-beh 'f3))
(actor-spawn 'f3 (fwd-beh 'sink))
(actor-spawn 'sink sink-beh)
(actor-send 'f1 (list 'forward 'hello))
(actor-run '(f1 f2 f3 sink) 12)

(print "--- Reroute f2 -> sink ---")
(actor-send 'f2 (list 'reroute 'sink))
(actor-run '(f2) 2)
(actor-send 'f1 (list 'forward 'direct))
(actor-run '(f1 f2 f3 sink) 12)

(print "=== Actors done ===")
t
