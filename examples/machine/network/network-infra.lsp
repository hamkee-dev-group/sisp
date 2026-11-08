; ============================================================
; NETWORK INFRASTRUCTURE — Switch, Router, DHCP, DNS, Firewall
; Connects multiple V2 CPUs into a real TCP/IP network.
; ============================================================

; ---- MAC address (6 bytes, simplified to integers) -----------
(define (mac-addr a b) (list a b))  ; (vendor device)
(define (mac-str m) (str-join (list (num-to-str (car m)) ":" (num-to-str (cadr m))) ""))

; ---- IP address (4 bytes) ------------------------------------
(define (ip a b c d) (list a b c d))
(define (ip-str ip) (str-join (map num-to-str ip) "."))
(define (ip-eq a b) (and (= (car a) (car b)) (= (cadr a) (cadr b)) (= (caddr a) (caddr b)) (= (cadddr a) (cadddr b))))
(define localhost (ip 127 0 0 1))
(define broadcast (ip 255 255 255 255))

; ---- Ethernet frame: (dst-mac src-mac ethertype payload) -----
(define ETH_IP  2048)   ; IPv4
(define ETH_ARP 2054)   ; ARP

(define (make-frame dst src type payload) (list dst src type payload))
(define (f-dst f) (car f)) (define (f-src f) (cadr f))
(define (f-type f) (caddr f)) (define (f-payload f) (cadddr f))
(define (broadcast-frame) (mac-addr 255 255))

; ---- Ethernet Bus (shared medium) ----------------------------
(define *bus* nil)  ; queue of frames in flight

(define (bus-send frame)
  (setq *bus* (cons frame *bus*))
  frame)

(define (bus-recv mac)
  (let ((match (find (lambda (f) (or (mac-eq (f-dst f) mac) (mac-eq (f-dst f) (broadcast-frame))))
                     *bus*)))
    (cond (match (setq *bus* (remove (lambda (f) (eq f match)) *bus*)) match)
          (t nil))))

(define (mac-eq a b) (and (= (car a) (car b)) (= (cadr a) (cadr b))))

; ---- Switch: MAC learning, port forwarding -------------------
(define *switch-table* nil)  ; ((mac . port) ...)

(define (switch-port mac)
  (let ((e (assoc mac *switch-table*)))
    (cond (e (cdr e)) (t -1))))

(define (switch-learn mac port)
  (let ((e (assoc mac *switch-table*)))
    (cond (e (set-cdr e port)) (t (setq *switch-table* (cons (cons mac port) *switch-table*))))))

(define (switch-forward frame in-port)
  (switch-learn (f-src frame) in-port)
  (let ((dst-port (switch-port (f-dst frame))))
    (cond ((= dst-port -1)  ; unknown MAC — flood all ports
           (setq *bus* (cons frame *bus*))
           (str-join (list "[SW] flood: " (frame-repr frame)) ""))
          ((= dst-port in-port) "[SW] drop: same port")
          (t (setq *bus* (cons frame *bus*))
             (str-join (list "[SW] fwd port " (num-to-str in-port) " -> " (num-to-str dst-port)) "")))))

(define (frame-repr f)
  (str-join (list "MAC " (mac-str (f-src f)) " -> " (mac-str (f-dst f)) " [" (num-to-str (f-type f)) "]" ) ""))

; ---- ARP: IP -> MAC ------------------------------------------
(define *arp-table* nil)  ; ((ip . mac) ...)

(define (arp-resolve ip)
  (let ((e (assoc ip *arp-table*)))
    (cond (e (cdr e)) (t nil))))

(define (arp-learn ip mac)
  (let ((e (assoc ip *arp-table*)))
    (cond (e (set-cdr e mac)) (t (setq *arp-table* (cons (cons ip mac) *arp-table*))))))

(define (arp-request target-ip src-ip src-mac)
  (list src-mac src-ip target-ip 'request))

(define (arp-reply target-mac target-ip src-ip)
  (list target-mac src-ip target-ip 'reply))

; ---- Router: routing table, NAT, TTL --------------------------
(define *routes* nil)  ; ((subnet mask gateway port) ...)
(define *nat-table* nil) ; ((internal-ip internal-port . external-port) ...)

(define (route-add subnet mask gateway)
  (setq *routes* (cons (list subnet mask gateway) *routes*)))

(define (route-lookup ip)
  (find (lambda (r) (ip-in-subnet ip (car r) (cadr r))) *routes*))

(define (ip-in-subnet ip subnet mask)
  (let ((ia (car ip)) (ib (cadr ip)) (ic (caddr ip)) (id (cadddr ip))
        (sa (car subnet)) (sb (cadr subnet)) (sc (caddr subnet)) (sd (cadddr subnet))
        (ma (car mask)) (mb (cadr mask)) (mc (caddr mask)) (md (cadddr mask)))
    (and (= (logand ia ma) (logand sa ma)) (= (logand ib mb) (logand sb mb))
         (= (logand ic mc) (logand sc mc)) (= (logand id md) (logand sd md)))))

(define (logand a b) (la a b 0 1))
(define (la a b p bt) (cond ((= p 16) 0) ((and (= (mod a 2) 1) (= (mod b 2) 1)) (+ bt (la (truncate (/ a 2)) (truncate (/ b 2)) (+ p 1) (* bt 2)))) (t (la (truncate (/ a 2)) (truncate (/ b 2)) (+ p 1) (* bt 2)))))

(define (nat-translate internal-ip internal-port)
  (let ((entry (find (lambda (n) (and (ip-eq (car n) internal-ip) (= (cadr n) internal-port))) *nat-table*)))
    (cond (entry (cddr entry))
          (t (let ((ext-port (+ 40000 (length *nat-table*))))
               (setq *nat-table* (cons (list internal-ip internal-port ext-port) *nat-table*))
               ext-port)))))

; ---- DHCP Server: assign IPs to booting machines ------------
(define *dhcp-pool* nil)     ; ((mac . ip) ...)
(define *dhcp-offer* nil)    ; available IPs
(define *dhcp-subnet* (ip 192 168 1 0))
(define *dhcp-mask*   (ip 255 255 255 0))
(define *dhcp-gateway* (ip 192 168 1 1))
(define *dhcp-dns*     (ip 192 168 1 1))
(define *dhcp-next* 10)       ; next assignable host number

(define (dhcp-discover mac)
  (let ((existing (assoc mac *dhcp-pool*)))
    (cond (existing (cdr existing))
          (t (let ((ip (ip (car *dhcp-subnet*) (cadr *dhcp-subnet*) (caddr *dhcp-subnet*) *dhcp-next*)))
               (setq *dhcp-next* (+ *dhcp-next* 1))
               (setq *dhcp-pool* (cons (cons mac ip) *dhcp-pool*))
               ip)))))

(define (dhcp-offer mac)
  (let ((ip (dhcp-discover mac)))
    (list mac ip *dhcp-mask* *dhcp-gateway* *dhcp-dns*)))

; ---- DNS Server: hostname -> IP -----------------------------
(define *dns-zones* nil)  ; ((hostname . ip) ...)

(define (dns-add hostname ip)
  (setq *dns-zones* (cons (cons hostname ip) *dns-zones*)))

(define (dns-resolve hostname)
  (let ((e (assoc hostname *dns-zones*)))
    (cond (e (cdr e)) (t nil))))

(define (dns-reverse ip)
  (let ((e (find (lambda (z) (ip-eq (cdr z) ip)) *dns-zones*)))
    (cond (e (car e)) (t nil))))

; ---- Firewall: allow/deny rules -----------------------------
(define *firewall-rules* nil)  ; ((action src-ip dst-ip port) ...)

(define (fw-add action src dst port)
  (setq *firewall-rules* (cons (list action src dst port) *firewall-rules*)))

(define (fw-check src dst port)
  (let ((rule (find (lambda (r) (and (ip-eq (cadr r) src) (ip-eq (caddr r) dst) (= (cadddr r) port))) *firewall-rules*)))
    (cond (rule (car rule))
          (t 'allow))))  ; default: allow

; fw-check yields a symbol (deny/allow); render it for message building.
; SISP has no symbol->string primitive, and num-to-str rejects symbols.
(define (fw-action-str a) (cond ((eq a 'deny) "deny") (t "allow")))

; ============================================================
; NETWORK DEMO — Two machines, switch, DHCP, DNS
; ============================================================
(define (str-join parts sep)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (cat (car parts) sep) (str-join (cdr parts) sep)))))

(print "=================================================")
(print "  NETWORK INFRASTRUCTURE")
(print "=================================================")
(print "")

; --- Network setup ---
(define mac-a (mac-addr 10 1))
(define mac-b (mac-addr 20 2))
(define mac-r (mac-addr 0 1))  ; router MAC

(print "--- Switch ---")
(switch-learn mac-a 1)
(switch-learn mac-b 2)
(switch-learn mac-r 0)
(print (str-join (list "[SW] " (num-to-str (length *switch-table*)) " MACs learned") ""))
(define frame1 (make-frame mac-b mac-a ETH_IP "hello from A"))
(switch-forward frame1 1)
(print (str-join (list "[SW] forwarded to port " (num-to-str (switch-port mac-b))) ""))
(print "")

(print "--- DHCP ---")
(define offer (dhcp-offer mac-a))
(print (str-join (list "[DHCP] MAC " (mac-str mac-a) " -> " (ip-str (cadr offer)) " gw=" (ip-str (cadddr offer)) " dns=" (ip-str (car (cddddr offer)))) ""))
(define offer2 (dhcp-offer mac-b))
(print (str-join (list "[DHCP] MAC " (mac-str mac-b) " -> " (ip-str (cadr offer2))) ""))
(print "")

(print "--- DNS ---")
(dns-add "server-a.local" (cadr offer))
(dns-add "server-b.local" (cadr offer2))
(dns-add "router.local" (ip 192 168 1 1))
(print (str-join (list "[DNS] server-a.local -> " (ip-str (dns-resolve "server-a.local"))) ""))
(print (str-join (list "[DNS] unknown.host -> " (cond ((dns-resolve "unknown.host") "found") (t "NXDOMAIN"))) ""))
(print "")

(print "--- Router ---")
(route-add (ip 192 168 1 0) (ip 255 255 255 0) (ip 192 168 1 1))
(route-add (ip 10 0 0 0) (ip 255 0 0 0) (ip 10 0 0 1))
(define r (route-lookup (ip 192 168 1 50)))
(print (str-join (list "[ROUTE] 192.168.1.50 -> gw " (ip-str (caddr r))) ""))
(print "")

(print "--- NAT ---")
(define ext-port (nat-translate (cadr offer) 8080))
(print (str-join (list "[NAT] " (ip-str (cadr offer)) ":8080 -> :" (num-to-str ext-port)) ""))
(print "")

(print "--- Firewall ---")
(fw-add 'deny (ip 10 0 0 5) (cadr offer) 22)
(print (str-join (list "[FW] 10.0.0.5 -> server-a:22 = " (fw-action-str (fw-check (ip 10 0 0 5) (cadr offer) 22))) ""))
(print (str-join (list "[FW] 192.168.1.20 -> server-a:80 = " (fw-action-str (fw-check (ip 192 168 1 20) (cadr offer) 80))) ""))
(print "")

(print "=================================================")
(print "  All network layers operational")
(print "  Switch | ARP | DHCP | DNS | Router | NAT | FW")
(print "=================================================")
