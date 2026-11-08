; matrix-checkpoint.lsp — resumable text checkpoints using existing SISP I/O.
;
; Each parameter component is a separate reader-loadable file.  This keeps every
; file well below read-file's limit and lets the top-level collector discard the
; temporary numeric list after restoring one matrix.  The runner publishes the
; completed directory and retains the previous checkpoint.

(define *matrix-checkpoint-format* 3)
(define *matrix-model-contract* 4)
(define *matrix-checkpoint-loaded-parts* 0)
(define *matrix-checkpoint-loaded-keys* nil)
(define *matrix-inference-mode* nil)

(define (matrix-checkpoint-fail message)
  (print (list (quote checkpoint-error) message))
  (quit 2))

(define (matrix-checkpoint-require condition message)
  (cond (condition t)
        (t (matrix-checkpoint-fail message))))

(define (matrix-checkpoint-part-name directory index kind)
  (cat directory
    (cat "/part-"
      (cat (num-to-str index)
        (cat "-" (cat kind ".lsp"))))))

; Build one row at a time.  The largest temporary string is therefore one row,
; rather than an entire multi-megabyte parameter matrix.
(define (matrix-checkpoint-row-pieces matrix row column)
  (cond ((= column (mat-cols matrix)) (list " "))
        (t (cons (num-to-str (mat-ref matrix row column))
             (cons " "
               (matrix-checkpoint-row-pieces matrix row (+ column 1)))))))

(define (matrix-checkpoint-write-rows path matrix row)
  (cond ((= row (mat-rows matrix)) nil)
        (t (append-file path
             (list-to-str (matrix-checkpoint-row-pieces matrix row 0)))
           (matrix-checkpoint-write-rows path matrix (+ row 1)))))

(define (matrix-checkpoint-write-part directory index kind matrix)
  (let ((path (matrix-checkpoint-part-name directory index kind)))
    (write-file path
      (list-to-str
        (list "(matrix-checkpoint-restore-part " (num-to-str index)
          " (quote " kind ") (matrix-checkpoint-matrix "
          (num-to-str (mat-rows matrix)) " " (num-to-str (mat-cols matrix))
          " (list ")))
    (matrix-checkpoint-write-rows path matrix 0)
    (append-file path "))) ")
    path))

(define (matrix-checkpoint-matrix rows cols values)
  (matrix-checkpoint-require
    (= (foldl (lambda (count ignored) (+ count 1)) 0 values) (* rows cols))
    "matrix payload has the wrong number of values")
  (matrix-from-list rows cols values))

(define (matrix-checkpoint-save-component directory index kind)
  (matrix-checkpoint-require
    (and (>= index 0) (< index (length *matrix-parameters*)))
    "cannot save a parameter index outside this model")
  (let ((parameter (nth index *matrix-parameters*)))
    (cond ((eq kind (quote data))
      (matrix-checkpoint-write-part directory index "data" (mp-data parameter)))
          ((eq kind (quote m))
      (matrix-checkpoint-write-part directory index "m" (mp-m parameter)))
          ((eq kind (quote v))
      (matrix-checkpoint-write-part directory index "v" (mp-v parameter)))
          (t (matrix-checkpoint-fail "unknown parameter component to save")))))

; The state file is deliberately assembled explicitly.  Profile is supplied as
; a reader-safe name by the runner; all other fields are numeric.
(define (matrix-checkpoint-write-state directory parameter-count)
  (write-file (cat directory "/state.lsp")
    (list-to-str
      (list "(matrix-checkpoint-restore-state "
        (num-to-str *matrix-checkpoint-format*) " "
        (num-to-str *matrix-model-contract*) " (quote "
        *matrix-profile-name* ") "
        (num-to-str *matrix-context*) " "
        (num-to-str *matrix-dimension*) " "
        (num-to-str *matrix-heads*) " "
        (num-to-str *matrix-layers*) " "
        (num-to-str *matrix-hidden*) " "
        (num-to-str (length *matrix-vocabulary*)) " "
        (num-to-str parameter-count) " "
        (num-to-str *matrix-vocabulary-checksum*) " "
        (num-to-str (length *matrix-source*)) " "
        (num-to-str *matrix-corpus-checksum*) " "
        (num-to-str (length *matrix-validation-source*)) " "
        (num-to-str *matrix-validation-corpus-checksum*) " "
        (num-to-str *matrix-step*) " "
        (num-to-str *matrix-window-index*) " "
        (num-to-str *matrix-epoch*) " "
        (num-to-str *matrix-sequences-processed*) " "
        (num-to-str *matrix-characters-processed*) " "
        "(quote " *matrix-sampling-mode-name* ") "
        (num-to-str *matrix-sampler-seed*) " "
        (num-to-str *matrix-learning-rate*) " "
        (num-to-str *matrix-weight-decay*) " "
        (num-to-str *matrix-warmup-steps*) " "
        (num-to-str *matrix-decay-steps*) " "
        (num-to-str *matrix-min-learning-rate-ratio*) " "
        (num-to-str *matrix-microbatch*) " "
        (num-to-str *matrix-beta2*) " "
        (num-to-str *matrix-clip-norm*) ") "))))

(define (matrix-checkpoint-save-finish directory)
  (let ((count (length *matrix-parameters*)))
    (matrix-checkpoint-write-state directory count)
    (print (list (quote checkpoint-save-complete) directory
      (quote step) *matrix-step* (quote parameters) count))
    count))

(define (matrix-checkpoint-restore-part index kind matrix)
  (matrix-checkpoint-require
    (and (>= index 0) (< index (length *matrix-parameters*)))
    "parameter index is outside this model")
  (let ((key (list index kind)))
    (matrix-checkpoint-require
      (eq (in key *matrix-checkpoint-loaded-keys* (quote equal)) nil)
      "checkpoint contains a duplicate parameter component")
    (setq *matrix-checkpoint-loaded-keys*
      (cons key *matrix-checkpoint-loaded-keys*)))
  (let ((parameter (nth index *matrix-parameters*)))
    (matrix-checkpoint-require
      (and (= (mat-rows matrix) (mat-rows (mp-data parameter)))
           (= (mat-cols matrix) (mat-cols (mp-data parameter))))
      "parameter shape does not match this model")
    (cond ((eq kind (quote data)) (set-car (cdr parameter) matrix))
          ((eq kind (quote m)) (set-car (cdddr parameter) matrix))
          ((eq kind (quote v)) (set-car (cddddr parameter) matrix))
          (t (matrix-checkpoint-fail "unknown parameter component")))
    (setq *matrix-checkpoint-loaded-parts*
      (+ *matrix-checkpoint-loaded-parts* 1))))

(define (matrix-checkpoint-restore-state format contract profile context dimension heads
          layers hidden vocab parameter-count vocabulary-checksum
          source-length corpus-checksum validation-source-length
          validation-corpus-checksum step window-index epoch sequences-processed
          characters-processed sampling-mode sampler-seed learning-rate
          weight-decay warmup-steps decay-steps min-learning-rate-ratio
          microbatch beta2 clip-norm)
  (matrix-checkpoint-require (= format *matrix-checkpoint-format*)
    "unsupported checkpoint format")
  (matrix-checkpoint-require (= contract *matrix-model-contract*)
    "checkpoint model/training contract is incompatible")
  (matrix-checkpoint-require (eq profile *matrix-profile*)
    "checkpoint profile does not match SISP_LLM_PROFILE")
  (matrix-checkpoint-require
    (and (= context *matrix-context*) (= dimension *matrix-dimension*)
      (= heads *matrix-heads*) (= layers *matrix-layers*)
      (= hidden *matrix-hidden*)
      (= vocab (length *matrix-vocabulary*))
      (= parameter-count (length *matrix-parameters*)))
    "checkpoint architecture does not match this model")
  (matrix-checkpoint-require
    (= vocabulary-checksum *matrix-vocabulary-checksum*)
    "checkpoint vocabulary order does not match this model")
  (matrix-checkpoint-require
    (= *matrix-checkpoint-loaded-parts* (* 3 parameter-count))
    "checkpoint has missing or duplicate parameter components")
  (matrix-checkpoint-require
    (and (= microbatch *matrix-microbatch*)
      (eq sampling-mode *matrix-sampling-mode*)
      (= sampler-seed *matrix-sampler-seed*)
      (= learning-rate *matrix-learning-rate*)
      (= weight-decay *matrix-weight-decay*)
      (= warmup-steps *matrix-warmup-steps*)
      (= decay-steps *matrix-decay-steps*)
      (= min-learning-rate-ratio *matrix-min-learning-rate-ratio*)
      (= beta2 *matrix-beta2*)
      (= clip-norm *matrix-clip-norm*)
      (>= window-index 0) (>= step 0) (>= epoch 0))
    "checkpoint training configuration/position is incompatible")
  (matrix-checkpoint-require (mparams-finite *matrix-parameters*)
    "checkpoint contains non-finite model/optimizer state")
  (setq *matrix-step* step)
  (cond (*matrix-inference-mode*
    ; Inference validates the architecture, vocabulary, complete parameter
    ; payload, finite values, and optimizer contract above.  Corpus identity is
    ; a training/resume property and must not make a deployed model depend on
    ; the examples from which it was learned.
    (setq *matrix-window-index* window-index)
    (setq *matrix-epoch* epoch))
    ((eq *matrix-corpus-mode* (quote strict))
    (matrix-checkpoint-require
      (and (= source-length (length *matrix-source*))
        (= corpus-checksum *matrix-corpus-checksum*)
        (= validation-source-length (length *matrix-validation-source*))
        (= validation-corpus-checksum *matrix-validation-corpus-checksum*)
        (< window-index (- (length *matrix-source*) *matrix-context*)))
      "checkpoint corpus does not match this strict resume")
    (setq *matrix-window-index* window-index)
    (setq *matrix-epoch* epoch))
    (t
      (matrix-checkpoint-require
        (and (= validation-source-length (length *matrix-validation-source*))
             (= validation-corpus-checksum *matrix-validation-corpus-checksum*))
        "checkpoint validation corpus must remain fixed across training shards")
      (setq *matrix-window-index* 0)
      (setq *matrix-epoch* 0)
      (print (list (quote checkpoint-new-corpus)
        (quote previous-characters) characters-processed))))
  (setq *matrix-sequences-processed* sequences-processed)
  (setq *matrix-characters-processed* characters-processed)
  (print (list (quote checkpoint-loaded) (quote step) *matrix-step*
    (quote next-window) *matrix-window-index*
    (quote parameters) parameter-count))
  t)

(define (matrix-checkpoint-validate-save)
  (matrix-checkpoint-require
    (= (length *matrix-parameters*) *matrix-expected-parameter-objects*)
    "refusing checkpoint with stale parameter-object count")
  (matrix-checkpoint-require
    (= (mparams-count *matrix-parameters*) *matrix-expected-parameters*)
    "refusing checkpoint with stale scalar parameter count")
  (matrix-checkpoint-require (mparams-finite *matrix-parameters*)
    "refusing checkpoint with non-finite model/optimizer state")
  t)
