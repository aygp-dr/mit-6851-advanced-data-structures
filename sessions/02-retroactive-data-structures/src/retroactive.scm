(use-modules (srfi srfi-9)    ; For define-record-type
             (srfi srfi-1)    ; For fold, filter, etc.
             (srfi srfi-11))  ; For let-values

;; Operation record
(define-record-type <retro-operation>
  (make-retro-operation time op-type op-data)
  retro-operation?
  (time retro-op-time)
  (op-type retro-op-type)      ; 'insert, 'delete, 'query
  (op-data retro-op-data))

;; Timeline structure
(define-record-type <timeline>
  (make-timeline operations present-time)
  timeline?
  (operations timeline-operations set-timeline-operations!)
  (present-time timeline-present set-timeline-present!))

;; Insert an operation at time t
(define (retro-insert timeline t operation)
  "Insert operation into timeline at time t"
  (let ((new-op (make-retro-operation t 'insert operation))
        (ops (timeline-operations timeline)))
    (set-timeline-operations! 
     timeline 
     (insert-sorted ops new-op 
                    (lambda (a b) (< (retro-op-time a) (retro-op-time b))))))
  timeline)

;; Delete operation at time t
(define (retro-delete timeline t)
  "Delete the operation that occurred at time t"
  (let ((ops (timeline-operations timeline)))
    (set-timeline-operations!
     timeline
     (filter (lambda (op) (not (= (retro-op-time op) t))) ops)))
  timeline)

;; Query at time t (relative to current timeline)
(define (retro-query timeline t query-fn)
  "Execute query at time t"
  (let ((relevant-ops (filter (lambda (op) (<= (retro-op-time op) t))
                               (timeline-operations timeline))))
    (replay-operations relevant-ops query-fn)))

;; Checkpoint structure
(define-record-type <checkpoint>
  (make-checkpoint time state)
  checkpoint?
  (time checkpoint-time)
  (state checkpoint-state))

;; Rollback-based retroactive data structure
(define-record-type <rollback-retro-ds>
  (make-rollback-retro-ds checkpoints operations base-ds-constructor)
  rollback-retro-ds?
  (checkpoints rb-checkpoints set-rb-checkpoints!)
  (operations rb-operations set-rb-operations!)
  (base-ds-constructor rb-constructor))

(define (create-rollback-retro make-base-ds)
  "Create a rollback-based retroactive version of a data structure"
  (make-rollback-retro-ds 
   (list (make-checkpoint 0 (make-base-ds)))
   '()
   make-base-ds))

;; Insert with rollback
(define (rollback-insert retro-ds t operation)
  "Insert operation at time t using rollback method"
  (let* ((ops (rb-operations retro-ds))
         (new-ops (insert-sorted ops (cons t operation)
                                 (lambda (a b) (< (car a) (car b)))))
         (checkpoint (find-checkpoint-before (rb-checkpoints retro-ds) t)))
    ;; Replay from checkpoint
    (let ((new-state (replay-from-checkpoint checkpoint new-ops t)))
      (set-rb-operations! retro-ds new-ops)
      ;; Optionally create new checkpoint
      (maybe-add-checkpoint retro-ds t new-state))
    retro-ds))

;; Performance tracking
(define-record-type <performance-metrics>
  (make-performance-metrics replay-count total-ops)
  performance-metrics?
  (replay-count pm-replay-count set-pm-replay-count!)
  (total-ops pm-total-ops set-pm-total-ops!))

(define (track-replay-performance retro-ds from-time to-time)
  "Track how many operations need to be replayed"
  (let ((ops-in-range (count-ops-between 
                       (rb-operations retro-ds) from-time to-time)))
    (make-performance-metrics ops-in-range (length (rb-operations retro-ds)))))

;; Retroactive priority queue using bridges
(define-record-type <retro-priority-queue>
  (make-retro-priority-queue insertions deletions bridges)
  retro-pq?
  (insertions rpq-insertions set-rpq-insertions!)
  (deletions rpq-deletions set-rpq-deletions!)
  (bridges rpq-bridges set-rpq-bridges!))

;; Bridge structure for tracking max elements
(define-record-type <bridge>
  (make-bridge time key)
  bridge?
  (time bridge-time)
  (key bridge-key))

(define (create-retro-priority-queue)
  "Create an empty retroactive priority queue"
  (make-retro-priority-queue '() '() '()))

;; Insert operation
(define (rpq-insert pq t key)
  "Insert key at time t"
  (let* ((insertions (rpq-insertions pq))
         (new-insertions (insert-sorted insertions (cons t key)
                                        (lambda (a b) (< (car a) (car b)))))
         (bridges (rpq-bridges pq)))
    ;; Update bridges
    (let ((new-bridges (update-bridges-after-insert bridges t key)))
      (set-rpq-insertions! pq new-insertions)
      (set-rpq-bridges! pq new-bridges))
    pq))

;; Delete-min operation  
(define (rpq-delete-min pq t)
  "Delete minimum at time t"
  (let ((deletions (rpq-deletions pq))
        (bridges (rpq-bridges pq)))
    ;; Find what would be deleted at time t
    (let* ((min-at-t (find-min-at-time pq t))
           (new-deletions (insert-sorted deletions (cons t min-at-t)
                                         (lambda (a b) (< (car a) (car b))))))
      (set-rpq-deletions! pq new-deletions)
      ;; Update bridges
      (set-rpq-bridges! pq (update-bridges-after-delete bridges t min-at-t)))
    pq))

(define (update-bridges-after-insert bridges t key)
  "Update bridge structure after insertion"
  (let ((affected-bridge (find-bridge-at-time bridges t)))
    (cond
     ((null? affected-bridge) 
      ;; Create new bridge if needed
      (if (is-new-maximum? bridges t key)
          (insert-sorted bridges (make-bridge t key)
                         (lambda (a b) (< (bridge-time a) (bridge-time b))))
          bridges))
     ((> key (bridge-key affected-bridge))
      ;; Update existing bridge
      (update-bridge-key bridges affected-bridge key))
     (else bridges))))

(define (find-min-at-time pq t)
  "Find minimum element at time t"
  (let* ((insertions-before-t (filter (lambda (ins) (<= (car ins) t))
                                      (rpq-insertions pq)))
         (deletions-before-t (filter (lambda (del) (<= (car del) t))
                                     (rpq-deletions pq)))
         (active-elements (compute-active-elements 
                           insertions-before-t deletions-before-t)))
    (if (null? active-elements)
        #f
        (minimum active-elements cdr))))

;; Retroactive queue - O(1) partial, O(log m) full
(define-record-type <retro-queue>
  (make-retro-queue enqueues dequeues)
  retro-queue?
  (enqueues rq-enqueues set-rq-enqueues!)
  (dequeues rq-dequeues set-rq-dequeues!))

(define (rq-enqueue queue t element)
  "Enqueue element at time t"
  (let ((enqueues (rq-enqueues queue)))
    (set-rq-enqueues! queue 
                      (insert-sorted enqueues (cons t element)
                                     (lambda (a b) (< (car a) (car b)))))
    queue))

(define (rq-dequeue queue t)
  "Dequeue at time t"
  (let ((dequeues (rq-dequeues queue)))
    (set-rq-dequeues! queue
                      (insert-sorted dequeues t <))
    queue))

(define (rq-query queue t)
  "Get queue state at time t"
  (let* ((enqueues-before (filter (lambda (e) (<= (car e) t))
                                  (rq-enqueues queue)))
         (dequeues-before (filter (lambda (d) (<= d t))
                                  (rq-dequeues queue)))
         (num-dequeues (length dequeues-before)))
    ;; Queue contains elements that were enqueued but not yet dequeued
    (drop enqueues-before num-dequeues)))

;; Retroactive deque - O(log m) for all operations
(define-record-type <retro-deque>
  (make-retro-deque front-ops back-ops)
  retro-deque?
  (front-ops rd-front-ops set-rd-front-ops!)
  (back-ops rd-back-ops set-rd-back-ops!))

(define (rd-push-front deque t element)
  "Push element to front at time t"
  (let ((front-ops (rd-front-ops deque)))
    (set-rd-front-ops! deque
                       (insert-sorted front-ops (list t 'push element)
                                      (lambda (a b) (< (car a) (car b)))))
    deque))

(define (rd-push-back deque t element)
  "Push element to back at time t"
  (let ((back-ops (rd-back-ops deque)))
    (set-rd-back-ops! deque
                      (insert-sorted back-ops (list t 'push element)
                                     (lambda (a b) (< (car a) (car b)))))
    deque))

;; Retroactive union-find (incremental connectivity)
(define-record-type <retro-union-find>
  (make-retro-union-find unions finds)
  retro-uf?
  (unions ruf-unions set-ruf-unions!)
  (finds ruf-finds set-ruf-finds!))

(define (ruf-union uf t x y)
  "Union x and y at time t"
  (let ((unions (ruf-unions uf)))
    (set-ruf-unions! uf
                     (insert-sorted unions (list t x y)
                                    (lambda (a b) (< (car a) (car b)))))
    uf))

(define (ruf-find uf t x query-time)
  "Find representative of x at query-time, given operation at time t"
  (let ((unions-before (filter (lambda (u) (<= (car u) query-time))
                               (ruf-unions uf))))
    (compute-representative x unions-before)))

;; Example: Priority queue in Dijkstra's algorithm
(define-record-type <nonoblivious-retro-ds>
  (make-nonoblivious-retro-ds timeline queries error-corrections)
  nonoblivious-retro-ds?
  (timeline nrd-timeline)
  (queries nrd-queries set-nrd-queries!)
  (error-corrections nrd-corrections set-nrd-corrections!))

(define (nonoblivious-update ds t operation)
  "Update that may invalidate future queries"
  (let* ((affected-queries (find-affected-queries (nrd-queries ds) t))
         (corrections (compute-corrections affected-queries operation)))
    ;; Apply corrections in time order
    (for-each (lambda (correction)
                (apply-correction ds correction))
              (sort corrections (lambda (a b) (< (correction-time a)
                                                 (correction-time b)))))
    ds))

;; Helper functions for retroactive data structures

(define (insert-sorted lst item less?)
  "Insert item into sorted list maintaining order"
  (cond
   ((null? lst) (list item))
   ((less? item (car lst)) (cons item lst))
   (else (cons (car lst) (insert-sorted (cdr lst) item less?)))))

(define (replay-operations operations initial-state)
  "Replay a sequence of operations from initial state"
  (fold (lambda (op state)
          (apply-operation op state))
        initial-state
        operations))

(define (count-ops-between operations from-time to-time)
  "Count operations in time range"
  (length (filter (lambda (op)
                    (and (>= (retro-op-time op) from-time)
                         (<= (retro-op-time op) to-time)))
                  operations)))

(define (find-checkpoint-before checkpoints time)
  "Find the latest checkpoint before given time"
  (let ((valid-checkpoints (filter (lambda (cp)
                                     (<= (checkpoint-time cp) time))
                                   checkpoints)))
    (if (null? valid-checkpoints)
        (error "No checkpoint found before time" time)
        (maximum valid-checkpoints checkpoint-time))))

(define (compute-active-elements insertions deletions)
  "Compute elements that are inserted but not deleted"
  (let ((deleted-elements (map cdr deletions)))
    (filter (lambda (ins)
              (not (member (cdr ins) deleted-elements)))
            insertions)))

;; Additional helper functions (stubs for now)
(define (replay-from-checkpoint checkpoint ops time) '())
(define (maybe-add-checkpoint ds time state) #t)
(define (find-bridge-at-time bridges t) '())
(define (is-new-maximum? bridges t key) #f)
(define (update-bridge-key bridges bridge key) bridges)
(define (update-bridges-after-delete bridges t element) bridges)
(define (minimum lst key-fn) (if (null? lst) #f (car lst)))
(define (compute-representative x unions) x)
(define (find-affected-queries queries t) '())
(define (compute-corrections queries op) '())
(define (apply-correction ds correction) #t)
(define (correction-time c) 0)
(define (apply-operation op state) state)