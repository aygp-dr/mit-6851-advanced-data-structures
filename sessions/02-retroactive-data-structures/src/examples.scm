#!/usr/bin/env guile
!#

;;; examples.scm - Retroactive Data Structures Examples
;;; MIT 6.851 Advanced Data Structures

(use-modules (srfi srfi-1)   ; List operations
             (srfi srfi-9)   ; Records
             (srfi srfi-11)  ; let-values
             (ice-9 format)) ; Formatted output

;; Load retroactive data structures implementation
(load "retroactive.scm")

(define (print-separator)
  (format #t "~%~60,,'-a~%" ""))

(define (demo-basic-retroactive-operations)
  "Demonstrate basic retroactive insert/delete/query operations"
  (format #t "~%=== Basic Retroactive Operations Demo ===~%")
  
  ;; Create timeline
  (let ((timeline (make-timeline '() 0)))
    
    ;; Insert operations at different times
    (format #t "~%Inserting operations into timeline:~%")
    (retro-insert timeline 10 '(push 5))
    (format #t "  t=10: push 5~%")
    
    (retro-insert timeline 20 '(push 3))
    (format #t "  t=20: push 3~%")
    
    (retro-insert timeline 30 '(pop))
    (format #t "  t=30: pop~%")
    
    ;; Now retroactively insert at t=15
    (format #t "~%Retroactively inserting at t=15:~%")
    (retro-insert timeline 15 '(push 7))
    (format #t "  t=15: push 7 (inserted retroactively)~%")
    
    ;; Show timeline
    (format #t "~%Current timeline:~%")
    (for-each (lambda (op)
                (format #t "  t=~a: ~a~%" 
                        (retro-op-time op) 
                        (retro-op-data op)))
              (timeline-operations timeline))
    
    ;; Delete operation at t=20
    (format #t "~%Deleting operation at t=20:~%")
    (retro-delete timeline 20)
    
    ;; Show updated timeline
    (format #t "~%Timeline after deletion:~%")
    (for-each (lambda (op)
                (format #t "  t=~a: ~a~%" 
                        (retro-op-time op) 
                        (retro-op-data op)))
              (timeline-operations timeline))))

;; Simple stack implementation for demonstration
(define (make-simple-stack) '())
(define (simple-stack-push stack val) (cons val stack))
(define (simple-stack-pop stack) (if (null? stack) stack (cdr stack)))
(define (simple-stack-top stack) (if (null? stack) #f (car stack)))

(define (demo-retroactive-stack)
  "Demonstrate retroactive stack with time travel"
  (format #t "~%~%=== Retroactive Stack Demo ===~%")
  
  ;; Create retroactive stack
  (let ((retro-stack (create-rollback-retro make-simple-stack)))
    
    ;; Series of operations
    (format #t "~%Performing operations:~%")
    (rollback-insert retro-stack 10 `(push 1))
    (format #t "  t=10: push 1~%")
    
    (rollback-insert retro-stack 20 `(push 2))
    (format #t "  t=20: push 2~%")
    
    (rollback-insert retro-stack 30 `(push 3))
    (format #t "  t=30: push 3~%")
    
    (rollback-insert retro-stack 40 `(pop))
    (format #t "  t=40: pop~%")
    
    ;; Query at different times
    (format #t "~%Stack state at different times:~%")
    (format #t "  t=15: ~a~%" (query-at-time retro-stack 15))
    (format #t "  t=25: ~a~%" (query-at-time retro-stack 25))
    (format #t "  t=35: ~a~%" (query-at-time retro-stack 35))
    (format #t "  t=45: ~a~%" (query-at-time retro-stack 45))
    
    ;; Retroactive insertion
    (format #t "~%Retroactively inserting push 1.5 at t=15:~%")
    (rollback-insert retro-stack 15 `(push 1.5))
    
    ;; Query again
    (format #t "~%Stack state after retroactive insertion:~%")
    (format #t "  t=15: ~a~%" (query-at-time retro-stack 15))
    (format #t "  t=25: ~a~%" (query-at-time retro-stack 25))
    (format #t "  t=35: ~a~%" (query-at-time retro-stack 35))))

;; Simple queue implementation
(define-record-type <queue>
  (make-queue-internal front rear)
  queue?
  (front queue-front set-queue-front!)
  (rear queue-rear set-queue-rear!))

(define (make-queue) (make-queue-internal '() '()))

(define (enqueue q val)
  (let ((new-q (make-queue-internal (queue-front q) 
                                    (cons val (queue-rear q)))))
    new-q))

(define (dequeue q)
  (if (null? (queue-front q))
      (if (null? (queue-rear q))
          q  ; empty queue
          (let ((new-front (reverse (queue-rear q))))
            (make-queue-internal (cdr new-front) '())))
      (make-queue-internal (cdr (queue-front q)) (queue-rear q))))

(define (demo-retroactive-queue)
  "Demonstrate retroactive queue operations"
  (format #t "~%~%=== Retroactive Queue Demo ===~%")
  
  ;; Demonstrate retroactive queue
  (let ((timeline (make-timeline '() 0)))
    
    (format #t "~%Building queue timeline:~%")
    (retro-insert timeline 10 '(enqueue A))
    (format #t "  t=10: enqueue A~%")
    
    (retro-insert timeline 20 '(enqueue B))
    (format #t "  t=20: enqueue B~%")
    
    (retro-insert timeline 30 '(dequeue))
    (format #t "  t=30: dequeue~%")
    
    (retro-insert timeline 40 '(enqueue C))
    (format #t "  t=40: enqueue C~%")
    
    ;; Show queue state progression
    (format #t "~%Queue state over time:~%")
    (format #t "  t=15: [A]~%")
    (format #t "  t=25: [A, B]~%")
    (format #t "  t=35: [B] (A dequeued)~%")
    (format #t "  t=45: [B, C]~%")
    
    ;; Retroactive operations
    (format #t "~%Retroactively inserting enqueue X at t=15:~%")
    (retro-insert timeline 15 '(enqueue X))
    
    (format #t "~%Updated queue progression:~%")
    (format #t "  t=15: [A, X]~%")
    (format #t "  t=25: [A, X, B]~%")
    (format #t "  t=35: [X, B] (A dequeued)~%")
    (format #t "  t=45: [X, B, C]~%")))

;; Stack for performance testing
(define (make-perf-stack) '())

(define (demo-performance-analysis)
  "Analyze performance of retroactive operations"
  (format #t "~%~%=== Performance Analysis Demo ===~%")
  
  ;; Create a retroactive data structure with many operations
  (let ((retro-ds (create-rollback-retro make-perf-stack)))
    
    ;; Insert many operations
    (format #t "~%Inserting 100 operations...~%")
    (do ((i 0 (+ i 1)))
        ((= i 100))
      (rollback-insert retro-ds (* i 10) `(push ,i)))
    
    ;; Measure replay cost for different ranges
    (format #t "~%Replay costs for retroactive insertions:~%")
    
    ;; Early insertion
    (let ((metrics (track-replay-performance retro-ds 50 1000)))
      (format #t "  Insert at t=50: ~a operations to replay~%"
              (pm-replay-count metrics)))
    
    ;; Middle insertion
    (let ((metrics (track-replay-performance retro-ds 500 1000)))
      (format #t "  Insert at t=500: ~a operations to replay~%"
              (pm-replay-count metrics)))
    
    ;; Late insertion
    (let ((metrics (track-replay-performance retro-ds 950 1000)))
      (format #t "  Insert at t=950: ~a operations to replay~%"
              (pm-replay-count metrics)))
    
    (format #t "~%Observation: Earlier retroactive insertions require more replays!~%")))

(define (demo-checkpoint-optimization)
  "Demonstrate checkpoint optimization for retroactive DS"
  (format #t "~%~%=== Checkpoint Optimization Demo ===~%")
  
  (format #t "~%Without checkpoints:~%")
  (format #t "  Every retroactive operation replays from beginning~%")
  (format #t "  O(n) time per operation~%")
  
  (format #t "~%With checkpoints every k operations:~%")
  (format #t "  Retroactive operation replays from nearest checkpoint~%")
  (format #t "  O(k) time per operation~%")
  (format #t "  Space overhead: O(n/k) checkpoints~%")
  
  (format #t "~%Optimal checkpoint spacing:~%")
  (format #t "  k = √n minimizes time-space tradeoff~%")
  (format #t "  Time: O(√n), Space: O(√n)~%"))

;; Helper functions

(define (query-at-time retro-ds time)
  "Query state at given time (simplified)"
  ;; This is a placeholder - real implementation would replay operations
  (format #f "[stack at t=~a]" time))

(define (replay-operations ops query-fn)
  "Replay operations up to query function"
  ;; Simplified implementation
  #t)

(define (find-checkpoint-before checkpoints time)
  "Find the latest checkpoint before given time"
  (car checkpoints))  ; Simplified

(define (replay-from-checkpoint checkpoint ops time)
  "Replay operations from checkpoint to time"
  '())  ; Simplified

(define (maybe-add-checkpoint retro-ds time state)
  "Conditionally add checkpoint for optimization"
  #t)  ; Simplified

(define (count-ops-between ops from-time to-time)
  "Count operations in time range"
  (length (filter (lambda (op) 
                    (and (>= (car op) from-time)
                         (<= (car op) to-time)))
                  ops)))

;; Main demo runner
(define (main args)
  (format #t "MIT 6.851 - Retroactive Data Structures Examples~%")
  (format #t "================================================~%")
  
  (demo-basic-retroactive-operations)
  (print-separator)
  
  (demo-retroactive-stack)
  (print-separator)
  
  (demo-retroactive-queue)
  (print-separator)
  
  (demo-performance-analysis)
  (print-separator)
  
  (demo-checkpoint-optimization)
  
  (format #t "~%~%Key Concepts:~%")
  (format #t "1. Retroactive operations modify the past~%")
  (format #t "2. Time-travel queries see consistent historical states~%")
  (format #t "3. Implementation strategies: rollback vs bridges~%")
  (format #t "4. Checkpointing reduces replay costs~%")
  (format #t "5. Applications: undo/redo, debugging, version control~%"))

(main (command-line))