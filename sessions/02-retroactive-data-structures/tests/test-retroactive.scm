#!/usr/bin/env guile
!#

(use-modules (srfi srfi-64)
             (srfi srfi-1)
             (srfi srfi-11))

;; Load the retroactive implementation
(load "../src/retroactive.scm")

;; Start test suite
(test-begin "retroactive-data-structures")

;; Test timeline operations
(test-group "timeline-operations"
  (let ((timeline (make-timeline '() 0)))
    
    (test-assert "empty timeline"
      (null? (timeline-operations timeline)))
    
    ;; Insert operations
    (retro-insert timeline 5 '(push x))
    (retro-insert timeline 3 '(push y))
    (retro-insert timeline 7 '(push z))
    
    (test-eq "timeline has 3 operations"
      3 (length (timeline-operations timeline)))
    
    ;; Test ordering
    (let ((ops (timeline-operations timeline)))
      (test-assert "operations are time-ordered"
        (and (= 3 (retro-op-time (first ops)))
             (= 5 (retro-op-time (second ops)))
             (= 7 (retro-op-time (third ops))))))
    
    ;; Delete operation
    (retro-delete timeline 5)
    (test-eq "timeline has 2 operations after delete"
      2 (length (timeline-operations timeline)))))

;; Test retroactive queue
(test-group "retroactive-queue"
  (let ((queue (make-retro-queue '() '())))
    
    ;; Enqueue operations at different times
    (rq-enqueue queue 1 'a)
    (rq-enqueue queue 3 'b)
    (rq-enqueue queue 5 'c)
    (rq-dequeue queue 4)
    
    ;; Query at different times
    (test-equal "queue at time 2"
      '((1 . a)) (rq-query queue 2))
    
    (test-equal "queue at time 4 (after dequeue)"
      '((3 . b)) (rq-query queue 4))
    
    (test-equal "queue at time 6"
      '((3 . b) (5 . c)) (rq-query queue 6))))

;; Test retroactive priority queue
(test-group "retroactive-priority-queue"
  (let ((pq (create-retro-priority-queue)))
    
    ;; Insert elements
    (rpq-insert pq 1 10)
    (rpq-insert pq 3 5)
    (rpq-insert pq 5 15)
    
    (test-eq "priority queue has 3 insertions"
      3 (length (rpq-insertions pq)))
    
    ;; Delete minimum at time 4
    (rpq-delete-min pq 4)
    
    (test-eq "priority queue has 1 deletion"
      1 (length (rpq-deletions pq)))))

;; Test retroactive deque
(test-group "retroactive-deque"
  (let ((deque (make-retro-deque '() '())))
    
    ;; Push to front and back
    (rd-push-front deque 1 'a)
    (rd-push-back deque 2 'b)
    (rd-push-front deque 3 'c)
    
    (test-eq "deque has 2 front operations"
      2 (length (rd-front-ops deque)))
    
    (test-eq "deque has 1 back operation"
      1 (length (rd-back-ops deque)))))

;; Test retroactive union-find
(test-group "retroactive-union-find"
  (let ((uf (make-retro-union-find '() '())))
    
    ;; Union operations
    (ruf-union uf 1 'a 'b)
    (ruf-union uf 3 'b 'c)
    (ruf-union uf 5 'c 'd)
    
    (test-eq "union-find has 3 unions"
      3 (length (ruf-unions uf)))
    
    ;; Test find at different times
    (test-eq "find a at time 0"
      'a (ruf-find uf 0 'a 0))
    
    (test-eq "find a at time 2 (after union with b)"
      'a (ruf-find uf 0 'a 2))))

;; Test helper functions
(test-group "helper-functions"
  (test-equal "insert-sorted empty list"
    '(5) (insert-sorted '() 5 <))
  
  (test-equal "insert-sorted beginning"
    '(1 3 5) (insert-sorted '(3 5) 1 <))
  
  (test-equal "insert-sorted middle"
    '(1 3 5) (insert-sorted '(1 5) 3 <))
  
  (test-equal "insert-sorted end"
    '(1 3 5) (insert-sorted '(1 3) 5 <))
  
  (test-eq "count-ops-between"
    2 (count-ops-between 
       (list (make-retro-operation 1 'insert 'a)
             (make-retro-operation 3 'insert 'b)
             (make-retro-operation 5 'insert 'c)
             (make-retro-operation 7 'insert 'd))
       2 5)))

;; Test performance metrics
(test-group "performance-metrics"
  (let ((metrics (make-performance-metrics 10 50)))
    (test-eq "replay count"
      10 (pm-replay-count metrics))
    
    (test-eq "total ops"
      50 (pm-total-ops metrics))))

(test-end "retroactive-data-structures")

;; Exit with appropriate code
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))