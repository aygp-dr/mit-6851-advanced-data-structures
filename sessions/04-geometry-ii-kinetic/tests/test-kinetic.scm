#!/usr/bin/env guile
!#

(use-modules (srfi srfi-64)
             (srfi srfi-1))

;; Load the kinetic implementation
(load "../src/kinetic.scm")

;; Start test suite
(test-begin "kinetic-data-structures")

;; Test 3D point and box structures
(test-group "3d-structures"
  (let ((p1 (make-point-3d 1 2 3))
        (p2 (make-point-3d 4 5 6)))
    
    (test-eq "3d point x coordinate"
      1 (point-3d-x p1))
    
    (test-eq "3d point y coordinate"
      2 (point-3d-y p1))
    
    (test-eq "3d point z coordinate"
      3 (point-3d-z p1))
    
    (let ((box (make-box-3d 0 5 0 5 0 5)))
      (test-assert "3d box creation"
        (box-3d? box))
      
      (test-eq "box x-min"
        0 (box-3d-x-min box))
      
      (test-eq "box x-max"
        5 (box-3d-x-max box)))))

;; Test kinetic point trajectories
(test-group "kinetic-points"
  (let* ((traj (make-affine-trajectory 10 2))  ; position = 10 + 2t
         (kpoint (make-kinetic-point 'p1 
                   (lambda (t) (evaluate-trajectory traj t)))))
    
    (test-eq "kinetic point id"
      'p1 (kpoint-id kpoint))
    
    (test-eq "position at t=0"
      10 ((kpoint-position-fn kpoint) 0))
    
    (test-eq "position at t=5"
      20 ((kpoint-position-fn kpoint) 5))))

;; Test certificates
(test-group "certificates"
  (let* ((traj1 (make-affine-trajectory 0 1))   ; position = t
         (traj2 (make-affine-trajectory 10 -1))  ; position = 10 - t
         (kp1 (make-kinetic-point 'p1 (lambda (t) (evaluate-trajectory traj1 t))))
         (kp2 (make-kinetic-point 'p2 (lambda (t) (evaluate-trajectory traj2 t)))))
    
    (let ((cert (make-order-certificate kp1 kp2 0)))
      (test-assert "certificate creation"
        (certificate? cert))
      
      (test-eq "certificate failure time"
        5.0 (cert-failure-time cert))  ; They cross at t=5
      
      (test-equal "certificate condition"
        `(order ,kp1 ,kp2) (cert-condition cert)))))

;; Test crossing time computation
(test-group "crossing-times"
  (let ((traj1 (make-affine-trajectory 0 2))    ; 2t
        (traj2 (make-affine-trajectory 10 1)))   ; 10 + t
    
    (test-eq "crossing time calculation"
      10.0 (compute-crossing-time traj1 traj2 0))
    
    (test-eq "parallel trajectories"
      +inf.0 (compute-crossing-time traj1 traj1 0))
    
    (test-eq "crossing in past"
      +inf.0 (compute-crossing-time traj2 traj1 20))))

;; Test kinetic events
(test-group "kinetic-events"
  (let* ((cert (make-certificate '(test) 5.0))
         (event (make-kinetic-event 5.0 cert)))
    
    (test-eq "event time"
      5.0 (event-time event))
    
    (test-eq "event certificate"
      cert (event-certificate event))))

;; Test kinetic data structure base
(test-group "kinetic-ds"
  (let ((kds (make-kinetic-ds 0 '() '())))
    
    (test-eq "initial time"
      0 (kds-time kds))
    
    (set-kds-time! kds 10)
    (test-eq "updated time"
      10 (kds-time kds))))

;; Test fractional cascading helpers
(test-group "fractional-cascading-3d"
  (let ((points (list (make-point-3d 1 2 3)
                      (make-point-3d 4 5 6)
                      (make-point-3d 7 8 9))))
    
    (let ((segments (points->horizontal-segments points)))
      (test-eq "number of segments"
        3 (length segments))
      
      (test-assert "segments are horizontal"
        (every horizontal-segment? segments)))))

;; Test range tree nodes
(test-group "range-tree-3d"
  (let ((node (make-range-node-3d 5 #f #f #f)))
    
    (test-eq "node key"
      5 (rnode-3d-key node))
    
    (test-assert "node creation"
      (range-node-3d? node))))

;; Test inverted range nodes
(test-group "inverted-range-tree"
  (let ((node (make-inverted-range-node 10 8 #f #f)))
    
    (test-eq "inverted node key"
      10 (irn-key node))
    
    (test-eq "max-left value"
      8 (irn-max-left node))))

;; Test kinetic BST
(test-group "kinetic-bst"
  (let ((kbst (make-kinetic-bst #f '())))
    
    (test-assert "kinetic BST creation"
      (kinetic-bst? kbst))
    
    (set-kbst-certs! kbst '(cert1 cert2))
    (test-eq "certificate count"
      2 (length (kbst-certs kbst)))))

;; Test kinetic heap
(test-group "kinetic-heap"
  (let ((kheap (make-kinetic-heap '() '())))
    
    (test-assert "kinetic heap creation"
      (kinetic-heap? kheap))
    
    (let ((metrics (kinetic-heap-metrics)))
      (test-equal "heap responsiveness"
        "O(log n) per event" (assoc-ref metrics 'responsive))
      
      (test-equal "heap locality"
        "O(1) certificates per element" (assoc-ref metrics 'local)))))

;; Test efficiency analysis
(test-group "efficiency-analysis"
  (let ((affine-analysis (analyze-kinetic-efficiency #f 'affine))
        (pseudo-alg-analysis (analyze-kinetic-efficiency #f 'pseudo-algebraic)))
    
    (test-equal "affine total events"
      "O(n²)" (assoc-ref affine-analysis 'total-events))
    
    (test-equal "pseudo-algebraic total events"
      "O(n² · λₛ(n))" (assoc-ref pseudo-alg-analysis 'total-events))))

;; Test known results
(test-group "kinetic-survey-results"
  (test-assert "results defined"
    (list? *kinetic-results*))
  
  (let ((convex-hull (assoc 'convex-hull-2d *kinetic-results*)))
    (test-assert "convex hull result exists"
      convex-hull)
    
    (when convex-hull
      (test-equal "convex hull efficiency"
        "O(n²+ε)" (assoc-ref (cdr convex-hull) 'efficiency)))))

;; Test open problems
(test-group "open-problems"
  (test-assert "open problems list exists"
    (list? *kinetic-open-problems*))
  
  (test-assert "has 3D convex hull problem"
    (member "3D convex hull efficiency" *kinetic-open-problems*)))

;; Test utility functions
(test-group "utilities"
  (test-equal "group by z - empty"
    '() (group-by-z '()))
  
  (let ((points (list (make-point-3d 1 2 3)
                      (make-point-3d 4 5 3)
                      (make-point-3d 7 8 6))))
    (let ((grouped (group-by-z points)))
      (test-assert "grouping result"
        (list? grouped)))))

;; Test certificate management
(test-group "certificate-management"
  (let ((cert1 (make-certificate '(order a b) 5))
        (cert2 (make-certificate '(order c d) 10))
        (cert3 (make-certificate '(order a c) 15)))
    
    (let ((filtered (remove-old-certificates (list cert1 cert2 cert3) 'a 'b)))
      (test-eq "removed certificates with a or b"
        1 (length filtered))
      
      (test-eq "kept unrelated certificate"
        cert2 (car filtered)))))

(test-end "kinetic-data-structures")

;; Exit with appropriate code
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))