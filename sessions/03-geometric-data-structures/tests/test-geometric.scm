#!/usr/bin/env guile
!#

(use-modules (srfi srfi-64)
             (srfi srfi-1))

;; Load the geometric implementation
(load "../src/geometric.scm")

;; Start test suite
(test-begin "geometric-data-structures")

;; Test point and edge creation
(test-group "basic-structures"
  (let ((p1 (make-point 0 0))
        (p2 (make-point 10 5)))
    
    (test-eq "point x coordinate"
      0 (point-x p1))
    
    (test-eq "point y coordinate"
      5 (point-y p2))
    
    (let ((edge (make-edge p1 p2 'left 'right)))
      (test-assert "edge creation"
        (edge? edge))
      
      (test-eq "edge start"
        p1 (edge-start edge))
      
      (test-eq "edge end"
        p2 (edge-end edge)))))

;; Test multi-dimensional points
(test-group "multi-dimensional-points"
  (let ((p1 (make-d-point '(1 2 3)))
        (p2 (make-d-point '(4 5 6))))
    
    (test-equal "d-point coordinates"
      '(1 2 3) (d-point-coords p1))
    
    (let ((box (make-box '(0 0 0) '(5 5 5))))
      (test-assert "point in box"
        (point-in-box? p1 box))
      
      (test-assert "point not in box"
        (not (point-in-box? p2 box))))))

;; Test sweep line events
(test-group "sweep-line"
  (let* ((p1 (make-point 0 0))
         (p2 (make-point 10 5))
         (seg (make-edge p1 p2 #f #f))
         (event1 (make-sweep-event 0 'start seg))
         (event2 (make-sweep-event 10 'end seg)))
    
    (test-eq "event x-coordinate"
      0 (event-x event1))
    
    (test-eq "event type"
      'start (event-type event1))
    
    (test-assert "event comparison"
      (event<? event1 event2))))

;; Test range tree construction
(test-group "range-tree"
  (let ((points (list (make-d-point '(1))
                      (make-d-point '(3))
                      (make-d-point '(5))
                      (make-d-point '(7))
                      (make-d-point '(9)))))
    
    (let ((tree (build-1d-range-tree points)))
      (test-assert "1D range tree built"
        (range-node? tree))
      
      (test-assert "tree has median key"
        (d-point? (node-key tree))))))

;; Test layered range tree
(test-group "layered-range-tree"
  (let ((points (list (make-point 1 2)
                      (make-point 3 4)
                      (make-point 5 6)
                      (make-point 7 8))))
    
    (let ((tree (build-layered-range-tree points)))
      (test-assert "layered tree built"
        (layered-node? tree))
      
      (test-assert "has y-array"
        (vector? (lnode-y-array tree)))
      
      (test-eq "y-array length"
        4 (vector-length (lnode-y-array tree))))))

;; Test weight balance
(test-group "weight-balance"
  (let ((node (make-wb-range-node 5 #f #f #f 1)))
    
    (test-eq "node size"
      1 (wb-size node))
    
    (test-assert "single node is balanced"
      (wb-balanced? node 0.25))))

;; Test fractional cascading
(test-group "fractional-cascading"
  (test-equal "sample every other - empty"
    '() (sample-every-other '()))
  
  (test-equal "sample every other - single"
    '(1) (sample-every-other '(1)))
  
  (test-equal "sample every other - multiple"
    '(1 3 5) (sample-every-other '(1 2 3 4 5)))
  
  (let ((lists '((1 3 5 7) (2 4 6 8) (1 2 3 4))))
    (test-assert "build cascading structure"
      (list? (build-fractional-cascading lists)))))

;; Test binary search
(test-group "binary-search"
  (let ((array #(1 3 5 7 9)))
    
    (test-eq "binary search - found"
      2 (binary-search array 5))
    
    (test-eq "binary search - insertion point before"
      0 (binary-search array 0))
    
    (test-eq "binary search - insertion point after"
      5 (binary-search array 10))
    
    (test-eq "binary search - insertion point middle"
      2 (binary-search array 4))))

;; Test utility functions
(test-group "utilities"
  (let ((lst '(5 2 8 1 9)))
    (test-eq "minimum with key function"
      1 (minimum lst identity)))
  
  (let ((points (list (make-d-point '(3 1))
                      (make-d-point '(1 3))
                      (make-d-point '(2 2)))))
    (let ((sorted (sort-by-dimension points 1)))
      (test-equal "sort by dimension 1"
        1 (car (d-point-coords (car sorted))))))
  
  (test-equal "merge sorted lists"
    '(1 2 3 4 5 6) (merge '(1 3 5) '(2 4 6) <)))

(test-end "geometric-data-structures")

;; Exit with appropriate code
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))