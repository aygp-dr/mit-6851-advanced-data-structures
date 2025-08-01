#!/usr/bin/env guile
!#

(use-modules (srfi srfi-64)
             (srfi srfi-1))

;; Load the dynamic optimality implementation
(load "../src/dynamic-optimality.scm")

;; Start test suite
(test-begin "dynamic-optimality")

;; Test BST node creation
(test-group "bst-nodes"
  (let ((node (make-bst-node 5 #f #f #f)))
    
    (test-eq "node key"
      5 (node-key node))
    
    (test-assert "node creation"
      (bst-node? node))
    
    (test-eq "no left child"
      #f (node-left node))
    
    (test-eq "no right child"
      #f (node-right node))))

;; Test access sequence tracking
(test-group "access-sequences"
  (let ((seq (make-access-sequence '(3 1 4 1 5) '(0 1 2 3 4))))
    
    (test-equal "sequence keys"
      '(3 1 4 1 5) (seq-keys seq))
    
    (test-equal "sequence times"
      '(0 1 2 3 4) (seq-times seq))))

;; Test tracked BST
(test-group "tracked-bst"
  (let ((tree (make-tracked-bst #f '() 0)))
    
    (test-eq "empty root"
      #f (bst-root tree))
    
    (test-eq "initial node count"
      0 (bst-node-count tree))
    
    (set-bst-node-count! tree 5)
    (test-eq "updated node count"
      5 (bst-node-count tree))))

;; Test sequential access cost
(test-group "sequential-access"
  (test-eq "sequential cost for n=10"
    10 (sequential-access-cost 10))
  
  (test-eq "sequential cost for n=100"
    100 (sequential-access-cost 100)))

;; Test dynamic finger property
(test-group "dynamic-finger"
  (test-approximate "dynamic finger cost"
    6.0 (dynamic-finger-cost '(1 2 4 8 16)) 0.1)
  
  (test-approximate "adjacent elements"
    0.0 (dynamic-finger-cost '(1 2 3 4 5)) 0.1))

;; Test entropy bound
(test-group "entropy-bound"
  (test-approximate "uniform distribution"
    2.0 (entropy-bound '(0.25 0.25 0.25 0.25)) 0.01)
  
  (test-approximate "skewed distribution"
    1.75 (entropy-bound '(0.5 0.25 0.125 0.125)) 0.01)
  
  (test-eq "zero probability"
    0 (entropy-bound '(1.0 0.0 0.0 0.0))))

;; Test working set tracker
(test-group "working-set"
  (let ((tracker (make-working-set-tracker '() '())))
    
    (test-assert "tracker creation"
      (working-set-tracker? tracker))
    
    (set-ws-last-access! tracker '((1 . 0) (2 . 1)))
    (test-equal "last access update"
      '((1 . 0) (2 . 1)) (ws-last-access tracker))))

;; Test space-time points
(test-group "space-time-points"
  (let ((point (make-st-point 42 7)))
    
    (test-eq "point space"
      42 (point-space point))
    
    (test-eq "point time"
      7 (point-time point)))
  
  (let ((points (access-sequence->point-set '(3 1 4 1 5))))
    (test-eq "number of points"
      5 (length points))
    
    (test-equal "first point"
      3 (point-space (car points)))
    
    (test-equal "first point time"
      0 (point-time (car points)))))

;; Test splay operations
(test-group "splay-trees"
  (let ((tree (make-empty-splay-tree)))
    
    (test-assert "empty splay tree"
      (tracked-bst? tree))
    
    (test-eq "empty root"
      #f (bst-root tree))))

;; Test rotation logic
(test-group "rotations"
  (let* ((root (make-bst-node 10 #f #f #f))
         (left (make-bst-node 5 #f #f root))
         (right (make-bst-node 15 #f #f root)))
    
    (set-node-left! root left)
    (set-node-right! root right)
    
    (test-assert "left child check"
      (is-left-child? left))
    
    (test-assert "right child not left"
      (not (is-left-child? right)))))

;; Test ASS properties
(test-group "arborally-satisfied-sets"
  (let ((points '()))
    (test-assert "empty set is ASS"
      (is-arborally-satisfied? points)))
  
  (let ((points (list (make-st-point 1 1)
                      (make-st-point 2 2))))
    (test-assert "two points on diagonal is ASS"
      (is-arborally-satisfied? points))))

;; Test rectangle containment
(test-group "rectangle-operations"
  (let ((p (make-st-point 5 5))
        (p1 (make-st-point 0 0))
        (p2 (make-st-point 10 10)))
    
    (test-assert "point in rectangle"
      (point-in-rectangle? p p1 p2))
    
    (let ((p-out (make-st-point 15 5)))
      (test-assert "point outside rectangle"
        (not (point-in-rectangle? p-out p1 p2))))))

;; Test split tree
(test-group "split-trees"
  (let ((tree (make-split-tree '(1 2 3 4 5) 1 5)))
    
    (test-equal "split tree items"
      '(1 2 3 4 5) (split-items tree))
    
    (test-eq "min pointer"
      1 (split-min tree))
    
    (test-eq "max pointer"
      5 (split-max tree))))

;; Test utility functions
(test-group "utilities"
  (test-approximate "log2 of 8"
    3.0 (log2 8) 0.01)
  
  (test-approximate "log2 of 1"
    0.0 (log2 1) 0.01)
  
  (let ((indexed (map-indexed (lambda (i x) (cons i x)) '(a b c))))
    (test-equal "map-indexed"
      '((0 . a) (1 . b) (2 . c)) indexed))
  
  (test-eq "count distinct between"
    3 (count-distinct-between '(1 2 3 2 4 3 5) 1 5))
  
  (test-assert "exists - true"
    (exists? '(1 2 3 4) (lambda (x) (= x 3))))
  
  (test-assert "exists - false"
    (not (exists? '(1 2 3 4) (lambda (x) (= x 5))))))

;; Test BST search
(test-group "bst-search"
  (let* ((root (make-bst-node 10 #f #f #f))
         (left (make-bst-node 5 #f #f root))
         (right (make-bst-node 15 #f #f root)))
    
    (set-node-left! root left)
    (set-node-right! root right)
    
    (test-eq "find root"
      root (bst-search root 10))
    
    (test-eq "find left"
      left (bst-search root 5))
    
    (test-eq "find right"
      right (bst-search root 15))
    
    (test-eq "not found"
      #f (bst-search root 7))))

;; Test performance metrics
(test-group "performance-metrics"
  (let ((metrics (analyze-splay-performance '(1 2 3 4 5))))
    (test-assert "splay performance returns number"
      (number? metrics)))
  
  (let ((ratio (competitive-ratio 'balanced '((1 2 3 4 5)))))
    (test-assert "competitive ratio is positive"
      (> ratio 0))))

(test-end "dynamic-optimality")

;; Exit with appropriate code
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))