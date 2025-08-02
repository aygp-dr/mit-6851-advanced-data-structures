#!/usr/bin/env guile
!#

;;; bst-comparison.scm - Compare different BST implementations

(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-43)  ; Vector operations
             (ice-9 format))

;; Simple BST implementation for comparison
(define-record-type <bst-node>
  (make-bst-node key left right)
  bst-node?
  (key node-key)
  (left node-left set-node-left!)
  (right node-right set-node-right!))

(define empty-bst #f)

(define (bst-insert tree key)
  "Insert key into BST (functional)"
  (cond
   ((not tree) (make-bst-node key #f #f))
   ((< key (node-key tree))
    (make-bst-node (node-key tree)
                   (bst-insert (node-left tree) key)
                   (node-right tree)))
   ((> key (node-key tree))
    (make-bst-node (node-key tree)
                   (node-left tree)
                   (bst-insert (node-right tree) key)))
   (else tree))) ; Key already exists

(define (bst-search tree key)
  "Search for key in BST"
  (cond
   ((not tree) #f)
   ((= key (node-key tree)) tree)
   ((< key (node-key tree)) (bst-search (node-left tree) key))
   (else (bst-search (node-right tree) key))))

(define (bst-height tree)
  "Compute height of BST"
  (if (not tree)
      0
      (+ 1 (max (bst-height (node-left tree))
                (bst-height (node-right tree))))))

;; Timing utilities
(define (time-operation thunk)
  "Time the execution of THUNK"
  (let ((start (current-time)))
    (let ((result (thunk)))
      (let ((end (current-time)))
        (cons result 
              (+ (time-second (time-difference end start))
                 (/ (time-nanosecond (time-difference end start)) 1e9)))))))

;; Benchmark scenarios
(define (generate-random-sequence n)
  "Generate n random integers"
  (map (lambda (i) (random 1000000)) (iota n)))

(define (generate-sequential-sequence n)
  "Generate sequential integers 1..n"
  (iota n 1))

(define (generate-alternating-sequence n)
  "Generate alternating small and large values"
  (let loop ((i 0) (small 0) (large 1000000) (result '()))
    (if (>= i n)
        (reverse result)
        (if (even? i)
            (loop (+ i 1) (+ small 1) large (cons small result))
            (loop (+ i 1) small (- large 1) (cons large result))))))

(define (benchmark-insertions name sequence)
  "Benchmark inserting all elements from sequence"
  (format #t "~%=== ~a (n=~a) ===~%" name (length sequence))
  
  (let* ((timing (time-operation
                  (lambda ()
                    (fold (lambda (key tree)
                            (bst-insert tree key))
                          empty-bst
                          sequence))))
         (tree (car timing))
         (time (cdr timing)))
    
    (format #t "  Insertion time: ~,3f seconds~%" time)
    (format #t "  Average per insert: ~,6f ms~%" 
            (* 1000 (/ time (length sequence))))
    (format #t "  Final tree height: ~a~%" (bst-height tree))
    (format #t "  Height/log₂(n) ratio: ~,2f~%" 
            (/ (bst-height tree) (log (length sequence) 2)))
    
    tree))

(define (benchmark-searches tree sequence num-searches)
  "Benchmark searching for random elements"
  (let* ((n (length sequence))
         (search-keys (map (lambda (i) (list-ref sequence (random n)))
                          (iota num-searches)))
         (timing (time-operation
                  (lambda ()
                    (for-each (lambda (key)
                                (bst-search tree key))
                              search-keys)))))
    
    (format #t "  Search time (~a searches): ~,3f seconds~%" 
            num-searches (cdr timing))
    (format #t "  Average per search: ~,6f ms~%"
            (* 1000 (/ (cdr timing) num-searches)))))

(define (analyze-tree-shape tree)
  "Analyze the shape/balance of a tree"
  (define (count-nodes tree)
    (if (not tree)
        0
        (+ 1 (count-nodes (node-left tree))
             (count-nodes (node-right tree)))))
  
  (define (sum-depths tree depth)
    (if (not tree)
        0
        (+ depth
           (sum-depths (node-left tree) (+ depth 1))
           (sum-depths (node-right tree) (+ depth 1)))))
  
  (let ((n (count-nodes tree))
        (total-depth (sum-depths tree 0)))
    (when (> n 0)
      (format #t "  Average node depth: ~,2f~%" (/ total-depth n))
      (format #t "  Optimal height: ~a~%" (ceiling (log (+ n 1) 2)))
      (format #t "  Height efficiency: ~,1f%~%"
              (* 100 (/ (ceiling (log (+ n 1) 2))
                        (bst-height tree)))))))

(define (compare-bst-performance)
  "Compare BST performance on different input patterns"
  (let ((sizes '(1000 5000 10000)))
    
    (format #t "~%BST Performance Comparison~%")
    (format #t "=========================~%")
    
    (for-each
     (lambda (n)
       (format #t "~%~%Dataset size: ~a~%" n)
       (format #t "-------------------~%")
       
       ;; Random sequence
       (let* ((random-seq (generate-random-sequence n))
              (tree (benchmark-insertions "Random insertion" random-seq)))
         (benchmark-searches tree random-seq 1000)
         (analyze-tree-shape tree))
       
       ;; Sequential sequence (worst case for naive BST)
       (let* ((seq-seq (generate-sequential-sequence n))
              (tree (benchmark-insertions "Sequential insertion" seq-seq)))
         (benchmark-searches tree seq-seq 1000)
         (analyze-tree-shape tree))
       
       ;; Alternating sequence
       (let* ((alt-seq (generate-alternating-sequence n))
              (tree (benchmark-insertions "Alternating insertion" alt-seq)))
         (benchmark-searches tree alt-seq 1000)
         (analyze-tree-shape tree)))
     
     sizes)))

(define (print-analysis)
  (format #t "~%~%=== Analysis ===~%")
  (format #t "1. Random insertions produce reasonably balanced trees~%")
  (format #t "2. Sequential insertions create degenerate (linear) trees~%")
  (format #t "3. Alternating patterns create specific unbalanced shapes~%")
  (format #t "~%")
  (format #t "This motivates the need for self-balancing BSTs like:~%")
  (format #t "  - AVL trees (height-balanced)~%")
  (format #t "  - Red-Black trees (approximately balanced)~%")
  (format #t "  - Splay trees (self-adjusting)~%")
  (format #t "  - B-trees (for external memory)~%")
  (format #t "~%")
  (format #t "In 6.851, we'll see how to make these persistent!~%"))

;; Main
(define (main args)
  (format #t "MIT 6.851 - Binary Search Tree Comparison~%")
  (format #t "=========================================~%")
  
  ;; Set random seed for reproducibility
  (set! *random-state* (random-state-from-platform))
  
  (compare-bst-performance)
  (print-analysis))

(main (command-line))