#!/usr/bin/env guile
!#

;;; examples.scm - Dynamic Optimality Examples
;;; MIT 6.851 Advanced Data Structures

(use-modules (srfi srfi-1)   ; List operations
             (srfi srfi-9)   ; Records
             (srfi srfi-11)  ; let-values
             (ice-9 format)) ; Formatted output

;; Load dynamic optimality implementation
(load "dynamic-optimality.scm")

(define (print-separator)
  (format #t "~%~60,,'-a~%" ""))

;; Helper functions
(define (log2 n)
  "Logarithm base 2"
  (/ (log n) (log 2)))

(define infinity 1000000000000.0)  ; Large number for infinity

(define (map-indexed fn lst)
  "Map with index"
  (let loop ((lst lst) (i 0) (result '()))
    (if (null? lst)
        (reverse result)
        (loop (cdr lst) (+ i 1) 
              (cons (fn i (car lst)) result)))))

(define (count-distinct-between seq i j)
  "Count distinct elements between positions i and j"
  (let ((subseq (take (drop seq i) (- j i))))
    (length (delete-duplicates subseq))))

(define (search-with-bound tree finger target bound)
  "Search with bounded cost (simplified)"
  target)

(define (build-from-sorted-frequencies items)
  "Build tree from frequency-sorted items (simplified)"
  '())

(define (process-node node)
  "Process a node during traversal"
  #t)

(define (demo-bst-models)
  "Demonstrate different BST cost models"
  (format #t "~%=== Binary Search Tree Cost Models ===~%")
  
  (format #t "~%1. Static BST Model:~%")
  (format #t "   - Fixed tree structure~%")
  (format #t "   - Access cost = depth of node~%")
  (format #t "   - Example: Balanced BST on {1,2,3,4,5}~%")
  (format #t "             3~%")
  (format #t "           /   \\~%")
  (format #t "          2     4~%")
  (format #t "         /       \\~%")
  (format #t "        1         5~%")
  (format #t "   - Cost to access 1: 3 comparisons~%")
  
  (format #t "~%2. Dynamic BST Model (Rotations):~%")
  (format #t "   - Can rotate after each access~%")
  (format #t "   - Cost = depth + rotation cost~%")
  (format #t "   - Allows adaptation to access pattern~%")
  
  (format #t "~%3. Comparison:~%")
  (format #t "   - Static: Simple but inflexible~%")
  (format #t "   - Dynamic: Can achieve better amortized cost~%"))

(define (demo-sequential-access)
  "Demonstrate sequential access theorem"
  (format #t "~%~%=== Sequential Access Theorem ===~%")
  
  (format #t "~%Accessing elements 1, 2, 3, ..., n in order:~%")
  
  (format #t "~%Static balanced BST:~%")
  (format #t "  - Each access costs O(log n)~%")
  (format #t "  - Total cost: O(n log n)~%")
  
  (format #t "~%Splay tree (dynamic):~%")
  (format #t "  - First access: O(log n)~%")
  (format #t "  - Subsequent accesses: O(1) amortized~%")
  (format #t "  - Total cost: O(n)~%")
  
  (format #t "~%Example sequence [1,2,3,4,5]:~%")
  (let ((costs '(3 2 1 2 1)))  ; Splay tree costs
    (format #t "  Access costs: ~a~%" costs)
    (format #t "  Total: ~a (vs ~a for static BST)~%"
            (apply + costs)
            (* 5 (log2 5)))))

(define (demo-dynamic-finger)
  "Demonstrate dynamic finger property"
  (format #t "~%~%=== Dynamic Finger Property ===~%")
  
  (format #t "~%Property: Cost to access y after x is O(log |x-y|)~%")
  
  (let ((sequence '(10 12 11 20 21 5 4 6)))
    (format #t "~%Access sequence: ~a~%" sequence)
    (format #t "~%Finger costs:~%")
    (let loop ((seq sequence) (prev #f))
      (unless (null? seq)
        (let ((curr (car seq)))
          (when prev
            (let ((dist (abs (- curr prev))))
              (format #t "  ~a → ~a: distance=~a, cost=O(~,1f)~%"
                      prev curr dist (log2 (max 2 dist)))))
          (loop (cdr seq) curr))))
    
    (format #t "~%Total cost: O(~,1f)~%"
            (dynamic-finger-cost sequence))))

(define (demo-working_set)
  "Demonstrate working set property"
  (format #t "~%~%=== Working Set Property ===~%")
  
  (format #t "~%Property: Recently accessed items are cheap~%")
  (format #t "Cost to access x = O(log w(x))~%")
  (format #t "where w(x) = # distinct items since last access to x~%")
  
  (let ((sequence '(1 2 3 1 2 4 1 3)))
    (format #t "~%Access sequence: ~a~%" sequence)
    (format #t "~%Working set analysis:~%")
    
    (let ((last-seen (make-hash-table)))
      (map-indexed
       (lambda (i key)
         (let ((last-i (hash-ref last-seen key #f)))
           (if (not last-i)
               (format #t "  Access ~a (pos ~a): first access, w=~a~%"
                       key i i)
               (let ((distinct (length (delete-duplicates
                                        (take (drop sequence last-i)
                                              (- i last-i))))))
                 (format #t "  Access ~a (pos ~a): w=~a (distinct since pos ~a)~%"
                         key i distinct last-i)))
           (hash-set! last-seen key i)))
       sequence))))

(define (demo-entropy_bound)
  "Demonstrate entropy bound"
  (format #t "~%~%=== Entropy Bound ===~%")
  
  (format #t "~%Shannon's entropy gives lower bound on average access cost~%")
  
  (let ((frequencies '((A . 0.5) (B . 0.25) (C . 0.125) (D . 0.125))))
    (format #t "~%Item frequencies:~%")
    (for-each (lambda (pair)
                (format #t "  ~a: ~a%~%"
                        (car pair)
                        (* 100 (cdr pair))))
              frequencies)
    
    (let ((entropy (apply + (map (lambda (p)
                                    (let ((freq (cdr p)))
                                      (if (zero? freq) 0
                                          (* freq (log2 (/ 1 freq))))))
                                  frequencies))))
      (format #t "~%Entropy H = ~,2f bits~%" entropy)
      (format #t "~%Optimal static BST:~%")
      (format #t "      A(1)~%")
      (format #t "       \\~%")
      (format #t "        B(2)~%")
      (format #t "         \\~%")
      (format #t "          C(3)~%")
      (format #t "           \\~%")
      (format #t "            D(3)~%")
      (format #t "~%Average access cost = ~,2f~%"
              (+ (* 0.5 1) (* 0.25 2) (* 0.125 3) (* 0.125 3))))))

(define (demo-unified_property)
  "Demonstrate unified property"
  (format #t "~%~%=== Unified Property ===~%")
  
  (format #t "~%Combines working set and dynamic finger:~%")
  (format #t "Cost = O(min over y of log(|x-y| + w(y)))~%")
  
  (format #t "~%Benefits:~%")
  (format #t "  - Can use spatial locality (finger)~%")
  (format #t "  - Can use temporal locality (working set)~%")
  (format #t "  - Takes best of both worlds~%")
  
  (format #t "~%Example: Accessing x=50 after accessing 10,20,30,40,45,48:~%")
  (format #t "  - Finger from 48: log(|50-48|) = log(2) = 1~%")
  (format #t "  - Working set: log(6) ≈ 2.58~%")
  (format #t "  - Unified takes minimum: 1~%"))

(define (demo-dynamic_optimality_conjecture)
  "Explain dynamic optimality conjecture"
  (format #t "~%~%=== Dynamic Optimality Conjecture ===~%")
  
  (format #t "~%CONJECTURE: Splay trees are O(1)-competitive~%")
  (format #t "with the optimal offline BST algorithm~%")
  
  (format #t "~%What we know:~%")
  (format #t "  ✓ Splay trees satisfy:")
  (format #t "    - Sequential access: O(n)~%")
  (format #t "    - Working set property~%")
  (format #t "    - Dynamic finger property~%")
  (format #t "    - Entropy bound~%")
  
  (format #t "~%What we don't know:~%")
  (format #t "  ? Are splay trees optimal?~%")
  (format #t "  ? Is there a better online algorithm?~%")
  (format #t "  ? What is the optimal offline algorithm?~%")
  
  (format #t "~%Recent progress:~%")
  (format #t "  - Tango trees: O(log log n)-competitive~%")
  (format #t "  - Multi-splay trees: O(log log n)-competitive~%")
  (format #t "  - Optimal BST in polynomial time (2013)~%"))

(define (demo-tango_trees)
  "Explain Tango trees"
  (format #t "~%~%=== Tango Trees ===~%")
  
  (format #t "~%First O(log log n)-competitive BST!~%")
  
  (format #t "~%Key ideas:~%")
  (format #t "  1. Decompose into preferred paths~%")
  (format #t "  2. Store paths in auxiliary trees~%")
  (format #t "  3. Link auxiliary trees together~%")
  
  (format #t "~%Example decomposition:~%")
  (format #t "       4~%")
  (format #t "      / \\~%")
  (format #t "     2   6     → Preferred paths:~%")
  (format #t "    / \\ / \\      [4→6→7] (heavy)~%") 
  (format #t "   1  3 5  7     [4→2→1] (light)~%")
  (format #t "                 [3], [5] (singletons)~%")
  
  (format #t "~%Performance:~%")
  (format #t "  - Competitive ratio: O(log log n)~%")
  (format #t "  - Not as good as conjectured O(1)~%")
  (format #t "  - But provably good!~%"))

;; Main demo runner
(define (main args)
  (format #t "MIT 6.851 - Dynamic Optimality Examples~%")
  (format #t "=======================================~%")
  
  (demo-bst-models)
  (print-separator)
  
  (demo-sequential-access)
  (print-separator)
  
  (demo-dynamic-finger)
  (print-separator)
  
  (demo-working_set)
  (print-separator)
  
  (demo-entropy_bound)
  (print-separator)
  
  (demo-unified_property)
  (print-separator)
  
  (demo-dynamic_optimality_conjecture)
  (print-separator)
  
  (demo-tango_trees)
  
  (format #t "~%~%Key Takeaways:~%")
  (format #t "1. Dynamic BSTs can adapt to access patterns~%")
  (format #t "2. Multiple properties capture different localities~%")
  (format #t "3. Splay trees satisfy many optimality properties~%")
  (format #t "4. Dynamic optimality remains an open problem~%")
  (format #t "5. O(log log n)-competitive algorithms exist~%"))

(main (command-line))