#!/usr/bin/env guile
!#

;;; node-copying-analysis.scm - Analyze node copying method for persistence

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 format))

;; Load persistent implementation
(load "../src/persistent.scm")

;;; Analysis of node copying costs

(define (analyze-node-copying-costs)
  "Analyze the costs of node copying for different operations"
  (format #t "~%=== Node Copying Method Analysis ===~%")
  (format #t "~%")
  
  ;; Path copying example
  (format #t "Path Copying Example:~%")
  (format #t "~%")
  (format #t "Original tree:~%")
  (format #t "       5~%")
  (format #t "      / \\~%")
  (format #t "     3   8~%") 
  (format #t "    / \\ / \\~%")
  (format #t "   1  4 6  9~%")
  (format #t "~%")
  
  (format #t "After inserting 7 (nodes marked with * are copied):~%")
  (format #t "       5*~%")
  (format #t "      / \\~%")
  (format #t "     3   8*~%")
  (format #t "    / \\ / \\~%")
  (format #t "   1  4 6* 9~%")
  (format #t "         \\~%")
  (format #t "          7~%")
  (format #t "~%")
  
  (format #t "Cost Analysis:~%")
  (format #t "- Nodes copied: 3 (path from root to insertion point)~%")
  (format #t "- Time complexity: O(log n) for balanced tree~%")
  (format #t "- Space complexity: O(log n) new nodes~%")
  (format #t "- Old version remains unchanged~%"))

(define (benchmark-version-branching n branches)
  "Benchmark creating multiple versions from a single base"
  (format #t "~%~%=== Version Branching Analysis ===~%")
  (format #t "Base stack size: ~a elements~%" n)
  (format #t "Creating ~a different versions...~%" branches)
  
  ;; Create base stack
  (let ((base (fold (lambda (i stack)
                      (stack-push stack i))
                    empty-stack
                    (iota n))))
    
    ;; Create multiple versions
    (let ((versions
           (map (lambda (branch-id)
                  (stack-push base (+ n branch-id)))
                (iota branches))))
      
      (format #t "~%Results:~%")
      (format #t "- Base nodes: ~a~%" n)
      (format #t "- New nodes created: ~a (1 per version)~%" branches)
      (format #t "- Total nodes: ~a~%" (+ n branches))
      (format #t "- Nodes per version if not shared: ~a~%" (+ n 1))
      (format #t "- Total without sharing: ~a~%" (* branches (+ n 1)))
      (format #t "- Space savings: ~,1f%~%"
              (* 100 (- 1 (/ (+ n branches) 
                             (* branches (+ n 1) 1.0)))))
      
      ;; Demonstrate independence
      (format #t "~%Independence verification:~%")
      (let ((v1 (car versions))
            (v2 (cadr versions)))
        (format #t "- Version 1 top: ~a~%" (stack-top v1))
        (format #t "- Version 2 top: ~a~%" (stack-top v2))
        (format #t "- Modifying version 1...~%")
        (let ((v1-modified (stack-pop v1)))
          (format #t "- Version 1 after pop: ~a~%"
                  (if (stack-empty? v1-modified)
                      "empty"
                      (stack-top v1-modified)))
          (format #t "- Version 2 unchanged: ~a~%" (stack-top v2))
          (format #t "- Base unchanged: ~a~%" (stack-top base)))))))

(define (analyze-operation-sequences)
  "Analyze different operation sequences and their costs"
  (format #t "~%~%=== Operation Sequence Analysis ===~%")
  
  ;; Sequence 1: Linear history
  (format #t "~%1. Linear History (push only):~%")
  (format #t "   v0 → v1 → v2 → v3 → v4~%")
  (format #t "   Cost: 1 new node per operation~%")
  (format #t "   Total nodes: n~%")
  
  ;; Sequence 2: Branching history
  (format #t "~%2. Branching History:~%")
  (format #t "       v0~%")
  (format #t "      / \\~%")
  (format #t "     v1  v2~%")
  (format #t "    / \\   \\~%")
  (format #t "   v3  v4  v5~%")
  (format #t "   Cost: Shared prefix, 1 new node per branch~%")
  (format #t "   Much better than copying entire structure~%")
  
  ;; Sequence 3: Time travel
  (format #t "~%3. Time Travel Pattern:~%")
  (format #t "   Create v1, v2, v3, then go back to v1~%")
  (format #t "   All versions remain accessible~%")
  (format #t "   No reconstruction needed~%"))

(define (compare-with-alternatives)
  "Compare node copying with other persistence methods"
  (format #t "~%~%=== Comparison with Other Methods ===~%")
  
  (format #t "~%1. Node Copying (what we use):~%")
  (format #t "   ✓ Simple to implement~%")
  (format #t "   ✓ Good locality of reference~%")
  (format #t "   ✓ No overhead on queries~%")
  (format #t "   ✗ O(log n) update cost~%")
  
  (format #t "~%2. Fat Node Method:~%")
  (format #t "   ✓ In-place updates possible~%")
  (format #t "   ✓ Can be more space efficient~%")
  (format #t "   ✗ Complex implementation~%")
  (format #t "   ✗ Overhead on queries~%")
  
  (format #t "~%3. Path Copying with Modifications:~%")
  (format #t "   ✓ Can combine with other techniques~%")
  (format #t "   ✓ Basis for advanced methods~%")
  (format #t "   Example: Sleator-Tarjan method for O(1) updates~%"))

(define (demonstrate-persistence-benefits)
  "Show practical benefits of persistence"
  (format #t "~%~%=== Practical Benefits of Persistence ===~%")
  
  (format #t "~%1. Undo/Redo functionality:~%")
  (let* ((s0 empty-stack)
         (s1 (stack-push s0 'edit1))
         (s2 (stack-push s1 'edit2))
         (s3 (stack-push s2 'edit3)))
    (format #t "   Current state: ~a~%" (stack->list s3))
    (format #t "   Undo to previous: ~a~%" (stack->list s2))
    (format #t "   Undo again: ~a~%" (stack->list s1))
    (format #t "   Original: ~a~%" (stack->list s0)))
  
  (format #t "~%2. Parallel algorithms:~%")
  (format #t "   Multiple threads can work on different versions~%")
  (format #t "   No locking needed for read operations~%")
  
  (format #t "~%3. Debugging and auditing:~%")
  (format #t "   Complete history of data structure evolution~%")
  (format #t "   Can replay operations for debugging~%"))

;;; Main
(define (main args)
  (format #t "MIT 6.851 Session 1 - Node Copying Analysis~%")
  (format #t "==========================================~%")
  
  (analyze-node-copying-costs)
  (benchmark-version-branching 100 10)
  (analyze-operation-sequences)
  (compare-with-alternatives)
  (demonstrate-persistence-benefits)
  
  (format #t "~%~%=== Summary ===~%")
  (format #t "Node copying provides elegant persistence with:~%")
  (format #t "- O(log n) time per update (for balanced trees)~%")
  (format #t "- O(log n) space per update~%")
  (format #t "- No query overhead~%")
  (format #t "- Simple implementation~%")
  (format #t "~%Perfect for functional programming and many applications!~%"))

(main (command-line))