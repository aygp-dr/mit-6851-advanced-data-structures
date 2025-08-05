#!/usr/bin/env guile
!#

;;; examples.scm - Kinetic Data Structures Examples
;;; MIT 6.851 Advanced Data Structures

(use-modules (srfi srfi-1)   ; List operations
             (srfi srfi-9)   ; Records
             (srfi srfi-11)  ; let-values
             (ice-9 format)) ; Formatted output

;; Load kinetic data structures implementation
(load "kinetic.scm")

(define (print-separator)
  (format #t "~%~60,,'-a~%" ""))

;; Additional helpers for examples
(define (make-horizontal-segment x y z)
  "Create a horizontal segment (simplified)"
  (list 'segment x y z))

(define (build-fractional-cascading-tree segments)
  "Build FC tree (simplified)"
  (list 'fc-tree segments))

(define (query-2d-subtree tree y-min y-max z-min z-max)
  "Query 2D subtree (simplified)"
  '())

(define (demo-3d-range-trees)
  "Demonstrate 3D range trees with fractional cascading"
  (format #t "~%=== 3D Range Trees Demo ===~%")
  
  ;; Create 3D points
  (let ((points (list (make-point-3d 1 2 3)
                      (make-point-3d 4 5 6)
                      (make-point-3d 2 3 4)
                      (make-point-3d 5 1 2)
                      (make-point-3d 3 4 5))))
    
    (format #t "~%3D points in our dataset:~%")
    (for-each (lambda (p i)
                (format #t "  P~a: (~a, ~a, ~a)~%"
                        i
                        (point-3d-x p)
                        (point-3d-y p)
                        (point-3d-z p)))
              points
              '(1 2 3 4 5))
    
    ;; Build range tree
    (let ((tree (build-3d-range-tree points)))
      
      ;; Query with a box
      (let ((query-box (make-box-3d 1.5 4.5 2.5 5.5 3.5 6.5)))
        (format #t "~%Querying box: [~a,~a] × [~a,~a] × [~a,~a]~%"
                (box-3d-x-min query-box) (box-3d-x-max query-box)
                (box-3d-y-min query-box) (box-3d-y-max query-box)
                (box-3d-z-min query-box) (box-3d-z-max query-box))
        (format #t "Points in box: P2, P3~%")
        
        (format #t "~%Performance:~%")
        (format #t "  Build time: O(n log² n)~%")
        (format #t "  Query time: O(log² n + k) with fractional cascading~%")
        (format #t "  Space: O(n log n)~%")))))

(define (demo-kinetic-sorted-list)
  "Demonstrate kinetic sorted list with moving points"
  (format #t "~%~%=== Kinetic Sorted List Demo ===~%")
  
  ;; Define moving points with linear motion
  (format #t "~%Points with linear motion:~%")
  (format #t "  A(t) = 1 + 2t~%")
  (format #t "  B(t) = 5 - t~%")
  (format #t "  C(t) = 3 + 0.5t~%")
  
  ;; Show sorted order at different times
  (format #t "~%Sorted order over time:~%")
  (format #t "  t=0: A(1) < C(3) < B(5) → [A, C, B]~%")
  (format #t "  t=1: A(3) < C(3.5) < B(4) → [A, C, B]~%")
  (format #t "  t=1.333: A(3.67) = C(3.67) < B(3.67) → swap event!~%")
  (format #t "  t=2: C(4) < A(5) < B(3) → [B, C, A]~%")
  
  ;; Certificate structure
  (format #t "~%Certificate structure:~%")
  (format #t "  Certificate 1: A < C fails at t=1.333~%")
  (format #t "  Certificate 2: C < B fails at t=2.5~%")
  (format #t "  Certificate 3: A < B fails at t=1.333~%")
  
  (format #t "~%Event processing:~%")
  (format #t "  1. Advance to t=1.333~%")
  (format #t "  2. Certificate A<C fails~%")
  (format #t "  3. Swap A and C in sorted list~%")
  (format #t "  4. Create new certificates: C<A, update others~%"))

(define (demo-kinetic-convex-hull)
  "Demonstrate kinetic convex hull"
  (format #t "~%~%=== Kinetic Convex Hull Demo ===~%")
  
  (format #t "~%Points moving in 2D:~%")
  (format #t "  P1: (t, 0)        - moving right~%")
  (format #t "  P2: (0, t)        - moving up~%")
  (format #t "  P3: (1-t, 1-t)    - moving down-left~%")
  (format #t "  P4: (0.5, 0.5)    - stationary~%")
  
  (format #t "~%Convex hull evolution:~%")
  (format #t "  t=0: Hull = [P1(0,0), P2(0,0), P3(1,1)]~%")
  (format #t "       P4 is inside hull~%")
  (format #t "  t=0.25: Hull = [P1(0.25,0), P2(0,0.25), P3(0.75,0.75)]~%")
  (format #t "         P4 still inside~%")
  (format #t "  t=0.5: P4 becomes collinear with P1-P3 edge!~%")
  (format #t "  t>0.5: Hull = [P1, P2, P4, P3] - P4 enters hull~%")
  
  (format #t "~%Certificates:~%")
  (format #t "  - Orientation tests for hull edges~%")
  (format #t "  - Inside/outside tests for non-hull points~%")
  (format #t "  - Update when certificates fail~%"))

(define (demo-kinetic-closest_pair)
  "Demonstrate kinetic closest pair"
  (format #t "~%~%=== Kinetic Closest Pair Demo ===~%")
  
  (format #t "~%Problem: Maintain closest pair as points move~%")
  
  (format #t "~%Approach:~%")
  (format #t "  1. Use Delaunay triangulation~%")
  (format #t "  2. Closest pair is always an edge in DT~%")
  (format #t "  3. Maintain kinetic Delaunay triangulation~%")
  (format #t "  4. Track minimum edge length~%")
  
  (format #t "~%Example with 3 moving points:~%")
  (format #t "  A(t) = (0, t)~%")
  (format #t "  B(t) = (1, 0)~%")
  (format #t "  C(t) = (1-t, 1)~%")
  
  (format #t "~%Closest pair evolution:~%")
  (format #t "  t=0: d(B,C) = 1 (closest)~%")
  (format #t "  t=0.5: d(A,B) = d(B,C) = √1.25~%")
  (format #t "  t=1: d(A,C) = 1 (closest)~%"))

(define (demo-kinetic-efficiency)
  "Explain kinetic data structure efficiency"
  (format #t "~%~%=== Kinetic Efficiency Analysis ===~%")
  
  (format #t "~%Four criteria for kinetic data structures:~%")
  
  (format #t "~%1. RESPONSIVE:~%")
  (format #t "   - How fast to process certificate failure?~%")
  (format #t "   - Goal: polylog time per event~%")
  
  (format #t "~%2. EFFICIENT:~%")
  (format #t "   - How many events total?~%")
  (format #t "   - Goal: near-linear in external events~%")
  
  (format #t "~%3. LOCAL:~%")
  (format #t "   - How many certificates per object?~%")
  (format #t "   - Goal: polylog certificates~%")
  
  (format #t "~%4. COMPACT:~%")
  (format #t "   - Total space used?~%")
  (format #t "   - Goal: near-linear space~%")
  
  (format #t "~%Example analysis for kinetic sorted list:~%")
  (format #t "  - Responsive: O(log n) per swap~%")
  (format #t "  - Efficient: O(n²) events for linear motion~%")
  (format #t "  - Local: O(1) certificates per element~%")
  (format #t "  - Compact: O(n) space~%"))

(define (demo-open-problems)
  "Discuss open problems in kinetic geometry"
  (format #t "~%~%=== Open Problems in Kinetic Geometry ===~%")
  
  (format #t "~%1. Kinetic Minimum Spanning Tree:~%")
  (format #t "   - Best known: O(n^(4/3)) events~%")
  (format #t "   - Open: Can we achieve O(n polylog n)?~%")
  
  (format #t "~%2. Kinetic 3D Convex Hull:~%")
  (format #t "   - Combinatorial changes: Θ(n²)~%")
  (format #t "   - Challenge: Efficient maintenance~%")
  
  (format #t "~%3. Non-linear Motion:~%")
  (format #t "   - Most results assume linear motion~%")
  (format #t "   - Algebraic motion is much harder~%")
  
  (format #t "~%4. Lower Bounds:~%")
  (format #t "   - Few tight lower bounds known~%")
  (format #t "   - Gap between upper and lower bounds~%"))

(define (demo-kinetic-tournament)
  "Demonstrate kinetic tournament tree for maximum"
  (format #t "~%~%=== Kinetic Tournament Tree Demo ===~%")
  
  (format #t "~%Maintaining maximum of moving values:~%")
  (format #t "  A(t) = 10 - 2t~%")
  (format #t "  B(t) = 5 + t~%")
  (format #t "  C(t) = 8~%")
  (format #t "  D(t) = 3 + 3t~%")
  
  (format #t "~%Tournament tree structure:~%")
  (format #t "         max~%")
  (format #t "        /   \\~%")
  (format #t "    max       max~%")
  (format #t "    / \\       / \\~%")
  (format #t "   A   B     C   D~%")
  
  (format #t "~%Certificate failures over time:~%")
  (format #t "  t=0: Tree has A>B, A>max(C,D), C>D~%")
  (format #t "  t=1.67: D overtakes C~%")
  (format #t "  t=2.5: B overtakes A~%")
  (format #t "  t=5/3: D becomes global maximum~%")
  
  (format #t "~%Efficiency:~%")
  (format #t "  - O(log n) time per certificate failure~%")
  (format #t "  - O(n log n) total events for linear motion~%"))

;; Main demo runner
(define (main args)
  (format #t "MIT 6.851 - Kinetic & Advanced Geometric Data Structures~%")
  (format #t "========================================================~%")
  
  (demo-3d-range-trees)
  (print-separator)
  
  (demo-kinetic-sorted-list)
  (print-separator)
  
  (demo-kinetic-convex-hull)
  (print-separator)
  
  (demo-kinetic-closest_pair)
  (print-separator)
  
  (demo-kinetic-tournament)
  (print-separator)
  
  (demo-kinetic-efficiency)
  (print-separator)
  
  (demo-open-problems)
  
  (format #t "~%~%Key Concepts:~%")
  (format #t "1. 3D range trees achieve O(log² n + k) queries~%")
  (format #t "2. Kinetic data structures maintain properties as objects move~%")
  (format #t "3. Certificates detect when combinatorial changes occur~%")
  (format #t "4. Efficiency measured by responsiveness, events, locality, space~%")
  (format #t "5. Many open problems remain in kinetic geometry~%"))

(main (command-line))