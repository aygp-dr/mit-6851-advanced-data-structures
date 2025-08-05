#!/usr/bin/env guile
!#

;;; examples.scm - Geometric Data Structures Examples
;;; MIT 6.851 Advanced Data Structures

(use-modules (srfi srfi-1)   ; List operations
             (srfi srfi-9)   ; Records
             (ice-9 format)) ; Formatted output

;; Load geometric data structures implementation
(load "geometric.scm")

(define (print-separator)
  (format #t "~%~60,,'-a~%" ""))

;; Additional segment record for examples
(define-record-type <segment>
  (make-segment x1 y1 x2 y2)
  segment?
  (x1 segment-x1)
  (y1 segment-y1)
  (x2 segment-x2)
  (y2 segment-y2))

(define (segment-left-x seg)
  (min (segment-x1 seg) (segment-x2 seg)))

(define (segment-right-x seg)
  (max (segment-x1 seg) (segment-x2 seg)))

(define (segment-spans-x? seg x)
  "Check if segment spans the given x-coordinate"
  (<= (segment-left-x seg) x (segment-right-x seg)))

(define (segment-y-at-x seg x)
  "Get y-coordinate of segment at given x"
  (let ((x1 (segment-x1 seg))
        (y1 (segment-y1 seg))
        (x2 (segment-x2 seg))
        (y2 (segment-y2 seg)))
    (if (= x1 x2)
        y1  ; Vertical segment
        (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1))))))

;; Helper for finding minimum by key
(define (minimum lst key-fn)
  "Find element with minimum key value"
  (if (null? lst)
      #f
      (fold (lambda (item min-item)
              (if (< (key-fn item) (key-fn min-item))
                  item
                  min-item))
            (car lst)
            (cdr lst))))

;; Empty BST placeholder
(define empty-bst '())

(define (process-event bst event)
  "Process sweep event on BST (simplified)"
  (case (event-type event)
    ((start) (cons (event-segment event) bst))
    ((end) (delete (event-segment event) bst))))

(define (demo-point-location)
  "Demonstrate point location in planar subdivision"
  (format #t "~%=== Point Location Demo ===~%")
  
  ;; Create a simple planar subdivision
  (format #t "~%Creating planar subdivision with 3 faces:~%")
  (format #t "  Face 1: Triangle (0,0)-(4,0)-(2,3)~%")
  (format #t "  Face 2: Rectangle (5,0)-(8,0)-(8,3)-(5,3)~%")
  (format #t "  Face 3: Outside (unbounded)~%")
  
  ;; Test points
  (let ((test-points (list (make-point 2 1)    ; Inside triangle
                           (make-point 6.5 1.5) ; Inside rectangle
                           (make-point 4.5 2)   ; Between shapes
                           (make-point 0 0)     ; On vertex
                           (make-point 3 1.5)))) ; On edge
    
    (format #t "~%Testing point locations:~%")
    (for-each (lambda (p)
                (format #t "  Point (~a, ~a): ~a~%"
                        (point-x p) (point-y p)
                        (locate-point-description p)))
              test-points)))

(define (locate-point-description p)
  "Describe location of point (simplified)"
  (let ((x (point-x p))
        (y (point-y p)))
    (cond
     ;; Triangle check
     ((and (>= x 0) (<= x 4) (>= y 0) (<= y 3)
           (<= y (- 3 (* 0.75 (abs (- x 2))))))
      "Inside triangle (Face 1)")
     ;; Rectangle check
     ((and (>= x 5) (<= x 8) (>= y 0) (<= y 3))
      "Inside rectangle (Face 2)")
     ;; Otherwise outside
     (else "Outside (Face 3)"))))

(define (demo-vertical-ray-shooting)
  "Demonstrate vertical ray shooting queries"
  (format #t "~%~%=== Vertical Ray Shooting Demo ===~%")
  
  ;; Create horizontal segments
  (let ((segments (list
                   (make-segment 1 2 5 2)   ; Horizontal at y=2
                   (make-segment 2 4 6 4)   ; Horizontal at y=4
                   (make-segment 0 6 4 6)   ; Horizontal at y=6
                   (make-segment 3 1 7 5)))) ; Diagonal
    
    (format #t "~%Segments in our scene:~%")
    (for-each (lambda (seg i)
                (format #t "  Segment ~a: (~a,~a) to (~a,~a)~%"
                        i 
                        (segment-x1 seg) (segment-y1 seg)
                        (segment-x2 seg) (segment-y2 seg)))
              segments
              '(1 2 3 4))
    
    ;; Create ray shooting data structure
    (let ((rs-ds (make-ray-shooting-ds segments)))
      
      ;; Test queries
      (format #t "~%Vertical ray shooting queries (shooting upward):~%")
      (let ((query-points (list (make-point 3 0)
                                (make-point 3 3)
                                (make-point 5 1)
                                (make-point 1 5))))
        (for-each (lambda (p)
                    (let ((hit (vertical-ray-shoot rs-ds p)))
                      (format #t "  From (~a,~a): ~a~%"
                              (point-x p) (point-y p)
                              (if hit
                                  (format #f "hits segment at y=~,1f"
                                          (segment-y-at-x hit (point-x p)))
                                  "no hit"))))
                  query-points)))))

(define (demo-sweep-line)
  "Demonstrate sweep line algorithm"
  (format #t "~%~%=== Sweep Line Algorithm Demo ===~%")
  
  ;; Create segments
  (let ((segments (list
                   (make-segment 1 2 4 3)
                   (make-segment 2 1 5 4)
                   (make-segment 3 5 6 2))))
    
    (format #t "~%Processing segments with sweep line:~%")
    (for-each (lambda (seg i)
                (format #t "  Segment ~a: (~a,~a) to (~a,~a)~%"
                        i
                        (segment-x1 seg) (segment-y1 seg)
                        (segment-x2 seg) (segment-y2 seg)))
              segments
              '(1 2 3))
    
    ;; Generate and show events
    (let ((events (generate-events segments)))
      (format #t "~%Sweep events in order:~%")
      (for-each (lambda (evt)
                  (format #t "  x=~a: ~a segment ~a~%"
                          (event-x evt)
                          (event-type evt)
                          (find-segment-id (event-segment evt) segments)))
                (sort events event<?))
      
      ;; Process sweep
      (format #t "~%Sweep line status at each event:~%")
      (process-and-show-sweep events))))

(define (find-segment-id seg segments)
  "Find segment ID for display"
  (+ 1 (list-index (lambda (s) (equal? s seg)) segments)))

(define (process-and-show-sweep events)
  "Process sweep events and show status"
  (let loop ((events (sort events event<?))
             (active '())
             (x-prev #f))
    (unless (null? events)
      (let* ((event (car events))
             (x (event-x event)))
        (when (not (equal? x x-prev))
          (format #t "  At x=~a: active segments = ~a~%"
                  x 
                  (map (lambda (s) (find-segment-id s active))
                       active)))
        (let ((new-active
               (case (event-type event)
                 ((start) (cons (event-segment event) active))
                 ((end) (delete (event-segment event) active)))))
          (loop (cdr events) new-active x))))))

(define (demo-persistent-vrs)
  "Demonstrate persistent vertical ray shooting"
  (format #t "~%~%=== Persistent Vertical Ray Shooting Demo ===~%")
  
  (let ((segments (list
                   (make-segment 0 3 4 3)
                   (make-segment 2 5 6 5)
                   (make-segment 1 1 3 1))))
    
    (format #t "~%Building persistent VRS for segments:~%")
    (for-each (lambda (seg i)
                (format #t "  Segment ~a: (~a,~a) to (~a,~a)~%"
                        i
                        (segment-x1 seg) (segment-y1 seg)
                        (segment-x2 seg) (segment-y2 seg)))
              segments
              '(1 2 3))
    
    (let ((pvrs (build-persistent-vrs segments)))
      (format #t "~%Querying at different x-coordinates:~%")
      (format #t "  x=0.5: Can see segments above any y~%")
      (format #t "  x=2.5: Different set of segments visible~%")
      (format #t "  x=5.0: Yet another configuration~%")
      
      (format #t "~%This allows O(log n) queries at any x!~%"))))

(define (demo-fractional-cascading)
  "Explain fractional cascading optimization"
  (format #t "~%~%=== Fractional Cascading Demo ===~%")
  
  (format #t "~%Problem: Search in k sorted lists~%")
  (format #t "  Naive: O(k log n) time~%")
  
  (format #t "~%Fractional Cascading solution:~%")
  (format #t "  1. Create auxiliary catalog structures~%")
  (format #t "  2. Add bridges between catalogs~%")
  (format #t "  3. Search first list: O(log n)~%")
  (format #t "  4. Follow bridges for remaining: O(1) each~%")
  (format #t "  Total: O(log n + k) time!~%")
  
  (format #t "~%Example with 3 lists:~%")
  (let ((lists '((1 5 9 13)
                 (2 6 10 14)
                 (3 7 11 15))))
    (format #t "  List 1: ~a~%" (car lists))
    (format #t "  List 2: ~a~%" (cadr lists))
    (format #t "  List 3: ~a~%" (caddr lists))
    (format #t "~%Searching for 8:~%")
    (format #t "  Binary search in List 1: between 5 and 9~%")
    (format #t "  Follow bridge to List 2: between 6 and 10~%")
    (format #t "  Follow bridge to List 3: between 7 and 11~%")))

;; Main demo runner
(define (main args)
  (format #t "MIT 6.851 - Geometric Data Structures Examples~%")
  (format #t "==============================================~%")
  
  (demo-point-location)
  (print-separator)
  
  (demo-vertical-ray-shooting)
  (print-separator)
  
  (demo-sweep-line)
  (print-separator)
  
  (demo-persistent-vrs)
  (print-separator)
  
  (demo-fractional-cascading)
  
  (format #t "~%~%Key Concepts:~%")
  (format #t "1. Point location in planar subdivisions~%")
  (format #t "2. Vertical ray shooting for segment queries~%")
  (format #t "3. Sweep line algorithms for geometric problems~%")
  (format #t "4. Persistence enables historical queries~%")
  (format #t "5. Fractional cascading optimizes repeated searches~%"))

(main (command-line))