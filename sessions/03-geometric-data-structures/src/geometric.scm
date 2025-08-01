(use-modules (srfi srfi-9)    ; For define-record-type
             (srfi srfi-1))   ; For list operations

;; Face record
(define-record-type <face>
  (make-face id edges vertices)
  face?
  (id face-id)
  (edges face-edges set-face-edges!)
  (vertices face-vertices set-face-vertices!))

;; Edge record  
(define-record-type <edge>
  (make-edge start end left-face right-face)
  edge?
  (start edge-start)
  (end edge-end)
  (left-face edge-left-face)
  (right-face edge-right-face))

;; Point record
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

;; Vertical ray shooting structure
(define-record-type <ray-shooting-ds>
  (make-ray-shooting-ds segments)
  ray-shooting-ds?
  (segments rs-segments set-rs-segments!))

(define (vertical-ray-shoot ds point)
  "Find first segment hit by vertical ray from point upward"
  (let ((x (point-x point))
        (y (point-y point)))
    (find-first-intersection (rs-segments ds) x y)))

(define (find-first-intersection segments x y)
  "Find lowest segment above point (x,y)"
  (let ((candidates (filter (lambda (seg)
                              (and (segment-spans-x? seg x)
                                   (> (segment-y-at-x seg x) y)))
                            segments)))
    (if (null? candidates)
        #f
        (minimum candidates 
                 (lambda (s) (segment-y-at-x s x))))))

;; Sweep line event
(define-record-type <sweep-event>
  (make-sweep-event x type segment)
  sweep-event?
  (x event-x)
  (type event-type)      ; 'start or 'end
  (segment event-segment))

(define (line-sweep segments)
  "Process segments using sweep line algorithm"
  (let ((events (generate-events segments)))
    (process-sweep-events (sort events event<?))))

(define (generate-events segments)
  "Generate start and end events for each segment"
  (append-map (lambda (seg)
                (list (make-sweep-event (segment-left-x seg) 'start seg)
                      (make-sweep-event (segment-right-x seg) 'end seg)))
              segments))

(define (event<? e1 e2)
  "Compare events by x-coordinate"
  (< (event-x e1) (event-x e2)))

;; Persistent BST for vertical ray shooting
(define-record-type <persistent-vrs>
  (make-persistent-vrs bst-versions)
  persistent-vrs?
  (bst-versions pvrs-versions))

(define (build-persistent-vrs segments)
  "Build persistent vertical ray shooting structure"
  (let ((events (sort (generate-events segments) event<?)))
    (make-persistent-vrs
     (build-versions events))))

(define (build-versions events)
  "Build BST versions for each x-coordinate"
  (let loop ((events events)
             (current-bst empty-bst)
             (versions '()))
    (if (null? events)
        (reverse versions)
        (let* ((event (car events))
               (new-bst (process-event current-bst event)))
          (loop (cdr events)
                new-bst
                (cons (cons (event-x event) new-bst) versions))))))

(define (query-persistent-vrs pvrs x y)
  "Query at point (x,y) using persistent structure"
  (let ((version (find-version (pvrs-versions pvrs) x)))
    (if version
        (successor-query (cdr version) y)
        #f)))

;; Dynamic vertical ray shooting
(define-record-type <dynamic-vrs>
  (make-dynamic-vrs retro-bst)
  dynamic-vrs?
  (retro-bst dvrs-retro-bst set-dvrs-retro-bst!))

(define (insert-segment dvrs time segment)
  "Insert segment at given time"
  (let* ((start-time (segment-left-x segment))
         (end-time (segment-right-x segment))
         (retro-bst (dvrs-retro-bst dvrs)))
    ;; Insert edge at start time
    (retro-insert retro-bst start-time 
                  `(insert ,(segment-y-at-x segment start-time)))
    ;; Delete edge at end time  
    (retro-insert retro-bst end-time
                  `(delete ,(segment-y-at-x segment end-time)))))

(define (delete-segment dvrs time segment)
  "Delete segment operations"
  (let ((retro-bst (dvrs-retro-bst dvrs)))
    ;; Delete the insert/delete operations
    (retro-delete retro-bst (segment-left-x segment))
    (retro-delete retro-bst (segment-right-x segment))))

;; Multi-dimensional point
(define-record-type <d-point>
  (make-d-point coords)
  d-point?
  (coords d-point-coords))

;; Axis-aligned box
(define-record-type <box>
  (make-box min-coords max-coords)
  box?
  (min-coords box-min)
  (max-coords box-max))

(define (point-in-box? point box)
  "Check if point is inside box"
  (let ((coords (d-point-coords point))
        (mins (box-min box))
        (maxs (box-max box)))
    (every (lambda (c min max)
             (and (>= c min) (<= c max)))
           coords mins maxs)))

;; Range tree node
(define-record-type <range-node>
  (make-range-node key left right subtree)
  range-node?
  (key node-key)
  (left node-left)
  (right node-right)
  (subtree node-subtree))  ; (d-1)-dimensional tree on points

;; 1D range tree (base case)
(define (build-1d-range-tree points)
  "Build 1D balanced BST on points"
  (if (null? points)
      #f
      (let* ((sorted (sort points (lambda (p1 p2)
                                    (< (car (d-point-coords p1))
                                       (car (d-point-coords p2))))))
             (mid (quotient (length sorted) 2))
             (median (list-ref sorted mid))
             (left-points (take sorted mid))
             (right-points (drop sorted (+ mid 1))))
        (make-range-node median
                         (build-1d-range-tree left-points)
                         (build-1d-range-tree right-points)
                         #f))))  ; No subtree for 1D

;; Multi-dimensional range tree
(define (build-range-tree points dimension)
  "Build d-dimensional range tree"
  (if (= dimension 1)
      (build-1d-range-tree points)
      (build-higher-d-tree points dimension)))

(define (build-higher-d-tree points dimension)
  "Build tree for dimension > 1"
  (if (null? points)
      #f
      (let* ((sorted (sort-by-dimension points dimension))
             (mid (quotient (length sorted) 2))
             (median (list-ref sorted mid))
             (left-points (take sorted mid))
             (right-points (drop sorted (+ mid 1))))
        (make-range-node median
                         (build-higher-d-tree left-points dimension)
                         (build-higher-d-tree right-points dimension)
                         ;; Build (d-1)-dimensional tree on all points
                         (build-range-tree points (- dimension 1))))))

(define (range-query tree box dimension)
  "Query range tree for points in box"
  (if (not tree)
      '()
      (if (= dimension 1)
          (query-1d tree (car (box-min box)) (car (box-max box)))
          (query-higher-d tree box dimension))))

;; Layered range tree with arrays
(define-record-type <layered-node>
  (make-layered-node key left right y-array)
  layered-node?
  (key lnode-key)
  (left lnode-left)
  (right lnode-right)
  (y-array lnode-y-array))  ; Sorted array of y-coordinates

(define (build-layered-range-tree points)
  "Build 2D layered range tree"
  (if (null? points)
      #f
      (let* ((x-sorted (sort points (lambda (p1 p2)
                                      (< (point-x p1) (point-x p2)))))
             (mid (quotient (length x-sorted) 2))
             (median (list-ref x-sorted mid))
             (left-points (take x-sorted mid))
             (right-points (drop x-sorted (+ mid 1)))
             (y-sorted (sort points (lambda (p1 p2)
                                      (< (point-y p1) (point-y p2))))))
        (make-layered-node median
                           (build-layered-range-tree left-points)
                           (build-layered-range-tree right-points)
                           (list->vector (map point-y y-sorted))))))

(define (layered-query tree x1 x2 y1 y2)
  "Query layered range tree"
  (let ((nodes (find-canonical-nodes tree x1 x2)))
    (apply append
           (map (lambda (node)
                  (binary-search-range (lnode-y-array node) y1 y2))
                nodes))))

;; Weight-balanced range tree
(define-record-type <wb-range-node>
  (make-wb-range-node key left right subtree size)
  wb-range-node?
  (key wb-node-key)
  (left wb-node-left set-wb-node-left!)
  (right wb-node-right set-wb-node-right!)
  (subtree wb-node-subtree set-wb-node-subtree!)
  (size wb-node-size set-wb-node-size!))

(define (wb-size node)
  "Get size of weight-balanced node"
  (if node (wb-node-size node) 0))

(define (wb-balanced? node alpha)
  "Check if node satisfies weight balance"
  (if (not node)
      #t
      (let ((left-size (wb-size (wb-node-left node)))
            (right-size (wb-size (wb-node-right node)))
            (total-size (wb-node-size node)))
        ;; For weight balance: size of each child >= alpha * (total - 1)
        ;; This allows single nodes to be balanced
        (and (>= left-size (* alpha (- total-size 1)))
             (>= right-size (* alpha (- total-size 1)))))))

(define (wb-insert tree point alpha)
  "Insert point maintaining weight balance"
  (let ((new-tree (standard-insert tree point)))
    (if (needs-rebuild? new-tree alpha)
        (rebuild-subtree new-tree)
        new-tree)))

(define (rebuild-subtree node)
  "Rebuild subtree to perfect balance"
  (let ((points (collect-points node)))
    (build-perfect-tree points)))

;; Fractional cascading structure
(define-record-type <cascading-node>
  (make-cascading-node elements pointers-down pointers-up)
  cascading-node?
  (elements cn-elements)
  (pointers-down cn-down)    ; Pointers to positions in child lists
  (pointers-up cn-up))       ; Pointers to positions in parent list

(define (build-fractional-cascading lists)
  "Build fractional cascading structure from sorted lists"
  (if (null? lists)
      '()
      (let* ((n (length lists))
             (augmented-lists (augment-lists lists)))
        (add-pointers augmented-lists))))

(define (augment-lists lists)
  "Augment each list with samples from next level"
  (let loop ((lists lists) (result '()))
    (if (null? (cdr lists))
        (reverse (cons (car lists) result))
        (let* ((current (car lists))
               (next (cadr lists))
               (sampled (sample-every-other next))
               (augmented (merge current sampled element<?)))
          (loop (cdr lists)
                (cons augmented result))))))

(define (sample-every-other lst)
  "Take every other element from list"
  (let loop ((lst lst) (i 0) (result '()))
    (cond ((null? lst) (reverse result))
          ((even? i) (loop (cdr lst) (+ i 1) (cons (car lst) result)))
          (else (loop (cdr lst) (+ i 1) result)))))

(define (cascading-search structure x)
  "Search using fractional cascading"
  (let loop ((levels structure) (pos 0))
    (if (null? levels)
        '()
        (let* ((level (car levels))
               (elements (cn-elements level))
               (new-pos (binary-search-from elements x pos)))
          (cons new-pos (loop (cdr levels) 
                              (follow-pointer level new-pos)))))))

;; Helper functions for geometric data structures

(define (segment-spans-x? segment x)
  "Check if segment spans x-coordinate"
  (and (<= (segment-left-x segment) x)
       (>= (segment-right-x segment) x)))

(define (segment-y-at-x segment x)
  "Get y-coordinate of segment at given x"
  (let ((x1 (segment-left-x segment))
        (y1 (segment-left-y segment))
        (x2 (segment-right-x segment))
        (y2 (segment-right-y segment)))
    (if (= x1 x2)
        y1  ; Vertical segment
        (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1)))))))

(define (minimum lst key-fn)
  "Find minimum element by key function"
  (if (null? lst)
      #f
      (fold (lambda (x min)
              (if (< (key-fn x) (key-fn min)) x min))
            (car lst)
            (cdr lst))))

(define (binary-search-range array low high)
  "Find elements in range [low, high] in sorted array"
  (let ((start (binary-search array low))
        (end (binary-search array high)))
    (vector->list (vector-copy array start (+ end 1)))))

(define (sort-by-dimension points dim)
  "Sort points by given dimension"
  (sort points (lambda (p1 p2)
                 (< (list-ref (d-point-coords p1) (- dim 1))
                    (list-ref (d-point-coords p2) (- dim 1))))))

(define (find-canonical-nodes tree x1 x2)
  "Find canonical nodes for range [x1, x2]"
  (let loop ((node tree) (result '()))
    (cond ((not node) result)
          ((and (>= (node-key node) x1)
                (<= (node-key node) x2))
           (cons node result))
          ((< (node-key node) x1)
           (loop (node-right node) result))
          (else
           (loop (node-left node) result)))))

;; Segment operations
(define (segment-left-x seg) (point-x (edge-start seg)))
(define (segment-right-x seg) (point-x (edge-end seg)))
(define (segment-left-y seg) (point-y (edge-start seg)))
(define (segment-right-y seg) (point-y (edge-end seg)))

;; Empty BST
(define empty-bst #f)

;; Process event for sweep line
(define (process-event bst event)
  "Update BST based on sweep event"
  (case (event-type event)
    ((start) (bst-insert bst (event-segment event)))
    ((end) (bst-delete bst (event-segment event)))
    (else bst)))

;; Binary search utilities
(define (binary-search array target)
  "Standard binary search, returns insertion position"
  (let loop ((low 0) (high (vector-length array)))
    (if (>= low high)
        low
        (let* ((mid (quotient (+ low high) 2))
               (mid-val (vector-ref array mid)))
          (cond ((< mid-val target) (loop (+ mid 1) high))
                ((> mid-val target) (loop low mid))
                (else mid))))))

(define (binary-search-from array target start-pos)
  "Binary search starting from given position"
  (binary-search array target))  ; Simplified

(define (follow-pointer node pos)
  "Follow pointer to next level"
  (if (< pos (vector-length (cn-down node)))
      (vector-ref (cn-down node) pos)
      pos))

;; Stub functions for BST operations
(define (bst-insert bst segment) bst)
(define (bst-delete bst segment) bst)
(define (successor-query bst y) #f)
(define (find-version versions x) 
  (find (lambda (v) (<= (car v) x)) (reverse versions)))
(define (element<? e1 e2) (< e1 e2))
(define (standard-insert tree point) tree)
(define (needs-rebuild? tree alpha) #f)
(define (collect-points node) '())
(define (build-perfect-tree points) #f)
(define (merge lst1 lst2 less?) (sort (append lst1 lst2) less?))
(define (add-pointers lists) lists)

;; Additional stubs for missing functions
(define (retro-insert bst time op) #t)
(define (retro-delete bst time) #t)
(define (process-sweep-events events) '())
(define (query-1d tree min max) '())
(define (query-higher-d tree box dim) '())