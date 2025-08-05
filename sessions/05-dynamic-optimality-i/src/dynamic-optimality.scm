(use-modules (srfi srfi-9)    ; For define-record-type
             (srfi srfi-1)    ; For list operations
             (srfi srfi-11))  ; For let-values

;; BST node structure
(define-record-type <bst-node>
  (make-bst-node key left right parent)
  bst-node?
  (key node-key set-node-key!)
  (left node-left set-node-left!)
  (right node-right set-node-right!)
  (parent node-parent set-node-parent!))

;; Access sequence tracker
(define-record-type <access-sequence>
  (make-access-sequence keys times)
  access-sequence?
  (keys seq-keys)      ; List of accessed keys
  (times seq-times))   ; Access times

;; BST with access tracking
(define-record-type <tracked-bst>
  (make-tracked-bst root accesses node-count)
  tracked-bst?
  (root bst-root set-bst-root!)
  (accesses bst-accesses set-bst-accesses!)
  (node-count bst-node-count set-bst-node-count!))

(define (sequential-access-cost n)
  "Cost of accessing 1, 2, ..., n in order"
  n)  ; Total O(n) for n accesses

(define (in-order-traversal node)
  "Perform in-order traversal of BST"
  (when node
    (in-order-traversal (node-left node))
    (process-node node)
    (in-order-traversal (node-right node))))

(define (dynamic-finger-cost keys)
  "Compute cost under dynamic finger property"
  (if (null? (cdr keys))
      0
      (+ (log2 (abs (- (car keys) (cadr keys))))
         (dynamic-finger-cost (cdr keys)))))

(define (finger-search tree finger target)
  "Search starting from finger position"
  (let ((distance (abs (- (node-key finger) target))))
    (search-with-bound tree finger target (ceiling (log2 distance)))))

(define (entropy-bound frequencies)
  "Compute entropy bound for access sequence"
  (fold + 0
        (map (lambda (p)
               (if (zero? p) 0
                   (* p (log2 (/ 1 p)))))
             frequencies)))

(define (build-entropy-optimal-tree keys frequencies)
  "Build BST with nodes at optimal heights based on frequency"
  (let ((items (sort (map cons keys frequencies)
                     (lambda (a b) (> (cdr a) (cdr b))))))
    (build-from-sorted-frequencies items)))

(define-record-type <working-set-tracker>
  (make-working-set-tracker last-access distinct-since)
  working-set-tracker?
  (last-access ws-last-access set-ws-last-access!)
  (distinct-since ws-distinct set-ws-distinct!))

(define (working-set-cost access-sequence)
  "Compute cost under working set property"
  (let ((tracker (make-hash-table)))
    (fold + 0
          (map-indexed
           (lambda (i key)
             (let ((last (hash-ref tracker key -1)))
               (if (= last -1)
                   (log2 i)  ; First access
                   (let ((distinct (count-distinct-between 
                                    access-sequence last i)))
                     (log2 distinct)))))
           access-sequence))))

(define (unified-cost access-sequence positions)
  "Compute cost under unified property"
  (map-indexed
   (lambda (j xj)
     (let ((min-cost
            (fold min 1000000000000.0
                  (map-indexed
                   (lambda (i xi)
                     (if (>= i j) 1000000000000.0
                         (let ((space (abs (- xi xj)))
                               (time (count-distinct-between
                                      access-sequence i j)))
                           (log2 (+ space time 2)))))
                   access-sequence))))
       min-cost))
   access-sequence))

(define (is-dynamically-optimal? algorithm access-sequence)
  "Check if algorithm achieves O(OPT) cost"
  (let ((alg-cost (run-algorithm algorithm access-sequence))
        (opt-cost (compute-opt-cost access-sequence)))
    (<= alg-cost (* constant opt-cost))))

(define (competitive-ratio algorithm sequences)
  "Compute competitive ratio of algorithm"
  (apply max
         (map (lambda (seq)
                (/ (run-algorithm algorithm seq)
                   (compute-opt-cost seq)))
              sequences)))

;; Splay tree operations
(define (splay tree x)
  "Splay node x to root"
  (while (not (eq? x (bst-root tree)))
    (let ((p (node-parent x))
          (g (and p (node-parent p))))
      (cond
       ;; Zig case - x is child of root
       ((not g) (rotate tree x))
       ;; Zig-zig case - x and parent same direction
       ((eq? (is-left-child? p) (is-left-child? x))
        (rotate tree p)
        (rotate tree x))
       ;; Zig-zag case - x and parent opposite directions
       (else
        (rotate tree x)
        (rotate tree x)))))
  x)

(define (is-left-child? node)
  "Check if node is left child of its parent"
  (and (node-parent node)
       (eq? node (node-left (node-parent node)))))

(define (rotate tree node)
  "Rotate node with its parent"
  (let ((parent (node-parent node)))
    (cond
     ((eq? node (node-left parent))
      ;; Right rotation
      (set-node-left! parent (node-right node))
      (when (node-right node)
        (set-node-parent! (node-right node) parent))
      (set-node-right! node parent))
     (else
      ;; Left rotation
      (set-node-right! parent (node-left node))
      (when (node-left node)
        (set-node-parent! (node-left node) parent))
      (set-node-left! node parent)))
    ;; Update parent pointers
    (set-node-parent! node (node-parent parent))
    (set-node-parent! parent node)
    ;; Update root if necessary
    (when (not (node-parent node))
      (set-bst-root! tree node))))

(define (splay-search tree key)
  "Search for key and splay it to root"
  (let ((node (bst-search (bst-root tree) key)))
    (when node
      (splay tree node))
    node))

(define (analyze-splay-performance access-sequence)
  "Analyze splay tree performance on access sequence"
  (let ((tree (make-empty-splay-tree))
        (total-cost 0))
    (for-each
     (lambda (key)
       (let ((cost (splay-search-cost tree key)))
         (set! total-cost (+ total-cost cost))
         (splay-search tree key)))
     access-sequence)
    total-cost))

;; Potential function for amortized analysis
(define (splay-potential tree)
  "Compute potential Φ = ∑ rank(node)"
  (define (rank node)
    (if (not node) 0
        (+ 1 (rank (node-left node))
             (rank (node-right node)))))
  (sum-tree-nodes tree rank))

;; Geometric representation
(define-record-type <space-time-point>
  (make-st-point space time)
  space-time-point?
  (space point-space)  ; Key value
  (time point-time))   ; Access time

(define (access-sequence->point-set sequence)
  "Convert access sequence to space-time points"
  (map-indexed (lambda (i key)
                 (make-st-point key i))
               sequence))

(define (bst-execution->touched-points tree sequence)
  "Record which nodes touched during execution"
  (let ((touched '()))
    (for-each-indexed
     (lambda (i key)
       (let ((path (search-path tree key)))
         (set! touched 
               (append touched
                       (map (lambda (node)
                              (make-st-point (node-key node) i))
                            path)))))
     sequence)
    touched))

(define (is-arborally-satisfied? points)
  "Check if point set is arborally satisfied"
  (for-all-pairs
   points
   (lambda (p1 p2)
     (if (and (not (= (point-space p1) (point-space p2)))
              (not (= (point-time p1) (point-time p2))))
         (exists?
          points
          (lambda (p3)
            (and (not (eq? p3 p1))
                 (not (eq? p3 p2))
                 (point-in-rectangle? p3 p1 p2)
                 (on-rectangle-side? p3 p1 p2))))
         #t))))

(define (point-in-rectangle? p p1 p2)
  "Check if point p is inside rectangle spanned by p1 and p2"
  (let ((x-min (min (point-space p1) (point-space p2)))
        (x-max (max (point-space p1) (point-space p2)))
        (t-min (min (point-time p1) (point-time p2)))
        (t-max (max (point-time p1) (point-time p2))))
    (and (>= (point-space p) x-min)
         (<= (point-space p) x-max)
         (>= (point-time p) t-min)
         (<= (point-time p) t-max))))

(define (compute-opt-cost access-sequence)
  "Compute optimal BST cost for access sequence"
  (let ((input-points (access-sequence->point-set access-sequence)))
    (find-smallest-ass input-points)))

(define (find-smallest-ass input-points)
  "Find smallest arborally satisfied set containing input"
  ;; This is computationally hard!
  ;; Known approaches use ILP or approximation algorithms
  (error "Computing exact OPT is NP-hard"))

(define (greedy-algorithm access-sequence)
  "Greedy BST algorithm"
  (let ((tree (make-empty-bst))
        (touched-points '()))
    (for-each-indexed
     (lambda (time key)
       ;; Find path to key
       (let ((path (search-path tree key)))
         ;; Add necessary points for this row
         (let ((row-points (compute-greedy-row path time)))
           (set! touched-points (append touched-points row-points))
           ;; Rearrange path optimally
           (rearrange-path tree path))))
     access-sequence)
    touched-points))

(define (compute-greedy-row path time)
  "Compute points needed for current row"
  (map (lambda (node)
         (make-st-point (node-key node) time))
       path))

(define (rearrange-path tree path)
  "Rearrange path optimally for future accesses"
  ;; This is the key insight of greedy:
  ;; Put accessed node at root, maintain BST property
  (when (not (null? path))
    (splay tree (car (last-pair path)))))

(define (ass-to-bst ass-algorithm)
  "Convert online ASS algorithm to online BST"
  (lambda (tree key)
    (let* ((current-points (get-current-points tree))
           (new-point (make-st-point key (current-time)))
           (ass-points (ass-algorithm (cons new-point current-points))))
      ;; Convert ASS solution back to BST operations
      (execute-bst-operations tree ass-points))))

(define (online-greedy-bst tree key)
  "Online greedy BST implementation"
  (let ((node (bst-search (bst-root tree) key)))
    (when node
      ;; Greedy: bring accessed node to root
      (splay tree node))
    node))

;; Split tree implementation
(define-record-type <split-tree>
  (make-split-tree items min-ptr max-ptr)
  split-tree?
  (items split-items set-split-items!)
  (min-ptr split-min set-split-min!)
  (max-ptr split-max set-split-max!))

(define (split! tree x)
  "Split tree at x, returning two trees"
  (let ((pos (find-position tree x)))
    (if pos
        (let ((left-items (take (split-items tree) pos))
              (right-items (drop (split-items tree) (+ pos 1))))
          (values (make-split-tree left-items 
                                   (split-min tree)
                                   (if (null? left-items) #f
                                       (last left-items)))
                  (make-split-tree right-items
                                   (if (null? right-items) #f
                                       (car right-items))
                                   (split-max tree))))
        (values tree #f))))

;; Treap of split trees
(define-record-type <split-treap-node>
  (make-st-node split-tree priority left right)
  split-treap-node?
  (split-tree st-split-tree)
  (priority st-priority)     ; Previous touch time
  (left st-left set-st-left!)
  (right st-right set-st-right!))

(define (maintain-treap-property! treap node)
  "Maintain heap property by priority (touch time)"
  (while (and (st-parent node)
              (< (st-priority node)
                 (st-priority (st-parent node))))
    (rotate treap node)))

;; Utility functions
(define (log2 n)
  "Compute log base 2"
  (/ (log n) (log 2)))

(define (map-indexed proc lst)
  "Map with index"
  (let loop ((lst lst) (i 0) (result '()))
    (if (null? lst)
        (reverse result)
        (loop (cdr lst) (+ i 1)
                (cons (proc i (car lst)) result)))))

(define (for-each-indexed proc lst)
  "For-each with index"
  (let loop ((lst lst) (i 0))
    (unless (null? lst)
      (proc i (car lst))
      (loop (cdr lst) (+ i 1)))))

(define (count-distinct-between sequence start end)
  "Count distinct elements between indices"
  (let ((seen (make-hash-table)))
    (let loop ((i start) (count 0))
      (if (>= i end)
          count
          (let ((key (list-ref sequence i)))
            (if (hash-ref seen key)
                (loop (+ i 1) count)
                (begin
                  (hash-set! seen key #t)
                  (loop (+ i 1) (+ count 1)))))))))

(define (for-all-pairs lst proc)
  "Check property for all pairs"
  (let loop ((l1 lst))
    (if (null? l1)
        #t
        (and (let loop2 ((l2 (cdr l1)))
               (if (null? l2)
                   #t
                   (and (proc (car l1) (car l2))
                        (loop2 (cdr l2)))))
             (loop (cdr l1))))))

(define (exists? lst pred)
  "Check if any element satisfies predicate"
  (and (not (null? lst))
       (or (pred (car lst))
           (exists? (cdr lst) pred))))

(define (bst-search node key)
  "Standard BST search"
  (cond
   ((not node) #f)
   ((= key (node-key node)) node)
   ((< key (node-key node)) (bst-search (node-left node) key))
   (else (bst-search (node-right node) key))))

(define (search-path tree key)
  "Return path from root to key"
  (let loop ((node (bst-root tree)) (path '()))
    (if (not node)
        (reverse path)
        (let ((new-path (cons node path)))
          (cond
           ((= key (node-key node)) (reverse new-path))
           ((< key (node-key node)) (loop (node-left node) new-path))
           (else (loop (node-right node) new-path)))))))

(define (make-empty-bst)
  "Create empty BST"
  (make-tracked-bst #f '() 0))

(define (make-empty-splay-tree)
  "Create empty splay tree"
  (make-tracked-bst #f '() 0))

; +inf.0 is built-in to Guile

;; Stub implementations
(define (process-node node) #t)
(define (search-with-bound tree finger target bound) #f)
(define (build-from-sorted-frequencies items) #f)
(define (sum-tree-nodes tree fn) 0)
(define (splay-search-cost tree key) 1)
(define (on-rectangle-side? p p1 p2) #t)
(define (get-current-points tree) '())
(define (current-time) 0)
(define (execute-bst-operations tree points) #t)
(define-syntax when
  (syntax-rules ()
    ((when cond body ...)
     (if cond (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless cond body ...)
     (if (not cond) (begin body ...)))))

;; Additional stub for missing variables
(define constant 1)
(define (run-algorithm alg seq) 1)
(define (find-position tree x) #f)
(define (st-parent node) #f)