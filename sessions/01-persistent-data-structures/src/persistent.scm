(use-modules (srfi srfi-9))  ; For define-record-type

;; Define the stack node structure
(define-record-type <stack-node>
  (make-stack-node value next)
  stack-node?
  (value stack-node-value)
  (next stack-node-next))

;; Empty stack sentinel
(define empty-stack '())

;; Check if stack is empty
(define (stack-empty? stack)
  (null? stack))

(define (stack-push stack value)
  "Push a value onto the stack, returning a new stack"
  (make-stack-node value stack))

(define (stack-pop stack)
  "Pop a value from the stack, returning (values value remaining-stack)"
  (if (stack-empty? stack)
      (error "Cannot pop from empty stack")
      (values (stack-node-value stack)
              (stack-node-next stack))))

(define (stack-peek stack)
  "Look at the top value without modifying the stack"
  (if (stack-empty? stack)
      (error "Cannot peek at empty stack")
      (stack-node-value stack)))

(define (stack->list stack)
  "Convert a stack to a list for display purposes"
  (let loop ((s stack) (acc '()))
    (if (stack-empty? s)
        (reverse acc)
        (loop (stack-node-next s)
              (cons (stack-node-value s) acc)))))

(define (stack-size stack)
  "Count the number of elements in the stack"
  (let loop ((s stack) (count 0))
    (if (stack-empty? s)
        count
        (loop (stack-node-next s) (+ count 1)))))

(define-record-type <versioned-stack>
  (make-versioned-stack current-version versions)
  versioned-stack?
  (current-version versioned-stack-current)
  (versions versioned-stack-versions))

;; Create a new versioned stack
(define (make-new-versioned-stack)
  (make-versioned-stack 0 (list (cons 0 empty-stack))))

;; Push with version tracking
(define (versioned-push vstack value)
  (let* ((current-ver (versioned-stack-current vstack))
         (versions (versioned-stack-versions vstack))
         (current-stack (cdr (assoc current-ver versions)))
         (new-ver (+ current-ver 1))
         (new-stack (stack-push current-stack value)))
    (make-versioned-stack 
     new-ver
     (cons (cons new-ver new-stack) versions))))

;; Access a specific version
(define (versioned-at vstack version)
  (let ((versions (versioned-stack-versions vstack)))
    (cond ((assoc version versions) => cdr)
          (else (error "Version not found" version)))))

(define-record-type <fat-node>
  (make-fat-node values-list next-list)
  fat-node?
  (values-list fat-node-values)
  (next-list fat-node-nexts))

(define-record-type <timestamped-value>
  (make-timestamped-value timestamp value)
  timestamped-value?
  (timestamp tv-timestamp)
  (value tv-value))

(define (fat-node-value-at node time)
  "Get the value at a specific time"
  (let loop ((values (fat-node-values node)))
    (cond ((null? values) (error "No value at time" time))
          ((<= (tv-timestamp (car values)) time)
           (tv-value (car values)))
          (else (loop (cdr values))))))