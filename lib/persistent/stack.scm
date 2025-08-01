;;; Persistent Stack Implementation
;;; Based on Lecture 1: Persistent Data Structures

(define-module (persistent stack)
  #:export (make-stack
            stack-push
            stack-pop
            stack-top
            stack-empty?))

;; Node structure
(define (make-node value next)
  (cons value next))

(define (node-value node)
  (car node))

(define (node-next node)
  (cdr node))

;; Stack operations
(define (make-stack)
  "Create an empty persistent stack"
  '())

(define (stack-push stack value)
  "Push a value onto the stack, returning a new stack"
  (make-node value stack))

(define (stack-pop stack)
  "Pop a value from the stack, returning the new stack"
  (if (stack-empty? stack)
      (error "Cannot pop from empty stack")
      (node-next stack)))

(define (stack-top stack)
  "Get the top value of the stack"
  (if (stack-empty? stack)
      (error "Cannot get top of empty stack")
      (node-value stack)))

(define (stack-empty? stack)
  "Check if the stack is empty"
  (null? stack))