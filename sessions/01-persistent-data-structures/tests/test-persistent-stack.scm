#!/usr/bin/env guile
!#

(use-modules (srfi srfi-64)
             (srfi srfi-11))  ; for let-values

;; Load the persistent stack implementation
(load "../src/persistent.scm")

;; Start test suite
(test-begin "persistent-stack")

;; Test empty stack
(test-group "empty-stack"
  (test-assert "empty-stack is empty" 
    (stack-empty? empty-stack))
  
  (test-eq "empty-stack size is 0"
    0 (stack-size empty-stack))
  
  (test-equal "empty-stack converts to empty list"
    '() (stack->list empty-stack))
  
  (test-error "cannot pop from empty stack"
    (stack-pop empty-stack))
  
  (test-error "cannot peek at empty stack"
    (stack-peek empty-stack)))

;; Test push operations
(test-group "push-operations"
  (let* ((s1 (stack-push empty-stack 'a))
         (s2 (stack-push s1 'b))
         (s3 (stack-push s2 'c)))
    
    (test-assert "s1 is not empty"
      (not (stack-empty? s1)))
    
    (test-eq "s1 size is 1"
      1 (stack-size s1))
    
    (test-eq "s2 size is 2"
      2 (stack-size s2))
    
    (test-eq "s3 size is 3"
      3 (stack-size s3))
    
    (test-equal "s1 contents"
      '(a) (stack->list s1))
    
    (test-equal "s2 contents"
      '(b a) (stack->list s2))
    
    (test-equal "s3 contents"
      '(c b a) (stack->list s3))))

;; Test persistence
(test-group "persistence"
  (let* ((s1 (stack-push empty-stack 'x))
         (s2 (stack-push s1 'y))
         (s3 (stack-push s2 'z)))
    
    ;; Original stacks should remain unchanged
    (test-equal "s1 unchanged after s2 creation"
      '(x) (stack->list s1))
    
    (test-equal "s2 unchanged after s3 creation"
      '(y x) (stack->list s2))
    
    ;; Pop should not affect original
    (let-values (((val s3-popped) (stack-pop s3)))
      (test-eq "popped value is z"
        'z val)
      
      (test-equal "s3-popped contents"
        '(y x) (stack->list s3-popped))
      
      (test-equal "original s3 unchanged"
        '(z y x) (stack->list s3)))))

;; Test peek operation
(test-group "peek-operation"
  (let ((s (stack-push (stack-push empty-stack 1) 2)))
    (test-eq "peek returns top value"
      2 (stack-peek s))
    
    (test-equal "peek doesn't modify stack"
      '(2 1) (stack->list s))))

;; Test versioned stack
(test-group "versioned-stack"
  (let* ((vs0 (make-new-versioned-stack))
         (vs1 (versioned-push vs0 'a))
         (vs2 (versioned-push vs1 'b))
         (vs3 (versioned-push vs2 'c)))
    
    (test-equal "version 0 is empty"
      '() (stack->list (versioned-at vs3 0)))
    
    (test-equal "version 1 has one element"
      '(a) (stack->list (versioned-at vs3 1)))
    
    (test-equal "version 2 has two elements"
      '(b a) (stack->list (versioned-at vs3 2)))
    
    (test-equal "version 3 has three elements"
      '(c b a) (stack->list (versioned-at vs3 3)))
    
    (test-error "accessing non-existent version"
      (versioned-at vs3 4))))

;; Test multiple branches (persistence)
(test-group "multiple-branches"
  (let* ((base (stack-push empty-stack 'base))
         (branch1 (stack-push (stack-push base 'b1-1) 'b1-2))
         (branch2 (stack-push base 'b2-1)))
    
    (test-equal "branch1 contents"
      '(b1-2 b1-1 base) (stack->list branch1))
    
    (test-equal "branch2 contents"
      '(b2-1 base) (stack->list branch2))
    
    (test-equal "base unchanged"
      '(base) (stack->list base))))

(test-end "persistent-stack")

;; Exit with appropriate code
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))