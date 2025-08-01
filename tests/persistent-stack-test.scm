;;; Tests for Persistent Stack

(use-modules (srfi srfi-64)
             (persistent stack))

(test-begin "persistent-stack")

(test-group "Basic operations"
  (let* ((s0 (make-stack))
         (s1 (stack-push s0 1))
         (s2 (stack-push s1 2))
         (s3 (stack-push s2 3)))
    
    (test-assert "Empty stack" (stack-empty? s0))
    (test-assert "Non-empty stack" (not (stack-empty? s1)))
    
    (test-equal "Top of stack" 3 (stack-top s3))
    (test-equal "Previous version unchanged" 2 (stack-top s2))
    
    (let ((s3-popped (stack-pop s3)))
      (test-equal "Pop returns previous version" 2 (stack-top s3-popped))
      (test-equal "Original unchanged after pop" 3 (stack-top s3)))))

(test-group "Error conditions"
  (let ((empty (make-stack)))
    (test-error "Pop from empty" (stack-pop empty))
    (test-error "Top of empty" (stack-top empty))))

(test-end "persistent-stack")