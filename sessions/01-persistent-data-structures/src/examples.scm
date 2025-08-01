(use-modules (ice-9 format))

;; Load the persistent data structures
(load "persistent.scm")

(define (demonstrate-persistent-stack)
  (format #t "=== Persistent Stack Demo ===~%~%")
  
  ;; Create stacks
  (let* ((s0 empty-stack)
         (s1 (stack-push s0 'a))
         (s2 (stack-push s1 'b))
         (s3 (stack-push s2 'c)))
    
    (format #t "s0 (empty): ~a~%" (stack->list s0))
    (format #t "s1 (push a): ~a~%" (stack->list s1))
    (format #t "s2 (push b): ~a~%" (stack->list s2))
    (format #t "s3 (push c): ~a~%" (stack->list s3))
    
    ;; Pop from s3
    (call-with-values
        (lambda () (stack-pop s3))
      (lambda (val s3-popped)
        (format #t "~%After popping from s3:~%")
        (format #t "  Popped value: ~a~%" val)
        (format #t "  s3 after pop: ~a~%" (stack->list s3-popped))
        (format #t "  Original s3: ~a~%" (stack->list s3))
        (format #t "  s2 unchanged: ~a~%" (stack->list s2))))))

(define (demonstrate-versioned-stack)
  (format #t "~%~%=== Versioned Stack Demo ===~%~%")
  
  (let* ((vs (make-new-versioned-stack))
         (vs1 (versioned-push vs 'first))
         (vs2 (versioned-push vs1 'second))
         (vs3 (versioned-push vs2 'third)))
    
    (format #t "Version 0: ~a~%" (stack->list (versioned-at vs3 0)))
    (format #t "Version 1: ~a~%" (stack->list (versioned-at vs3 1)))
    (format #t "Version 2: ~a~%" (stack->list (versioned-at vs3 2)))
    (format #t "Version 3: ~a~%" (stack->list (versioned-at vs3 3)))))

;; Run demonstrations
(demonstrate-persistent-stack)
(demonstrate-versioned-stack)