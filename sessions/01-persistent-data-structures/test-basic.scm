#!/usr/bin/env guile
!#

(add-to-load-path "src")
(load "src/persistent.scm")

;; Simple test
(display "Testing basic persistent stack...\n")

(let* ((s1 (stack-push empty-stack 'a))
       (s2 (stack-push s1 'b))
       (s3 (stack-push s2 'c)))
  (display "Stack contents: ")
  (display (stack->list s3))
  (newline)
  (display "Success!\n"))