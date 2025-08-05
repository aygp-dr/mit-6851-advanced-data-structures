#!/usr/bin/env guile
!#

;;; persistence-benchmark.scm - Benchmark persistent vs ephemeral data structures

(use-modules (srfi srfi-1)   ; List operations
             (srfi srfi-19)  ; Time operations
             (ice-9 format)) ; Formatted output

;; Load persistent stack implementation
(load "../sessions/01-persistent-data-structures/src/persistent.scm")

;;; Timing utilities
(define (time-operation thunk)
  "Time the execution of THUNK and return (result . elapsed-seconds)"
  (let ((start (current-time)))
    (let ((result (thunk)))
      (let ((end (current-time)))
        (cons result 
              (+ (time-second (time-difference end start))
                 (/ (time-nanosecond (time-difference end start)) 1e9)))))))

(define (benchmark-operation name iterations thunk)
  "Run THUNK ITERATIONS times and report timing"
  (format #t "~%Benchmarking ~a (~a iterations)...~%" name iterations)
  (let ((timing (time-operation
                 (lambda ()
                   (do ((i 0 (+ i 1)))
                       ((= i iterations))
                     (thunk))))))
    (let ((total-time (cdr timing)))
      (format #t "  Total time: ~,3f seconds~%" total-time)
      (format #t "  Average time per operation: ~,6f ms~%" 
              (* 1000 (/ total-time iterations)))
      (format #t "  Operations per second: ~,0f~%"
              (/ iterations total-time)))))

;;; Ephemeral (mutable) stack for comparison
(define (make-ephemeral-stack)
  (cons 'ephemeral-stack '()))

(define (ephemeral-push! stack value)
  (set-cdr! stack (cons value (cdr stack))))

(define (ephemeral-pop! stack)
  (if (null? (cdr stack))
      (error "Cannot pop from empty ephemeral stack")
      (let ((value (cadr stack)))
        (set-cdr! stack (cddr stack))
        value)))

(define (ephemeral-empty? stack)
  (null? (cdr stack)))

;;; Benchmark scenarios

(define (benchmark-push-only n)
  "Benchmark pushing N items"
  (format #t "~%=== Push-only Benchmark (n=~a) ===~%" n)
  
  ;; Persistent version
  (benchmark-operation
   "Persistent push"
   1
   (lambda ()
     (let loop ((i 0) (stack empty-stack))
       (if (< i n)
           (loop (+ i 1) (stack-push stack i))
           stack))))
  
  ;; Ephemeral version
  (benchmark-operation
   "Ephemeral push"
   1
   (lambda ()
     (let ((stack (make-ephemeral-stack)))
       (do ((i 0 (+ i 1)))
           ((= i n))
         (ephemeral-push! stack i))
       stack))))

(define (benchmark-push-pop n)
  "Benchmark alternating push and pop operations"
  (format #t "~%=== Push-Pop Benchmark (n=~a) ===~%" n)
  
  ;; Persistent version
  (benchmark-operation
   "Persistent push-pop"
   1
   (lambda ()
     (let loop ((i 0) (stack empty-stack))
       (if (< i n)
           (let* ((s1 (stack-push stack i))
                  (s2 (stack-push s1 (+ i 1)))
                  (val (stack-peek s2))
                  (s3 (stack-pop s2)))
             (loop (+ i 2) s3))
           stack))))
  
  ;; Ephemeral version
  (benchmark-operation
   "Ephemeral push-pop"
   1
   (lambda ()
     (let ((stack (make-ephemeral-stack)))
       (do ((i 0 (+ i 2)))
           ((>= i n))
         (ephemeral-push! stack i)
         (ephemeral-push! stack (+ i 1))
         (ephemeral-pop! stack))
       stack))))

(define (benchmark-version-creation n versions)
  "Benchmark creating multiple versions from a single stack"
  (format #t "~%=== Version Creation Benchmark ===~%")
  (format #t "Base stack size: ~a, versions to create: ~a~%" n versions)
  
  ;; Build base stack
  (let ((base-stack 
         (let loop ((i 0) (stack empty-stack))
           (if (< i n)
               (loop (+ i 1) (stack-push stack i))
               stack))))
    
    ;; Create versions - persistent
    (benchmark-operation
     "Persistent version creation"
     1
     (lambda ()
       (let loop ((i 0) (version-list '()))
         (if (< i versions)
             (loop (+ i 1)
                   (cons (stack-push base-stack (+ n i))
                         version-list))
             version-list))))
    
    ;; Ephemeral can't efficiently create versions
    (format #t "  Ephemeral: N/A (cannot efficiently create independent versions)~%")))

(define (benchmark-memory-sharing n)
  "Demonstrate memory sharing in persistent structures"
  (format #t "~%=== Memory Sharing Analysis ===~%")
  (format #t "Creating ~a versions of stacks with shared structure...~%" n)
  
  (let* ((base (fold (lambda (i s) (stack-push s i))
                     empty-stack
                     (iota 100)))
         (versions (map (lambda (i)
                          (stack-push base (+ 100 i)))
                        (iota n))))
    
    (format #t "  Base stack has 100 elements~%")
    (format #t "  Created ~a versions, each with 101 elements~%" n)
    (format #t "  Total unique nodes: ~a (vs ~a without sharing)~%"
            (+ 100 n)
            (* 101 n))
    (format #t "  Memory savings: ~,1f%~%"
            (* 100 (- 1 (/ (+ 100 n) (* 101.0 n)))))))

;;; Performance comparison summary
(define (print-summary)
  (format #t "~%=== Summary ===~%")
  (format #t "Persistent Data Structures:~%")
  (format #t "  ✓ All operations create new versions~%")
  (format #t "  ✓ Old versions remain accessible~%")
  (format #t "  ✓ Memory sharing between versions~%")
  (format #t "  ✗ Slight overhead per operation~%")
  (format #t "~%")
  (format #t "Ephemeral Data Structures:~%")
  (format #t "  ✓ Faster individual operations~%")
  (format #t "  ✗ Destructive updates lose history~%")
  (format #t "  ✗ Cannot efficiently create versions~%")
  (format #t "  ✗ No memory sharing possible~%"))

;;; Main benchmark runner
(define (main args)
  (format #t "MIT 6.851 - Persistent Data Structures Benchmark~%")
  (format #t "================================================~%")
  
  ;; Run benchmarks with different sizes
  (for-each (lambda (size)
              (benchmark-push-only size)
              (benchmark-push-pop size))
            '(1000 10000 100000))
  
  ;; Version creation benchmark
  (benchmark-version-creation 1000 100)
  
  ;; Memory sharing analysis
  (benchmark-memory-sharing 100)
  
  ;; Summary
  (print-summary)
  
  (format #t "~%Benchmark complete.~%"))

;; Run if executed as script
(main (command-line))