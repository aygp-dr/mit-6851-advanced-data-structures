;;; MIT 6.851 Test Runner

(use-modules (srfi srfi-64)
             (ice-9 ftw))

;; Add lib directory to load path
(add-to-load-path (string-append (getcwd) "/lib"))

(define (find-test-files dir)
  "Find all test files in directory"
  (let ((files '()))
    (ftw dir
         (lambda (filename statinfo flag)
           (when (and (eq? flag 'regular)
                      (string-suffix? "-test.scm" filename))
             (set! files (cons filename files)))
           #t))
    files))

(define (run-all-tests)
  "Run all test files"
  (test-runner-factory
   (lambda () (test-runner-simple)))
  
  (let ((test-files (find-test-files "tests")))
    (format #t "Found ~a test files~%" (length test-files))
    (for-each 
     (lambda (file)
       (format #t "Running tests in ~a...~%" file)
       (load file))
     test-files)))

;; Run tests
(run-all-tests)