#lang racket

;; ========================================================================
;; RACKET MACROS TEST RUNNER
;; ========================================================================
;; This script runs all tests for the Racket macro examples and provides
;; a comprehensive test report.

(require racket/system
         racket/match
         racket/string)

;; ========================================================================
;; TEST CONFIGURATION
;; ========================================================================

(define test-files
  '("tests/test-01-simple-macros.rkt"
    "tests/test-02-pattern-matching.rkt"
    "tests/test-03-syntax-transformers.rkt"
    "tests/test-04-hygienic-macros.rkt"
    "tests/test-05-complex-macros.rkt"))

(define test-descriptions
  '("Simple Macros"
    "Pattern Matching"
    "Syntax Transformers (with with-syntax)"
    "Hygienic Macros"
    "Complex Macros & DSLs"))

;; ========================================================================
;; TEST EXECUTION
;; ========================================================================

(define (run-single-test test-file description)
  (printf "Running ~a tests...\n" description)
  (define-values (proc out in err)
    (subprocess #f #f #f (find-executable-path "racket") test-file))
  (subprocess-wait proc)
  (define exit-code (subprocess-status proc))
  (define output (port->string out))
  (close-input-port out)
  (close-input-port err)
  (close-output-port in)
  
  (values exit-code output))

(define (print-header)
  (printf "\n")
  (printf "========================================\n")
  (printf "   RACKET MACROS TEST SUITE\n")
  (printf "========================================\n")
  (printf "\n")
  (printf "Testing comprehensive macro examples:\n")
  (printf "• Simple syntax transformations\n")
  (printf "• Advanced pattern matching\n")
  (printf "• Syntax transformers with with-syntax\n")
  (printf "• Hygienic macro system\n")
  (printf "• Complex DSLs and metaprogramming\n")
  (printf "\n"))

(define (print-summary results)
  (printf "\n")
  (printf "========================================\n")
  (printf "   TEST RESULTS SUMMARY\n")
  (printf "========================================\n")
  (printf "\n")
  
  (define total-tests (length results))
  (define passed-tests (length (filter (lambda (r) (= (car r) 0)) results)))
  (define failed-tests (- total-tests passed-tests))
  
  (for ([result results]
        [desc test-descriptions])
    (match result
      [(list 0 output)
       (printf "✓ ~a: PASSED\n" desc)]
      [(list code output)
       (printf "✗ ~a: FAILED (exit code ~a)\n" desc code)]))
  
  (printf "\n")
  (printf "Total tests: ~a\n" total-tests)
  (printf "Passed: ~a\n" passed-tests)
  (printf "Failed: ~a\n" failed-tests)
  (printf "\n")
  
  (if (= failed-tests 0)
      (begin
        (printf "🎉 ALL TESTS PASSED! 🎉\n")
        (printf "\n")
        (printf "All macro examples are working correctly:\n")
        (printf "• Basic macros and transformations ✓\n")
        (printf "• Pattern matching and destructuring ✓\n")
        (printf "• Syntax transformers and with-syntax ✓\n")
        (printf "• Hygienic identifier handling ✓\n")
        (printf "• Complex DSLs and metaprogramming ✓\n")
        (printf "\n")
        (printf "The Racket macros repository is ready for learning!\n"))
      (begin
        (printf "❌ Some tests failed. Please check the output above.\n")))
  
  (printf "\n")
  (= failed-tests 0))

;; ========================================================================
;; MAIN EXECUTION
;; ========================================================================

(define (run-all-tests)
  (print-header)
  
  (define results
    (for/list ([test-file test-files]
               [description test-descriptions])
      (printf "----------------------------------------\n")
      (define-values (exit-code output) (run-single-test test-file description))
      (printf "~a" output)
      (if (= exit-code 0)
          (printf "✓ ~a tests completed successfully!\n\n" description)
          (printf "✗ ~a tests failed with exit code ~a!\n\n" description exit-code))
      (list exit-code output)))
  
  (print-summary results))

;; ========================================================================
;; COMMAND LINE INTERFACE
;; ========================================================================

(define (main)
  (match (current-command-line-arguments)
    [(vector)
     (run-all-tests)]
    [(vector "help")
     (printf "Racket Macros Test Runner\n")
     (printf "\n")
     (printf "Usage: racket run-tests.rkt [command]\n")
     (printf "\n")
     (printf "Commands:\n")
     (printf "  (no args)  Run all tests\n")
     (printf "  help       Show this help message\n")
     (printf "  list       List available test modules\n")]
    [(vector "list")
     (printf "Available test modules:\n")
     (for ([file test-files]
           [desc test-descriptions])
       (printf "• ~a: ~a\n" file desc))]
    [(vector unknown)
     (printf "Unknown command: ~a\n" unknown)
     (printf "Use 'racket run-tests.rkt help' for usage information.\n")]))

;; Run main when script is executed directly
(main)