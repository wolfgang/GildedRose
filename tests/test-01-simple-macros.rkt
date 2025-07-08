#lang racket

;; ========================================================================
;; TESTS FOR SIMPLE MACROS
;; ========================================================================

(require rackunit
         "../01-simple-macros.rkt")

;; ========================================================================
;; SIMPLE MACROS TESTS
;; ========================================================================

(printf "Running simple macros tests...\n")

(require rackunit/text-ui)
(run-tests 
 (test-suite "Simple Macros Test Suite"
   
   (test-case "say macro outputs correctly"
     (check-not-exn 
      (lambda () (say "test message"))
      "say macro should not raise exceptions"))
   
   (test-case "def-constant creates proper constants"
     (def-constant TEST-CONST 42)
     (check-equal? TEST-CONST 42 "def-constant should create working constant"))
   
   (test-case "unless macro with single expression"
     (define result #f)
     (unless #f (set! result #t))
     (check-true result "unless should execute when condition is false"))
   
   (test-case "unless macro does not execute when condition is true"
     (define result2 #f)
     (unless #t (set! result2 #t))
     (check-false result2 "unless should not execute when condition is true"))
   
   (test-case "arithmetic shortcuts work"
     (check-equal? (square 4) 16 "square macro should work")
     (check-equal? (cube 2) 8 "cube macro should work")
     (check-equal? (square 0) 0 "square of 0")
     (check-equal? (cube -2) -8 "cube of negative number"))
   
   (test-case "repeat macro works"
     (define test-counter 0)
     (repeat 3 (set! test-counter (+ test-counter 1)))
     (check-equal? test-counter 3 "repeat should execute correct number of times"))
   
   (test-case "let-values-simple works"
     (define result
       (let-values-simple ([a 10] [b 20])
         (+ a b)))
     (check-equal? result 30 "let-values-simple should bind variables correctly"))
   
   (test-case "when-then macro works"
     (define executed #f)
     (when-then #t then (set! executed #t))
     (check-true executed "when-then should execute when condition is true"))
   
   (test-case "double-list works"
     (check-equal? (double-list '(1 2 3)) '(2 4 6) "double-list should work")
     (check-equal? (double-list '()) '() "double-list should handle empty lists"))
   
   (test-case "demo function runs without errors"
     (check-not-exn 
      (lambda () (demo-simple-macros))
      "demo-simple-macros should run without exceptions"))))

(printf "Simple macros tests completed!\n")