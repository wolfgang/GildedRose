#lang racket

;; ========================================================================
;; TESTS FOR PATTERN MATCHING MACROS
;; ========================================================================

(require rackunit
         "../02-pattern-matching.rkt")

(printf "Running pattern matching tests...\n")

(require rackunit/text-ui)
(run-tests 
 (test-suite "Pattern Matching Test Suite"
   
   (test-case "debug-print works with variable arguments"
     (check-not-exn 
      (lambda () (debug-print test-values 1 2 3 "hello"))
      "debug-print should handle variable arguments"))
   
   (test-case "defun creates functions with documentation"
     (defun test-greet (name) "Greets a person" (format "Hello, ~a!" name))
     (check-equal? (test-greet "Alice") "Hello, Alice!" "defun should create working function"))
   
   (test-case "defun creates functions without documentation"
     (defun test-add (x y) (+ x y))
     (check-equal? (test-add 5 3) 8 "defun should work without documentation"))
   
   (test-case "match-list handles empty list"
     (define empty-result #f)
     (match-list '()
       [empty (set! empty-result #t)]
       [cons head tail (set! empty-result #f)])
     (check-true empty-result "match-list should detect empty list"))
   
   (test-case "match-list handles non-empty list"
     (define non-empty-result #f)
     (define captured-head #f)
     (match-list '(1 2 3)
       [empty (set! non-empty-result #f)]
       [cons head tail (begin
                         (set! non-empty-result #t)
                         (set! captured-head head))])
     (check-true non-empty-result "match-list should detect non-empty list")
     (check-equal? captured-head 1 "match-list should capture head correctly"))
   
   (test-case "destructure works with list patterns"
     (define extracted-values '())
     (destructure (list a b c) '(1 2 3)
       (set! extracted-values (list a b c)))
     (check-equal? extracted-values '(1 2 3) "destructure should extract list elements"))
   
   (test-case "classify-number function works"
     (check-equal? (classify-number 0) "zero" "classify zero")
     (check-equal? (classify-number -5) "negative" "classify negative")
     (check-equal? (classify-number 150) "large" "classify large")
     (check-equal? (classify-number 8) "even" "classify even")
     (check-equal? (classify-number 7) "odd positive" "classify odd positive"))
   
   (test-case "math-expr evaluates expressions correctly"
     (check-equal? (math-expr (+ 3 4)) 7 "math-expr addition")
     (check-equal? (math-expr (* 3 4)) 12 "math-expr multiplication")
     (check-equal? (math-expr (expt 2 3)) 8 "math-expr exponentiation")
     (check-= (math-expr (sqrt 16)) 4.0 0.001 "math-expr square root"))
   
   (test-case "list-process handles different patterns"
     (check-not-exn 
      (lambda () 
        (list-process '() empty (displayln "Empty list handled")))
      "list-process should handle empty pattern")
     
     (check-not-exn 
      (lambda () 
        (list-process '(42) single (displayln "Single element handled")))
      "list-process should handle single element")
     
     (check-not-exn 
      (lambda () 
        (list-process '(1 2) pair (displayln "Pair handled")))
      "list-process should handle pair pattern"))
   
   (test-case "demo function runs without errors"
     (check-not-exn 
      (lambda () (demo-pattern-matching))
      "demo-pattern-matching should run without exceptions"))))

(printf "Pattern matching tests completed!\n")