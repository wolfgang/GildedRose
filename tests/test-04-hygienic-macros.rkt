#lang racket

;; ========================================================================
;; TESTS FOR HYGIENIC MACROS
;; ========================================================================

(require rackunit
         "../04-hygienic-macros.rkt")

(printf "Running hygienic macros tests...\n")

(require rackunit/text-ui)
(run-tests 
 (test-suite "Hygienic Macros Test Suite"
   
   (test-case "safe-let demonstrates hygiene"
     (define temp-var "outside-value")
     (define result 
       (safe-let ([x 10] [y 20])
         (+ x y)))
     (check-equal? result 30 "safe-let should bind variables correctly")
     (check-equal? temp-var "outside-value" "temp-var should remain unchanged"))
   
   (test-case "with-temporary creates temporary bindings"
     (define temp "global-temp")
     (define test-var 'original)
     (define result 
       (with-temporary test-var 42
         (displayln "Inside macro")
         temp))
     (check-equal? result 42 "with-temporary should return temporary value")
     (check-equal? temp "global-temp" "global temp should be unchanged"))
   
   (test-case "with-it introduces 'it' identifier"
     (check-not-exn 
      (lambda () 
        (with-it 100
          (displayln "with-it macro works")))
      "with-it should introduce 'it' identifier"))
   
   (test-case "with-current-value works with syntax parameters"
     (define result 
       (with-current-value 42
         (+ (current-value) 10)))
     (check-equal? result 52 "with-current-value should work"))
   
   (test-case "define-prefixed creates prefixed identifiers"
     (define-prefixed test (alpha 1) (beta 2) (gamma 3))
     (check-equal? test-alpha 1 "prefixed alpha")
     (check-equal? test-beta 2 "prefixed beta")
     (check-equal? test-gamma 3 "prefixed gamma"))
   
   (test-case "generate-vars creates variables"
     (generate-vars (delta epsilon) 42)
     (check-equal? var-delta 42 "generated delta variable")
     (check-equal? var-epsilon 42 "generated epsilon variable"))
   
   (test-case "define-tracked creates tracked variables"
     (define-tracked test-tracked-var 123)
     (check-equal? test-tracked-var 123 "tracked variable should work"))
   
   (test-case "make-counter creates working counters"
     (define test-counter1 (make-counter 0))
     (define test-counter2 (make-counter 100))
     (check-equal? (test-counter1) 1 "counter1 first call")
     (check-equal? (test-counter1) 2 "counter1 second call")
     (check-equal? (test-counter2) 101 "counter2 first call")
     (check-equal? (test-counter1) 3 "counter1 third call"))
   
   (test-case "identifier=? compares identifiers"
     (check-not-exn 
      (lambda () (identifier=? global-x global-x))
      "identifier=? should not raise exceptions"))
   
   (test-case "expand-and-analyze expands expressions"
     (check-equal? (expand-and-analyze (+ 1 2)) 3 "expand-and-analyze should return result")
     (check-equal? (expand-and-analyze (if #t 'yes 'no)) 'yes "expand-and-analyze conditional"))
   
   (test-case "fresh-let creates fresh bindings"
     (define outer-test-x 'outer)
     (fresh-let ([inner-test-x 'inner])
       (check-equal? inner-test-x 'inner "fresh-let should create inner binding"))
     (check-equal? outer-test-x 'outer "outer variable should be unchanged"))
   
   (test-case "safe-define handles identifier collisions"
     (check-not-exn 
      (lambda () 
        (safe-define test-safe-var 'value)
        'success)
      "safe-define should not raise exceptions"))
   
   (test-case "demo function runs without errors"
     (check-not-exn 
      (lambda () (demo-hygienic-macros))
      "demo-hygienic-macros should run without exceptions"))))

(printf "Hygienic macros tests completed!\n")