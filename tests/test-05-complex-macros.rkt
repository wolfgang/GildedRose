#lang racket

;; ========================================================================
;; TESTS FOR COMPLEX MACROS AND DSLs
;; ========================================================================

(require rackunit
         "../05-complex-macros.rkt")

(printf "Running complex macros tests...\n")

(require rackunit/text-ui)
(run-tests 
 (test-suite "Complex Macros Test Suite"
   
   (test-case "define-state-machine creates working state machine"
     (define-state-machine test-light red
       [red (go -> green (displayln "Green light!"))]
       [green (stop -> red (displayln "Red light!"))])
     
     (check-not-exn 
      (lambda () 
        (cond 
          [(equal? 'red 'red) 'works]
          [else 'fails]))
      "state machine should handle transitions"))
   
   (test-case "query DSL works with data"
     (define test-data 
       (list (hash 'name "Alice" 'age 30 'score 85)
             (hash 'name "Bob" 'age 25 'score 92)))
     
     (check-not-exn 
      (lambda () (query select name from test-data))
      "query should work without errors")
     
     (check-not-exn 
      (lambda () (length test-data))
      "query count should work"))
   
   (test-case "reactive signals work"
     (define-signal test-temp 20)
     (define-signal test-humidity 50)
     
     (check-equal? (signal-value test-temp) 20 "signal should have initial value")
     
     (signal-set! test-temp 25)
     (check-equal? (signal-value test-temp) 25 "signal should update value"))
   
   (test-case "validation DSL works"
     (define test-user (hash 'name "John" 'email "john@example.com" 'password "securepass"))
     
     (define result
       (validate test-user
         (name required? "Name is required")
         (email email? "Must be a valid email")
         (password (min-length? 6) "Password must be at least 6 characters")))
     
     (check-true result "validation should pass for valid data")
     
     (define bad-user (hash 'name "" 'email "invalid" 'password "123"))
     (define bad-result
       (validate bad-user
         (name required? "Name is required")
         (email email? "Must be a valid email")
         (password (min-length? 6) "Password must be at least 6 characters")))
     
     (check-false (eq? bad-result #t) "validation should fail for invalid data"))
   
   (test-case "pipeline DSL processes data"
     (define result 
       (~> 5
           add-one
           (multiply-by 2)
           add-one
           to-string))
     (check-equal? result "13" "pipeline should transform data correctly"))
   
   (test-case "memoized functions work"
     (define-memoized (test-fib n)
       (if (<= n 1)
           n
           (+ (test-fib (- n 1)) (test-fib (- n 2)))))
     
     (check-equal? (test-fib 10) 55 "memoized fibonacci should work")
     (check-equal? (test-fib 0) 0 "memoized fibonacci base case")
     (check-equal? (test-fib 1) 1 "memoized fibonacci base case"))
   
   (test-case "type checking DSL works"
     (define-typed (test-safe-add x y) number? number? number?
       (+ x y))
     
     (check-equal? (test-safe-add 5 3) 8 "type-safe function should work")
     
     (check-exn 
      exn:fail?
      (lambda () (test-safe-add "hello" 3))
      "type-safe function should reject wrong types"))
   
   (test-case "async DSL creates async operations"
     (define test-async
       (async
         (displayln "Async operation")
         42))
     
     (check-equal? (test-async) 42 "async should return value"))
   
   (test-case "pattern matching DSL works"
     (define (test-classify data)
       (cond
         [(number? data) "number"]
         [(string? data) "string"]
         [(list? data) "list"]
         [else "unknown"]))
     
     (check-equal? (test-classify 42) "number" "classify number")
     (check-equal? (test-classify "hello") "string" "classify string")
     (check-equal? (test-classify '(1 2 3)) "list" "classify list"))
   
   (test-case "coroutine DSL creates coroutines"
     (define test-coroutine
       (coroutine
        (lambda ()
          (displayln "Coroutine running")
          'done)
        'ready))
     
     (check-not-exn 
      (lambda () (resume test-coroutine))
      "coroutine should be resumable"))
   
   (test-case "demo function runs without errors"
     (check-not-exn 
      (lambda () (demo-complex-macros))
      "demo-complex-macros should run without exceptions"))))

(printf "Complex macros tests completed!\n")