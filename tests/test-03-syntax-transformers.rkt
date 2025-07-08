#lang racket

;; ========================================================================
;; TESTS FOR SYNTAX TRANSFORMERS
;; ========================================================================

(require rackunit
         "../03-syntax-transformers.rkt")

(printf "Running syntax transformers tests...\n")

(require rackunit/text-ui)
(run-tests 
 (test-suite "Syntax Transformers Test Suite"
   
   (test-case "simple-when macro works"
     (define executed #f)
     (simple-when #t (set! executed #t))
     (check-true executed "simple-when should execute when condition is true"))
   
   (test-case "compile-time-math performs compile-time computation"
     (check-equal? (compile-time-math + 10 5) 15 "compile-time addition")
     (check-equal? (compile-time-math * 7 8) 56 "compile-time multiplication")
     (check-equal? (compile-time-math - 20 5) 15 "compile-time subtraction")
     (check-equal? (compile-time-math / 20 4) 5 "compile-time division"))
   
   (test-case "typed-define creates definitions with type info"
     (typed-define test-var number 42)
     (check-equal? test-var 42 "typed-define should create working variable"))
   
   (test-case "define-accessors generates accessor functions"
     (define-accessors test-person (name 1) (age 2) (email 3))
     (define test-john '(test-person "John" 30 "john@example.com"))
     (check-equal? (test-person-name test-john) "John" "generated name accessor")
     (check-equal? (test-person-age test-john) 30 "generated age accessor")
     (check-equal? (test-person-email test-john) "john@example.com" "generated email accessor"))
   
   (test-case "transform-nested transforms expressions"
     (check-= (transform-nested (+ 1 2)) 3.0 0.001 "transform simple addition")
     (check-= (transform-nested (* 2 3)) 6.0 0.001 "transform multiplication"))
   
   (test-case "with-trace adds tracing to expressions"
     (check-equal? (with-trace test-calc (+ 2 3)) 5 "with-trace should return correct result")
     (check-not-exn 
      (lambda () (with-trace test-calc2 (* 4 5)))
      "with-trace should not raise exceptions"))
   
   (test-case "define-constants creates compile-time constants"
     (check-equal? seconds-in-day 86400 "seconds-in-day constant")
     (check-= pi-squared 9.869587728099999 0.001 "pi-squared constant")
     (check-equal? fibonacci-10 55 "fibonacci-10 constant"))
   
   (test-case "generated binary operations work"
     (check-equal? (my-add 1 2 3 4) 10 "my-add should work with multiple args")
     (check-equal? (my-mul 2 3 4) 24 "my-mul should work with multiple args"))
   
   (test-case "with-context maintains context"
     (check-not-exn 
      (lambda () 
        (with-context test-context
          (displayln "testing context")))
      "with-context should not raise exceptions"))
   
   ;; WITH-SYNTAX TESTS
   
   (test-case "define-getters creates getter functions"
     (define-getters test-struct field1 field2)
     (define test-data (hash 'field1 "value1" 'field2 "value2"))
     (check-equal? (get-test-struct-field1 test-data) "value1" "generated getter 1")
     (check-equal? (get-test-struct-field2 test-data) "value2" "generated getter 2"))
   
   (test-case "let-fresh creates fresh bindings"
     (define outer-var 'outer)
     (define result #f)
     (let-fresh ([test-var 'inner])
       (set! result test-var))
     (check-equal? result 'inner "let-fresh should create fresh binding")
     (check-equal? outer-var 'outer "outer variable should be unchanged"))
   
   (test-case "define-simple-struct creates struct operations"
     (define-simple-struct test-item name value)
     (define item (make-test-item "test" 42))
     (check-equal? (test-item-name item) "test" "struct getter for name")
     (check-equal? (test-item-value item) 42 "struct getter for value")
     (check-true (test-item? item) "struct predicate"))
   
   (test-case "define-math-ops generates math operations"
     (define-math-ops test-calc)
     (check-equal? (test-calc-add 10 5) 15 "generated add operation")
     (check-equal? (test-calc-mul 3 7) 21 "generated multiply operation"))
   
   (test-case "define-config creates configuration system"
     (define-config test-settings 
       (timeout 30) 
       (retries 3))
     (check-equal? (get-timeout) 30 "config getter for timeout")
     (check-equal? (get-retries) 3 "config getter for retries")
     (set-timeout! 60)
     (check-equal? (get-timeout) 60 "config setter works")
     (check-not-exn 
      (lambda () (show-test-settings))
      "config show function works"))
   
   (test-case "demo function runs without errors"
     (check-not-exn 
      (lambda () (demo-syntax-transformers))
      "demo-syntax-transformers should run without exceptions"))))

(printf "Syntax transformers tests completed!\n")