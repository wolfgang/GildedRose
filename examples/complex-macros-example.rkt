#lang racket

;; Example usage of complex macros and DSLs from 05-complex-macros.rkt

(require "../05-complex-macros.rkt")

;; ========================================================================
;; PRACTICAL EXAMPLES OF COMPLEX MACROS AND DSLs
;; ========================================================================

;; 1. Building a Traffic Light State Machine
(define-state-machine traffic-light red
  [red (timer -> green (displayln "🟢 GO!"))
       (emergency -> yellow (displayln "⚠️ Emergency override!"))]
  [green (timer -> yellow (displayln "🟡 SLOW DOWN!"))
         (emergency -> red (displayln "🔴 Emergency stop!"))]
  [yellow (timer -> red (displayln "🔴 STOP!"))
          (emergency -> red (displayln "🔴 Emergency stop!"))])

;; 2. User Management with Validation
(define users-db 
  (list (hash 'name "admin" 'email "admin@company.com" 'role "administrator")
        (hash 'name "alice" 'email "alice@company.com" 'role "user")
        (hash 'name "bob" 'email "bob@company.com" 'role "manager")))

(define (find-admins)
  (query select name email from users-db where (equal? (hash-ref row 'role) "administrator")))

(define (count-managers)
  (query count from users-db where (equal? (hash-ref row 'role) "manager")))

;; 3. Form Validation System
(define (validate-registration form-data)
  (validate form-data
    (username required? "Username is required")
    (username (min-length? 3) "Username must be at least 3 characters")
    (email required? "Email is required")
    (email email? "Must be a valid email address")
    (password required? "Password is required")
    (password (min-length? 8) "Password must be at least 8 characters")
    (age (lambda (x) (and (number? x) (>= x 13))) "Must be at least 13 years old")))

;; 4. Data Processing Pipeline
(define (process-order-data raw-data)
  (~> raw-data
      (map (lambda (item) (hash-set item 'processed-at (current-seconds))))
      (filter (lambda (item) (> (hash-ref item 'amount 0) 0)))
      (map (lambda (item) (hash-set item 'tax (* (hash-ref item 'amount) 0.08))))
      (sort (lambda (a b) (> (hash-ref a 'amount) (hash-ref b 'amount))))))

;; 5. Reactive Temperature Monitoring System
(define-signal room-temp 22)
(define-signal humidity 45)
(define-signal outside-temp 18)

(define comfort-score 
  (signal-map (lambda (rt h ot) 
                (+ (* rt 0.5) (* (- 50 h) 0.3) (* ot 0.2))) 
              room-temp humidity outside-temp))

(define (update-environment room-t humid outside-t)
  (signal-set! room-temp room-t)
  (signal-set! humidity humid)
  (signal-set! outside-temp outside-t)
  (printf "Comfort score: ~a\n" (signal-value comfort-score)))

;; 6. Cached Expensive Computations
(define-memoized (expensive-calculation x y)
  (printf "Computing expensive operation for ~a, ~a\n" x y)
  (sleep 0.5)  ; Simulate expensive computation
  (+ (* x x) (* y y) (* x y)))

;; 7. Type-Safe Mathematical Operations
(define-typed (safe-sqrt x) number? positive-number?
  (sqrt x))

(define-typed (safe-factorial n) number? (lambda (x) (and (integer? x) (>= x 0)))
  (if (= n 0) 1 (* n (safe-factorial (- n 1)))))

;; ========================================================================
;; DEMO FUNCTIONS
;; ========================================================================

(define (demo-traffic-system)
  (displayln "=== Traffic Light Demo ===")
  (traffic-light 'timer)
  (traffic-light 'timer)
  (traffic-light 'emergency)
  (traffic-light 'timer))

(define (demo-user-management)
  (displayln "\n=== User Management Demo ===")
  (printf "Administrators: ~a\n" (find-admins))
  (printf "Number of managers: ~a\n" (count-managers)))

(define (demo-validation)
  (displayln "\n=== Form Validation Demo ===")
  
  (define valid-form (hash 'username "alice123" 'email "alice@example.com" 
                          'password "securepass123" 'age 25))
  (define invalid-form (hash 'username "ab" 'email "invalid-email" 
                            'password "123" 'age 10))
  
  (printf "Valid form result: ~a\n" (validate-registration valid-form))
  (printf "Invalid form errors: ~a\n" (validate-registration invalid-form)))

(define (demo-pipeline)
  (displayln "\n=== Data Pipeline Demo ===")
  
  (define orders '((hash amount 100 customer "Alice")
                   (hash amount 0 customer "Bob")
                   (hash amount 250 customer "Charlie")
                   (hash amount 75 customer "Diana")))
  
  (define processed (process-order-data orders))
  (printf "Processed orders: ~a\n" processed))

(define (demo-reactive-system)
  (displayln "\n=== Reactive System Demo ===")
  
  (printf "Initial comfort: ~a\n" (signal-value comfort-score))
  (update-environment 24 55 20)
  (update-environment 26 65 25))

(define (demo-memoization)
  (displayln "\n=== Memoization Demo ===")
  
  (printf "First call: ~a\n" (expensive-calculation 5 3))
  (printf "Second call (cached): ~a\n" (expensive-calculation 5 3))
  (printf "Different args: ~a\n" (expensive-calculation 4 2)))

(define (demo-type-safety)
  (displayln "\n=== Type Safety Demo ===")
  
  (printf "Safe sqrt(16): ~a\n" (safe-sqrt 16))
  (printf "Safe factorial(5): ~a\n" (safe-factorial 5))
  
  ;; These would raise errors:
  ;; (safe-sqrt -4)
  ;; (safe-factorial -1))

;; ========================================================================
;; MAIN DEMO
;; ========================================================================

(define (demo-complex-examples)
  (displayln "=== Complex Macros Practical Examples ===")
  
  (demo-traffic-system)
  (demo-user-management)
  (demo-validation)
  (demo-pipeline)
  (demo-reactive-system)
  (demo-memoization)
  (demo-type-safety)
  
  (displayln "\nComplex macros examples complete!"))

;; Run the demo
(demo-complex-examples)