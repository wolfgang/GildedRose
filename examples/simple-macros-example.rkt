#lang racket

;; Example usage of simple macros from 01-simple-macros.rkt

(require "../01-simple-macros.rkt")

;; ========================================================================
;; PRACTICAL EXAMPLES OF SIMPLE MACROS
;; ========================================================================

;; 1. Using the say macro for logging
(define (log-operation name)
  (say (format "Starting operation: ~a" name))
  (sleep 1)  ; Simulate work
  (say (format "Completed operation: ~a" name)))

;; 2. Using def-constant for configuration
(def-constant DATABASE-URL "localhost:5432")
(def-constant MAX-CONNECTIONS 10)
(def-constant TIMEOUT-SECONDS 30)

;; 3. Using unless for guard clauses
(define (process-user user)
  (unless user
    (error "User cannot be null"))
  (unless (hash-has-key? user 'name)
    (error "User must have a name"))
  (say (format "Processing user: ~a" (hash-ref user 'name))))

;; 4. Using mathematical shortcuts
(define (calculate-circle-area radius)
  (say (format "Calculating area for radius ~a" radius))
  (* PI (square radius)))

(define (calculate-volume side)
  (say (format "Calculating volume for cube with side ~a" side))
  (cube side))

;; 5. Using repeat for initialization
(define counter 0)
(define (increment-counter)
  (set! counter (+ counter 1))
  (say (format "Counter is now: ~a" counter)))

;; 6. Using list processing
(define students 
  '((name "Alice" grade 85)
    (name "Bob" grade 92)
    (name "Charlie" grade 78)
    (name "Diana" grade 94)))

(define (extract-field field lst)
  (map (lambda (x) (cadr x)) 
       (filter (lambda (x) (eq? (car x) field)) lst)))

;; ========================================================================
;; DEMO FUNCTION
;; ========================================================================

(define (demo-simple-examples)
  (say "\n=== Simple Macros Practical Examples ===")
  
  (say "\n1. Logging operations:")
  (log-operation "database-backup")
  
  (say "\n2. Configuration constants:")
  (say (format "Database: ~a, Max connections: ~a" DATABASE-URL MAX-CONNECTIONS))
  
  (say "\n3. User validation:")
  (define valid-user (hash 'name "John" 'email "john@example.com"))
  (process-user valid-user)
  
  (say "\n4. Mathematical calculations:")
  (say (format "Circle area (radius 5): ~a" (calculate-circle-area 5)))
  (say (format "Cube volume (side 3): ~a" (calculate-volume 3)))
  
  (say "\n5. Repetitive operations:")
  (repeat 3 (increment-counter))
  
  (say "\n6. Data extraction:")
  (define names (extract-field 'name students))
  (say (format "Student names: ~a" names))
  
  (say "\nSimple macros examples complete!"))

;; Run the demo
(demo-simple-examples)