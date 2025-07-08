#lang racket

;; ========================================================================
;; SIMPLE MACROS
;; ========================================================================
;; This file introduces the basics of Racket macros with simple examples
;; that demonstrate core concepts and syntax transformation.

(provide (all-defined-out))

;; ========================================================================
;; 1. BASIC MACRO DEFINITION
;; ========================================================================

;; The simplest macro: define-syntax with syntax-rules
;; This macro creates a simple shorthand for console output
(define-syntax say
  (syntax-rules ()
    [(_ message)
     (displayln message)]))

;; Usage examples:
(say "Hello, macros!")
(say "This is a simple transformation")

;; ========================================================================
;; 2. SIMPLE SUBSTITUTION MACROS
;; ========================================================================

;; A macro that creates a more readable way to define constants
(define-syntax def-constant
  (syntax-rules ()
    [(_ name value)
     (define name value)]))

;; Usage:
(def-constant PI 3.14159)
(def-constant GREETING "Hello, World!")

;; Verify our constants work:
(displayln PI)
(displayln GREETING)

;; ========================================================================
;; 3. MACROS WITH MULTIPLE PATTERNS
;; ========================================================================

;; A macro that can handle different numbers of arguments
(define-syntax unless
  (syntax-rules ()
    [(_ condition body)
     (if (not condition) body (void))]
    [(_ condition body1 body2 ...)
     (if (not condition) (begin body1 body2 ...) (void))]))

;; Usage examples:
(unless #f (say "This will print"))
(unless #t (say "This won't print"))
(unless #f 
  (say "Multiple")
  (say "statements")
  (say "work too"))

;; ========================================================================
;; 4. SIMPLE ARITHMETIC SHORTHAND
;; ========================================================================

;; Macros for common mathematical operations
(define-syntax square
  (syntax-rules ()
    [(_ x)
     (* x x)]))

(define-syntax cube
  (syntax-rules ()
    [(_ x)
     (* x x x)]))

;; Usage:
(displayln (square 5))    ; Prints 25
(displayln (cube 3))      ; Prints 27

;; ========================================================================
;; 5. SIMPLE CONTROL FLOW MACRO
;; ========================================================================

;; A macro that repeats an expression n times
(define-syntax repeat
  (syntax-rules ()
    [(_ n expr)
     (for ([i (in-range n)])
       expr)]))

;; Usage:
(repeat 3 (say "Repeated message"))

;; ========================================================================
;; 6. SIMPLE BINDING MACRO
;; ========================================================================

;; A macro that creates multiple variable bindings at once
(define-syntax let-values-simple
  (syntax-rules ()
    [(_ ([var val] ...) body)
     (let ([var val] ...) body)]))

;; Usage:
(let-values-simple ([x 10] [y 20])
  (say (+ x y)))

;; ========================================================================
;; 7. MACRO WITH LITERALS
;; ========================================================================

;; A macro that uses literal keywords
(define-syntax when-then
  (syntax-rules (then)
    [(_ condition then action)
     (when condition action)]))

;; Usage:
(when-then #t then (say "Condition was true"))

;; ========================================================================
;; 8. SIMPLE LIST PROCESSING MACRO
;; ========================================================================

;; A simple list doubling macro
(define-syntax double-list
  (syntax-rules ()
    [(_ lst)
     (map (lambda (x) (* x 2)) lst)]))

;; Usage:
(define numbers '(1 2 3 4 5))
(displayln (double-list numbers))
(displayln (map (lambda (x) (* x x)) numbers))

;; ========================================================================
;; DEMONSTRATION FUNCTION
;; ========================================================================

;; Function to demonstrate all the simple macros
(define (demo-simple-macros)
  (say "\n=== Simple Macros Demo ===")
  
  (say "\n1. Basic substitution:")
  (say GREETING)
  
  (say "\n2. Conditional execution:")
  (unless #f (say "Unless macro works!"))
  
  (say "\n3. Mathematical shortcuts:")
  (say (format "Square of 4: ~a" (square 4)))
  (say (format "Cube of 3: ~a" (cube 3)))
  
  (say "\n4. Repetition:")
  (repeat 2 (say "  Repeated line"))
  
  (say "\n5. List processing:")
  (say (format "Doubled numbers: ~a" (double-list '(1 2 3))))
  
  (say "\nSimple macros demo complete!"))

;; Run the demo when this file is executed directly
(when (equal? (find-system-path 'run-file) 
              (find-system-path 'orig-dir))
  (demo-simple-macros))