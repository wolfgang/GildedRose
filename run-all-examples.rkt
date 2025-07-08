#lang racket

;; ========================================================================
;; RUN ALL RACKET MACRO EXAMPLES
;; ========================================================================
;; This file demonstrates all the macro examples from simple to complex,
;; showing the progression of Racket's macro system capabilities.

(require "01-simple-macros.rkt"
         "02-pattern-matching.rkt"
         "03-syntax-transformers.rkt"
         "04-hygienic-macros.rkt"
         "05-complex-macros.rkt")

;; ========================================================================
;; INTRODUCTION AND EXPLANATION
;; ========================================================================

(define (print-header title)
  (displayln "\n" )
  (displayln (make-string (string-length title) #\=))
  (displayln title)
  (displayln (make-string (string-length title) #\=)))

(define (print-section section)
  (displayln "")
  (displayln (format "--- ~a ---" section)))

(define (explain-macros)
  (print-header "RACKET MACROS COMPREHENSIVE DEMO")
  
  (displayln "")
  (displayln "This demonstration showcases Racket's powerful macro system,")
  (displayln "progressing from simple syntax transformations to complex")
  (displayln "domain-specific languages (DSLs).")
  (displayln "")
  (displayln "Racket macros operate at compile time, transforming syntax")
  (displayln "before code execution. This enables:")
  (displayln "")
  (displayln "• Custom syntax and control structures")
  (displayln "• Domain-specific languages")
  (displayln "• Code generation and optimization")
  (displayln "• Hygienic macro expansion (prevents variable capture)")
  (displayln "• Compile-time computation")
  (displayln "")
  (displayln "Let's explore these capabilities step by step...")
  (displayln ""))

;; ========================================================================
;; PROGRESSIVE DEMONSTRATION
;; ========================================================================

(define (demo-progression)
  (explain-macros)
  
  ;; 1. Simple Macros
  (print-section "LEVEL 1: SIMPLE MACROS")
  (displayln "Starting with basic syntax transformations...")
  (displayln "• Simple substitution and transformation")
  (displayln "• Multiple patterns and basic control flow")
  (displayln "• Arithmetic shortcuts and repetition")
  (demo-simple-macros)
  
  ;; 2. Pattern Matching
  (print-section "LEVEL 2: PATTERN MATCHING")
  (displayln "Adding sophisticated pattern matching...")
  (displayln "• Ellipsis patterns for variable arguments")
  (displayln "• Nested pattern destructuring")
  (displayln "• Recursive pattern transformations")
  (demo-pattern-matching)
  
  ;; 3. Syntax Transformers
  (print-section "LEVEL 3: SYNTAX TRANSFORMERS")
  (displayln "Introducing compile-time computation...")
  (displayln "• Manual syntax transformation with syntax-case")
  (displayln "• Compile-time arithmetic and code generation")
  (displayln "• Recursive transformations and syntax properties")
  (demo-syntax-transformers)
  
  ;; 4. Hygienic Macros
  (print-section "LEVEL 4: HYGIENIC MACROS")
  (displayln "Exploring Racket's hygiene system...")
  (displayln "• Automatic variable capture prevention")
  (displayln "• Explicit identifier introduction and manipulation")
  (displayln "• Syntax parameters and fresh bindings")
  (demo-hygienic-macros)
  
  ;; 5. Complex Macros and DSLs
  (print-section "LEVEL 5: COMPLEX MACROS & DSLs")
  (displayln "Building sophisticated domain-specific languages...")
  (displayln "• State machines and query languages")
  (displayln "• Reactive programming and validation DSLs")
  (displayln "• Type checking and memoization systems")
  (demo-complex-macros)
  
  ;; Final summary
  (print-header "DEMONSTRATION COMPLETE")
  (displayln "")
  (displayln "You've seen the full spectrum of Racket's macro capabilities:")
  (displayln "")
  (displayln "✓ Simple syntax transformations")
  (displayln "✓ Pattern matching and destructuring") 
  (displayln "✓ Compile-time computation")
  (displayln "✓ Hygienic identifier handling")
  (displayln "✓ Complex DSL construction")
  (displayln "")
  (displayln "Racket's macro system enables you to:")
  (displayln "• Extend the language with custom syntax")
  (displayln "• Create domain-specific languages")
  (displayln "• Optimize code at compile time")
  (displayln "• Safely manipulate program structure")
  (displayln "")
  (displayln "Explore the individual files to study specific techniques:")
  (displayln "• 01-simple-macros.rkt - Basic transformations")
  (displayln "• 02-pattern-matching.rkt - Advanced patterns")
  (displayln "• 03-syntax-transformers.rkt - Compile-time computation")
  (displayln "• 04-hygienic-macros.rkt - Identifier handling")
  (displayln "• 05-complex-macros.rkt - DSL construction")
  (displayln "• examples/ - Practical usage examples")
  (displayln ""))

;; ========================================================================
;; INDIVIDUAL CONCEPT DEMONSTRATIONS
;; ========================================================================

(define (demo-specific-concept concept)
  (case concept
    [(simple)
     (print-header "SIMPLE MACROS DEMO")
     (demo-simple-macros)]
    [(patterns)
     (print-header "PATTERN MATCHING DEMO")
     (demo-pattern-matching)]
    [(transformers)
     (print-header "SYNTAX TRANSFORMERS DEMO")
     (demo-syntax-transformers)]
    [(hygiene)
     (print-header "HYGIENIC MACROS DEMO")
     (demo-hygienic-macros)]
    [(complex)
     (print-header "COMPLEX MACROS & DSLs DEMO")
     (demo-complex-macros)]
    [else
     (displayln "Unknown concept. Available: simple, patterns, transformers, hygiene, complex")]))

;; ========================================================================
;; INTERACTIVE EXAMPLES
;; ========================================================================

(define (interactive-examples)
  (print-header "INTERACTIVE MACRO EXAMPLES")
  (displayln "")
  (displayln "Try these examples in the REPL:")
  (displayln "")
  
  (displayln "1. Simple transformations:")
  (displayln "   (say \"Hello, macros!\")")
  (displayln "   (unless #f (displayln \"This prints\"))")
  (displayln "   (square 5)")
  (displayln "")
  
  (displayln "2. Pattern matching:")
  (displayln "   (debug-print values 1 2 3)")
  (displayln "   (defun greet (name) (format \"Hello, ~a!\" name))")
  (displayln "   (math-expr (+ 3 (* 4 5)))")
  (displayln "")
  
  (displayln "3. Syntax transformers:")
  (displayln "   (compile-time-math * 7 8)")
  (displayln "   (with-trace calculation (+ 2 3 4))")
  (displayln "   (transform-nested (+ 1 (* 2 3)))")
  (displayln "")
  
  (displayln "4. Hygienic macros:")
  (displayln "   (with-current-value 42 (+ (current-value) 10))")
  (displayln "   (fresh-let ([x 'inner]) (displayln x))")
  (displayln "")
  
  (displayln "5. Complex DSLs:")
  (displayln "   (validate user-data (name required? \"Name required\"))")
  (displayln "   (~> 5 add-one (multiply-by 3) to-string)")
  (displayln "   (fibonacci 10)  ; memoized version")
  (displayln ""))

;; ========================================================================
;; MAIN EXECUTION
;; ========================================================================

;; Function to run specific demonstrations
(define (run-demo [type 'all])
  (case type
    [(all) (demo-progression)]
    [(interactive) (interactive-examples)]
    [(simple patterns transformers hygiene complex) (demo-specific-concept type)]
    [else 
     (displayln "Usage: (run-demo [type])")
     (displayln "Types: all, interactive, simple, patterns, transformers, hygiene, complex")]))

;; Run the full demonstration by default
(when (equal? (find-system-path 'run-file) 
              (find-system-path 'orig-dir))
  (run-demo 'all))

;; Export the demo function for interactive use
(provide run-demo demo-specific-concept interactive-examples)