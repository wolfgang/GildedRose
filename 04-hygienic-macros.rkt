#lang racket

;; ========================================================================
;; HYGIENIC MACROS AND IDENTIFIER HANDLING
;; ========================================================================
;; This file demonstrates Racket's hygienic macro system, which automatically
;; prevents variable capture and provides sophisticated identifier handling.

(require syntax/parse
         syntax/stx
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide (all-defined-out))

;; ========================================================================
;; 1. BASIC HYGIENE DEMONSTRATION
;; ========================================================================

;; This macro demonstrates automatic hygiene - it won't capture variables
(define-syntax (safe-let stx)
  (syntax-case stx ()
    [(_ ([var val] ...) body ...)
     #'(let ([temp-var 'macro-internal])  ; This won't interfere
         (let ([var val] ...)
           body ...))]))

;; Usage - this is safe even if we use 'temp-var' outside
(define temp-var "outside-value")
(safe-let ([x 10] [y 20])
  (displayln (format "x=~a, y=~a, temp-var=~a" x y temp-var)))

;; ========================================================================
;; 2. DEMONSTRATING VARIABLE CAPTURE PREVENTION
;; ========================================================================

;; This macro shows how Racket prevents accidental variable capture
(define-syntax (with-temporary stx)
  (syntax-case stx ()
    [(_ name value body ...)
     #'(let ([temp value]     ; 'temp' is hygienic - won't capture
             [backup name])   ; Save original value if it exists
         body ...
         temp)]))            ; Return the temporary value

;; Usage - even if we have a variable named 'temp', it won't interfere
(define temp "global-temp")
(displayln (with-temporary x 42
  (displayln "Inside macro")
  (displayln temp)))  ; This prints "global-temp", not 42

;; ========================================================================
;; 3. EXPLICIT IDENTIFIER INTRODUCTION
;; ========================================================================

;; Sometimes we want to introduce identifiers that ARE visible to user code
(define-syntax (with-it stx)
  (syntax-case stx ()
    [(_ value body ...)
     #'(let ([it value])      ; 'it' is intentionally introduced
         body ...)]))

;; Usage:
(displayln (with-it 100
  (+ it 23)))  ; 'it' refers to 100

;; ========================================================================
;; 4. SYNTAX-PARAMETERIZE FOR IDENTIFIER REBINDING
;; ========================================================================

;; Define a syntax parameter that can be rebound
(define-syntax-parameter current-value
  (lambda (stx)
    (raise-syntax-error 'current-value 
                        "can only be used inside with-current-value")))

(define-syntax (with-current-value stx)
  (syntax-case stx ()
    [(_ val body ...)
     #'(syntax-parameterize ([current-value (lambda (stx) #'val)])
         body ...)]))

;; Usage:
(displayln (with-current-value 42
  (+ (current-value) 10)))

;; ========================================================================
;; 5. IDENTIFIER MANIPULATION WITH SYNTAX-CASE
;; ========================================================================

;; A macro that creates prefixed identifiers
(define-syntax (define-prefixed stx)
  (syntax-case stx ()
    [(_ prefix (name value) ...)
     (with-syntax ([(prefixed-name ...)
                    (map (lambda (n)
                           (format-id stx "~a-~a" #'prefix n))
                         (syntax->list #'(name ...)))])
       #'(begin
           (define prefixed-name value) ...))]))

;; Usage:
(define-prefixed math (pi 3.14159) (e 2.71828) (golden 1.618))
(displayln math-pi)
(displayln math-e)
(displayln math-golden)

;; ========================================================================
;; 6. DATUM->SYNTAX FOR CONTROLLED INTRODUCTION
;; ========================================================================

;; A macro that generates identifiers based on data
(define-syntax (generate-vars stx)
  (syntax-case stx ()
    [(_ (name ...) init-value)
     (with-syntax ([(var-name ...)
                    (map (lambda (n)
                           (datum->syntax stx 
                                          (string->symbol 
                                           (format "var-~a" 
                                                   (syntax->datum n)))))
                         (syntax->list #'(name ...)))])
       #'(begin
           (define var-name init-value) ...))]))

;; Usage:
(generate-vars (alpha beta gamma) 0)
(set! var-alpha 10)
(set! var-beta 20)
(displayln (+ var-alpha var-beta))

;; ========================================================================
;; 7. SYNTAX PROPERTIES FOR IDENTIFIER TRACKING
;; ========================================================================

;; A macro that tracks identifier sources using syntax properties
(define-syntax (define-tracked stx)
  (syntax-case stx ()
    [(_ name value)
     (with-syntax ([tracked-name (syntax-property #'name 
                                                  'source-location
                                                  (syntax-source stx))])
       #'(define tracked-name value))]))

;; Usage:
(define-tracked tracked-variable 42)
(displayln tracked-variable)

;; ========================================================================
;; 8. MACRO THAT RESPECTS LEXICAL SCOPE
;; ========================================================================

;; A macro that creates closures while respecting lexical scope
(define-syntax (make-counter stx)
  (syntax-case stx ()
    [(_ initial-value)
     #'(let ([count initial-value])
         (lambda ()
           (set! count (+ count 1))
           count))]))

;; Usage:
(define counter1 (make-counter 0))
(define counter2 (make-counter 100))
(displayln (counter1))  ; 1
(displayln (counter1))  ; 2
(displayln (counter2))  ; 101

;; ========================================================================
;; 9. IDENTIFIER COMPARISON AND MANIPULATION
;; ========================================================================

;; A macro that compares identifiers for hygiene
(define-syntax (identifier=? stx)
  (syntax-case stx ()
    [(_ id1 id2)
     (if (free-identifier=? #'id1 #'id2)
         #'#t
         #'#f)]))

;; A macro that demonstrates bound-identifier=?
(define-syntax (show-identifier-equality stx)
  (syntax-case stx ()
    [(_ id)
     #'(begin
         (printf "Free identifier=? with 'x: ~a\n" 
                 (identifier=? id x))
         (printf "Identifier info: ~a\n" 'id))]))

;; Usage:
(define x 'global-x)
(show-identifier-equality x)

;; ========================================================================
;; 10. ADVANCED HYGIENE WITH LOCAL-EXPAND
;; ========================================================================

;; A macro that uses local-expand for controlled expansion
(define-syntax (expand-and-analyze stx)
  (syntax-case stx ()
    [(_ expr)
     (let ([expanded (local-expand #'expr 'expression '())])
       (printf "Original: ~a\n" (syntax->datum #'expr))
       (printf "Expanded: ~a\n" (syntax->datum expanded))
       expanded)]))

;; Usage:
(expand-and-analyze (+ 1 2))
(expand-and-analyze (if #t 'yes 'no))

;; ========================================================================
;; 11. MACRO THAT INTRODUCES HYGIENIC BINDINGS
;; ========================================================================

;; A macro that creates a let-like form with guaranteed fresh variables
(define-syntax (fresh-let stx)
  (syntax-parse stx
    [(_ ([var:id init:expr] ...) body:expr ...)
     #:with (fresh-var ...) (generate-temporaries #'(var ...))
     #'(let ([fresh-var init] ...)
         (let-syntax ([var (lambda (stx) #'fresh-var)] ...)
           body ...))]))

;; Usage:
(define x 'outer)
(fresh-let ([x 'inner])
  (displayln x))  ; Prints 'inner'
(displayln x)     ; Prints 'outer'

;; ========================================================================
;; 12. MACRO THAT HANDLES IDENTIFIER COLLISIONS
;; ========================================================================

;; A macro that automatically renames conflicting identifiers
(define-syntax (safe-define stx)
  (syntax-parse stx
    [(_ name:id value:expr)
     #:with safe-name (if (identifier-binding #'name)
                          (generate-temporary #'name)
                          #'name)
     #'(begin
         (when (identifier-binding #'name)
           (printf "Warning: ~a already bound, using fresh name\n" 'name))
         (define safe-name value))]))

;; Usage:
(define x 'first)
(safe-define x 'second)  ; This will generate a warning and use fresh name

;; ========================================================================
;; DEMONSTRATION FUNCTION
;; ========================================================================

(define (demo-hygienic-macros)
  (displayln "\n=== Hygienic Macros Demo ===")
  
  (displayln "\n1. Automatic hygiene prevention:")
  (define temp-demo "outer-temp")
  (safe-let ([a 5] [b 10])
    (printf "  Inside macro: a=~a, b=~a, temp-demo=~a\n" a b temp-demo))
  
  (displayln "\n2. Explicit identifier introduction:")
  (printf "  Using 'it': ~a\n" (with-it "hello" (string-upcase it)))
  
  (displayln "\n3. Syntax parameterization:")
  (printf "  Current value + 5 = ~a\n" 
          (with-current-value 15 (+ (current-value) 5)))
  
  (displayln "\n4. Prefixed identifiers:")
  (printf "  Math constants: π=~a, e=~a\n" math-pi math-e)
  
  (displayln "\n5. Generated variables:")
  (printf "  Generated vars: var-alpha=~a, var-beta=~a\n" var-alpha var-beta)
  
  (displayln "\n6. Counter closures (lexical scope):")
  (define demo-counter (make-counter 50))
  (printf "  Counter: ~a, ~a, ~a\n" 
          (demo-counter) (demo-counter) (demo-counter))
  
  (displayln "\n7. Fresh let bindings:")
  (define outer-var 'outer)
  (fresh-let ([outer-var 'fresh-inner])
    (printf "  Inside fresh-let: ~a\n" outer-var))
  (printf "  Outside fresh-let: ~a\n" outer-var)
  
  (displayln "\nHygienic macros demo complete!"))

;; Run demo when file is executed directly
(when (equal? (find-system-path 'run-file) 
              (find-system-path 'orig-dir))
  (demo-hygienic-macros))