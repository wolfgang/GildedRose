#lang racket

;; ========================================================================
;; SYNTAX TRANSFORMERS
;; ========================================================================
;; This file demonstrates advanced syntax transformers that go beyond
;; syntax-rules, allowing for compile-time computation and more sophisticated
;; macro transformations.

(require syntax/parse
         syntax/stx
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide (all-defined-out))

;; ========================================================================
;; 1. BASIC SYNTAX TRANSFORMER
;; ========================================================================

;; Using define-syntax with a lambda for manual syntax transformation
(define-syntax (simple-when stx)
  (syntax-case stx ()
    [(_ condition body ...)
     #'(if condition (begin body ...) (void))]))

;; Usage:
(simple-when #t 
  (displayln "This will print")
  (displayln "So will this"))

;; ========================================================================
;; 2. SYNTAX TRANSFORMERS WITH COMPILE-TIME COMPUTATION
;; ========================================================================

;; A macro that performs compile-time arithmetic
(define-syntax (compile-time-math stx)
  (syntax-case stx ()
    [(_ op a b)
     (let ([result (case (syntax->datum #'op)
                     [(+) (+ (syntax->datum #'a) (syntax->datum #'b))]
                     [(-) (- (syntax->datum #'a) (syntax->datum #'b))]
                     [(*) (* (syntax->datum #'a) (syntax->datum #'b))]
                     [(/) (/ (syntax->datum #'a) (syntax->datum #'b))])])
       (datum->syntax stx result))]))

;; Usage:
(displayln (compile-time-math + 10 5))    ; Computed at compile time
(displayln (compile-time-math * 7 8))     ; Result: 56

;; ========================================================================
;; 3. SYNTAX/PARSE FOR BETTER ERROR MESSAGES
;; ========================================================================

;; Using syntax/parse for better pattern matching and error messages
(define-syntax (typed-define stx)
  (syntax-parse stx
    [(_ name:id type:id value:expr)
     #'(begin
         (define name value)
         (printf "Defined ~a of type ~a with value ~a\n" 
                 'name 'type name))]
    [(_ name:id value:expr)
     #'(begin
         (define name value)
         (printf "Defined ~a with value ~a\n" 'name name))]))

;; Usage:
(typed-define x number 42)
(typed-define greeting "Hello, world!")

;; ========================================================================
;; 4. GENERATING MULTIPLE DEFINITIONS
;; ========================================================================

;; A macro that generates multiple related functions
(define-syntax (define-accessors stx)
  (syntax-parse stx
    [(_ struct-name (field-name:id field-index:number) ...)
     #:with (getter-name ...) (generate-temporaries #'(field-name ...))
     #:with (setter-name ...) (generate-temporaries #'(field-name ...))
     #'(begin
         (define (getter-name instance)
           (list-ref instance field-index)) ...
         (define (setter-name instance new-value)
           (list-set instance field-index new-value)) ...)]))

;; Usage:
(define-accessors person (name 1) (age 2) (email 3))

;; Test the generated accessors
(define john '(person "John" 30 "john@example.com"))
(displayln (name john))     ; Extracts name
(displayln (age john))      ; Extracts age

;; ========================================================================
;; 5. RECURSIVE SYNTAX TRANSFORMATION
;; ========================================================================

;; A macro that recursively transforms nested expressions
(define-syntax (transform-nested stx)
  (define (transform-expr expr)
    (syntax-case expr (+ - * /)
      [(+ a b)
       #`(+ #,(transform-expr #'a) #,(transform-expr #'b))]
      [(- a b)
       #`(- #,(transform-expr #'a) #,(transform-expr #'b))]
      [(* a b)
       #`(* #,(transform-expr #'a) #,(transform-expr #'b))]
      [(/ a b)
       #`(/ #,(transform-expr #'a) #,(transform-expr #'b))]
      [number
       (if (number? (syntax->datum #'number))
           #`(exact->inexact number)
           #'number)]))
  
  (syntax-case stx ()
    [(_ expr)
     (transform-expr #'expr)]))

;; Usage:
(displayln (transform-nested (+ 1 (* 2 3))))
(displayln (transform-nested (/ 10 (- 8 3))))

;; ========================================================================
;; 6. MACRO WITH SYNTAX PROPERTIES
;; ========================================================================

;; A macro that adds syntax properties for debugging
(define-syntax (with-trace stx)
  (syntax-case stx ()
    [(_ name expr)
     (with-syntax ([traced-expr 
                    (syntax-property #'expr 
                                     'trace-info 
                                     (syntax->datum #'name))])
       #'(begin
           (printf "Tracing ~a\n" 'name)
           (let ([result traced-expr])
             (printf "~a result: ~a\n" 'name result)
             result)))]))

;; Usage:
(with-trace calculation (+ 2 3 4))
(with-trace greeting-length (string-length "Hello"))

;; ========================================================================
;; 7. COMPILE-TIME VALUE COMPUTATION
;; ========================================================================

;; A macro that computes values at compile time and embeds them
(define-syntax (define-constants stx)
  (syntax-parse stx
    [(_ (name:id expr:expr) ...)
     #:with (computed-value ...)
     (map (lambda (e) 
            (datum->syntax stx (eval (syntax->datum e))))
          (syntax->list #'(expr ...)))
     #'(begin
         (define name computed-value) ...)]))

;; Usage:
(define-constants 
  (seconds-in-day (* 24 60 60))
  (pi-squared (* 3.14159 3.14159))
  (fibonacci-10 (let fib ([n 10])
                  (if (<= n 1) n
                      (+ (fib (- n 1)) (fib (- n 2)))))))

(displayln seconds-in-day)
(displayln pi-squared)
(displayln fibonacci-10)

;; ========================================================================
;; 8. MACRO THAT GENERATES MACROS
;; ========================================================================

;; A macro that creates other macros
(define-syntax (define-binary-ops stx)
  (syntax-parse stx
    [(_ op-name:id racket-op:id)
     #'(define-syntax (op-name stx)
         (syntax-parse stx
           [(_ a:expr b:expr)
            #'(racket-op a b)]
           [(_ a:expr b:expr c:expr ...)
            #'(racket-op a (op-name b c ...))]))]))

;; Generate some binary operation macros
(define-binary-ops my-add +)
(define-binary-ops my-mul *)

;; Usage:
(displayln (my-add 1 2 3 4))      ; 10
(displayln (my-mul 2 3 4))        ; 24

;; ========================================================================
;; 9. ADVANCED SYNTAX TRANSFORMATION WITH CONTEXT
;; ========================================================================

;; A macro that maintains context during transformation
(define-syntax (with-context stx)
  (define context-stack '())
  
  (define (add-context ctx)
    (set! context-stack (cons ctx context-stack)))
  
  (define (get-current-context)
    (if (null? context-stack) 
        'global 
        (car context-stack)))
  
  (syntax-parse stx
    [(_ context-name:id body:expr ...)
     (add-context (syntax->datum #'context-name))
     #'(begin
         (printf "Entering context: ~a\n" 'context-name)
         body ...
         (printf "Exiting context: ~a\n" 'context-name))]))

;; Usage:
(with-context database-operations
  (displayln "Connecting to database")
  (displayln "Executing query")
  (displayln "Closing connection"))

;; ========================================================================
;; 10. SYNTAX TRANSFORMER WITH CUSTOM PATTERN MATCHING
;; ========================================================================

;; A sophisticated macro using custom pattern matching
(define-syntax (match-and-transform stx)
  (define (custom-match pattern expr)
    (syntax-case pattern ()
      [var 
       (identifier? #'var)
       #`(let ([var #,expr]) var)]
      [(type val)
       (case (syntax->datum #'type)
         [(number) #`(if (number? #,expr) #,expr (error "Expected number"))]
         [(string) #`(if (string? #,expr) #,expr (error "Expected string"))]
         [(list) #`(if (list? #,expr) #,expr (error "Expected list"))])]
      [literal
       #`(if (equal? '#,pattern #,expr) #,expr (error "Pattern mismatch"))]))
  
  (syntax-parse stx
    [(_ pattern:expr value:expr body:expr ...)
     #:with matched-value (custom-match #'pattern #'value)
     #'(let ([result matched-value])
         body ...)]))

;; Usage:
(match-and-transform (number x) 42
  (displayln (+ x 10)))

(match-and-transform (string s) "hello"
  (displayln (string-upcase s)))

;; ========================================================================
;; DEMONSTRATION FUNCTION
;; ========================================================================

(define (demo-syntax-transformers)
  (displayln "\n=== Syntax Transformers Demo ===")
  
  (displayln "\n1. Basic syntax transformation:")
  (simple-when #t (displayln "  Conditional executed"))
  
  (displayln "\n2. Compile-time computation:")
  (printf "  Compile-time: 15 * 4 = ~a\n" (compile-time-math * 15 4))
  
  (displayln "\n3. Generated accessors:")
  (define sample-person '(person "Alice" 25 "alice@example.com"))
  (printf "  Name: ~a, Age: ~a\n" (name sample-person) (age sample-person))
  
  (displayln "\n4. Nested expression transformation:")
  (printf "  Transformed: ~a\n" (transform-nested (+ 5 (* 3 2))))
  
  (displayln "\n5. Traced computation:")
  (with-trace my-calculation (+ 7 8))
  
  (displayln "\n6. Compile-time constants:")
  (printf "  Seconds in day: ~a\n" seconds-in-day)
  (printf "  10th Fibonacci: ~a\n" fibonacci-10)
  
  (displayln "\n7. Generated binary operations:")
  (printf "  Custom add 1+2+3: ~a\n" (my-add 1 2 3))
  (printf "  Custom multiply 2*3*4: ~a\n" (my-mul 2 3 4))
  
  (displayln "\nSyntax transformers demo complete!"))

;; Run demo when file is executed directly
(when (equal? (find-system-path 'run-file) 
              (find-system-path 'orig-dir))
  (demo-syntax-transformers))