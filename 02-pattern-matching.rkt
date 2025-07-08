#lang racket

;; ========================================================================
;; PATTERN MATCHING IN MACROS
;; ========================================================================
;; This file demonstrates advanced pattern matching capabilities in Racket
;; macros, showing how to destructure and transform complex syntax patterns.

(provide (all-defined-out))

;; ========================================================================
;; 1. ELLIPSIS PATTERNS (...)
;; ========================================================================

;; A macro that handles variable numbers of arguments using ellipsis
(define-syntax debug-print
  (syntax-rules ()
    [(_ name value ...)
     (begin
       (printf "~a: " 'name)
       (printf "~a " value) ...
       (newline))]))

;; Usage:
(debug-print variables 1 2 3 "hello")
(debug-print single-value 42)

;; ========================================================================
;; 2. NESTED PATTERN MATCHING
;; ========================================================================

;; A macro that matches nested list structures
(define-syntax defstruct-simple
  (syntax-rules ()
    [(_ struct-name (field-name field-type) ...)
     (begin
       (define (make-struct-name field-name ...)
         (list 'struct-name field-name ...))
       (define (struct-name-field-name s)
         (list-ref s (add1 field-name))) ...)]))

;; More practical example: matching function definitions with optional documentation
(define-syntax defun
  (syntax-rules ()
    [(_ name (param ...) doc-string body ...)
     (begin
       (define (name param ...)
         doc-string
         body ...))]
    [(_ name (param ...) body ...)
     (begin
       (define (name param ...)
         body ...))]))

;; Usage:
(defun greet (name) "Greets a person" (format "Hello, ~a!" name))
(defun add-numbers (x y) (+ x y))

(displayln (greet "Alice"))
(displayln (add-numbers 5 3))

;; ========================================================================
;; 3. PATTERN MATCHING WITH GUARDS
;; ========================================================================

;; Using pattern matching to handle different cases
(define-syntax cond-match
  (syntax-rules (else)
    [(_ [condition action] ...)
     (cond [condition action] ...)]
    [(_ [condition action] ... [else default])
     (cond [condition action] ... [else default])]))

;; Usage with a more complex pattern matcher
(define-syntax match-list
  (syntax-rules (empty cons)
    [(_ lst [empty action])
     (if (null? lst) action (void))]
    [(_ lst [cons head tail action])
     (if (pair? lst)
         (let ([head (car lst)] [tail (cdr lst)])
           action)
         (void))]
    [(_ lst [empty empty-action] [cons head tail cons-action])
     (if (null? lst)
         empty-action
         (let ([head (car lst)] [tail (cdr lst)])
           cons-action))]))

;; Usage:
(match-list '(1 2 3)
  [empty (displayln "List is empty")]
  [cons head tail (printf "Head: ~a, Tail: ~a\n" head tail)])

;; ========================================================================
;; 4. COMPLEX PATTERN DECOMPOSITION
;; ========================================================================

;; A macro that creates multiple binding patterns
(define-syntax let-destructure
  (syntax-rules ()
    [(_ ([var val] ...) body ...)
     (let ([var val] ...) body ...)]
    [(_ ([(var ...) list-val]) body ...)
     (let ([var (list-ref list-val (- (length '(var ...)) (length (member 'var '(var ...))) 1))] ...)
       body ...)]
    [(_ ([var list-val index]) body ...)
     (let ([var (list-ref list-val index)])
       body ...)]))

;; Better destructuring macro using match
(define-syntax destructure
  (syntax-rules ()
    [(_ pattern value body ...)
     (match value
       [pattern body ...])]))

;; Usage:
(destructure (list a b c) '(1 2 3)
  (printf "a=~a, b=~a, c=~a\n" a b c))

;; ========================================================================
;; 5. RECURSIVE PATTERN MATCHING
;; ========================================================================

;; A macro that builds recursive data transformations
(define-syntax transform-tree
  (syntax-rules (leaf node)
    [(_ (leaf val) transformer)
     (transformer val)]
    [(_ (node left right) transformer)
     (list 'node 
           (transform-tree left transformer)
           (transform-tree right transformer))]))

;; Usage example:
;; (transform-tree (node (leaf 1) (leaf 2)) (lambda (x) (* x 2)))
;; Note: This macro works with syntax forms, not quoted data

;; ========================================================================
;; 6. PATTERN MATCHING WITH MULTIPLE TRANSFORMATIONS
;; ========================================================================

;; A macro that applies different transformations based on patterns
(define-syntax pattern-transform
  (syntax-rules (-> when)
    [(_ value)
     value]
    [(_ value [pattern -> result] rest ...)
     (match value
       [pattern result]
       [_ (pattern-transform value rest ...)])]
    [(_ value [pattern when condition -> result] rest ...)
     (match value
       [pattern #:when condition result]
       [_ (pattern-transform value rest ...)])]))

;; Usage:
(define (classify-number n)
  (pattern-transform n
    [0 -> "zero"]
    [x when (< x 0) -> "negative"]
    [x when (> x 100) -> "large"]
    [x when (even? x) -> "even"]
    [_ -> "odd positive"]))

(displayln (classify-number 0))
(displayln (classify-number -5))
(displayln (classify-number 150))
(displayln (classify-number 8))
(displayln (classify-number 7))

;; ========================================================================
;; 7. ADVANCED LIST PATTERN MATCHING  
;; ========================================================================

;; A simplified macro for list processing
(define-syntax list-process
  (syntax-rules (head tail empty single pair)
    [(_ lst empty action)
     (if (null? lst) action (void))]
    [(_ lst single action)
     (if (and (pair? lst) (null? (cdr lst)))
         (let ([head (car lst)]) action)
         (void))]
    [(_ lst pair action)
     (if (and (pair? lst) (pair? (cdr lst)) (null? (cddr lst)))
         (let ([first (car lst)] [second (cadr lst)]) action)
         (void))]
    [(_ lst [head tail] action)
     (if (pair? lst)
         (let ([head (car lst)] [tail (cdr lst)]) action)
         (void))]))

;; ========================================================================
;; 8. PATTERN-BASED DSL CREATION
;; ========================================================================

;; A mini-DSL for mathematical expressions using pattern matching
(define-syntax math-expr
  (syntax-rules (+ - * / expt sqrt log)
    [(_ n) n]
    [(_ (+ a b)) (+ (math-expr a) (math-expr b))]
    [(_ (- a b)) (- (math-expr a) (math-expr b))]
    [(_ (* a b)) (* (math-expr a) (math-expr b))]
    [(_ (/ a b)) (/ (math-expr a) (math-expr b))]
    [(_ (expt a b)) (expt (math-expr a) (math-expr b))]
    [(_ (sqrt a)) (sqrt (math-expr a))]
    [(_ (log a)) (log (math-expr a))]))

;; Usage:
(displayln (math-expr (+ 3 (* 4 5))))
(displayln (math-expr (expt 2 3)))
(displayln (math-expr (sqrt (+ (* 3 3) (* 4 4)))))

;; ========================================================================
;; DEMONSTRATION FUNCTION
;; ========================================================================

(define (demo-pattern-matching)
  (displayln "\n=== Pattern Matching Macros Demo ===")
  
  (displayln "\n1. Variable arguments with ellipsis:")
  (debug-print test-values 1 2 3 "hello")
  
  (displayln "\n2. Function definition with optional documentation:")
  (displayln (greet "Bob"))
  
  (displayln "\n3. List pattern matching:")
  (match-list '(10 20 30)
    [cons head tail (printf "First: ~a, Rest: ~a\n" head tail)])
  
  (displayln "\n4. Tree transformation:")
  (printf "Tree transformation works with syntax forms at compile time\n")
  
  (displayln "\n5. Pattern-based classification:")
  (printf "42 is: ~a\n" (classify-number 42))
  (printf "13 is: ~a\n" (classify-number 13))
  
  (displayln "\n6. Mathematical DSL:")
  (printf "3 + 4 * 5 = ~a\n" (math-expr (+ 3 (* 4 5))))
  
  (displayln "\nPattern matching demo complete!"))

;; Run demo when file is executed directly
(when (equal? (find-system-path 'run-file) 
              (find-system-path 'orig-dir))
  (demo-pattern-matching))