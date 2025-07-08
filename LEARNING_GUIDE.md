# Racket Macros Learning Guide

This repository provides a comprehensive introduction to Racket's powerful macro system, organized from simple concepts to advanced techniques.

## What You'll Learn

By working through these examples, you'll understand:

- **Syntax Transformation**: How macros transform code at compile time
- **Pattern Matching**: Using patterns to match and transform syntax
- **Hygiene**: Racket's automatic variable capture prevention
- **Compile-time Computation**: Performing calculations during macro expansion
- **DSL Creation**: Building domain-specific languages

## Learning Path

### 🎯 Level 1: Simple Macros (`01-simple-macros.rkt`)

**Start here if you're new to macros**

**Concepts covered:**
- Basic `define-syntax` and `syntax-rules`
- Simple substitution patterns
- Multiple pattern matching
- Literal keywords in macros

**Key examples:**
```racket
(say "Hello, macros!")           ; Simple output macro
(unless #f (displayln "Hello"))  ; Control flow macro
(square 5)                       ; Mathematical shorthand
```

**Time to spend:** 30-45 minutes

### 🎯 Level 2: Pattern Matching (`02-pattern-matching.rkt`)

**After understanding basic macros**

**Concepts covered:**
- Ellipsis patterns (`...`) for variable arguments
- Nested pattern destructuring
- Pattern guards and conditions
- Recursive pattern transformations

**Key examples:**
```racket
(debug-print values 1 2 3 "hello")  ; Variable arguments
(defun greet (name) "doc" body)      ; Optional documentation
(math-expr (+ 3 (* 4 5)))           ; Expression DSL
```

**Time to spend:** 45-60 minutes

### 🎯 Level 3: Syntax Transformers (`03-syntax-transformers.rkt`)

**After mastering pattern matching**

**Concepts covered:**
- Manual syntax transformation with `syntax-case`
- Compile-time computation and code generation
- Syntax properties and advanced transformations
- Generating multiple definitions

**Key examples:**
```racket
(compile-time-math * 7 8)        ; Compile-time arithmetic
(with-trace calculation expr)     ; Syntax properties
(define-accessors person ...)     ; Code generation
```

**Time to spend:** 60-90 minutes

### 🎯 Level 4: Hygienic Macros (`04-hygienic-macros.rkt`)

**Advanced identifier handling**

**Concepts covered:**
- Automatic variable capture prevention
- Explicit identifier introduction
- Syntax parameters and rebinding
- Fresh identifier generation

**Key examples:**
```racket
(with-current-value 42 expr)     ; Syntax parameters
(fresh-let ([x 'inner]) body)    ; Hygienic bindings
(define-prefixed math ...)        ; Identifier generation
```

**Time to spend:** 60-90 minutes

### 🎯 Level 5: Complex Macros & DSLs (`05-complex-macros.rkt`)

**Building sophisticated domain-specific languages**

**Concepts covered:**
- State machine DSLs
- Query languages
- Reactive programming
- Validation systems
- Type checking
- Memoization

**Key examples:**
```racket
(define-state-machine door-lock ...)  ; State machines
(query select name from data ...)     ; SQL-like queries
(validate user-data ...)              ; Data validation
(~> data step1 step2 step3)          ; Pipeline processing
```

**Time to spend:** 90-120 minutes

## Running the Examples

### Individual Files
```bash
# Run a specific level
racket 01-simple-macros.rkt
racket 02-pattern-matching.rkt
# ... etc
```

### Complete Demonstration
```bash
# Run the comprehensive demo
racket run-all-examples.rkt
```

### Interactive Examples
```bash
# Start a REPL and load macros
racket
> (require "01-simple-macros.rkt")
> (say "Hello from REPL!")
```

## Practical Examples

The `examples/` directory contains real-world usage scenarios:

- **simple-macros-example.rkt**: Practical applications of basic macros
- **complex-macros-example.rkt**: Building real applications with DSLs

## Best Practices

### When to Use Macros

✅ **Good use cases:**
- Creating domain-specific languages
- Eliminating boilerplate code
- Compile-time optimizations
- Custom control structures

❌ **Avoid macros when:**
- A function would work just as well
- The logic is complex and error-prone
- You need runtime flexibility

### Macro Development Tips

1. **Start simple**: Begin with `syntax-rules` before moving to `syntax-case`
2. **Test thoroughly**: Macros can be hard to debug
3. **Use hygiene**: Let Racket handle variable capture automatically
4. **Provide good error messages**: Use `syntax/parse` for better errors
5. **Document patterns**: Make your macro's syntax clear

## Common Pitfalls

### Variable Capture
```racket
;; BAD - can capture variables
(define-syntax bad-let
  (syntax-rules ()
    [(_ body) (let ([temp 'internal]) body)]))

;; GOOD - hygienic by default
(define-syntax good-let
  (syntax-rules ()
    [(_ body) (let ([temp 'internal]) body)]))
```

### Pattern Variable Scope
```racket
;; BAD - x not in scope
(define-syntax bad-collect
  (syntax-rules ()
    [(_ expr lst) (map (lambda (item) expr) lst)]))

;; GOOD - x properly bound
(define-syntax good-collect
  (syntax-rules ()
    [(_ expr lst) (map (lambda (x) expr) lst)]))
```

## Further Reading

- [Racket Reference: Macros](https://docs.racket-lang.org/reference/Macros.html)
- [Fear of Macros](http://www.greghendershott.com/fear-of-macros/)
- [Racket Style Guide](https://docs.racket-lang.org/style/index.html)

## Getting Help

1. **Racket documentation**: Comprehensive and well-written
2. **Racket community**: Active on Discord, Reddit, and mailing lists
3. **DrRacket IDE**: Built-in help and macro stepper
4. **Stack Overflow**: Tag your questions with `racket`

## Summary

Racket's macro system is one of the most powerful and elegant in any programming language. By working through these examples progressively, you'll gain:

- Deep understanding of compile-time metaprogramming
- Ability to create custom languages and DSLs
- Skills to eliminate repetitive code patterns
- Knowledge of advanced programming language concepts

Start with Level 1 and work your way up. Each level builds on the previous one, giving you a solid foundation in one of programming's most powerful tools.

Happy macro programming! 🚀