# Racket Macros Examples

A comprehensive collection of Racket macro examples, ranging from simple transformations to complex metaprogramming constructs.

## What are Racket Macros?

Macros in Racket are compile-time transformations that allow you to extend the language's syntax. They operate on the structure of code (syntax objects) before it's evaluated, enabling powerful metaprogramming capabilities.

## Repository Structure

This repository is organized from simple to complex examples:

1. **01-simple-macros.rkt** - Basic macro definitions and simple transformations
2. **02-pattern-matching.rkt** - Using pattern matching in macros for more flexible transformations
3. **03-syntax-transformers.rkt** - Advanced syntax transformers, with-syntax, and compile-time computation
4. **04-hygienic-macros.rkt** - Hygienic macros and identifier handling
5. **05-complex-macros.rkt** - Complex macro examples including DSLs and advanced patterns
6. **examples/** - Directory containing usage examples for each macro

## Getting Started

To run these examples, you'll need Racket installed on your system. You can download it from [racket-lang.org](https://racket-lang.org/).

### Running Examples

```bash
# Run individual files
racket 01-simple-macros.rkt

# Or load them in the REPL
racket
> (require "01-simple-macros.rkt")
```

## Key Concepts Covered

- **Syntax Transformation**: How macros transform syntax at compile time
- **Pattern Matching**: Using pattern variables and templates
- **Hygiene**: How Racket prevents variable capture in macros
- **Syntax Objects**: Working with the structure of code
- **DSL Creation**: Building domain-specific languages with macros
- **Compile-time Computation**: Performing computations during macro expansion

## Learning Path

1. Start with simple macros to understand basic transformation
2. Learn pattern matching for more flexible macro definitions
3. Explore syntax transformers for advanced use cases
4. Understand hygiene and identifier handling
5. Build complex macros and mini-DSLs

Each file contains detailed explanations and multiple examples to help you understand the concepts progressively.
