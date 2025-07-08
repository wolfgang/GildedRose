# Racket Macro Lab 🚀

A comprehensive learning repository for Racket's powerful macro system, featuring progressive examples, complete test coverage, and practical applications.

![Racket](https://img.shields.io/badge/Language-Racket-blue?style=flat-square&logo=racket)
![Tests](https://img.shields.io/badge/Tests-59%20Passing-brightgreen?style=flat-square)
![License](https://img.shields.io/badge/License-MIT-blue?style=flat-square)

## 🎯 Overview

This repository provides a structured journey through Racket's macro system, from simple transformations to sophisticated domain-specific languages (DSLs). Each module builds progressively on previous concepts, with comprehensive examples, automated tests, and practical applications.

**Perfect for:**
- Racket developers learning macros
- Programming language enthusiasts
- Metaprogramming exploration
- DSL creation studies

## 📚 Learning Path

### 1. **Simple Macros** (`01-simple-macros.rkt`)
Start your macro journey with fundamental concepts:
- Basic syntax transformations
- Substitution patterns
- Control flow macros
- Arithmetic shortcuts
- List processing

**Key Concepts:** `syntax-rules`, pattern matching, template expansion

### 2. **Pattern Matching** (`02-pattern-matching.rkt`)
Master advanced pattern matching techniques:
- Ellipsis patterns (`...`)
- Nested pattern matching
- Variable argument handling
- Recursive transformations
- Mathematical expression DSLs

**Key Concepts:** Complex patterns, recursion in macros, DSL foundations

### 3. **Syntax Transformers** (`03-syntax-transformers.rkt`)
Dive into powerful syntax transformation tools:
- `syntax-case` macros
- Compile-time computation
- Identifier generation with `format-id`
- **`with-syntax` patterns**
- Code generation techniques

**Key Concepts:** `syntax-case`, `with-syntax`, compile-time evaluation

### 4. **Hygienic Macros** (`04-hygienic-macros.rkt`)
Understand Racket's automatic hygiene system:
- Variable capture prevention
- Identifier manipulation
- Fresh identifier generation
- Syntax parameters
- Lexical scope preservation

**Key Concepts:** Hygiene, identifier binding, scope management

### 5. **Complex Macros & DSLs** (`05-complex-macros.rkt`)
Build sophisticated domain-specific languages:
- State machine DSL
- Query language (SQL-like)
- Reactive programming
- Validation framework
- Pipeline processing
- Memoization system
- Type checking DSL

**Key Concepts:** DSL design, advanced metaprogramming, real-world applications

## 🚀 Quick Start

### Prerequisites
- [Racket](https://racket-lang.org/) (version 8.0 or later)

### Installation
```bash
git clone <this-repository>
cd racket-macro-lab
```

### Running Examples
```bash
# Run individual modules
racket 01-simple-macros.rkt
racket 02-pattern-matching.rkt
racket 03-syntax-transformers.rkt
racket 04-hygienic-macros.rkt
racket 05-complex-macros.rkt

# Run all examples
racket run-all-examples.rkt
```

### Running Tests
```bash
# Run complete test suite
racket run-tests.rkt

# Run individual test modules
racket tests/test-01-simple-macros.rkt
racket tests/test-02-pattern-matching.rkt
# ... etc
```

## 📁 Repository Structure

```
racket-macro-lab/
├── README.md                          # This file
├── LEARNING_GUIDE.md                  # Detailed learning guide
├── TEST_REPORT.md                     # Comprehensive test report
│
├── 01-simple-macros.rkt              # Basic macro concepts
├── 02-pattern-matching.rkt           # Advanced pattern matching
├── 03-syntax-transformers.rkt        # Syntax-case and with-syntax
├── 04-hygienic-macros.rkt           # Hygiene and identifier handling
├── 05-complex-macros.rkt            # DSLs and metaprogramming
├── run-all-examples.rkt              # Execute all examples
│
├── examples/                          # Practical applications
│   ├── web-dsl-example.rkt          # Web routing DSL
│   ├── data-validation-example.rkt   # Data validation framework
│   ├── state-machine-example.rkt     # State machine implementation
│   └── mini-language-example.rkt     # Complete mini-language
│
├── tests/                            # Comprehensive test suite
│   ├── test-01-simple-macros.rkt    # 10 tests
│   ├── test-02-pattern-matching.rkt  # 10 tests
│   ├── test-03-syntax-transformers.rkt # 15 tests
│   ├── test-04-hygienic-macros.rkt  # 13 tests
│   └── test-05-complex-macros.rkt   # 11 tests
│
└── run-tests.rkt                     # Test runner with reporting
```

## ✨ Key Features

### 🧪 **Comprehensive Test Coverage**
- **59 automated tests** across all modules
- Tests serve as additional usage examples
- Continuous integration ready
- Fast execution (full suite runs in seconds)

### 📖 **Progressive Learning**
- Each module builds on previous concepts
- Clear explanations and documentation
- Practical examples and use cases
- Best practices and common patterns

### 🔧 **Practical Applications**
- Real-world DSL examples
- Web routing systems
- Data validation frameworks
- State machines
- Query languages

### 🎯 **Quality Assurance**
- All examples tested and verified
- Clean, well-documented code
- Consistent coding patterns
- Error handling examples

## 🎓 Learning Outcomes

After completing this lab, you'll be able to:

1. **Write Basic Macros**: Create simple syntax transformations
2. **Handle Complex Patterns**: Use ellipsis and nested patterns effectively
3. **Generate Code**: Build macros that generate complex code structures
4. **Maintain Hygiene**: Understand and work with Racket's hygiene system
5. **Design DSLs**: Create domain-specific languages for specific problems
6. **Debug Macros**: Understand common pitfalls and debugging techniques

## 📚 Additional Resources

- **[LEARNING_GUIDE.md](LEARNING_GUIDE.md)** - Detailed progressive learning guide
- **[TEST_REPORT.md](TEST_REPORT.md)** - Complete test coverage report
- **[Racket Guide - Macros](https://docs.racket-lang.org/guide/macros.html)** - Official documentation
- **[Fear of Macros](https://www.greghendershott.com/fear-of-macros/)** - Excellent macro tutorial
- **[Racket Reference - Macros](https://docs.racket-lang.org/reference/macros.html)** - Complete reference

## 🤝 Contributing

Contributions are welcome! Please feel free to:
- Add new macro examples
- Improve documentation
- Enhance test coverage
- Fix bugs or issues
- Suggest new learning modules

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- The Racket community for creating such a powerful macro system
- Contributors to Racket's excellent documentation
- Everyone who helped make metaprogramming accessible

---

**Happy macro hacking!** 🎉

*Start your journey with `01-simple-macros.rkt` and work your way through to building your own DSLs!*
