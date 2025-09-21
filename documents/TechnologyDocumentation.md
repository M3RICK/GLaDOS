# GLADOS LISP Compiler - Technology Choices Documentation

## Project Overview
This document outlines the technology choices made for the GLADOS LISP compiler project, including the rationale behind each decision and alternative options that were considered.

## Table of Contents
- [Programming Language](#programming-language)
- [Build System](#build-system)
- [Parsing Library](#parsing-library)
- [Project Organization](#project-organization)
- [Testing Framework](#testing-framework)
- [Build Automation](#build-automation)
- [CI/CD System](#cicd-system)

---

## Programming Language

### Chosen: Haskell
**Rationale:** Haskell was mandated by the project specifications, making it a non-negotiable requirement. The language's functional programming paradigm aligns well with compiler development due to its mathematical foundations, pattern matching capabilities, and strong type system that helps catch errors at compile-time. Our team also had previous experience with Haskell from coursework, which reduced the learning curve.

**Alternative Options:** Since Haskell was required, no alternatives were considered for the main implementation language.

---

## Build System

### Chosen: Stack
**Rationale:** Stack was recommended in the project PDF documentation and provided several advantages for our team. We had successfully used Stack in previous projects, allowing us to reuse code from last year and leverage existing knowledge. This choice minimized risk by avoiding time spent learning new build systems, letting us focus on core project requirements. Stack also offers reliable package management and reproducible builds.

**Alternative Options:**
- **Cabal**: The traditional Haskell build tool, lighter weight but with less comprehensive dependency management
- **Nix**: Provides reproducible builds across different systems but requires a steeper learning curve
- **Direct GHC**: Manual compilation approach, impractical for larger projects with dependencies

---

## Parsing Library

### Chosen: Parsec
**Rationale:** We selected Parsec over the other recommended option (Megaparsec) because it offered sufficient functionality for our LISP parsing requirements without unnecessary complexity. Parsec's simpler approach aligned with our philosophy that "simplicity is always better" - choosing tools that adequately solve the problem without added overhead. The library is well-documented with extensive examples, making implementation straightforward.

**Alternative Options:**
- **Megaparsec**: Offers more advanced features and better error messages, but the added complexity wasn't deemed necessary for this project
- **Attoparsec**: High-performance parser primarily designed for binary data and simple text formats
- **Happy**: Parser generator tool requiring additional syntax and workflow learning
- **Alex + Happy**: Lexer/parser combination offering a more traditional compiler approach but with higher complexity

---

## Project Organization

### Chosen: Notion
**Rationale:** Notion was selected over simpler alternatives because it provides comprehensive project tracking capabilities that extend beyond basic task management. The platform combines note-taking, task management, and documentation in a single solution, offering better team collaboration features. Learning Notion also provides transferable skills valuable for future projects and professional work, making it a strategic choice for long-term development.

**Alternative Options:**
- **Trello**: Simpler Kanban-style board management with less detailed tracking capabilities
- **Microsoft Loop**: Good integration with Microsoft ecosystem but less specialized for project management
- **Jira**: Professional project management tool potentially overcomplicated for academic projects
- **GitHub Projects**: Integrated with code repository but limited organizational features
- **Asana**: Strong task management features but less flexible for documentation needs

---

## Testing Framework

### Chosen: Hspec
**Rationale:** Hspec provides a clean, readable syntax for describing test behavior using a Behavior-Driven Development approach. It's particularly suitable for unit testing individual compiler components like the lexer, parser, and evaluator. The framework integrates seamlessly with our chosen Stack build system and offers comprehensive testing capabilities with well-documented examples for testing functional code.

**Alternative Options:**
- **HUnit**: More traditional unit testing framework with less expressive syntax
- **QuickCheck**: Property-based testing excellent for testing compiler properties but requires more setup
- **Tasty**: Test framework that can combine multiple testing libraries but adds unnecessary complexity
- **Doctest**: Tests embedded in documentation, less suitable for comprehensive compiler testing

---

## Build Automation

### Chosen: Makefile
**Rationale:** A Makefile was explicitly required by the project PDF, which must include `re`, `clean`, and `fclean` rules. Our implementation wraps Stack commands to provide a standardized build process that works across different development environments. This approach offers simplicity with a familiar interface for common build operations while meeting all project requirements.

**Alternative Options:**
- **Direct Stack usage**: Using Stack commands directly doesn't meet project requirements
- **Shell scripts**: Could automate builds but less standardized than Make
- **Just**: Modern command runner but not as widely supported as Make

---

## CI/CD System

### Chosen: GitHub Actions
**Rationale:** GitHub Actions was selected because CI/CD implementation is explicitly required by the project PDF. The platform provides native integration with our GitHub repository hosting, excellent support for Haskell Stack builds and testing, and automatically runs our Hspec test suite on every push to prevent bad code from being merged. It also produces functional executable builds automatically, satisfying the continuous delivery requirements.

**Alternative Options:**
- **Travis CI**: Popular CI service requiring external setup and configuration
- **CircleCI**: Powerful platform with more complex configuration requirements
- **Jenkins**: Self-hosted solution requiring dedicated infrastructure management

---

## Conclusion
The technology choices for the GLADOS project prioritized reliability, simplicity, and leveraging existing knowledge while meeting all project requirements. Each tool was selected to minimize risk while providing adequate functionality for successful compiler implementation.