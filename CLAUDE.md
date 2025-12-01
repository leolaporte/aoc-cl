# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Leo Laporte's Advent of Code solutions in Common Lisp, spanning years 2015-2024. Each year has its own directory with daily puzzle solutions. The repository demonstrates progression in Common Lisp style and library usage over time.

## Repository Structure

```
AOC/
├── template.lisp           # 2024 template (newest)
├── 2015/dayXX/dayXX.lisp   # Lowercase naming
├── 2022/dayXX/dayXX.lisp   # Lowercase naming
├── 2023/dayXX/dayXX.lisp   # Lowercase naming
└── 2024/dayXX/dayXX.lisp   # Lowercase naming + input.txt
```

**Naming conventions evolved over time:**
- 2015, 2022-2024: lowercase (day01, day02...)

## Development Commands

### Running Solutions
```lisp
;; Interactive REPL (recommended)
sbcl
(load "dayXX.lisp")  ; auto-runs tests and solution

;; Direct execution
sbcl --load dayXX.lisp
```

### Testing
- Tests run automatically on load via `(setf 5a:*run-test-when-defined* t)`
- Manual test runs: `(5a:run! 'test-name)` or `(5a:run-all-tests)`
- Each solution includes inline FiveAM tests with example data

### Dependencies
Solutions use Quicklisp for package management. Load at REPL start:

**2024 solutions (most recent):**
```lisp
(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
```

**2022-2023 solutions:**
```lisp
(ql:quickload '(:fiveam :cl-ppcre))
```

## Code Architecture

### Standard Package Setup

**2024 pattern (iterate-based):**
```lisp
(defpackage :dayXX
  (:use #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)    ; regex
   (:sr :serapeum)    ; utilities
   (:tr :trivia)      ; pattern matching
   (:5a :fiveam)))    ; testing

(use-package :iterate)  ; makes iter available without prefix
```

**2022-2023 pattern (loop-based):**
```lisp
(defpackage :dayXX
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))
```

### File Structure Pattern

Every solution follows this structure:

```lisp
;;;; DayXX.lisp header with year, author, dates

;; Prologue: package setup and dependencies

;; Configuration
(setf 5a:*run-test-when-defined* t)
(declaim (optimize (debug 3)))      ; or (speed 3) for performance

;; Data file path
(defparameter *data-file* "~/cl/AOC/YYYY/dayXX/input.txt")

;; Problem description in block comments
#| --- Day XX: Title ---
   Problem statement and Leo's notes
|#

;; Example data for testing
(defparameter *example* '(...))

;; Solution implementation with inline tests
(defun parse-input (lines) ...)
(5a:test parse-input-test ...)

(defun dayXX-1 (input) ...)
(5a:test dayXX-1-test ...)

(defun dayXX-2 (input) ...)
(5a:test dayXX-2-test ...)

;; Execution with timing
(time (format t "The answer to AOC YYYY Day XX Part 1 is ~a"
              (dayXX-1 (uiop:read-file-lines *data-file*))))

;; Timings section at bottom
;; Results with hardware specs (M3/M4 Mac, SBCL)
```

### Key Patterns and Conventions

**Data file paths:**
- Always use `~/cl/AOC/YYYY/dayXX/input.txt` format
- 2024 solutions use `*data-file*` parameter
- 2022 solutions sometimes use `+data-file+` constant

**Input reading:**
```lisp
(uiop:read-file-lines *data-file*)  ; returns list of strings
```

**Iteration:**
- 2024: prefer `iter` macro from iterate library
- 2022-2023: use standard `loop` macro
- Both approaches valid, but 2024 shows preference shift

**Problem-solving approach:**
- Extensive comments explaining logic (see day15 2024 for example)
- Helper functions tested independently
- Part 1 and Part 2 in same file
- Tests use example data from problem descriptions

**Common utilities:**
- `cl-ppcre` (`:re`) for regex parsing
- `serapeum` (`:sr`) for utility functions (2024+)
- `trivia` (`:tr`) for pattern matching (2024+)
- `str` library for string manipulation (2024+)

### Performance Considerations

When optimizing:
```lisp
(declaim (optimize (speed 3) (safety 0)))
```

Results documented at file bottom with hardware specs and timing.

## Working with This Repository

**Creating new solutions:**
1. Copy appropriate template (year-specific or root `template.lisp`)
2. Replace `###` placeholders with day number
3. Update year references if needed
4. Create corresponding `input.txt` file
5. Implement solution with tests

**File paths:**
- Primary: `~/cl/AOC/`
- Working directory: `/home/leo/Source/lisp/AOC/` (git repo)
- Solutions reference input via `~/cl/AOC/` path

**Testing strategy:**
- Write tests for helper functions first
- Use provided examples as test data
- Tests run automatically on file load
- All tests should pass before running on real input

**Git workflow:**
- Active development on main branch
- Sync conflicts present (ignore `.sync-conflict` files)
- Commit messages follow pattern: "Day XX part Y done"
