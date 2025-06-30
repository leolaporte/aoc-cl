# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is Leo Laporte's Advent of Code (AOC) solutions repository written in Common Lisp. The repository contains solutions spanning multiple years (2015-2024) with each day's solution in its own directory containing the Lisp source file and input data.

## Common Lisp Development Commands

### Running Solutions
- Load and execute a solution: `sbcl --load Day_XX.lisp`
- Run with timing: Solutions use `(time ...)` for performance measurement
- Interactive development: Start SBCL REPL and load with `(load "Day_XX.lisp")`

### Testing
- Tests are embedded using FiveAM framework: `(5a:run-all-tests)`
- Individual test runs: `(5a:run 'test-name)`
- Tests run automatically when defined due to `(setf 5a:*run-test-when-defined* t)`

### Dependencies
- Standard library stack: `(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))`
- 2024 solutions use additional utilities like :iterate, :serapeum, :trivia
- Older solutions (2022-2023) use simpler stack: `(:fiveam :cl-ppcre)`

## Code Architecture

### File Organization
- Each day: `Day_XX/Day_XX.lisp` + `input.txt`
- Template files available in root and year directories
- Solutions are self-contained with embedded test cases

### Standard Solution Structure
```lisp
;;;; Prologue - package definition and dependencies
(defpackage :dayXX (:use #:cl :iterate) ...)
(in-package :dayXX)

;;;; Configuration
(setf 5a:*run-test-when-defined* t)
(declaim (optimize (debug 3)))

;;;; Data file path
(defparameter *data-file* "~/cl/AOC/YYYY/Day_XX/input.txt")

;;;; Problem description in block comments
#| --- Day XX: Problem Title --- |#

;;;; Example data for testing
(defparameter *example* '(...))

;;;; Solution functions with embedded tests
(defun dayXX-1 (input) ...)
(5a:test dayXX-1-test ...)

;;;; Final execution with timing
(time (format t "The answer to AOC YYYY Day XX Part X is ~a" ...))
```

### Common Patterns
- Use `uiop:read-file-lines` for file input
- Extensive use of `iterate` macro instead of `loop` in 2024 solutions
- Pattern matching with `trivia` library in newer solutions
- Utility functions from `serapeum` library
- FiveAM testing framework with tests defined inline

### Development Workflow
1. Copy from template.lisp
2. Replace ### placeholders with day number
3. Implement solution with embedded test cases
4. Use `(time ...)` for performance measurement
5. Solutions work with both example data and real input

### File Paths
- Development path: `~/cl/AOC/YYYY/Day_XX/`
- Working directory varies but typically matches day structure
- Input files consistently named `input.txt`

## Package Conventions
- Package name: `:dayXX` (lowercase with day number)
- Local nicknames: `:re` (cl-ppcre), `:5a` (fiveam), `:sr` (serapeum), `:tr` (trivia)
- 2024 solutions include `:iterate` in use clause
- Older solutions use more minimal package setup