# AGENTS.md

## Build/Lint/Test Commands
- Run single test: `(5a:run! 'test-name)` or `(5a:run-all-tests)`
- Execute solution: `sbcl --load dayXX.lisp` 
- Interactive REPL: `sbcl` then `(load \"dayXX.lisp\")`

## Code Style Guidelines
- Use Common Lisp with Quicklisp dependencies
- Package naming: `defpackage :dayXX` with `:iterate`, `:fiveam`, `:cl-ppcre`
- 2024 solutions use `:serapeum`, `:trivia`, and `:str` libraries
- Input reading: `(uiop:read-file-lines *data-file*)`
- Tests run automatically on load via `(setf 5a:*run-test-when-defined* t)`
- Use `iter` macro from iterate library (2024) or `loop` macro (older)
- All solutions include inline FiveAM tests with example data
- File paths: `~/cl/AOC/YYYY/dayXX/input.txt`
