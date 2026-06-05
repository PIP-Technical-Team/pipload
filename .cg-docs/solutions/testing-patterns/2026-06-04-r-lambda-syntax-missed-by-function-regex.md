---
date: 2026-06-04
title: "R lambda syntax `\\()` missed by `<- function` regex audits"
category: "testing-patterns"
language: "R"
tags: [R, lambda, function-definition, regex, audit, grep, code-review]
root-cause: "R supports two function-definition syntaxes; grep for `<- function` silently skips `\\()` lambdas"
severity: "P3"
---

# R lambda syntax `\()` missed by `<- function` regex audits

## Problem

When auditing R source files for all exported or public function definitions
(e.g. to check that every function has a `verbose` argument), a grep pattern
like:

```regex
<- function
```

silently misses lambdas defined with the shorthand syntax introduced in R 4.1:

```r
load_gmd_valid_inv <- \() { ... }
load_gmd_valid_log <- \(x, verbose = TRUE) { ... }
```

The result is an incomplete audit — functions appear to have been checked
when they were not.

## Root Cause

R 4.1+ introduced `\(args) body` as a shorthand for `function(args) body`.
Both forms are semantically identical and both appear in production code.
A regex that only matches `function(` will never match `\(`.

## Solution

Use an alternation pattern that covers both syntaxes when auditing:

```regex
(<- function)|(<- \\()
```

In `grep_search` (VS Code tool):

```
query: "(<- function)|(<- \\()"
isRegexp: true
```

In `grep` / `rg` on the command line:

```bash
rg "(<- function)|(<- \\()" R/
```

## Prevention

- **Always use both patterns** when writing grep queries that enumerate
  function definitions in an R codebase.
- When reading a file to audit its public API, prefer reading the whole file
  (or at least scanning to EOF) rather than relying on a regex to enumerate
  functions — lambda definitions often appear at the bottom of a file after
  the primary `function`-style definitions.
- If writing a script to extract all exported functions from an R package,
  parse `NAMESPACE` (which lists every export regardless of syntax) and then
  locate definitions in source files.

## Related

- No direct existing solution cross-references.
