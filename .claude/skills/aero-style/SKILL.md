---
name: aero-style
description: Style guidance about the Aero programming language. Use after writing or editing an .aero file
---

# Aero Style

## S0001

If making a binary if/else, and the condition branch just returns void, flip the condition and
remove the else branch.

```
; DON'T
(if $condition => () else: (log "..."))

; DO
(if (not $condition) => (log "..."))

; DON'T
(if $a > $b => () else: (log "..."))

; DO
(if $a <= $b => (log "..."))
```

## S0002

When piping things to another function, don't use `$` when the first term is a call.

```
; DON'T
($ (some_function $arg1) | another_function)

; DO
(some_function $arg1 | another_function)
```
