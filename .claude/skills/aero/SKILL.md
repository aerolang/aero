---
name: aero
description: Knowledge about the Aero programming language. Use when working with .aero files or on the Aero compiler itself.
---

# Aero

Aero is a programming language being designed and implemented in this repository. Aero source files
use the extension `.aero`.

Aero is very early in its development. There are no outside resources to consult. Most features are
not implemented, there's no libraries, etc. Much of this skill is talking about what the language
will be when finished.

If the user is asking you to write or read Aero code, don't tell them about the language syntax of
features unless requested.

## Overview

At a high level, Aero is a purely functional programming language, somewhat in the Lisp tradition.
Side effects in Aero are done through effects. Otherwise, by design, there's no direct mutation.

## Syntax

Aero syntax is fundamentally S-expression based. Though it has many syntactic conveniences too.

We call syntax elements terms. A term is the generalized tree element that the language works with.
Terms can be used in 4 distinct contexts:

- Definitions: where things like functions, types, modules, etc. are defined.
- Expressions: things that have values at runtime.
- Types: things that describe the type of expressions.
- Patterns: things that can be bind variables to expressions.

### Comments

Comments in Aero start with `;`. There are no block comments.

Single `;` comments are for regular non-documentation text. `:` comments are for documentation.
Docs are written as markdown.

Example:

```aero
: This is a comment that supports **Markdown** and would be followed by something to document.

; This is normal comment that isn't documentation.
```

### Literals

Literals are just direct representations of basic expressions.

```aero
; Integers
1 2 3 10 100_000 -5

; Floats
1.0 -3. 500_000.123_435

; Strings
"Hello, world!" "one line\ntwo line"

; Symbols
.these .are .super_cool .symbols
```

`()` represents the expression for the unit type `void`.

### Identifiers

Identifiers are the ways you can express something with a name, like a function, type, variable,
label, and so on.

Names are alphanumeric, but must start with a letter and can also include underscores between
alphanumeric characters. Names are conventionally always lowercase, including acronyms. camelCase,
snake_case, ALL_CAPS, etc. are not conventionally used.

Functions, contants, types, modules, effects, structs, etc. use bare names. They don't have any
leading prefix character to define them.

Variables are expressions bound to a name and start with a `$` prefix.

`_` is a magic identifier which acts as a wildcard discard,. A name with a leading `_` is a named
discard. 

Type variables begin with a `'`, they don't have a closing single quote.

A label is a special identifier which has a `:` postfix. They aren't expressions but serve for
syntactical ergonomics.

Symbols are essentially literal terms with a name and start with a `.`.

A path is used for identifiers which are under a parent.

Operators are formed with other characters (`?!@#$%^&*-+()[]{}|/\<>,:`) and are used in prefix,
infix, and limited postfix settings.

```aero
; `func` and `log` are macros which use unprefixed identifiers.
; `say_hello` is the name of the function we're defining.
; `str` and `void` are types.
; `$name` and `$message` are variables bound to values.
; `to:` and `do:` are labels.
; `->` is an operator.
(func say_hello to: $name str -> void do:
  $message = "Hello, \$name!"
  (log $message)
)
``` 

### Expressions

Expressions include literals, along with identifiers like functions, constants, variables, and
symbols. More complex expressions show up with function calls, groups, blocks, and infix
operators.

Top-level expressions include all valid kinds. Subexpressions are those which are representable
by only one term. For instance `1 + 2` works in a top-level expression, but not a subexpression.

Function calls use parentheses where the first term is the callee and the remaining terms are
arguments. There are no commas between any arguments, and infix operators can't be used inside
directly. Each argument is a subexpression. If the function definition has labels, they must also
be present at the callsite.

```aero
; Calling our `say_hello` function with one argument.
(say_hello to: "Bradley")
```

Groups are a way to introduce a top-level expression scope. They're made using square brackets.

```aero
(do_something_with_numbers
  100      
  200
  [4 + 10]  ; to do arthmetic here we need the group [ ]
)
```

Groups are also needed when working with operators of mixed precedence, like `1 - 10 + [2 * 3]`.

Blocks allow expressions to be assigned to variables, and the last expression in a block is the
expression returned by the block itself. Blocks are created with the `do` macro.

```aero
$c_squared =
  (do $a = 1
      $b = 3
      [$a * $a] + [$b * $b]
  )
```

Blocks are often used implicitly, like in the body of a function with conventionally will be after
a `do:` label.

The vertical pipe `|` is an infix operator that supports piping the expression to the left as an
argument to the right. By default it's the first argument unless the value is captured to a
variable.

```aero
(read_string_from_user)
| (parse_that_string limit: 100)
| (hash_to_int)
| $int_hash (log "Your result is \$int_hash")
```

## Basic Types

- `int` - integer of machine pointer size (32/64, usually 64)
- `uint` - unsigned integer of machine pointer size (32/64, usually 64)
- `int8` `int16` `int32` `int64` - sized integers
- `uint8` `uint16` `uint32` `uint64` - unsigned sized integers
- `float` - 64-bit floating point number that doesn't support Inf or NaN
- `float16` `float32` `float64` - IEEE 754 floats
- `str` - UTF-8 string
- `bytes` - a buffer of bytes
- `sym` - symbols
- `bool` - represented by the constants `true` and `false`
- `void` - unit type

## Collections

Tuples have the representation in their type and expression form:

- `{int int str}` - type for a tuple of two ints and a string
- `{3 4 "test"}` - a tuple of two ints and a string

Arrays are contiguous in memory and consist of elements of the same type:

- `array[int]` - type for an array of integers
- `#(array 1 2 3)` - array of integers
- `#(array[int] 1 2 3)` - array of inters with an explicit type
- `#(array)` - empty array

Lists are linked-lists:

- `list[int]` - type for a list of integers
- `#(list 1 2 3)` - a list of integers
- `#(list[int] 1 2 3)` - a list of integers with an explicit type
- `#(list)` - empty list

Dicts are key-value dictionaries:

- `dict[str int]` - type for a dictionary with key `str` and value `int`
- `#(dict "hello" => 5 "world!" => 6)` - dictionary with key `str` and value `int`
- `#(dict[str int] "hello" => 5 "world!" => 6)` - dictionary with key `str` and value `int` with an
                                                  explicit type
- `#(dict[str][int])` - empty dictionary

Dicts have a special syntax when the key is a symbol:

- `#(dict one: "1" two: "2")` - dictionary of type `dict[sym str]`
- `#(dict one: "1" $some_sym => "2")` - dictionary with a variable symbol key.

## String Escapes

- `\0` - null
- `\a` - bell, alert
- `\b` - backspace
- `\e` - esc
- `\f` - form feed
- `\n` - newline
- `\r` - carriage return
- `\t` - horizontal tab
- `\v` - vertical tab
- `\\` - backslash
- `\"` - double quote

## String Interpolation

```aero
; You can use \$var_name in the simple case.
"Hello \$name."

; You can use \[...] to evaluate a group.
"If my math is correct, you owe \[100 + 20] dollars"

; This is also useful for printing a var with a character immediately after it.
"The plural of \$noun is \[$noun]s"

; You can use \(...) to call a function.
"The square root of 16 is \(find-sqrt 16)!"
```

## Control Flow

### Conditionals

The `if` macro is used for conditional branching. You can pass multiple conditions. The first to
give `true` will run. Otherwise the code after the `else:` label will.

```aero
(if $a % 2 == 0             => (log "Whoa, an even number!")
    $a == 7                 => (log "Lucky number 7.")
    $a <= -100 || $a >= 100 => (log "Lots of digits!")
    else:                      (log "I'm not interested in this number.")
)
```

`||` and `&&` are the short circuiting logic operators 'or' and 'and'. Keep in mind that you can't
mix them in the same infix expression without grouping.

```aero
(if $a == 0 || [$b /= 0 && $b == $c] =>
      (log "either a is zero or b and c are the same non-zero number")
      (log "multiple expressions are allowed in `if` arms")
    else:
      (log "else")
)
```

The `else:` branch is optional, in that case, the expression evaluates to `void`.

```aero
(if $a == 0 =>
  (log "I just wanted to log a is zero and do nothing else")
)
```

This is preferable to an if/else with one condition and a `()` as a result of one branch.

To negate a condition, use `(not $condition)`.

### Matches

Aero supports pattern matching through the `match` macro.

```aero
$int = 3
(match $int
  3        => (log "three")
  5        => (log "five")
  -10 | 10 => (log "ten, either positive or negative")
  _        => (log "something else")
)
```

Matches must be exhaustive. The wildcard `_` will handle any branch not covered.

You can include tuples in your match to check multiple things at once:

```aero
(match {"test", 4}
  {"hello", _} =>
    (log "world")
  {"test", $n} if: $n % 2 == 0 =>
    (log "got test with an even n")
  {"test", $n} =>
    (log "got test with an odd n")
  _ =>
    (log "nothing matched")
    (log "sad")
)
```

Use `^$var` to match against a set variable instead of binding to it in a pattern.

```aero
$page_limit = 20
(match (get_page_size)
  0                       => (log "we got nothing")
  ^$page_limit            => (log "we hit our page limit exactly!")
  $n if: $n > $page_limit => (log "we went over our limit")
  $n                      => (log "we have \$n pages which is okay")
)
```

### Loops

Loops in Aero are different than in usual imperative languages because Aero doesn't allow mutation.

A loop is a reducer on an accumulator and returns the last value as its result.
The accumulator starts with a default value. Use `for:` to iterate through values, and `while:`
stop when a condition evaluates to false. They can both be used at the same time.

A loop in Aero keeps iterating on an accumulated value, and then returns that expression as its
result. The accul starts with a default. Use `for:` to iterate through values, and `while:` to stop when a
condition evaluates to false. They can both be used at the same time.

Ranges in Aero look like `1..10`. That is 1 to 10 inclusive. An infinite range looks like `1..`.

```aero
; Implement an integer pow using `for:`.
$base = 3
$exp = 4
$result = (loop $r = 1 for: 1..$exp do: $r * $base)
(log $result)  ; 81
```

A non-infinite loop without an accumulator implicitly has one which is void.

```aero
$v =
  (loop for: $i <- 1..100 do:
    $remark =
      (match $i
        7  => "a lucky number"
        13 => "a scary number"
        _  => "a regular number"
      )
    (log "\$i is \$remark")
  )

; $v has type void. So it's a bit useless.
```

Use `while:` to stop accumulating. The loop will evaluate to the last value of the accumulator
before the condition became false.

```aero
$i =
  (loop $i = 2 while: $i < 100 do:
    $i * $i
  )

(log $i)  ; 64
```

Using both `for:` and `while:` will make iterating through elements stop if the condition stops.

```aero
$biggest_square_under_1000 =
  (loop $s <- 1 for: $i <- 1.. while: $i * $i < 100 do:
    $i * $i
  )

(log $biggest_square_under_1000)  ; 81
```

An infinite loop just uses `loop`, it'll never return a value!

```aero
(loop do:
  (log "logging this forever and ever and ever")
)
```

An infinite loop can still have an accumulator, but the loop itself will never return a value.
Anything after the loop is dead code.

```aero
(loop $i = 0 do:
  $i = [$i + 1] % 100
  (log $i)  ; 0, 1, 2, ..., 99, 0, 1, 2, ...

  $i  ; Remember we need $i at the end to set the next accumulator value.
)
```

Accumulators support patterns, so it can be a tuple also.

```aero
; Take the max until a negative number happens.
$values = #(array 1 2 3 -10 4)

{_, $max} =
  (loop {$stop $max} = {false 0}
        for: $v <- $values
        while: (not $stop) do:
    (if $v < 0 =>
          {true, $max}
        $v > $max =>
          {false, $v}
        else:
          {false, $max}
    )
  )

(log $max)  ; 3

; NOTE: we could have simplified this to just include the stop condition directly in the while.
```

You can also have multiple `for:` clauses. The inner ones run for each iteration of the outer.

```aero
: print 1 once, 2 twice, etc.
(loop for: $i <- 1..10
      for: $j <- 1..$i do:
  (log $i)
)
```

### Assignment

In blocks variables can be assigned with `=`. But very importantly, variables can not be mutated.

If a variable is assigned that has the same name as an existing one, it shadows the other variable.
The new variable can even have a different type.

```aero
$a = 10
(log $a)  ; 10

$a = "test"
(log $a)  ; test
```

With an if or loop expression, assignments inside DO NOT mutate the outer variable.

```aero
; This code incorrectly makes an attempt to change $a.
$a = 10
(if $a < 11 =>
  $a = $a + 1
  (log $a)  ; 11
)
(log $a)  ; 10

; To accomplish the above correctly, you need to do it like this:
$a = 10
$a =
  (if $a < 11 =>
        $a = $a + 1
        (log $a) ; 11
        $a
      else:
        $a
  )
(log $a)  ; 11

; If you want to rebind a variable in the same scope using an `if`, you need use the result of an
; if expression with any cases covered.
```

It's important to remember that loop expressions are just reduce operations, they cannot mutate
variables.

```aero
$c = 10

(loop for: $a <- 1..5 do:
  ; This is different variable called $c!
  $c = $a + 1
  (log $c)
)

(log $c)  ; 10, it was NEVER mutated!
```

## Main

Aero programs don't support expressions at the top level of a source file. To have code run when
an Aero program is invoked, you use the `main` macro.

Below shows a simple hello world example:

```aero
(main do:
  (log "Hello, world!")
)
```

Mains implicitly return `void`.

## Functions

Functions in Aero are defined with the `func` macro. You can only use this macro at the definition
level, i.e., not in a place where expressions, types, or patterns are expected.

Function definitions must have their parameter and return types annotated. Parameters can be
positional or labeled. Labeled arguments must follow all positional arguments, and can be optional.

Parameter types follow the parameter. The return type is after a `->`.

Labels shouldn't be used all the time, they sense to use when an argument can't be inferred easily
by context alone at the callsite. Often, labels will be words like with, on, by, etc. A default
parameter value is indicated with a `=` followed by the value.

The body of the function follows the `do:`.

```aero
: Split a string on a delimiter (by default a space).
(func split_str $str str on: $delim str = " " -> array[str] do:
  ; ...
)
```
