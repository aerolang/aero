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

A single `;` begins a regular, non-documentation comment. A bare `:` at the start of a line begins
a documentation comment. Doc comments support Markdown and should precede whatever they document.

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

Functions, constants, types, modules, effects, structs, etc. use bare names. They don't have any
leading prefix character to define them.

Variables are expressions bound to a name and start with a `$` prefix.

`_` is a magic identifier which acts as a wildcard discard. A name with a leading `_` is a named
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

### Paths

A path can reference definitions which are inside of others. This could be inside a module, a type,
a protocol, or an effect. The forward slash `/` is used for this.

A leading `/` goes into the root namespace. Referencing other packages requires the use of the root
namespace, as Aero doesn't allow identifiers to be introduced other than syntax, local definitions,
or from `use`.

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

The vertical pipe `|` is syntactic sugar that supports piping the expression to the left as an
argument to the right. By default it's the first argument unless the value is placed with `$$`.
Use `(some_func $arg1 $arg2 | another_function $arg1 | ...)` when the first thing is also a
function call. Use `($ $var | some_func $arg1 $arg2 | ...)` if there's no initial function call.

```aero
(read_string_from_user 123
 | parse_that_string limit: 100
 | hash_to_int
 | log "Your result is \$$"
)

($ "hello" | do_things_to_str | write_that_str_to "some-file.txt")
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
- `(#array 1 2 3)` - array of integers
- `(#array[int] 1 2 3)` - array of integers with an explicit type
- `(#array)` - empty array

Lists are linked-lists:

- `list[int]` - type for a list of integers
- `(#list 1 2 3)` - a list of integers
- `(#list[int] 1 2 3)` - a list of integers with an explicit type
- `(#list)` - empty list

Dicts are key-value dictionaries:

- `dict[str int]` - type for a dictionary with key `str` and value `int`
- `(#dict "hello" => 5 "world!" => 6)` - dictionary with key `str` and value `int`
- `(#dict[str int] "hello" => 5 "world!" => 6)` - dictionary with key `str` and value `int` with an
                                                  explicit type
- `(#dict)` - empty dictionary

Dicts have a special syntax when the key is a symbol:

- `(#dict one: "1" two: "2")` - dictionary of type `dict[sym str]`
- `(#dict one: "1" $some_sym => "2")` - dictionary with a variable symbol key.

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
"The square root of 16 is \(find_sqrt 16)!"
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

Matches must be exhaustive. The wildcard discard `_` will handle any branch not covered.

You can include tuples in your match to check multiple things at once:

```aero
(match {"test" 4}
  {"hello" _n} =>
    (log "world")
  {"test" $n} if: $n % 2 == 0 =>
    (log "got test with an even n")
  {"test" $n} =>
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

`match` supports being piped into as well:

```aero
; This is equivalent, though in this particular case it doesn't read as well.
$page_limit = 20
(get_page_size
 | match
    0                       => "we got nothing"
    ^$page_limit            => "we hit our page limit exactly!"
    $n if: $n > $page_limit => "we went over our limit"
    $n                      => "we have \$n pages which is okay"
 | log
)
```

### Loops

Loops in Aero are different than in usual imperative languages because Aero doesn't allow mutation.

A loop is a reducer on an accumulator and returns the last value as its result.
The accumulator starts with a default value. Use `for:` to iterate through values, and `while:` to
stop when a condition evaluates to false. They can both be used at the same time.

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
  (loop $s = 1 for: $i <- 1.. while: $i * $i < 100 do:
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
$values = (#array 1 2 3 -10 4)

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
; Because the while is after the for it is evaluated after consuming the next value. We can
; place it before also.
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

## Aliasing

`use` is a way to alias a definition from elsewhere.

```
(use /std/something)

(main do:
  ; We can reference something without it's full path now.
  (something 1 2 3)
)
```

We can also use multiple things at the same time. We can also change names.

```aero
; A normal use.
(use /std/something)

; Change the name of an alias.
(use /std/something_else as: different_name)

; Alias `one` from `/std/something/one` and alias `/std/something/two` as `not_two`.
(use /std/something/[one two as: not_two])
```

Aliases can't nest. You also can't alias a parent and child at the same time for each level. You'll
have to do it twice. Aliases must also always start from the root namespace.

An alias will only work after the `use` in its scope. You can `use` at the top level, and also
inside expression blocks.

## Variants

Variants are functional programming sum types in Aero. Variants are structurally typed. The syntax
for variant expressions appears as a function call on a symbol.

The `type` macro can define a type as an alias. This is convenient for variants. Each variant value
is separated by a `|` to form a type union. When using the `type` macro, a leading `|` is optional
but useful for defining it on multiple lines.

```aero
(type color = 
  | .red
  | .blue
  | .hex 
)

(type mood = .happy | .sad)
```

In the above, we can express colors like `(.red)` or `(.hex "#050505")`.

When using variant types anonymously inline, it uses the form `(.case_one | .case_two 'type)`.

Two postfix macros exist for convenience: `?` and `!`. `?` is the "optional" macro. `?'t` is
shorthand for `(.some 't | .none)`. If `'t` is defined with square brackets, it flattens out.
Similarly, `!'t`, the "fallible" macro, is shorthard for `(.ok 't | .err err)`. `err` is a protocol
for errors. More on that in the protocol section. `!` does allow a specific error type to be used
with the square bracket syntax as well using `!['t or: 'err]` which will give
`(.ok 't | .err 'err)`. `'err` must implement `err`.

`str` is the simplest implementor of `err`.

Examples of `?` and `!`:

```aero
int?         ; (.some int | .none)        : (.some 1) (.none)
[int str]?   ; (.some int str | .none)    : (.some 1 "hello")
{int str}?   ; (.some {int str} | .none)  : (.some {1 "hello"})
(list int)?  ; (.some (list int) | .none) : (.some (#list 1))

int!         ; (.ok int | .err err)        : (.ok 1) (.err "hello")
[int str]!   ; (.ok int str | .err err)    : (.ok 1 "hello")
{int str}!   ; (.ok {int str} | .err err)  : (.ok {1 "hello"})
(list int)!  ; (.ok (list int) | .err err) : (.ok (#list 1))

[int or: str]!      ; (.ok int | .err str)     : (.some 1) (.err "it failed")
[int str or: str]!  ; (.ok int str | .err str) : (.some 1 "hello") (.err "404")
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

Labels shouldn't be used all the time, they make sense to use when an argument can't be inferred
easily by context alone at the callsite. Often, labels will be words like with, on, by, etc. A
default parameter value is indicated with a `=` followed by the value.

The body of the function follows the `do:`.

```aero
: Split a string on a delimiter (by default a space).
(func split_str $str str on: $delim str = " " -> array[str] do:
  ; ...
)
```

### Lambda Functions

Lambdas functions in Aero start with a backslash. Below is one with explicit types:

```aero
$add = \($a int $b int -> int do: $a + $b)
```

More often, you'll see lambdas passed directly to another function and their types are inferred.

The lambda syntax is also used when typing functions, though notably there are no parameter names.
Labels are allowed (if they help), and appear as a required part of the signature.

```aero
: A contrived function that requires a lambda.
(func say_hello $name str $greeter \(str to: str -> void) -> void do:
  $message = "Hello"
  ($greeter $message to: $name)
)

(main do:
  (say_hello
    "Bradley"
    \($msg to: $name -> do: (log "\$msg, \$name"))
  )
)
```

A function which takes no arguments would look like:

```aero
\(-> do: (log "test"))
```

Note that the arrow is always required, even if there's no type listed.

Lambda functions are implied in pipelines if there's a `->`.

```aero
(take_something
 | do_something_else 1 2 3
 | $result -> do:
    (log "we're doing something with that result")
    10
 | do_something_with_an_int
)
```

## Structs

Structs in Aero are nominatively typed and are used to define custom types.

```aero
: A basic coordinate struct.
(struct coord
  ( x: int
    y: int
    z: int
  )
)
```

To create a data with a struct, you use the 'construct' syntax `(#struct_name [...])`. For `coord`,
you create it like `(#coord x: 1 y: 2 z: 3)`.

Structs can have positional and named fields. Though usually, you'd stick to one or the other.
With mixed syntax, the named fields must come after any positional.

Field access from a struct uses `.`. For named fields you use just the field name. For positional
fields, use the 0-based index.

Fields don't have any kind of visibility markers. They're accessible to anyone with the struct
value.

```aero
: A basic coordinate struct but with positional syntax.
(struct coord_pos
  ( int
    int
    int
  )
)

: A coordinate that spans universes.
(struct coord_multiverse
  ( int
    int
    int
    universe_number: int
  )
)

(main do:
  $c1 = (#coord_pos 4 5 6)
  $c2 = (#coord_multiverse 4 5 6 universe_number: 42)

  ; Field access uses `.`, positional ones use their 0-based index.
  $next_universe = $c2.universe_number + 1
  $manhattan_distance = $c1.0 + $c1.1 + $c1.2
)
```

### Pattern Matching

Structs support pattern matching to access fields. Matches can be open or closed. An open match
requires `...` to indicate that not all fields are being matched. A closed match must cover all
fields. Fields can be matched with wildcard and named discards.

`...` must be used before fields if any positional ones are ignored with it. A trailing `...` can
be combined with it only if there are more named fields as well.

```aero
(match $c
  (#coord x: $x y: 0 z: 0) => (log "only x is non-zero")
  (#coord y: $y ...)       => (log "here's y in case you were wondering: \$y")
)
```

```aero
(match $c
  (#coord_pos 0 0 0)                 => (log "all zeros")
  (#coord_pos 0 ...)                 => (log "at least starts with zero")
  (#coord_pos ... 0)                 => (log "ends with zero")
  (#coord_pos $x $y _z) if: $x == $y => (log "x and y are both \$x")
  (#coord_pos _ _ z) if: $z > 0      => (log "z is positive")
  (#coord_pos ...)                   => (log "something else")
)
```

The struct name can be inferred with `#_` as well. This helps when the struct is obvious and
the usage is verbose in context.

```aero
$c = (#coord_multiverse 1 2 3 universe_number: 100)

(match $c
  ; Note the leading `...` because we have position fields to skip!
  (#_ ... universe_number: $n) if: $n == 42 =>
    (log "we're in our universe!")

  ; A leading `...` is only used to skip all preceding positional fields.
  ; A trailing `...` when combined with a leading `...` can only mean to skip to skip remaining
  ; named fields.
  ;
  ; The fact that this is a bit confusing is generally why we don't mix positional and named fields
  ; in structs often.
  (#_ ... 0 ...) =>
    (log "our z is zero!")

  (#_ $x $y $z ...) =>
    (log "our location is (\$x, \$y, \$z)")
)
```

Naturally, these patterns can nest as well. Below you can see how we can bind `$cars` inside the
garage struct pattern inside the house struct pattern.

```aero
(struct house (address: str garage: garage?))
(struct garage (car_capacity: uint))

(main do:
  $h =
    (#house
      address: "1 Main St"
      garage: (.some (#garage car_capacity: 2))
    )

  (match $h
    (#house garage: (.none) ...) =>
      (log "this house has no garage")
    (#house garage: (.some (#_ car_capacity: $cars)) ...) =>
      (log "this house can fit \$cars!")
  )
)
```

### Open Structs

Structs can be defined as 'open' or closed'. An open struct allows more fields to be added without
it being considered a breaking change to consuming packages. Closed ones can't have fields added
without it being breaking. The biggest consequence of this is in how you pattern match on structs.
When consuming an open struct from another package, you always need to include a `...` at the end
to indicate that there may be more named fields in the future.

```aero
; Defined in /some/library
(pub struct person
  ( name: str
    age: uint
    ...  ; They can add more named fields when they want to!
  )
)

; Our program
(main do:
  $person = (#/some/library/person name: "johnny" age: 31)

  (match $person
    (#_ name: $name age: $age ...) if: $age >= 18 =>
      (log "'\$name' is old enough to be here.")
    (#_ name: $name ...) =>
      (log "Whoa, '\$name' is too young!")
  )

  ; Excluding `...` in the matches above is a type error!
  ; Because `person` in the other package could be changed at some point so we can't exhaustively
  ; reference all fields.
)
```

Now, say `/some/library` makes a backwards-compatible change to add a new field to `person`. To do
so, they need to give it a default value.

```aero
; Defined in /some/library
(pub struct person
  ( name: str
    age: uint
    favorite_color: (.red | .blue | .green | ...) = (.blue)
    ...
  )
)
```

Our program won't break because it was forced to use `...` and the default value is handled.

They could also add more favorite colors options too!

### Updating Fields

Fields in structs can be updated using a special form of the construct syntax. Use `...`
immediately followed by the struct value and any new fields. Notably, this just returns a new copy
of the struct with updated fields, always remember that the original wasn't mutated!

```aero
$c = (#coord x: 1 y: 1 z: 1)
$c = (#coord ...$c z: 10)

; We redefined $c with z as 10.

$c = (#coord_pos 1 1 1)
$d = (#coord_pos ...$c 2: 10)

; The last int, $d.2 is now updated. $c remains the same.
```

Like in patterns, we can use `#_` because the struct can always be inferred on update.

```aero
$c = (#coord x: 1 y: 1 z: 1)
$c = (#_ ...$c z: 100)
```

### Struct Functions

Functions can be contained within structs. These are not methods like in object-oriented
programming, but are for convenience for functions with operate on the struct.

```aero
(struct company
  ( name: str
    owner: company?
  )

  (pub func new $name str -> company do:
    (#company name: $name)
  )

  (pub func set_owner $c company $o company -> company do:
    (log "Welcoming \[$c.name] to \[$o.name]!")

    (#_ ...$c owner: (.some $o))
  )
)

(main do:
  $linkedin =
    (company/new "LinkedIn"
     | company/set_owner (company/new "Microsoft"))

  (log "LinkedIn is owned by \[$linkedin.owner?.name ?? "no one"].")
)
```

We use the visibility modifier macro `pub` to allow the function to be seen from outside.

### Conditional Field Access

In the last example, owner was an optional struct. We can use the conditional field access
operators `?.` and `!.` to access a property from an optional or fallible struct value.

These can chain and result in flattening any `(.none)` or `(.err [...])` values they encounter. If
the last property access itself is optional of fallible, it will not flatten that value too! This
must be opted in with a trailing `?` or `!`.

```aero
$third_owner_name = $company.owner?.owner?.owner?.name  ; str?
$third_owner = $company.owner?.owner?.owner?            ; company?

; If `$third_owner` was `$company.owner?.owner?.owner` it's type would be `company??`.
```

Commonly used with this are the optional and fallible default operators `??` and `!!`. These are
short-circuiting operators that can give a value to an optional or fallible value. They can also
be chained.

```aero
$code = $response!.status_code !! (get_status_code_another_way) !! 500
$name = $person_with_optional_name.name ?? "anonymous"
```

You can access the `.err` value when using `!!` with `$!`.

```aero
$message = $response!.text !! "(request error: \$!)"
```

### Opaque Structs

By default, fields in structs are public. There's no way to hide an individual field in a struct
from other code. Instead, it's all-or-nothing. Opaque structs allow hiding all fields and locking
down access to only functions contained in the struct. Structs are made opaque with the `opaque`
marker protocol.

```aero
(struct counter is: opaque
  ( value: int
  )

  (pub func new -> counter do:
    (#counter value: 0)
  )

  (pub func value $c counter -> int do:
    $c.value
  )

  (pub func incr $c counter -> counter do:
    (add_value $c 1)
  )

  (pub func decr $c counter -> counter do:
    (add_value $c -1)
  )

  (func add_value $c counter $i int -> counter do:
    (#_ ...$c value: $c.value + $i)
  )
)

(main do:
  ; We can't use `(#counter value: 0)` directly because it's opaque.
  ; We also can't access the value field. We're forced to use its public API.
  $c = (counter/new)
  $c = ($ $c | counter/incr | counter/incr | counter/incr | counter/decr)

  (log "The counter value is now \(counter/value $c)")  ; the value is 2.
)
```

Many times, you may want some fields exposed in a struct and some others not. In this case, it's
common to pack private information into an opaque struct and place that in a regular struct.

### Generic Structs

Structs support type variables to allow for generalizing it for different things. Aero type
variables look like `'t` or `'something`. They are not forward declared as they are in many
languages, since they already occupy a unique prefixed namespace. Also importantly, type variable
names can't be referenced at a function callsite or when referencing the type.

When a type variable is only the type of an output of a function, and is not present in the typing
of the parameters, it cannot be inferred from usage. This always requires the caller to specify the
type. This must be done with a label in the definition and at the callsite.

The type variable names when defining a struct are only consistent between where the type is
declared and the types of the fields. It does not extend to the functions. They introduce their own
type variables. Conventionally though, you'd want the type variable names to be the same. You would
introduce other ones when you would have different instances of the same struct to have different
types.

Here's an example of these concepts:

```aero
(struct cache['t] is: opaque
  ( value: 't?
  )

  (pub func new of: 't -> cache['t] do:
    (#cache (.none))
  )

  (pub func get $c cache['t] -> 't? do:
    $c.value
  )

  (pub func set $c cache['t] to: $value 't -> cache['t] do:
    (#_ ...$c value: (.some $value))
  )

  (pub func reset $c cache['t] -> cache['t] do:
    (#_ ...$c value: (.none))
  )
)

(main do:
  $cache = (cache/new of: str | cache/set to: "init")

  (log "Cache value is currently '\(cache/get $cache)'")
)
```

## Named Tuples

Similarly to how pattern matching and positional and named fields work with structs, we can do the
same with tuples.

```aero
$values = {1 2 3 hello: "world" a: 1}  ; the type is `{int int int hello: str a: int}`

(match $values
  {0 0 0 a: 0 ...}    => (log "all zero")
  {... hello: $s ...} => (log $s)
)
```

Again, usually tuples will be all positional, or have all named fields, not usually both.

## Protocols

Protocols are like interfaces or traits in other languages.

TODO

## Marker Protocols

Some protocols are 'markers', meaning they don't have anything to implement, but mark a struct in
some way.

TODO

## Derived Protocols

TODO
