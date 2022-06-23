# Ivy Syntax

The syntax for Ivy is very simple. 

Ivy is a interpreted functional programming language. Its main inspirations 
include Haskell, Python, and Go. These are the basic building blocks.

### Expressions
In Ivy, expressions produce values. There are four different kinds of 
expressions:

- Atoms: 
  - Basic structure:
    ```
    <atom>
    ```
  - Examples:
    - Integers: `5`, `12`
    - Floats: `3.1415`, `35.00`
    - Strings: `"ivy is so cool"`, `"333"`, `""`
    - Lists: `[]`, `[3, 5, 0]`
    - Booleans: `true`, `false`
- Prefix: 
  - Basic structure: `<operator> <expression>`
  - Examples:
    ```
    ivy> -32;
    -32

    ivy> ++10;
    11
    ```
- Infix:
  - Basic structure: 
    ```
    <expression> <operator> <expression>
    ``` 
  - Examples:
    ```
    ivy> 1500 + 2000;
    3500

    ivy> 3 + 3 * 6;
    21

    ivy> 3 >= 6;
    false
    ```
- If:
  - Basic structure: 
    ```
    if <condition> then <expression> else <expression>;
    ```
    In this case, a `<condition>` is an `<expression>` that produces a `true` 
    or `false` value only.
  - Examples:
    ```
    ivy> if false then "so true" else "nope";
    "nope"

    ivy> if 1 <= 8 then ++1;
    2
    ```

### Statements
In ivy, statements do not produce a values. For now, Ivy only has one type of
statement:

- Let:
  - Basic structure:
    ```
    let <symbol> = <expression>;
    ```
    Here, a `<symbol>` is a name that represents some data. An ivy `let` 
    statment binds a value (yielded by `<expression>`) to a `<symbol>`.

 - Examples:
    ```
    ivy> let pi = 3.14;
    None
    
    ivy> pi * 2;
    6.28 
    ```

### Functions
Since Ivy is a functional programming language, it adopts a more mathematical 
defintion for a function. In Ivy, a function maps an input to an output. 
Functions exist in a couple different variations:

- Anonymous functions:
  - Basic structure:
    ```
    fn(<params>) -> <expression>;
    ```
    Anonymous functions are functions that are not bound to a symbol. They do 
    not produce any values by just declaring them so for now, they are not too 
    useful on their own. However in the future, they will be very useful when 
    Ivy supports higher-order functions.
  - Examples:
    ```
    ivy> fn(x) -> x * x;
    None

    ivy> fn(a, b) -> a * a + b * b;
    None
    ```
- Bound functions:
  - Basic structure:
    ```
    let <symbol> = <anonymous-function>
    ```
    Bound functions bound an anonymous function to a symbol so that it can be 
    referenced elsewhere.
  - Examples:
    ```
    ivy> let square = fn(x) -> x * x;
    None

    ivy> square(8);
    64

    ivy> let hello = fn(name) -> "hello " + name;
    None

    ivy> hello("gerardo");
    "hello gerardo"
    ```

### Combining Expressions and Statements
I mentioned that statements do not produce any values. That was (sort of) a 
lie. Statements *do* produce values, it's just that the value will always be 
`None`. You probably noticed that whenever we define a statement or an 
anonymous function, the Ivy REPL returned `None`. 

This means that for functions and if expressions, you can also include a statement as an expression.