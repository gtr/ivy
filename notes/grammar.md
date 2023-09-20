# parsley :herb:

Implementing the Pratt parsing algorithm in Rust. I decided to use the 
following BNF grammar which is also the proposed new syntax for my programming
language, [Ivy](https://github.com/gtr/ivy).

```html
<statement> ::= <expression> ';' ;

<expression> ::= <letExpr>      [ ]
               | <mutExpr>      [ ]
               | <fnExpr>       [x]
               | <ifExpr>       [x]
               | <pubExpr>      [x]
               | <dataExpr>     [x]
               | <structStmt>   [ ]
               | <packageStmt>  [x]
               | <importStmt>   [x]
               | <matchExpr>    [x]
               | <whileExpr>    [x]
               | <doExpr>       [x]
               | <returnExpr>   [x]
               | <traitStmt>    [ ]
               | <implExpr>     [ ]
               | <or>           [ ]
               | <tupleAny> ;   [x]

<letExpr>       ::= 'let'['mut']? [<symbol>|<tupleSymbols>] ['::'<typeFn>]?'='<expression>;
<mutExpr>       ::= 'mut' [ <symbol> | <access> ] '=' <expression> ;
<fnExpr>        ::= <fnAnon> | <fnSignature> | <fnDeclaration> ;
<ifExpr>        ::= 'if' <expression> 'then' <expression> [ 'else' <expression> ]? ;
<pubExpr>       ::= 'pub' [ <fnSignature> | <fnDeclaration> | <typeExpr> | <structStmt> ] ;
<dataExpr>      ::= 'data' <symbol> [ <dataGenerics> ]? '(' <dataVariants> ')' ;
<structStmt>    ::= <structAnon> | <structDeclaration> ;
<packageStmt>   ::= 'package' <symbol> ;
<importStmt>    ::= 'import' [ <string> | <tupleStrings> ] ;
<matchExpr>     ::= 'match' <or> 'with' '(' [ <matchBranch> ]* ')' ;
<whileExpr>     ::= 'while' <or> '{' [ <statement> ]* '}' ;
<doExpr>        ::= 'do' '{' [ <statement> ]* '}' ;
<returnExpr>    ::= 'return' <expression> ;

<!-- [x] Functions -->
<fnAnon>        ::= 'fn' <fnArgs> [ ':' <typeFn> ]? '=>' <expression> ;
<fnSignature>   ::= 'fn' <symbol> '::' <typeFn> ;
<fnDeclaration> ::= 'fn' <symbol> <fnArgs> [ ':' <typeFn> ]? '=>' <expression> ;

<!-- [ ] Function Arguments -->
<fnArgs>        ::= '(' [ <fnArgsTyped> [ ',' <fnArgsTyped> ]* ]? ')' ;
<fnArgsTyped>   ::= <symbol> [ ':' <typeFn> ] ? ;

<!-- [x] Data Type Branches -->
<dataGenerics>  ::= '<' <symbol> [',' <symbol> ]* '>' ; ;
<dataVariants>  ::= [ '|' ]? <dataItem> [ '|' <dataItem> ]* ;
<dataItem>      ::= <symbol> [ '::' <typeFn> ]? ;

<!-- [ ] Structs -->
<structAnon>        ::= 'struct'          '(' <structFields> ')' ;
<structDeclaration> ::= 'struct' <symbol> '(' <structFields> ')' ;
<structFields>      ::= <structField> [ ',' <structField> ]* [ ',' ]? ;
<structField>       ::= <symbol> '::' <typeFn> ;
    
<!-- [x] Match Branches -->
<matchBranch>   ::= '|' <expression> '->' <expression>

<!-- [x] Type Literals -->
<typeFn>    ::= <typeCmpst> [ '->' <typeCmpst> ]? ;
<typeCmpst> ::= <typeLst> | <symbol> '<' [ <typeLst> [ ',' <typeLst> ]* ] '>' ;
<typeLst>   ::= <typeTuple> | '[' <typeFn> ']' ;
<typeTuple> ::= <type> | '(' <typeFn> [ ',' <typeFn> ]* ')' ;
<type>      ::= [ 'mut' ]? <symbol>  ;

<!-- [x] Binary & Unary Expressions, Operator Precedence -->
<or>            ::= <and> [ '||' <and> ]* ;
<and>           ::= <equality> [ '&&' <equality> ]* ;
<equality>      ::= <comparison> [ ( '==' | '!=' ) <comparison> ]* ;
<comparison>    ::= <addition> [ ( '>' | '>=' | '<' | '<=' ) <addition> ]* ;
<addition>      ::= <mult> [ ( '+' | '-' | '++' )  <mult> ]* ;
<mult>          ::= <unary>  [ ( '*' | '/' ) <unary> ]* ;
<unary>         ::= ( '!' | '-' ) <callExpr> | <callExpr> ;

<!-- [ ] Call & Access Expressions -->
<callExpr>      ::= <accessAttr> | <accessAttr> <tuple> ;
<accessAttr>    ::= <accessIndx> [ '.' <callExpr> ]* ;
<accessIndx>    ::= <factor> [ '[' <or> ']' ]* ;

<!-- [x] Factors & tuples -->
<factor>    ::= '(' [ <or> ]? ')' 
              | <tuple> 
              | <listExpr> 
              | <atom> ;
<tuple>     ::= '(' <expression> [ ',' <expression> ]* ')' ;

<!-- [x] List Literals -->
<listExpr>      ::= <listSplit> | <listLiteral> ;
<listLiteral>   ::= '[' [ <listItems> ]? ']' ;
<listSplit>     ::= '[' <symbol> '|' <symbol> ']' ;
<listItems>     ::= <expression> [ ',' <expression> ]* ;

<!-- [ ] Traits and Implementations -->
<traitStmt> ::= 'trait' <symbol> '(' [ <fnExpr> ';' ]* ')' ;
<implExpr>  ::= 'impl' <symbol> 'for' <typeFn> '(' [ <fnExpr> ';' ]* ')' ;

<!-- [x] Tuples -->
<tupleAny>      ::= '(' <expression> [ ','  <expression> ]* ')' ;
<tupleSymbols>  ::= '(' <symbol> [ ',' <symbol> ]* [ ',' ]? ')' ;
<tupleStrings>  ::= '(' <string> [ ',' <string> ]* [ ',' ]? ')' ;
    
<!-- [x] Atoms -->
<atom>  ::= <integer>
          | <symbol> 
          | <string> ;
```






















```html


<!-- [ ] Binary & Unary Expressions, Operator Precedence -->
<or>            ::= <and> [ '||' <and> ]* ;
<and>           ::= <equality> [ '&&' <equality> ]* ;
<equality>      ::= <comparison> [ [ '==' | '!=' ] <comparison> ]* ;
<comparison>    ::= <addition> [ [ '>' | '>=' | '<' | '<=' ] <addition> ]* ;
<addition>      ::= <mult> [ [ '+' | '-']  <mult> ]* ;
<mult>          ::= <unary> [ [ '*' | '/' ] <unary> ]* ;
<unary>         ::= [ '!' | '-' ] <accessIndx> | <accessIndx> ;

<!-- [ ] Call & Access Expressions -->
<callExpr>      ::= <accessAttr> | <accessAttr> <tuple> ;
<accessAttr>    ::= <accessIndx> [ '.' <callExpr> ]* ;
<accessIndx>    ::= <factor> [ '[' <or> ']' ]* ;

<!-- Factors & tuples -->
<factor>    ::= '(' [ <or> ]? ')' 
              | <tuple> 
              | <listExpr> 
              | <atom> ;
<tuple>     ::= '(' <expression> [ ',' <expression> ]* ')' ;

<!-- [x] List Literals -->
<listLiteral>   ::= '[' [ <listItems> ]? ']' ;
<listSplit>     ::= '[' <symbol> '|' <symbol> ']' ;
<listItems>     ::= <expression> [ ',' <expression> ]* ;

```













































