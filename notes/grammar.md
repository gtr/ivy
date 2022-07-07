# Ivy Grammar

Here is the full grammar for the Ivy programming language:

```html
<statement> ::= <expression> ';' ;

<expresion> ::= <letExpr> 
              | <funcExpr>
              | <ifExpr>
              | <matchExpr>
              | <tupleExpr>
              | <enumExpr>
              | <or> ;

<letExpr>   ::= 'let' [ <symbol> | <tuple> ] '=' <expression> ;
<funcExpr>  ::= 'fn' <tuple> '->' <expression> ;
<ifExpr>    ::= 'if' '(' <or> ')' 'then' <expression> [ 'else' <expression> ]? ;
<matchExpr> ::= 'match' <or> '(' <matchBranches> ')' ;
<tupleExpr> ::= 'tuple' <tuple> ;
<enumExpr>  ::= 'enum' '(' <enumPipes> ')' ;

<matchBranches> ::= '|' <expresion> '->' <expresion> [ <matchBranches> ]* ;
<enumPipes>     ::= [ '|' ]? <symbol> [ '|' <symbol> ]* ;
<tuple>         ::= '(' [ <commas> ]? ')' ;
<commas>        ::= <atom> [ ',' <atom> ]* ;

<or>            ::= <and> [ '||' <and> ]* ;
<and>           ::= <equality> [ '&&' <equality> ]* ;
<equality>      ::= <comparison> [ [ '==' | '!=' ] <comparison> ]* ;
<comparison>    ::= <addition> [ [ '>' | '>=' | '<' | '<=' ] <addition> ]* ;
<addition>      ::= <mult> [ ( '+' | '-' ) <mult> ]* ;
<mult>          ::= <unary> [ ( '*' | '/' ) <unary> ]* ;
<unary>         ::= [ '!' | '-' | '++' | '--' ] <call> | <call> ;
<call>          ::= <factor> <tuple> | <factor> ;
<factor>        ::= '(' <or> ')' | <atom> ;

<atom> ::= <integer>
         | <symbol> 
         | <string>
         | <boolean> ;

``` 
