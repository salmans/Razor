Razor's Input Syntax
====================
`
theory      ::= sequent*
sequent     ::=  body “=>” head
body        ::= conjunctive
head        ::= [existential "|"]* existential
existential ::= [["exists" variable]+ "."] {0|1} conjunctive
conjunctive ::= [atom “&”]* atom
atom        ::= identifier terms {0|1}| "Truth" | "Falsehood"
terms       ::= "(" term* ")"
term        ::= identifier terms | variable | constant
constant    ::= "'" identifier
variable    ::= identifier
identifier  ::= [_a-zA-Z][_a-zA-Z0-9]
`