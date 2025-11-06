Algebraic Programming System
============================

This is alternate implementation of the Algebraic Programming System described in the paper Algebraic Programming System APS v.3 (A.A. Letichevsky, A.A. Letichevsky Jr., V.S. Peschanenko ).

## How to build and run

To build
```
dotnet build
```

to run
```
dotnet run --project aps
```

to test
```
dotnet test
```

## Grammar

I have to change be explicit about the grammar rules for `prefix expression`, `application`, `infix expression`, `primary expression` otherwise expression become ambiguos. I specify priority of parsing for these three expression type.

```ebnf
<initial assignment> ::= <name> := <algebraic expression>;
<algebraic expression>::=<primary expression>|<prefix expression>
|<application>|<infix expression>
<primary expression>::=<integer or rational number> | <string>
| <empty object> | <name> | <atom> | VAL <name>
|(<algebraic expression>)
<empty object>::= ()
<prefix expression>::=<mark symbol>(<sequence of algebraic
expressions separated by ",")
<application>::=<prefix expression> <algebraic expression>
<infix expression>::=<application><infix notation>
<algebraic expression>
```

Array access not described seems to be. Have to look.

# References
- https://apsystems.org.ua/uploads/doc/aps/APSUM.eng.pdf
- https://apsystems.org.ua/uploads/doc/aps/APSv3.eng.pdf
- https://apsystems.org.ua/uploads/doc/aps/APSUM.rus.pdf
