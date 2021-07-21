
# Slyce

## Overview of `.slf` & `.sgf` files

There are 2 components to a parser, the `lexer` and the `grammar`.  
The `lexer` is in charge of breaking a raw text file up into keywords, or `tokens`.  
Then the `grammar` takes those tokens, and builds them into a tree structure in a way you specify.  

### Tokens (terminals)

In slyce, there are 2 types of tokens: `terminal` and `raw-terminal`.  
(More on this later, but here's a brief overview)
- `terminal's`
  - Yielded from the lexer by giving them a name (like `variable` or `int`)
  - Referenced in the grammar by using that name
  - Saved in `Tok.variable` or `Tok.int`
- `raw-terminal's`
  - Yielded from the lexer using the `@` symbol, which means yield whatever the text is
  - Referenced in the grammar using quoted raw text (like `"\n"` or `"="`)
  - Saved in `Tok.'\n'` or `Tok.'='` ( ' means ` for formatting purposes )

### Grammar (nonTerminals)

In slyce, the grammar consists of nonTerminals,
and there are several different types in order to make your life as simple as possible.  
These different types are then expanded into the more standard grammar definition you might be familiar with.

#### Standard NonTerminal

- The normal sort of NonTerminal you might already be familiar with
- Uses the `:` symbol
- Some basic examples:
  ```
  KeyWord    : "(" "keyword-1" ")" | "keyword-2" ;
  KWListHead : KeyWord KWListTail
             |
             ;
  KWListTail : "," KeyWord KWListTail
             |
             ;
  Expr1      : Expr1 addOp Expr2 // left assoc
             | Expr2
             ;
  Expr2      : Expr2 multOp Expr3 // left assoc
             | Expr3
             ;
  Expr3      : Expr4 powOp Expr3 // right assoc
             | Expr4
             ;
  Expr4      : int
             | float
             | variable
             | "(" Expr1 ")"
  ```
- That's a lot of writing...

#### Lift NonTerminal

- This helper doesn't really cut down on your `.sgf` files, but does make pattern matching the resulting
  `AST` `(Abstract Syntax Tree)` less verbose.
- Uses the `^` symbol
- 
  ```
  KeyWord    ^ "(" ^"keyword-1" ")" | "keyword-2" ;
  ```
- Things to notice:
  - We replaced the `:` with a `^`
  - We needed to add a `^` next to `"keyword-1"`
  - We didn't need to add one next to `"keyword-2"`
- The essential principle of a `Lift NonTerminal` is that in every production,
  there is exactly 1 piece of useful material (I don't care about those parens)
- We needed to select which element to lift for the first production
- We didn't for the second, because it was obvious, it only has one element
- So, instead of pattern matching on something like:
  ```scala
  sealed trait KeyWord
  object KeyWord {
    final case class _1(_0: Tok.`(`, _1: Tok.`keyword-1`, _2: Tok.`)`) extends KeyWord
    final case class _2(_0: Tok.`keyword-2`) extends KeyWord
  }
  ```
  I can call `(??? : KeyWord).lift`  
  And pattern match on something like this instead:
  ```scala
  object Tok {
    final case class `keyword-1`(text: String, span: Span.Highlight) extends KeyWord.LiftType
    final case class `keyword-2`(text: String, span: Span.Highlight) extends KeyWord.LiftType
  }
  object KeyWord {
    sealed trait LiftType
  }
  ```

#### List NonTerminal

- Inspired by regular expression `*` and `+`
- Can be defined both as a named NonTerminal, or an anonymous list
- 
  ```
  KWList + KeyWord . "," ^KeyWord
  // Or as an anonymous list
  KWList : (KeyWord . "," ^KeyWord)+
  KWList2 : you CouldAlso* add+ (KeyWord . "," ^KeyWord)+ OtherStuff hereToo
  ```
- The `.` represents a different first element compared to elements that repeat
- The most common usage of this would be adding something like a `","` between elements
- You could also do something like this to require a `";"` after each element.
- 
  ```
  KWList + ^KeyWord ";"
  ```
- Anywhere where you see a `+`, a `*` could be used instead, which allows for no elements
  (`+` requires at least 1 element)
- `*` lists provide a `.toList` helper
- `+` lists provide a `.toNonEmptyList` helper

#### Assoc NonTerminal

- Here is where things get really fun...
- With an `Assoc NonTerminal`, we use the `~` symbol
- 
  ```
  Expr ~ >powOp
       | <multOp
       | <addOp
       ^ int
       | float
       | variable
       | "(" ^Expr ")"
  ```
- This defines an operator precedence, as well as defining associativity
- Everything after the `:` or `^` is the `base`
- This expands to exactly what the previous example said, except we get a nice `.toExpr` method

## `.slf` & `.sgf` file in depth

- [`.slf` files](https://github.com/Kalin-Rudnicki/slyce-fp/wiki/Slf-Files)
- [`.sgf` files](https://github.com/Kalin-Rudnicki/slyce-fp/wiki/Sgf-Files)
