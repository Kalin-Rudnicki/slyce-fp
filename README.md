# Slyce

A flex/bison inspired parser generator for Scala.

The current easiest way to use slyce is to download the latest release on github, and run:  
`java -jar slyce-generate-x.x.x.jar generate all --help`  
`java -jar slyce-generate-x.x.x.jar generate all -i slyce-dir -o scala-dir`  
I also recommend running: `java -jar slyce-generate-x.x.x.jar --help --`
which will show you the help page explaining klib's `Executable` Logger config (`-l` is quite useful)

I am currently working on trying to publish the sources, but for now, if you would like to add `slyce-parse` as a
dependency, you will need to clone the project, and `sbt "project slyce-parse" publishLocal`.  
(You will actually need to do that with
[klib](https://github.com/Kalin-Rudnicki/klib) (my personal fp/utils library) as well).

## Usage

1) Create a directory for your `slyce` files
    - Recommended: `project-root/src/main/slyce`
2) Create a matching `basename`.(`slf`/`sgf`) file pair
    - Example: `slyce-root/pkgA/pkgB/example.slf` & `slyce-root/pkgA/pkgB/example.sgf`
3) Define your tokenizer in `___.slf`
4) Define your grammar in `___.sgf`
5) `java -jar slyce-generate-x.x.x.jar generate all -i slyce-dir -o scala-dir`
6) `libraryDependencies += "kalin-rudnicki" %% "slyce-parse" % "x.x.x"`
7)
   ```scala
   import klib.Implicits._
   import klib.fp.types._
   import pkgA.pkgB.example._
   
   for {
     source <- Source.fromFile(java.io.File)
     // Tokenize
     tokens = parser.tokenize(source)
     markedSource = parser.markTokens(source)
     // Build Tree
     ntRoot = tokens.flatMap(parser.buildTree(source, _))
     ntRoot2 = parser.parse(source) // much easier
     ntRoot3 = parser.parseAndMarkErrors(source)
   } yield ???
   ```

### Background Information

The way a parser works is in 2 steps.  
1) Break an input string into meaningful keywords (tokens) based on a lexer  
2) Combine those keywords into a tree structure based on a grammar

Ex:
- The lexer says what the tokens `variable`, `int`, and `"="` look like
- The grammar says `variable "=" int` is a valid ordering of these tokens

In general, it is recommended to have most of the logic in the grammar,
but sylce also supports some fun features in the lexer to make your life easier
when trying to parse whatever it is you are trying to parse (more on this later).

Ultimately, what slyce does, is take a raw text file, and convert it into a tree structure,
where you can access each branch or leaf of the tree.

### Feature Highlight
1) Tokens
   - Tokens yielded by slyce contain the `text` that was matched on,
     as well as a `Span` which marks the exact location of the token in the parsed `Source`
   - `(source: Source).mark(messages)` & `Source.markAll(messages)` allow you to easily mark-up the input `Source`
     with colored output (this is what `parser.markTokens(source)` uses)
   - `(source: Source).mark(messages)` assumes every message is from that source
   - `Source.markAll(messages)` splits up all the messages, and displays them marked on the appropriate source
   - `Upcoming feature:` NonTerminals will soon automatically calculate their spans as well
2) ExpandedGrammar
   - Slyce supports a nice shorthand way of specifying a grammar, inspired by regular expressions
   - `A?` defines an optional element, and gives you a handy `.toMaybe`
   - `A*` defines a list of any number of A's, and gives you a handy `.toList`
   - `A+` defines a list of at least 1 A, and gives you a handy `.toNonEmptyList`
   - `(A . "," ^A)+` defines of list of at least 1 A, each separated by a `","`
   - The `^` is used to tell slyce which element to lift, and the `.` is used to mark a different first element,
     compared to what will repeat
   - The `A*`, `A+`, and `(A .` examples dont need a `^`, because it is obvious what to lift
   - Defining a nonTerminal like `Num ^ int | float;` gives you a `.lift` to make matching easier
     ```scala
     object Tok {
       final case class int(text: String, span: Span.Highligh) extends Num.LiftType
       final case class float(text: String, span: Span.Highligh) extends Num.LiftType
     }
     
     sealed trait Num
     object Num {
       sealed trait LiftType
     
       final case class _1(_0: Tok.int) extends Num
       final case class _2(_0: Tok.float) extends Num
     }
     ```   
   - If you had to match on a `Num`, you could either match on `Num._1`/`Num._2`,
     or call `.lift` and match on `int`/`float`
   - Just like with `*`/`+`, we dont need a `^` if it is obvious what we are lifting
   - `Num2 ^ int | "(" ^float ")";`
   - Expressions:
     ```
     Expr ~ >powOp
          | <multOp
          | <addOp
          ^ variable
          | int
          | float
          | "(" ^Expr ")"
          ;
     ```
   - The `~` marks this as an Expression, and the `^` marks the base as liftable (just like with `Num`)
   - After the `~`, you define the operators and whether it is left or right associative (based on precedence),
     and then the base NonTerminal
   - This generates a handy `.toExpr`, which generates a `slyce.parse.Expression`
   - `Upcoming feature:` `.toExpr` is not currently generated for `^` base NonTerminal.  
     I have not gotten a change to implement the self-referential nature of making sure to recursively expand.
3) Lexer Modes
    - As mentioned before, the lexer has some tricks up its sleeve in order to help you parse complex files
    - `Modes` are a feature that allows you to group a certain set of possible tokens
    - You can then transition between modes on certain tokens
    - `>>` will "switch" you to another mode
    - `->` will "push" you to another mode
    - `<-` will "pop" you from your current mode to a previous mode
    - If no mode switch is provided, you will simply remain in the same mode
    - `Upcoming feature:` being able to push/pop multiple modes at once  
      (Maybe you want to push 2 modes at once, and then pop 1 at a time)  
      (Maybe you want to push 1 mode at a time, and then pop 2 or 3)
    - Know that this feature is always available to you, but using it might make your life harder
    - The grammar is a powerful tool that will specify how tokens can be arranged,  
      you dont need to set up a ton of modes just to get tokens in the right order
    - Using modes should be reserved for cases like Strings,
      where the type of tokens being yielded from inside a string are totally different than outside of one
5) Debug Output
   - There is a `-d` arg that generates a very handy (although also currently very ugly)
     html output specifying the generated parser
   - This is very useful if you are unsure why things are not working as expected
   - `NOTE:` slyce is still in its very early stages, and an issue could very well be its fault, not yours.  
     (If you think that is the case, please make an issue and include your `.slf`/`.sgf` files)

### Examples
Only have 1 real simple dedicated example at this point,
but you can also look at the `.slf`/`.sgf` files that slyce uses to parse your files `:)`.  
In the order of simlest to most complex:

[calc-lexer]: https://github.com/Kalin-Rudnicki/slyce-fp/blob/master/slyce-test/src/test/slyce/slyce/test/parsers/calc.slf
[calc-grammar]: https://github.com/Kalin-Rudnicki/slyce-fp/blob/master/slyce-test/src/test/slyce/slyce/test/parsers/calc.sgf
[grammar-lexer]: https://github.com/Kalin-Rudnicki/slyce-fp/blob/master/slyce-generate-parsers/src/main/slyce/slyce/generate/parsers/grammar.slf
[grammar-grammar]: https://github.com/Kalin-Rudnicki/slyce-fp/blob/master/slyce-generate-parsers/src/main/slyce/slyce/generate/parsers/grammar.sgf
[lexer-lexer]: https://github.com/Kalin-Rudnicki/slyce-fp/blob/master/slyce-generate-parsers/src/main/slyce/slyce/generate/parsers/lexer.slf
[lexer-grammar]: https://github.com/Kalin-Rudnicki/slyce-fp/blob/master/slyce-generate-parsers/src/main/slyce/slyce/generate/parsers/lexer.sgf

| Name | Lexer | Grammar |
| ---- | ----- | ------- |
| calc | [calc-lexer] | [calc-grammar] |
| grammar | [grammar-lexer] | [grammar-grammar] |
| lexer | [lexer-lexer] | [lexer-grammar] |
