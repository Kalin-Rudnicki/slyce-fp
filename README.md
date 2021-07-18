
# Slyce

A flex/bison inspired parser generator for Scala.

## What Slyce provides

As an overview, the main 3 things that Slyce provides are:
- `"slyce-generate"` jar
- `"slyce-parse"` library
- `"sylce-plugin"` plugin

Lets go over these each briefly...

- `"slyce-generate"` jar
  - Can be downloaded from the github repository
  - Current latest release is: `2.0.0`
  - Command-line utility for generating parsers
  - 
    ```sh
    # generate args help
    java -jar slyce-generate-2.0.0.jar -- generate all --help
    # logger args help
    java -jar slyce-generate-2.0.0.jar --help --
    # basic example usage
    java -jar slyce-generate-2.0.0.jar -s your-project-name/src/main
    ```
- `"sylce-plugin"` plugin
  - See [Adding Slyce as a dependency](#adding-slyce-as-a-dependency)
  -
    ```
    project
      .in(file("your-project-name"))
      .settings(
        // ... (other stuff)
        slycePairs += SlyceConfig(SlyceInput.SrcDir, SlyceOutput.SrcDir) // more on this later
      )
    ```
  -
    ```sh
    sbt slyce
    ```
- `"slyce-parse"` library
  - See [Adding Slyce as a dependency](#adding-slyce-as-a-dependency)
  - You have a choice of if you would rather use the plugin or command line utility,
    but this one is required. It will be used by the generated files.
  - You don't really need to interact with this at all, as everything is done for you
  - That being said, it might be worth familiarizing yourself with [Source](https://github.com/Kalin-Rudnicki/slyce-fp/blob/master/slyce-core/src/main/scala/slyce/core/Source.scala),  
    as it has a really great ability to display marked up versions of the files you parse.  
    (More on this [here-TODO])

## Adding Slyce as a dependency

Unfortunately, with the current state of the build,
it is borderline impossible for someone else to be able to add Slyce as a dependency
without me walking them through the stages of deleting and re-adding to sbt to get it to self-generate.

The good news is that I am currently in the process of trying to be able to publish to sonatype,
at which point in will be very easy to add it as a dependency just like you would expect.
At that point, I will update this section.



# Old-Stuff

## Usage

1) Create a directory for your `slyce` files
    - Recommended: `project-root/src/main/slyce`
2) Create a matching `basename`.(`slf`/`sgf`) file pair
    - Example: `slyce-root/pkgA/pkgB/example.slf` & `slyce-root/pkgA/pkgB/example.sgf`
3) Define your tokenizer in `___.slf`
4) Define your grammar in `___.sgf`
5) `java -jar slyce-generate-2.0.0.jar generate all -i slyce-dir -o scala-dir`
6) `libraryDependencies += "kalin-rudnicki" %% "slyce-parse" % "2.0.0"`
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
