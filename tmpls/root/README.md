
# Slyce

A flex/bison inspired parser generator for Scala.

## What Slyce provides

As an overview, the main 3 things that Slyce provides are:
- `"slyce-generate"` jar
- `"slyce-parse"` library
- `"sylce-plugin"` plugin

### `"slyce-generate"` jar
This can be downloaded from [github releases](https://github.com/Kalin-Rudnicki/slyce-fp/releases).  
It gives you a command line utility for generating parsers using slyce.  
(It is definitely going to be less straight-forward than using the plugin).  
```sh
# generate-help
java -jar slyce-generate-%latest-release%.jar --help
# logger-help
java -jar slyce-generate-%latest-release%.jar --help --
# basic usage
java -jar slyce-generate-%latest-release%.jar generate all -s your-project-name/src/main
```

### `"slyce-plugin"`

```scala
// project/plugins.sbt
addSbtPlugin("io.github.kalin-rudnicki" % "slyce-plugin" % "%version%")
```
```scala
// build.sbt
project
  .in(file("your-project-name"))
  .settings(
    // [your other settings]...
    slyceConfigs += SlyceConfig(SlyceInput.SrcDir, SlyceOutput.SrcDir),
  )
```

### `"slyce-parse"`

```scala
// build.sbt
project
  .in(file("your-project-name"))
  .settings(
    // [your other settings]...
    libraryDependencies += "io.github.kalin-rudnicki" %% "slyce-parse" % "%version%",
  )
```

You have a choice between using the command line utility or sbt plugin for generating your parsers,
but adding `slyce-parse` as a dependency is required, as it is used by the generated files.  
All `slyce-_____` libraries depend on my own FP/utils library [klib](https://github.com/Kalin-Rudnicki/klib).

### Basic setup with `slyce-plugin` & `slyce-parse`

Setup sbt:
```scala
// project/plugins.sbt
addSbtPlugin("io.github.kalin-rudnicki" % "slyce-plugin" % "%version%")
```
```scala
// build.sbt
lazy val `your-project-name-parsers` =
    project
      .in(file("your-project-name-parsers"))
      .settings(
        // [your other settings]...
        libraryDependencies += "io.github.kalin-rudnicki" %% "slyce-parse" % "%version%",
        slyceConfigs += SlyceConfig(SlyceInput.SrcDir, SlyceOutput.SrcDir),
      )
lazy val `your-project-name` =
    project
      .in(file("your-project-name"))
      .dependsOn(
        `your-project-name-parsers`,
      )
```

Add slyce files:
```
project-root
|-- your-project-name-parsers
|   |-- src
|   |   |-- main
|   |   |   |-- slyce
|   |   |   |   |-- pkg1
|   |   |   |   |   |-- pkg2
|   |   |   |   |   |   |-- parsers
|   |   |   |   |   |   |   |-- Parser1.slf
|   |   |   |   |   |   |   |-- Parser1.sgf
```

Then run:
```sh
sbt slyce
```

Which will generate:
```
project-root
|-- your-project-name-parsers
|   |-- src
|   |   |-- main
|   |   |   |-- scala
|   |   |   |   |-- pkg1
|   |   |   |   |   |-- pkg2
|   |   |   |   |   |   |-- parsers
|   |   |   |   |   |   |   |-- Parser1.scala
```

If you notice here, we actually generated the files in `your-project-name-parsers`, instead of `your-project-name`.
This is because the generated files can be relatively large,
and something about them at the time being causes sbt to struggle a little bit
(on the order of tens of seconds).  
Adding it to a separate project will mean sbt will not have to worry about re-compiling them unless you re-generate them.  
(as long as you're not putting any other code there).  
At least from my testing, if it is in the same project as the code that is using it,
it will have to be re-compiled every time.

## wiki

Check out the [wiki](https://github.com/Kalin-Rudnicki/slyce-fp/wiki)
