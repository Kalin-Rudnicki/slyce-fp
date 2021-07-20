
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
  - Current latest release is: `2.1.5`
  - Command-line utility for generating parsers
  - 
    ```sh
    # generate args help
    java -jar slyce-generate-2.1.5.jar -- generate all --help
    # logger args help
    java -jar slyce-generate-2.1.5.jar --help --
    # basic example usage
    java -jar slyce-generate-2.1.5.jar -s your-project-name/src/main
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
