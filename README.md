## [Advent of Code][aoc]

This repository holds my solution (attempts) to the advent of code challenges.
I am trying to solve them with Scala 3 just to keep some practise in Scala (again).
Maybe throwing in some [Cats] without claiming expertise in these topics, but judge yourself…

### Directory structure

The code for the tasks for each day can be found in the directory `days`, where each day is its own folder named after the day and contains a subproject for that day.
Each day expects its input under `src/main/resources/input[AB].txt` or just `…/inputA.txt` if the input is the same for both.
So this is what you should expect

    days/
    ├── 01
    │   ├── src
    │   │   ├── main
    │   │   │   ├── scala
    │   │   │   │   └── Day0x.scala (maybe also Part1.scala or the like)
    │   │   │   └── resources
    │   │   │       ├── inputA.txt
    │   │   │       └── inputB.txt
    │   │   │
    │   │   └── test
    │   │       └── scala
    │   │           └── … potential some unit tests …
    │   ├── 02
            …

with some common code under `days/common` and potential additional common code if some days share some code.
      
## SBT

You can list all the available days with

    projects
    [info] In file:~/AdventOfCode/2020/
    [info]     common
    [info]     day_01
    [info]   * root-2021

which will show you all the projects. To execute them, you can enter

    day_01/runMain part1

because I will try to stick to convention to name the objects containing the main class `Part1` and `Part2`.
If the input is available, that they are likely going to need (and I didn't make a mistake that I also commited) that should work.

[aoc]: https://adventofcode.com/
[Cats]: https://typelevel.org/cats/