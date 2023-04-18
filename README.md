# Scala based Z80 CPU/System simulator #

You might be thinking why would anybody write such a useless project. The obvious answer is: **for fun**.

Still it is a great learning tool for **Scala**, **functional programming**, **immutability** and **refactoring**.

## Background ##

I love retro-computing. I know Z80 quite well, some time ago I built a ZX Spectrum-like homebrew computer.
I learned Z80 assembler quite well and tweaked some publicly available ROMs to suit my needs.

I used different tools and simulators to make it easier. 
But one thing was always missing - a simulator that I could easily modify myself, with simple I/O.

### Why Scala ###

I use Scala at work, and I am still exploring useful functional patterns. 
I thought that it would be great to have simple side project to exercise handling application state in immutable fashion.  

And here it is :slightly_smiling_face:

## Current state of the project ##

### Instruction set ###

<img src="https://img.shields.io/badge/Load%208--bit-Done-green.svg"/></a>
<img src="https://img.shields.io/badge/Load%2016--bit-Done-green.svg"/></a>
<img src="https://img.shields.io/badge/Exchange-Done-green.svg"/></a>
<img src="https://img.shields.io/badge/Block%20transfer-Done-green.svg"/></a>
<img src="https://img.shields.io/badge/8--bit%20arithmetic-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/16--bit%20arithmetic-Done-green.svg"/></a>
<img src="https://img.shields.io/badge/Control%20incl. INT-Done-green.svg"/></a>
<img src="https://img.shields.io/badge/Rotate%20and%20shift-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Bit%20manipulation-Done-green.svg"/></a>
<img src="https://img.shields.io/badge/Jump%20call%20return-Done-green.svg"/></a>
<img src="https://img.shields.io/badge/Input%20and%20output-Done-green.svg"/></a> 

### Skipped instructions ###
I will not use non-maskable interrupts so I do not need <code>RETN</code>.

Most of IO operations except for: <code> IN A,(n) IN A,(C) OUT (n),A OUT (C),r</code>.
Actually I did not see many uses of other IOs, 
so it is not a great loss anyway.

## The goal ##

I thought that to be able to run **1978 Microsoft Basic** would be a good target for this fun project.
(If you've never used old Basic you should try it and for a moment feel like a kid in 70's and 80's!)

It seemed difficult since assembler programs leave no room for errors in handling operations. 

After extensive refactoring effort and (hopefully) necessary simplifications 
I was able to debug errors and run the most trivial Basic program, which was:

    10 FOR I=1 TO 10
    20 PRINT I;" ";I^2;" ";I^3
    30 NEXT I

I remember well that the power operator (^) in Basic was really slow, therefore I deliberately used it to check for performance.

## Performance ##

The whole code initially needed ~40 seconds to complete and ~0.5 million steps (11th gen i5) - without initial memory test, 
which I skip by entering "65536" at start of the Basic interpreter.

Profiler showed that most of the program time was spent looking up opcodes in various list (from OpCodes). 
After removing most of the lookup operations the reference program took ~4 seconds to complete - 10x improvement. 

Another optimization was to replace costly map handling in register file. After replacing it with just vals
(which added some complexity to the code) the reference program finishes in <3 seconds. 
This highly depends on whether it is run from IntelliJ or directly from command prompt.

I've added tracking of T cycles throughout the program, so it is possible to compare timings to actual Z80 performance @ 3.6864MHz. 
That's the clock speed I use in my Z80 projects as it allows serial communication @ 57600 / 115200.

The best I could normally get for a reference program (and others) was ~50-75%. However, for the **arithmetic.txt** 
program (see _input-files_ folder - no loops, just calculations) the ratio once peeked as high as almost 250%. 
Note that extensive use of console may degrade performance due to scrolling.

![demo](z80_sim_sample.gif)

## How much faster are mutable collections? ##

The initial idea was to use purely immutable data structures, including array/vector for the memory.
I verified the performance several times with _visualvm_ and the results were clear - the application spent ~50% of time
to manipulate the memory and another ~10% for registers. So why not test the mutable memory and registers for comparison?

I used native java array (to avoid boxing of integer values) and the same mutable-like interface (every function
returns an instance of the memory, which in case of mutable memory is just the same instance
every time). The boost in performance was quite substantial - for **arithmetic2.txt** it was x2 - x3 (from ~80%-100% to even ~200-400% or more).

## Run sample programs ##

In the main project directory you will find the _run.bat_ script with some examples. 
The parameters are:

     --mode           : batch (no user interaction)/
                        interactive (use console to interact with the program and Esc to break execution))
     --hex-file       : the ROM file to run
    [--basic-file]    : a file with Basic proram, 
                        actually this is just to simulate user input
    [--steps-m]       : steps to be executed in millions (real number) or -1 for infinity
    [--memory-type]   : fast/slow (default: fast)
    [--register-type] : fast/slow (default: fast)
    [--interrupts]    : true/false (default: true) - should the program use interrupts to read user input

## Copyright notice ##

Nascom/Microsoft Basic 4.7 is copyright of Microsoft. See also: [http://www.nascomhomepage.com/](http://www.nascomhomepage.com/)

Modifications to original source are copyright of Grant Searle. You cannot use it for commercial purposes. See: [http://searle.wales/](http://searle.wales/)
