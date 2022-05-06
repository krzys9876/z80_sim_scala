# Scala based Z80 CPU/System simulator #

You might be thinking why would anybody write such a useless project. The obvious answer is: **for fun**.

Still it is a great learning tool for **Scala**, **functional programming**, **immutability** and **refactoring**.

## Background ##

I love retro-computing. I know Z80 quite well, some time ago I built a ZX Spectrum-like homebrew computer.
I learned Z80 assembler quite well and tweaked some publicly available ROMs to suit my needs.

I used different tools and simulators to make it easier. 
But one thing was always missing - a simulator that I could easily modify myself, with simple I/O.

### Why Scala ###

I use Scala at work and I am still exploring usefull functional patterns. 
I thought that it would be great to have simple side project to excercise handling application state in immutable fashion.  

And here it is :slightly_smiling_face:

## Current state of the project ##

### Instruction set ###

<img src="https://img.shields.io/badge/Load%208--bit-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Load%2016--bit-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Exchange-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Block%20transfer-Planned-white.svg"/></a>

<img src="https://img.shields.io/badge/8--bit%20arithmetic-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/16--bit%20arithmetic-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Control%20incl INT-Planned-white.svg"/></a>

<img src="https://img.shields.io/badge/Rotate%20and%20shift-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Bit%20manipulation-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Jump%20call%20return-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Input%20and%20output-Done-green.svg"/></a> 
IN A,(n) | IN A,(C) | OUT (n),A | OUT (C),r

### Skipped instructions ###
RETN, EI, DI. At the moment I do not implement interrupts as the version of Basic does not need it.

I skipped most of IO operations except for those mentioned above. Actually I did not see many uses of other IOs so it is not a great loss anyway.

## The goal ##

I thought that to be able to run **1978 Microsoft Basic** would be a good target for this fun project.
(If you've never tried old Basic give it a try and for a moment feel like a kid in 70's and 80's).

It seemed difficult since assembler programs leave no room errors in handling operations. 

After extensive refactoring effort and (hopefully) necessary simplifications 
I was able to debug errors and run the most trivial Basic program, which was:

    10 FOR I=1 TO 10
    20 PRINT I;" ";I^2;" ";I^3
    30 NEXT I

I remembered well that the ^ (power) operator was really slow so I deliberatelly used it to check for performance.
The whole code needs ~40 seconds to complete and abount 0.5 million steps - without initial memory test, which I skip by entering "65536" at start of the Basic interpreter.
