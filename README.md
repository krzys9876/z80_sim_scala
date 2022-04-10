# Scala based Z80 CPU/System simulator #

You might be thinking why would anybody write such a useless project. The obvious answer is: **for fun**.

But it fact it is a rehersal and a learning tool for **Scala**, **functional programming**, **immutability** and **refactoring**.

## Background ##

I love retro-computing, I know Z80 quite well, I build a ZX Spectrum-like homebrew computer. 
It was a lot of fun and I used different tools and simulators to make all of this easier. 
I learned Z80 assembler quite well and tweaked some publicly available ROMs to suit my needs.

But one thing was always missing - a simulator that I could modify myself, with some kind of I/O.

### Why Scala ###

I use Scala at work and I was just exploring usefull functional patterns. 
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

<img src="https://img.shields.io/badge/Jump%20call%20return-Planned-white.svg"/></a>

<img src="https://img.shields.io/badge/Input%20and%20output-Planned-white.svg"/></a>

## Ultimate goal ##

To be able to run 1978 Microsoft Basic with simulated I/O console.

If you've never tried old Basic give it a try and for a moment feel like a kid in 70's and 80's. 