When we have a program saved in a source file, we can compile it. At the end of compilation we again have a source file. How do we run the program now?

Compiling is a multi-step procedure which depends on the PC architecture. For example Lean compiles to C then to sth else etc.

With interpreters, we can stop compiling at an arbitrary step and directly interpret (DIRECTLY RUNNING THE CODE THAT WE HAVE). 

In Java we usually compile before we interpret, because the compiler generates Java Bytecode. JVM interprets the byte code and delivers result. 

We want to interrupt compilation in order to ==improve compile time==. 

Another use case is the pedagogical - we use interpreters to understand the definition of the structures in the language better. 