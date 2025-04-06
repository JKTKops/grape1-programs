# Programs for the Grape1 Simulated ETCa System

Grape1 (aka uETCa v1) is a microcoded extension-heavy ETCa system simulated in TC;
it offers very reasonable simulation speed but not exceptional scoring.
Optimizing for Grape1 involves mostly avoiding instructions with prefixes immediately
following instructions with low cycle counts; ask Grape for the programmer's manual.

## Wozmon

A port of Wozmon to ETCa. This project has a dedicated repository under ETC-A group
ownership where you will find much more information.

## OS

The beginnings of a (very simple) single-tasking operating system.

## BIOS

The beginnings of a (very simple) bios-equivalent for ETCa. May include bootloading?

## Forth

This project has **moved** to a dedicated repository under ETC-A group ownership and
is no longer here.

## SECD

An ETCa implementation of the SECD abstract machine, which takes a bytecode program
as input from a file and executes it. The goal of this implementation is completeness
and not necessarily efficiency. It will support:

* Implicit memory allocation and garbage collection
* integer, boolean, and string builtin types, as well as support for dispatch on
  tagged unions via info+jump tables in the bytecode stream. (aka pattern matching)
* A few useful primitive operations, such as arithmetic, boolean manipulations,
  and output functions

This should be enough to execute bytecode compiled from a typed ML-style language or a lisp.

