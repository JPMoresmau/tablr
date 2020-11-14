# Tablr, a spreadsheet program written in Rust

The world runs on spreadsheets it seems, so here's another take at writing a spreadsheet program.

## Features

- A clean separation between a library with an API, and different UIs
- Typed cell values, no implicit conversion (text, integer, float, boolean, date, timestamp)
- Save/Load to proprietary JSON format, and also CSV
- A very basic terminal based UI
    - basic colors: green when a cell was just changed, red if a cell is in error
    - load/save
    - uses rustyline so does not work on MingGW terminal on Windows
    - Copy and paste between cells including formula rewriting
- A few numeric functions: SUM, MINUS, TIMES, DIVIDE, AVERAGE, MIN, MAX
    - Support direct arithmetic like A1+A2, and parens
    - You can register your own function (runtime.functions, add an instance of the Function trait)

## TODO

- Support test: the library supports having test cells, but nothing is implemented yet. The idea is to be able to have copies of sheets where you cannot change formulas, but you can put values in cell and indicate which values should the formula yield, kind of built in unit tests for your spreadsheet



## Getting started

`tablr_lib` is the library, `tablr_term` the basic terminal UI.

From source, just run `cargo run -p tablr_term` and press `h` to see the help.
