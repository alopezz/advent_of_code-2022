# Advent of Code, 2022

My general approach is to work the whole way through a problem
exclusively with tests. I use the examples given in the prompt as
tests, and sometimes tests of my own for some implementation details.

Then I write a test that reads from the actual input file and let it
fail, which should give me the answer to my version of the puzzle.

## Language for each day

- 1: OCaml
- 2: Go
- 3: Elixir

## OCaml

For OCaml, I'm using a barebones
[`dune`](https://dune.readthedocs.io/en/stable/) setup. This lets me
have a simple setup to use inline tests, relying on tools like
`ppx_inline_test`, `ppx_assert`, and `ppx_expect` (so this repo
assumes that those tools are installed).


