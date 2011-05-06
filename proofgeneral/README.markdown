# Proof General Mode for Abella #

## Installation ##

Create a directory `abella` in where Proof General is installed (i.e. in the same directory as `generic`).
Copy the files `abella.el` and `abella-syntax.el` in this directory.
Add a line `(abella "Abella" "thm")` to the definition of `proof-assistant-table-default` in the file `generic/proof-site.el` in your Proof General installation.

## Known bugs ##

* Retraction on finished proofs produces an error

