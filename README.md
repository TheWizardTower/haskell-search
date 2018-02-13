# haskell-search

This is a personal project I made to learn more about two things: Information Retrieval systems, and parser combinators.

In essence, this program is a repl with two commands: index and query.

## Index

The index command indexes a document, and makes it ready for searches. The command takes two arguments. The first is a document index, in integer form. The second is a list of words, separated by whitespace.

A few example commands:

* index 1 pizza wings beer
* index 2 chocolate eggs sugar butter salt
* index 3 fromRussiaWithLove goldfinger casinoRoyale skyfall spectre
* index 4 c cpp haskell lisp clojure java tcl bash
* index 1 pasta marinara parmesan

Note that if you re-use an index, the previous document is deleted in favor of the new one. So, if you run the above commands, and then search for pizza, you won't get any results.

Words in the word list must be comprised of letters. Numbers and symbols aren't valid, and will throw an error.

## Query

The query command runs a search on the documents you've previously indexed. The simplest query is of a single word:

* query eggs

But they can become more advanced and particular by joining terms with boolean operators -- | (or) and & (and), like so:

* query (chocolate | marinara)
  - (returns indexes 1 and 2)
* query ((chocolate | marinara) & pasta)
  - (only returns index 1)

You can nest these queries to arbitrary length, but as you add operators, the query will take longer to run. This is because the full-text-search library doesn't support boolean queries, just term searches. So this application has to join (either by union or  by intersection) the resulting index lists manually.

Note also that each term has to be nested in parens. Pretend you're in a strange variant of lisp and you'll be fine.
