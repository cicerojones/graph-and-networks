## Intro

Chapter 3 (Lists) of *ANSI Common Lisp* provides code for
working with breadth-first-search in a network. Chapter 7 (Going
Beyond Basic Lists) of *Land of Lisp* shows code for creating
visualizations in .png format of arbitrary graphs. I am trying to tie
these pieces of code together.

### Additional Info

There will also appear documentation, data and work-in-progress files
that are ancillary to the source code and will need to be organized.

#### Org-babel

Included with the Lisp source code is an org-mode file. This also
incorporates all the same code (though using the org-babel format) as
a possible exploration of Literate Programming. Ultimately, the
documentation that comes in the form of comments in the Lisp code
should be converted into a more expository form, per Knuth (or what I
imagine Knuth to have meant).

Thus, reading through the code in emacs, and selectively evaluating
and playing with the functions as you go along is a possible model for
understanding how it all works. Simply reading source code is one way,
but for pedagogical purposes an alternative method seems desirable.

#### .dot and .png

The main function in graph-theory-and-search.lisp generates dot and
png files for visualizing networks/graphs.