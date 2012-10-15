[![Build Status](https://secure.travis-ci.org/rolandwalker/list-utils.png)](http://travis-ci.org/rolandwalker/list-utils)

Overview
========

List-manipulation utility functions for Emacs.

Quickstart
----------

```lisp
(require 'list-utils)
 
(list-utils-flatten '(1 2 (3 4 (5 6 7))))
 
(let ((cyclic-list '(1 2 3 4 5 6 7)))
  (nconc cyclic-list (cdr cyclic-list))
  (list-utils-flatten cyclic-list))
 
(list-utils-cyclic-p '(1 2 3))
 
(list-utils-plist-del '(:one 1 :two 2 :three 3) :two)
```

list-utils
----------

List-utils is a collection of functions for list manipulation.
This library has no user-level interface; it is only useful
for programming in Emacs Lisp.

Notable functionality includes

* `list-utils-flatten`, a robust list-flattener which handles
   cyclic lists, non-nil-terminated lists, and preserves nils
   when they are found as list elements.

* `tconc`, a simple data structure for efficiently appending
   to a list

The following functions are provided:

	make-tconc
	tconc-p
	tconc-list
	tconc
	list-utils-cons-cell-p
	list-utils-cyclic-length
	list-utils-improper-p
	list-utils-make-proper
	list-utils-make-improper
	list-utils-linear-p
	list-utils-linear-subseq
	list-utils-cyclic-p
	list-utils-cyclic-subseq
	list-utils-safe-length
	list-utils-depth
	list-utils-flatten
	list-utils-alist-flatten
	list-utils-insert-before
	list-utils-insert-after
	list-utils-insert-before-pos
	list-utils-insert-after-pos
	list-utils-plist-reverse
	list-utils-plist-del

To use list-utils, place the list-utils.el library somewhere
Emacs can find it, and add the following to your ~/.emacs file:

```lisp
(require 'list-utils)
```

Notes
-----

This library includes an implementation of the classic LISP
`tconc` which is outside the `list-utils-` namespace.

Compatibility and Requirements
------------------------------

	GNU Emacs version 24.3-devel     : yes, at the time of writing
	GNU Emacs version 24.1 & 24.2    : yes
	GNU Emacs version 23.3           : yes
	GNU Emacs version 22.3           : yes
	GNU Emacs version 21.x and lower : unknown

No external dependencies
