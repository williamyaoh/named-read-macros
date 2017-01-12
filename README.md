# NAMED-READ-MACROS

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Copyright (c) 2017 William Yao

## Description

Read macros are pretty handy when Lispy sexps just don't cut it. Unfortunately,
having to hunt around in the dispatch table or in the read macro table to
avoid read macro collisions doesn't leave a whole bunch of usable characters.
So instead, `NAMED-READ-MACROS` lets you attach a read macro to any symbol,
and call it like any other function or macro.

```lisp
(named-readtables:in-readtable named-read-macros:readtable)

(named-read-macros:define escapify
  (with-output-to-string (output)
    (loop for char = (read-char *standard-input* nil nil t)
          while char
          do (write-char char output))))
          
(escapify
  Any characters I put in here will be put into the output string, even
  things that would normally require escapes, like backslashes!
  
  \ \ \ \
end-escapify)
 => "Any characters I put in here will be put into the output string, even
       things that would normally require escapes, like backslashes!
       
       \\ \\ \\ \\"
```

You can hijack the Lisp reader *without* having to carefully avoid read macro
conflicts! And since `NAMED-READ-MACROS` attaches read macros to symbols, you
don't have to fuss about with readtables either, and you can export and import
read macros the same way you do with functions and macros and all the other
symbol-based stuff.

## Usage

For any file which needs to use your named read macros, make sure to either
switch your readtable to `NAMED-READ-MACROS:READTABLE`, or fuse a read table
you're using with `NAMED-READ-MACROS:READTABLE-MIXIN`.

+ **READTABLE**

  Contains the standard readtable, but with the macro character for `#\(`
  rewritten to check for a named read macro in the head position.
  
+ **READTABLE-MIXIN**

  Just contains the rewritten macro for `#\(`.
  
Then define a named read macro with `NAMED-READ-MACROS:DEFINE`.

+ **DEFINE** NAME &BODY BODY

  Creates a named read macro, which executes `BODY` in a context where
  `*STANDARD-INPUT*` is bound to a stream containing just the contents which
  the read macro was called with, and associates this macro with `NAME`.
  
  Just like a normal macro, `BODY` should return a Lisp form to then get
  evaluated where the named read macro was called.
  
  Because `NAMED-READ-MACROS` hijacks the Lisp reader, we can't rely on matching
  parentheses to know when the newly-defined read macro *ends*, so we look for
  the sequence of characters `"END-${NAME}"`, *immediately* followed by a
  close parenthesis, in order to know when the read macro ends. Case is checked
  against `NAME` by transforming the ending string according to the case of the
  current readtable; if `(SYMBOL-NAME NAME)` matches the transforming ending
  string exactly, we've found the end tag. In particular, this means that
  using `DEFINE` with a pipe-enclosed string with mixed case will make such a
  read macro impossible to end under the standard readtable (though why one
  would define such a macro is another question entirely!)
  
  Leading whitespace after opening the read macro will not be passed to `BODY`,
  but trailing whitespace before the ending tag will.
  
  Note that `DEFINE` has no compile-time effects by default; the rationale
  is that doing so would also require any functions used by a read macro
  defined by `DEFINE` to be available at compile-time. Since this is not the
  usual, `DEFINE` explicitly has no compile-time effects to avoid this problem.
  If you want a named read macro to be available at compile-time, wrap `DEFINE`
  and any necessary functions in an explicit `EVAL-WHEN`.

## Going forward

An interesting usage would be to write an LALR(1) parser generator for
Common Lisp, but instead of generating a function or a standalone
executable, it instead generates read macros. Any parsers generated could
then be embedded directly within Lisp code, allowing for very complex
read macros to be easily written, and compile directly to Lisp forms.

Another use would be something like Perl's `__DATA__` directive, embedding
files and data directly in Lisp code.
