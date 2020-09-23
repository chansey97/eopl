;;; sllgen -- Scheme LL(1) parser generator

(let ((time-stamp "Time-stamp: <2000-09-25 11:48:47 wand>"))
  (display (string-append
            "sllgen.scm "
            (substring time-stamp 13 29)
            (string #\newline))))

;;; ****************************************************************        

;;; Table of contents:

;;; top.s                     top-level entries
;;; parser-gen.scm            organization of parser generator
;;; syntax.s                  concrete syntax for grammars, etc.
;;; eliminate-arbno.s         replaces (ARBNO lhs) items with new productions
;;; first-and-follow.s        calculate first and follow sets
;;; gen-table.s               take list of productions, first and
;;;                           follow tables, and generate parsing table
;;; check-table.s             take a parse table and check for conflicts
;;; scan.s                    scanner using streams
;;; parse.s                   run the generated parser
;;; error handling
;;; tests

;;; ****************************************************************

;;; Mon Sep 25 11:48:13 2000 added scanner outcomes symbol, number,
;;; string to replace make-symbol, make-number, and make-string.

;;; Wed Apr 12 14:15:24 2000 version intended to be R5RS-compliant,
;;; based on suggestions by Will Clinger.

;;; ****************************************************************

;;; be sure to load compatibility files!!

;;; ****************************************************************

;; See sllgen.files






















