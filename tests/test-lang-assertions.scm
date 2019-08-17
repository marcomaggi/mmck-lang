;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: test program for assertion functions
;;;Date: Aug 17, 2019
;;;
;;;Abstract
;;;
;;;	This program tests assertion functions.
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is  free software: you can redistribute  it and/or modify it under the  terms of the
;;;GNU Lesser General Public License as published  by the Free Software Foundation, either version 3
;;;of the License, or (at your option) any later version.
;;;
;;;This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;;;even the implied  warranty of MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
;;;Lesser General Public License for more details.
;;;
;;;You should have received a copy of the GNU Lesser General Public License along with this program.
;;;If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(module (test-lang-assertions)
    ()
  (import (scheme)
	  (only (chicken base)
		call/cc
		foldl)
	  (mmck lang)
	  (mmck exceptional-conditions)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing language extensions: assertion functions\n")


(parameterise ((check-test-name		'argument-type))

  ;;Assertion success.
  ;;
  (check
      (try
	  (internal-body
	    (define* (the-func arg)
	      (begin-checks
		(assert-argument-type (__who__) "this-type" string? arg 1))
	      #t)
	    (the-func "ciao"))
	(catch E
	  ((&assertion)
	   #f)
	  (else E)))
    => #t)

  ;;Assertion failure.
  ;;
  (check
      (try
	  (internal-body
	    (define* (the-func arg)
	      (begin-checks
		(assert-argument-type (__who__) "this-type" string? arg 1))
	      #t)
	    (the-func 123))
	(catch E
	  ((&assertion)
	   (vector (condition-who E)
		   (condition-message E)
		   (condition-irritants E)))
	  (else E)))
    => '#(the-func "expected argument 1 of type \"this-type\"" (123)))

  (values))


(parameterise ((check-test-name		'rest-argument-type))

  (define (list-of-strings? obj)
    (and (list? obj)
	 (call/cc
	     (lambda (escape)
	       (foldl (lambda (knil item)
			(or (string? item)
			    (escape #f)))
		      #t
		      obj)))))

  (define* (the-func arg . rest)
    (begin-checks
      (assert-argument-type/rest (__who__) "this-type" list-of-strings? rest))
    #t)

;;; --------------------------------------------------------------------

  ;;Assertion success.
  ;;
  (check
      (try
	  (the-func 123 "ciao" "hello")
	(catch E
	  ((&assertion)
	   #f)
	  (else E)))
    => #t)

  ;;Assertion failure.
  ;;
  (check
      (try
	  (the-func 123 "ciao" 'hello "salut")
	(catch E
	  ((&assertion)
	   (vector (condition-who E)
		   (condition-message E)
		   (condition-irritants E)))
	  (else E)))
    => '#(the-func "expected rest argument of type \"this-type\""
		   (("ciao" hello "salut"))))

  (values))


(parameterise ((check-test-name		'list-argument-type))

  (define* (the-func arg*)
    (begin-checks
      (assert-argument-list-of-type (__who__) "string" string? arg* 1))
    #t)

;;; --------------------------------------------------------------------

  ;;Assertion success.
  ;;
  (check
      (try
	  (the-func '("ciao" "hello"))
	(catch E
	  ((&assertion)
	   #f)
	  (else E)))
    => #t)

  ;;Assertion failure.
  ;;
  (check
      (try
	  (the-func '("ciao" hello "salut"))
	(catch E
	  ((&assertion)
	   (vector (condition-who E)
		   (condition-message E)
		   (condition-irritants E)))
	  (else E)))
    => '#(the-func "expected item of type \"string\" at index 1 of list argument 1"
		   (("ciao" hello "salut") hello)))

  (values))


(parameterise ((check-test-name		'vector-argument-type))

  (define* (the-func arg-vec)
    (begin-checks
      (assert-argument-vector-of-type (__who__) "string" string? arg-vec 1))
    #t)

;;; --------------------------------------------------------------------

  ;;Assertion success.
  ;;
  (check
      (try
	  (the-func '#("ciao" "hello"))
	(catch E
	  ((&assertion)
	   #f)
	  (else E)))
    => #t)

  ;;Assertion failure.
  ;;
  (check
      (try
	  (the-func '#("ciao" hello "salut"))
	(catch E
	  ((&assertion)
	   (vector (condition-who E)
		   (condition-message E)
		   (condition-irritants E)))
	  (else E)))
    => '#(the-func "expected item of type \"string\" at index 1 of vector argument 1"
		   (#("ciao" hello "salut")
		    hello)))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
