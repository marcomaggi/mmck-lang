;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: test program for core syntaxes
;;;Date: Jul 28, 2019
;;;
;;;Abstract
;;;
;;;	This program tests core syntaxes.
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms of the GNU  Lesser General Public License as published  by the Free Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
;;;
;;;You should  have received a  copy of the GNU  Lesser General Public  License along
;;;with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(module (test-lang-core)
    ()
  (import (scheme)
	  (prefix (chicken condition)
		  chicken::)
	  (mmck lang)
	  (mmck exceptional-conditions)
	  (mmck checks))
  (import-for-syntax (scheme)
		     (only (chicken syntax)
			   ir-macro-transformer
			   er-macro-transformer
			   syntax-error)
		     (only (matchable)
			   match))

(check-set-mode! 'report-failed)
(check-display "*** testing language extensions: core syntaxes\n")


(parameterise ((check-test-name		'define*))

  (check
      (let ()
	(define* (doit a b)
	  (list (__who__) a b))

	(doit 'A 'B))
    => '(doit A B))

  (check
      (let ()
	(define* (doit a b . rest)
	  (list (__who__) a b rest))

	(doit 'A 'B 'C 'D 'E))
    => '(doit A B (C D E)))

  (check
      (let ()
	(define* (doit . args)
	  (list (__who__) args))

	(doit 'A 'B 'C 'D 'E))
    => '(doit (A B C D E)))

;;; --------------------------------------------------------------------
;;; defining variables

  (check
      (let ()
	(define* it
	  (list (__who__) 1 2))

	it)
    => '(it 1 2))

;;; --------------------------------------------------------------------
;;; raising exceptions

  (check
      (let ()
	(define* (raise-it)
	  (chicken::abort
	   (chicken::make-property-condition 'mine 'location (__who__))))

	(chicken::condition-case (raise-it)
				 (E (mine)
				    (chicken::get-condition-property E 'mine 'location))
				 (()
				  #f)))
    => 'raise-it)

  (check
      (let ()
	(define* (raise-it)
	  (raise
	   (condition (make-error)
		      (make-who-condition (__who__)))))

	(guard (E ((error? E)
		   (condition-who E))
		  (else #f))
	  (raise-it)))
    => 'raise-it)

  (values))


(parameterise ((check-test-name		'case-define))

  (check
      (let ()
	(case-define doit
	  (()			0)
	  ((a)			(list 1 a))
	  ((a b)		(list 2 a b))
	  ((a b c . rest)	(list '* a b c rest)))

	(values (doit)
		(doit 'A)
		(doit 'A 'B)
		(doit 'A 'B 'C 'D 'E)))
    => 0 '(1 A) '(2 A B) '(* A B C (D E)))

  (check
      (let ()
	(case-define doit
	  (()		0)
	  (args		(list '* args)))

	(values (doit)
		(doit 'A 'B 'C 'D 'E)))
    => 0 '(* (A B C D E)))

  (values))


(parameterise ((check-test-name		'case-define*))

  (check
      (let ()
	(case-define* doit
	  (()			0)
	  ((a)			(list 1 a))
	  ((a b)		(list 2 a b))
	  ((a b c . rest)	(list '* a b c rest)))

	(values (doit)
		(doit 'A)
		(doit 'A 'B)
		(doit 'A 'B 'C 'D 'E)))
    => 0 '(1 A) '(2 A B) '(* A B C (D E)))

  (check
      (let ()
	(case-define* doit
	  (()		0)
	  (args		(list '* args)))

	(values (doit)
		(doit 'A 'B 'C 'D 'E)))
    => 0 '(* (A B C D E)))

  ;;Test for the "__who__" syntax.
  ;;
  (check
      (let ()
	(case-define* doit
	  (()			(list (__who__) 0))
	  ((a)			(list (__who__) 1 a))
	  ((a b)		(list (__who__) 2 a b))
	  ((a b c . rest)	(list (__who__) '* a b c rest)))

	(values (doit)
		(doit 'A)
		(doit 'A 'B)
		(doit 'A 'B 'C 'D 'E)))
    => '(doit 0) '(doit 1 A) '(doit 2 A B) '(doit * A B C (D E)))

  (values))


(parameterise ((check-test-name		'receive-and-return))

  (check
      (receive (a b c)
	  (receive-and-return (a b c)
	      (values 1 2 3)
	    (vector a b c))
	(list a b c))
    => '(1 2 3))

  (check
      (with-result
	(receive (a)
	    (receive-and-return (a)
		1
	      (add-result a))
	  a))
    => '(1 (1)))

  (check
      (with-result
	(receive-and-return ()
	    (values)
	  (add-result 1))
	#t)
    => '(#t (1)))

  (values))


(parameterise ((check-test-name		'define-auxiliary-syntaxes))

  (check
      (let ()
	(define-auxiliary-syntaxes ciao)

	(define-syntax doit
	  (syntax-rules (ciao)
	    ((_ ciao)
	     #t)
	    ((_ ?thing)
	     #f)))

	(values (doit ciao)
		(doit hello)))
    => #t #f)

  (check
      (let ()
	(define-auxiliary-syntaxes ciao)

	(define-syntax doit
	  (er-macro-transformer
	    (lambda (input-form.stx rename compare)
	      (define %ciao (rename 'ciao))
	      (define (ciao? obj)
		(compare %ciao obj))

	      (match input-form.stx
		((_ (? ciao?))
		 #t)
		(_
		 #f)))))

	(values (doit ciao)
		(doit hello)))
    => #t #f)

  (check
      (let ()
	(define-auxiliary-syntaxes ciao)

	(define-syntax doit
	  (ir-macro-transformer
	    (lambda (input-form.stx inject compare)
	      (define %ciao (inject 'ciao))
	      (define (ciao? obj)
		(compare %ciao obj))

	      (match input-form.stx
		((_ (? ciao?))
		 #t)
		(_
		 #f)))))

	(values (doit ciao)
		(doit hello)))
    => #t #f)

  (values))


(parameterise ((check-test-name		'define-syntax-rule))

  (check
      (let ()
	(define-syntax-rule (doit a b)
	  (list a b))

	(doit 1 2))
    => '(1 2))

  (check
      (let ()
	(define-syntax-rule (doit)
	  (list 1 2))

	(doit))
    => '(1 2))

  (check
      (let ()
	(define-syntax-rule (doit ?a ?b . ?rest)
	  (vector '?a '?b '?rest))

	(doit 1 2 3 4))
    => '#(1 2 (3 4)))

  (values))


(parameterise ((check-test-name	'begin0))

  (check
      (begin0
	  1)
    => 1)

  (check
      (call-with-values
	  (lambda ()
	    (begin0
		(values 1 2 3)))
	list)
    => '(1 2 3))

  (check
      (with-result
	(begin0
	    1
	  (add-result 2)
	  (add-result 3)))
    => '(1 (2 3)))

  (check
      (with-result
	(call-with-values
	    (lambda ()
	      (begin0
		  (values 1 10)
		(add-result 2)
		(add-result 3)))
	  list))
    => '((1 10) (2 3)))

  #t)


(parameterise ((check-test-name		'named-lambda))

  (check
      (let ()
	(define func
	  (named-lambda the-func (a b)
	    (list (__who__) a b)))

	(func 1 2))
    => '(the-func 1 2))

  (values))


(parameterise ((check-test-name	'internal-body))

  (check
      (with-result
	(let ()
	  (define a 2)
	  (add-result 1)
	  (add-result a)
	  (internal-body
	    (define b 4)
	    (define c 5)
	    (add-result b)
	    (add-result c)
	    (+ a b c))))
    => `(,(+ 2 4 5) (1 2 4 5)))

  (check
      (with-result
	(internal-body
	  (define a 1)
	  (define b 2)
	  (add-result a)
	  (add-result b)
	  (+ a b)))
    => `(,(+ 1 2) (1 2)))

  #t)


;;;; done

(check-report)

#| end of module |# )

;;; end of file
