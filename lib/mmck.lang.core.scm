;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: core module
;;;Date: Jul 28, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the core module: implementation of basic syntaxes.
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

(declare (unit mmck.lang.core)
	 (emit-import-library mmck.lang.core))

(module (mmck.lang.core)
    ((syntax: define-syntax-rule)
     (syntax: define*)
     (syntax: case-define)
     (syntax: case-define*)
     (syntax: alist-var-cons! cons)
     (syntax: receive-and-return apply values call-with-values)
     (syntax: begin0)
     (syntax: named-lambda)
     (syntax: internal-body)
     (syntax: define-auxiliary-syntaxes))
  (import (scheme)
          (only (chicken module)
		reexport))
  (reexport (only (chicken base)
		  case-lambda
		  receive))
  (import-for-syntax (scheme)
		     (only (chicken syntax)
			   ir-macro-transformer
			   er-macro-transformer
			   syntax-error)
		     (only (matchable)
			   match))


;;;; binding syntaxes

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (?name . ?args) ?body0 ?body ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ . ?args)
	  (begin ?body0 ?body ...)))))
    ))

(define-syntax case-define
  (syntax-rules ()
    ((_ ?who (?formals_ ?body0_ ?body_ ...) (?formals ?body0 ?body ...) ...)
     (define ?who
       (case-lambda (?formals_ ?body0_ ?body_ ...)
		    (?formals  ?body0  ?body  ...)
		    ...)))
    ))

(define-syntax case-define*
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define %case-lambda	(rename 'case-lambda))
      (define %define		(rename 'define))
      (define %let-syntax	(rename 'let-syntax))
      (define %quote		(rename 'quote))
      (define %syntax-rules	(rename 'syntax-rules))
      (define (synner message . args)
	(apply syntax-error 'case-define* message input-form.stx args))
      (match input-form.stx
	((_ ?who (?formals* ?body0* ?body** ...) ...)
	 (let ((clauses.stx (map (lambda (formals.stx body0.stx body*.stx)
				   `(,formals.stx
				     (,%let-syntax ((__who__ (,%syntax-rules () ((_) (,%quote ,?who)))))
						   ,body0.stx . ,body*.stx)))
			      ?formals* ?body0* ?body**)))
	   `(,%define ,?who (,%case-lambda . ,clauses.stx))))
	(_
	 (synner "invalid syntax use"))))))

(define-syntax define-auxiliary-syntaxes
  (syntax-rules ()
    ((_ ?name0)
     (define-syntax ?name0 (syntax-rules ())))
    ((_ ?name0 ?name1 ?name ...)
     (begin
       (define-auxiliary-syntaxes ?name0)
       (define-auxiliary-syntaxes ?name1 ?name ...)))
    ))

;;; --------------------------------------------------------------------

(define-syntax define*
  (ir-macro-transformer
    (lambda (input-form.stx inject compare)
      (match input-form.stx
	((_ (?who . ?formals) ?body0 ?body* ...)
	 (let ((%__who__ (inject '__who__)))
	   `(define (,?who . ,?formals)
	      (let-syntax ((,%__who__ (syntax-rules ()
					((_)
					 (quote ,?who)))))
		,?body0 ,@?body*))))

	((_ ?who ?expr)
	 (let ((%__who__ (inject '__who__)))
	   `(define ,?who
	      (let-syntax ((,%__who__ (syntax-rules ()
					((_)
					 (quote ,?who)))))
		,?expr))))
	))))

;;; --------------------------------------------------------------------

(define-syntax receive-and-return
  (ir-macro-transformer
    (lambda (input-form.stx inject compare)
      (define (synner message . args)
	(apply syntax-error 'receive-and-return message input-form.stx args))

      (match input-form.stx
	((_ () ?expr ?body0 ?body* ...)
	 `(begin
	    (call-with-values
		(lambda () ,?expr)
	      void)
	    ,?body0 . ,?body*))

	((_ ((? symbol? ?single-formal)) ?expr ?body0 ?body* ...)
	 `(let ((,?single-formal ,?expr))
	    (begin ,?body0 ,@?body*)
	    ,?single-formal))

	((_ (?formal0 ?formal* ...) ?expr ?body0 ?body* ...)
	 `(call-with-values
	      (lambda () ,?expr)
	    (lambda (,?formal0 ,@?formal*)
	      (begin ,?body0 ,@?body*)
	      (values ,?formal0 ,@?formal*))))

	((_ (?formal0 . ?formals-rest) ?expr ?body0 ?body* ...)
	 `(call-with-values
	      (lambda () ,?expr)
	    (lambda (,?formal0 ,@?formal* ...)
	      (begin ,?body0 ,@?body)
	      (values ,?formal0 . ,?formals-rest))))

	((_ (? symbol? ?formals-args) ?expr ?body0 ?body* ...)
	 `(call-with-values
	      (lambda () ,?expr)
	    (lambda ,?formals-args
	      (begin ,?body0 ,@?body*)
	      (apply values ,?formals-args))))

	(_
	 (synner "invalid syntax"))))))


;;;; expression syntaxes

(define-syntax __who__
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (syntax-error '__who__ "invalid syntax use of \"__who__\" outside of a binding form" input-form.stx))))

(define-syntax alist-var-cons!
  (syntax-rules ()
    ((_ ?alist-var ?key ?val)
     (set! ?alist-var (cons (cons ?key ?val) ?alist-var)))
    ))

(define-syntax internal-body
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (let () ?body0 ?body ...))
    ))

(define-syntax begin0
  (syntax-rules ()
    ((_ ?form0)
     (begin ?form0))
    ((_ ?form0 ?form1 ?form ...)
     (receive-and-return args
	 ?form0
       (begin ?form1 ?form ...)))
    ))

(define-syntax named-lambda
  (ir-macro-transformer
    (lambda (input-form.stx inject compare)
      (match input-form.stx
	((_ (? symbol? ?who) ?formals ?body0 ?body* ...)
	 (let ((%__who__	(inject '__who__)))
	   `(lambda ,?formals
	      (let-syntax ((,%__who__ (syntax-rules ()
					((_)
					 (quote ,?who)))))
		,?body0 ,@?body*))))
	))))


;;;; done

#| end of module |# )

;;; end of file
