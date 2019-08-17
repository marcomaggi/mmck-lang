;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: assertions module
;;;Date: Jul 29, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines more facilities to handle assertions.
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

(declare (unit mmck.lang.assertions)
	 (uses mmck.lang.debug)
	 (uses mmck.lang.core)
	 (emit-import-library mmck.lang.assertions))

(module (mmck.lang.assertions)
    (assert-argument-type
     assert-argument-type/rest
     assert-argument-list-of-type
     assert-argument-vector-of-type)
  (import (scheme)
	  (only (chicken base)
		unless)
	  (only (chicken format)
		format)
	  (only (chicken type)
		:
		define-type)
	  (mmck lang debug)
	  (mmck lang core)
	  (mmck exceptional-conditions))
    (import-for-syntax (scheme)
		     (only (chicken syntax)
			   #;ir-macro-transformer
			   er-macro-transformer
			   syntax-error)
		     (only (matchable)
			   match))


;;;; helpers

(: fold-left ((procedure (* *) . (*)) * list -> *))
(define (fold-left combine nil ell)
  (if (pair? ell)
      (fold-left combine (combine nil (car ell)) (cdr ell))
    nil))

(define-type <&who-condition-argument>
  (or string symbol false))

(define-type <&message-condition-argument>
  string)

(define-type <type-description>
  string)

(define-type <type-predicate>
  (procedure (*) . (boolean)))

(define-type <argument-index>
  fixnum)


;;;; assertions

(: assert-argument-type (<&who-condition-argument> <type-description> <type-predicate> * <argument-index> -> undefined))
(define (assert-argument-type who type-descr type-pred arg arg.idx)
  ;;Usage example:
  ;;
  ;;  (define* (fold-left combine ell)
  ;;    (assert-argument-type (__who__) "procedure" procedure? combine 1)
  ;;    (assert-argument-type (__who__) "list"      list?      ell     2)
  ;;    ---)
  ;;
  (unless (type-pred arg)
    (assertion-violation who
      (format #f "expected argument ~a of type \"~a\"" arg.idx type-descr)
      arg)))

(: assert-argument-type/rest  (<&who-condition-argument> <type-description> <type-predicate> * -> undefined))
(define (assert-argument-type/rest who type-descr type-pred rest-arg)
  ;;Usage example:
  ;;
  ;;  (define* (fold-left combine ell . ell*)
  ;;    (assert-argument-type (__who__) "procedure" procedure? combine 1)
  ;;    (assert-argument-type (__who__) "list"      list?      ell     2)
  ;;    (assert-argument-type/rest (__who__) "list of lists" list-of-lists? ell*)
  ;;    ---)
  ;;
  (unless (type-pred rest-arg)
    (assertion-violation who
      (format #f "expected rest argument of type \"~a\"" type-descr)
      rest-arg)))

(: assert-argument-list-of-type  (<&who-condition-argument> <type-description> <type-predicate> list <argument-index> -> undefined))
(define (assert-argument-list-of-type who type-descr type-pred arg* arg.idx)
  (unless (list? arg*)
    (assertion-violation who
      (string-append "expected list of \""
		     type-descr
		     "\" as argument at index "
		     (number->string arg.idx))
      arg*))
  (fold-left (lambda (item.idx item)
	       (if (type-pred item)
		   (+ 1 item.idx)
		 (assertion-violation who
		   (string-append "expected item of type \"" type-descr "\""
				  " at index " (number->string item.idx)
				  " of list argument " (number->string arg.idx))
		   arg* item)))
    0 arg*))

(: assert-argument-vector-of-type (<&who-condition-argument> <type-description> <type-predicate> vector <argument-index> -> undefined))
(define (assert-argument-vector-of-type who type-descr type-pred arg-vec arg.idx)
  (unless (vector? arg-vec)
    (assertion-violation who
      (string-append "expected vector of \""
		     type-descr
		     "\" as argument at index "
		     (number->string arg.idx))
      arg-vec))
  (do ((i 0 (+ 1 i)))
      ((= i (vector-length arg-vec)))
    (unless (type-pred (vector-ref arg-vec i))
      (assertion-violation who
	(string-append "expected item of type \"" type-descr "\""
		       " at index " (number->string i)
		       " of vector argument " (number->string arg.idx))
	arg-vec
	(vector-ref arg-vec i)))))


;;;; done

#| end of module |# )

;;; end of file
