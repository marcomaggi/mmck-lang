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
     assert-argument-list-of-type
     assert-argument-vector-of-type)
  (import (scheme)
	  (only (chicken base)
		unless)
	  (mmck lang debug)
	  (mmck lang core)
	  (mmck exceptional-conditions))


;;;; helpers

(define (fold-left combine nil ell)
  (if (pair? ell)
      (fold-left combine (combine nil (car ell)) (cdr ell))
    nil))


;;;; predicates

(define (assert-argument-type who type.str type-pred arg arg.idx)
  (unless (type-pred arg)
    (assertion-violation who
      (string-append "expected argument " (number->string arg.idx) " of type \"" type.str "\"")
      arg)))

(define (assert-argument-list-of-type who type.str type-pred arg* arg.idx)
  (fold-left (lambda (item.idx item)
	       (if (type-pred item)
		   (+ 1 item.idx)
		 (assertion-violation who
		   (string-append "expected item of type \"" type.str "\""
				  " at index " (number->string item.idx)
				  " of list argument " (number->string arg.idx))
		   item)))
    0 arg*))

(define (assert-argument-vector-of-type who type.str type-pred arg-vec arg.idx)
  (unless (vector? arg-vec)
    (assertion-violation who
      (string-append "expected vector as argument \""
		     type.str
		     "\" at index "
		     (number->string arg.idx))
      arg-vec))
  (do ((i 0 (+ 1 i)))
      ((= i (vector-length arg-vec)))
    (unless (type-pred (vector-ref arg-vec i))
      (assertion-violation who
	(string-append "expected item of type \"" type.str "\""
		       " at index " (number->string i)
		       " of list argument " )
	arg-vec
	(vector-ref arg-vec i)))))


;;;; done

#| end of module |# )

;;; end of file
