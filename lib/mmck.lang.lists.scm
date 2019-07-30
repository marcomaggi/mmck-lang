;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: lists module
;;;Date: Jul 29, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines more facilities to handle lists.
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

(declare (unit mmck.lang.lists)
	 (uses mmck.lang.core)
	 (uses mmck.lang.debug)
	 (emit-import-library mmck.lang.lists))

(module (mmck.lang.lists)
    (
     ;; unsafe operations

     ;; constructors
     cons*
     make-list

     ;; iteration
     fold-left
     fold-right
     exists
     for-all
     )
  (import (scheme)
	  (only (chicken fixnum)
		fx=
		fx<
		fx<=
		fx+
		fx-)
	  (mmck lang core)
	  (mmck lang debug))


;;;; unsafe operations




;;;;

(case-define cons*
  ((item)
   item)
  ((item ell)
   (cons item ell))
  ((item1 item2 ell)
   (cons item1 (cons item2 ell)))
  ((item . rest)
   (let loop ((item	item)
	      (rest	rest))
     (if (null? rest)
	 item
       (cons item (loop (car rest) (cdr rest)))))))

(define (make-list len fill)
  (if (fx= 0 len)
      '()
    (cons fill (make-list (- len 1) fill))))

(define (fold-left combine nil ell)
  (if (pair? ell)
      (fold-left combine (combine nil (car ell)) (cdr ell))
    nil))

(define (fold-right combine nil ell)
  (let loop ((combine	combine)
	     (nil	nil)
	     (ell	(reverse ell)))
    (if (pair? ell)
	(loop combine (combine (car ell) nil) (cdr ell))
      nil)))

(define (exists pred ell)
  (and (pair? ell)
       (or (pred (car ell))
	   (exists pred (cdr ell)))))

(case-define for-all
  ((pred ell)
   (if (pair? ell)
       (if (pred (car ell))
	   (for-all pred (cdr ell))
	 #f)
     #t))
  ((pred ell1 ell2)
   (if (and (pair? ell1)
	    (pair? ell2))
       (if (pred (car ell1) (car ell2))
	   (for-all pred (cdr ell1) (cdr ell2))
	 #f)
     #t)))




;;;; done

#| end of module |# )

;;; end of file
