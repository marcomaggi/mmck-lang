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
	 (uses mmck.lang.debug)
	 (uses mmck.lang.core)
	 (uses mmck.lang.assertions)
	 (emit-import-library mmck.lang.lists))

(module (mmck.lang.lists)
    (
     ;; unsafe operations
     $car
     $cdr
     $car-set!
     $cdr-set!
     $car-set-immediate!
     $cdr-set-immediate!

     ;; constructors
     cons*
     make-list

     ;; predicates
     list-of-lists? list-of-nulls? list-of-pairs?
     lists-of-equal-length? lists-of-lists-of-equal-length?

     ;; getters
     cars-and-cdrs
     gather-cars-in-reverse
     butlast-and-last

     ;; iteration and searching
     fold-left fold-right
     find exists for-all
     map-in-order for-each-in-order

     ;; unsafe iteration and searching
     $fold-left/1
     $fold-left/2
     $fold-left/3
     $fold-left/any
     ;;
     $fold-right/1
     $fold-right/2
     $fold-right/3
     $fold-right/any
     ;;
     $map/1
     $map/2
     $map/3
     $map/any
     ;;
     $for-each/1
     $for-each/2
     $for-each/3
     $for-each/any
     ;;
     $for-each-in-order/1
     $for-each-in-order/2
     $for-each-in-order/3
     $for-each-in-order/any
     ;;
     $map-in-order/1
     $map-in-order/2
     $map-in-order/3
     $map-in-order/any
     ;;
     $for-all/1
     $for-all/2
     $for-all/3
     $for-all/any
     ;;
     $exists/1
     $exists/2
     $exists/3
     $exists/any
     ;;
     $find

     ;; exceptional-condition object-types
     &lists-are-of-different-length
     make-lists-are-of-different-length-condition
     condition-lists-are-of-different-length?
     raise-exception-lists-are-of-different-length
     ;;
     &lists-are-empty-or-of-different-length
     make-lists-are-empty-or-of-different-length-condition
     condition-lists-are-empty-or-of-different-length?
     raise-exception-lists-are-empty-or-of-different-length
     )
  (import (scheme)
	  (only (chicken base)
		butlast
		void)
	  (only (chicken fixnum)
		fx=)
	  (mmck exceptional-conditions)
	  (mmck lang debug)
	  (mmck lang core)
	  (mmck lang assertions))


;;;; helpers

(define-syntax define-list-folder
  (syntax-rules ()
    ((_ ?who ?list-folder/1 ?list-folder/2 ?list-folder/3 ?list-folder/any)
     (case-define ?who
       ((combine nil ell)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell     2))
	(?list-folder/1 combine nil ell))

       ((combine nil ell1 ell2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1    2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2    3)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2))
	(?list-folder/2 combine nil ell1 ell2))

       ((combine nil ell1 ell2 ell3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1    2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2    3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3    4)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3))
	(?list-folder/3 combine nil ell1 ell2 ell3))

       ((combine nil ell1 ell2 ell3 . ell*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1    2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2    3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3    4)
	  (assert-argument-type/rest (quote ?who) "list of lists" list-of-lists? ell*)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3 ell*))
	(?list-folder/any combine nil ell1 ell2 ell3 ell*))))
    ))

(define-syntax define-list-mapper
  (syntax-rules ()
    ((_ ?who ?list-mapper/1 ?list-mapper/2 ?list-mapper/3 ?list-mapper/any)
     (case-define ?who
       ((func ell)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell  2))
	(?list-mapper/1 func ell))

       ((func ell1 ell2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2))
	(?list-mapper/2 func ell1 ell2))

       ((func ell1 ell2 ell3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3 4)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3))
	(?list-mapper/3 func ell1 ell2 ell3))

       ((func ell1 ell2 ell3 . ell*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3 4)
	  (assert-argument-type/rest (quote ?who) "list of lists" list-of-lists? ell*)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3 ell*))
	(?list-mapper/any func ell1 ell2 ell3 ell*))))
    ))

(define-syntax define-list-searcher
  (syntax-rules ()
    ((_ ?who ?list-searcher/1 ?list-searcher/2 ?list-searcher/3 ?list-searcher/any)
     (case-define ?who
       ((pred ell)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell  2))
	(?list-searcher/1 pred ell))

       ((pred ell1 ell2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2))
	(?list-searcher/2 pred ell1 ell2))

       ((pred ell1 ell2 ell3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3 4)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3))
	(?list-searcher/3 pred ell1 ell2 ell3))

       ((pred ell1 ell2 ell3 . ell*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3 4)
	  (assert-argument-type/rest (quote ?who) "list of lists" list-of-lists? ell*)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3 ell*))
	(?list-searcher/any pred ell1 ell2 ell3 ell*))))
    ))


;;;; exceptional-condition object-types

(define-condition-type &lists-are-of-different-length
    &assertion
  make-lists-are-of-different-length-condition
  condition-lists-are-of-different-length?)

(define (raise-exception-lists-are-of-different-length who list-of-lists)
  (raise
   (condition (make-lists-are-of-different-length-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid arguments, lists are of different length")
	      (make-irritants-condition (list list-of-lists)))))

;;; --------------------------------------------------------------------

(define-condition-type &lists-are-empty-or-of-different-length
    &assertion
  make-lists-are-empty-or-of-different-length-condition
  condition-lists-are-empty-or-of-different-length?)

(define (raise-exception-lists-are-empty-or-of-different-length who list-of-lists)
  (raise
   (condition (make-lists-are-empty-or-of-different-length-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid arguments, lists are empty or of different length")
	      (make-irritants-condition (list list-of-lists)))))


;;;; unsafe operations

(define-syntax-rule ($car ?pair)
  (##sys#slot ?pair 0))

(define-syntax-rule ($cdr ?pair)
  (##sys#slot ?pair 1))

(define-syntax-rule ($car-set! ?pair ?new-value)
  (##sys#setslot ?pair 0 ?new-value))

(define-syntax-rule ($cdr-set! ?pair ?new-value)
  (##sys#setslot ?pair 1 ?new-value))

(define-syntax-rule ($car-set-immediate! ?pair ?new-value)
  (##sys#setislot ?pair 0 ?new-value))

(define-syntax-rule ($cdr-set-immediate! ?pair ?new-value)
  (##sys#setislot ?pair 1 ?new-value))


;;;; predicates

(define (list-of-lists? objs)
  ;;Return  true if  OBJS  is a  (possibly  empty) list  of  (possibly empty)  lists;
  ;;otherwise return false.
  ;;
  ;;Notice that this  function returns null if OBJS  is not null or a  proper list of
  ;;pairs.
  ;;
  (or (null? objs)
      (and (pair? objs)
	   (list? (car objs))
	   (list-of-lists? (cdr objs)))))

(define (list-of-nulls? objs)
  ;;Return true if OBJS is a (possibly  empty) list of null objects; otherwise return
  ;;false.
  ;;
  ;;Notice that this  function returns null if OBJS  is not null or a  proper list of
  ;;nulls.
  ;;
  (or (null? objs)
      (and (pair? objs)
	   (null? (car objs))
	   (list-of-nulls? (cdr objs)))))

(define (list-of-pairs? objs)
  ;;Return true if OBJS is a (possibly  empty) list of pair objects; otherwise return
  ;;false.
  ;;
  ;;Notice that this  function returns null if OBJS  is not null or a  proper list of
  ;;pairs.
  ;;
  (or (null? objs)
      (and (pair? objs)
	   (pair? (car objs))
	   (list-of-pairs? (cdr objs)))))

(define (lists-of-lists-of-equal-length? ell*)
  ;;Return true if ELL* is a list of (possibly empty) lists of equal length; otherwise return false.
  ;;
  ;;Notice that this function returns null if ELL* is not null or a proper list of pairs.
  ;;
  (or (null? ell*)
      (and (pair? ell*)
	   (list? (car ell*))
	   (let loop ((len1 (length (car ell*)))
		      (ell* (cdr ell*)))
	     (or (null? ell*)
		 (and (pair? ell*)
		      (list? (car ell*))
		      (= len1 (length (car ell*)))
		      (loop len1 (cdr ell*))))))))

(case-define lists-of-equal-length?
  ;;Return true if  all the arguments are  (possibly empty) lists of equal  length; otherwise return
  ;;false.
  ;;
  ;;Notice that this function returns  null if one of the arguments is not null  or a proper list of
  ;;pairs.
  ;;
  ((ell1)
   #t)

  ((ell1 ell2)
   (let loop ((ell1 ell1)
	      (ell2 ell2))
     (or (and (null? ell1)
	      (null? ell2))
	 (and (pair? ell1)
	      (pair? ell2)
	      (loop (cdr ell1)
		    (cdr ell2))))))

  ((ell1 ell2 ell3)
   (let loop ((ell1 ell1)
	      (ell2 ell2)
	      (ell3 ell3))
     (or (and (null? ell1)
	      (null? ell2)
	      (null? ell3))
	 (and (pair? ell1)
	      (pair? ell2)
	      (pair? ell3)
	      (loop (cdr ell1)
		    (cdr ell2)
		    (cdr ell3))))))

  ((ell1 ell2 ell3 . ell*)
   (let loop ((ell1 ell1)
	      (ell2 ell2)
	      (ell3 ell3)
	      (ell* ell*))
     (or (and (null? ell1)
	      (null? ell2)
	      (null? ell3)
	      ($for-all/1 null? ell*))
	 (and (pair? ell1)
	      (pair? ell2)
	      (pair? ell3)
	      ($for-all/1 pair? ell*)
	      (loop (cdr ell1)
		    (cdr ell2)
		    (cdr ell3)
		    (map cdr ell*))))))
  #| end of CASE-DEFINE |# )


;;;; special exceptional-condition raisers

(case-define assert-lists-of-equal-length
  ((who ell1 ell2)
   (unless (lists-of-equal-length? ell1 ell2)
     (raise-exception-lists-are-of-different-length who (list ell1 ell2))))
  ((who ell1 ell2 ell3)
   (unless (lists-of-equal-length? ell1 ell2 ell3)
     (raise-exception-lists-are-of-different-length who (list ell1 ell2 ell3))))
  ((who ell1 ell2 ell3 ell*)
   (unless (lists-of-lists-of-equal-length? (cons* ell1 ell2 ell3 ell*))
     (raise-exception-lists-are-of-different-length who (cons* ell1 ell2 ell3 ell*)))))


;;;; constructors

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

(case-define make-list
  ((len)
   (make-list len (void)))
  ((len fill)
   (if (fx= 0 len)
       '()
     (cons fill (make-list (- len 1) fill)))))


;;;; getters

(case-define* cars-and-cdrs
  ((ell*)
   (cars-and-cdrs ell* '()))
  ((ell* car*-tail)
   ;;The argument ELL* must be a list of non-empty  lists.  Return two values: a list of the CARs of
   ;;the lists in ELL*; a list of the CDRs of the lists in ELL*.
   ;;
   ;;The optional argument CAR*-TAIL  must be a list: it is appended to  each cars list; it defaults
   ;;to null.
   ;;
   ;;NOTE Let's avoid doing this with a non-tail recursion!!!
   ;;
   (when (or (null? ell*)
	     (list-of-nulls? ell*))
     (raise-exception-lists-are-empty-or-of-different-length (__who__) ell*))
   (let loop ((lle* (reverse ell*))
	      (car* car*-tail)
	      (cdr* '()))
     (if (pair? lle*)
	 (let ((next-sublist (car lle*)))
	   (if (pair? next-sublist)
	       (loop (cdr lle*)
		     (cons (car next-sublist) car*)
		     (cons (cdr next-sublist) cdr*))
	     (raise-exception-lists-are-empty-or-of-different-length (__who__) ell*)))
       (values car* cdr*)))))

(case-define gather-cars-in-reverse
  ((ell*)
   (gather-cars-in-reverse ell* '()))
  ((ell* car*-tail)
   ;;The argument ELL* must be a list of lists of equal length.
   ;;
   ;;The call:
   ;;
   ;;   (gather-cars-in-reverse '((1 2 3)
   ;;                             (4 5 6)
   ;;                             (7 8 9))
   ;;
   ;;returns:
   ;;
   ;;   ((3 6 9)
   ;;    (2 5 8)
   ;;    (1 4 7))
   ;;
   (let loop ((car**	'())
	      (ell*	ell*))
     (if (list-of-nulls? ell*)
	 car**
       (receive (car* cdr*)
	   (cars-and-cdrs ell* car*-tail)
	 (loop (cons car* car**) cdr*))))))

(define* (butlast-and-last ell)
  ;;The argument ELL must be a proper list of 1 or more items. Return two values:
  ;;
  ;;1. A new list holding all the items in ELL, but the last one.
  ;;
  ;;2. The last item in ELL.
  ;;
  (cond ((null? ell)
	 (values '() (void)))
	((pair? ell)
	 (let ((next (cdr ell)))
	   (if (pair? next)
	       (let* ((head-items (cons (car ell) '())))
		 (let loop ((ell	next)
			    (last-pair  head-items))
		   (if (pair? ell)
		       (let ((next (cdr ell)))
			 (if (pair? next)
			     (let ((P (cons (car ell) '())))
			       ($cdr-set! last-pair P)
			       (loop next P))
			   (values head-items (car ell))))
		     (values head-items (void)))))
	     (values '() (car ell)))))
	(else
	 (assertion-violation (__who__) "expected proper list as argument" ell))))


;;;; folding functions

(define ($fold-left/1 combine nil ell)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (if (pair? ell)
      ($fold-left/1 combine (combine nil (car ell)) (cdr ell))
    nil))

(define ($fold-left/2 combine nil ell1 ell2)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (if (pair? ell1)
      ($fold-left/2 combine (combine nil (car ell1) (car ell2)) (cdr ell1) (cdr ell2))
    nil))

(define ($fold-left/3 combine nil ell1 ell2 ell3)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (if (pair? ell1)
      ($fold-left/3 combine
			 (combine nil (car ell1) (car ell2) (car ell3))
			 (cdr ell1) (cdr ell2) (cdr ell3))
    nil))

(define ($fold-left/any combine nil ell1 ell2 ell3 ell*)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (if (pair? ell1)
      ($fold-left/any combine
			   (apply combine nil (car ell1) (car ell2) (car ell3) ($map/1 car ell*))
			   (cdr ell1) (cdr ell2) (cdr ell3) ($map/1 cdr ell*))
    nil))

;;; --------------------------------------------------------------------

(define ($fold-right/1 combine nil ell)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (let loop ((nil	nil)
	     (rev-ell	(reverse ell)))
    (if (pair? rev-ell)
	(loop (combine (car rev-ell) nil)
	      (cdr rev-ell))
      nil)))

(define ($fold-right/2 combine nil ell1 ell2)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (let loop ((nil	nil)
	     (cars*	(gather-cars-in-reverse (list ell1 ell2))))
    (if (pair? cars*)
	(loop (apply combine (append (car cars*) (list nil)))
	      (cdr cars*))
      nil)))

(define ($fold-right/3 combine nil ell1 ell2 ell3)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (let loop ((nil	nil)
	     (cars*	(gather-cars-in-reverse (list ell1 ell2 ell3))))
    (if (pair? cars*)
	(loop (apply combine (append (car cars*) (list nil)))
	      (cdr cars*))
      nil)))

(define ($fold-right/any combine nil ell1 ell2 ell3 ell*)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (let loop ((nil	nil)
	     (cars*	(gather-cars-in-reverse (cons* ell1 ell2 ell3 ell*))))
    (if (pair? cars*)
	(loop (apply combine (append (car cars*) (list nil)))
	      (cdr cars*))
      nil)))

;;; --------------------------------------------------------------------

(define-list-folder fold-left
  $fold-left/1
  $fold-left/2
  $fold-left/3
  $fold-left/any)

(define-list-folder fold-right
  $fold-right/1
  $fold-right/2
  $fold-right/3
  $fold-right/any)


;;;; mapping functions

(define ($map/1 func ell)
  ($fold-right/1 (lambda (item nil)
			(cons (func item) nil))
		      '() ell))

(define ($map/2 func ell1 ell2)
  ($fold-right/2 (lambda (item1 item2 nil)
			(cons (func item1 item2) nil))
		      '() ell1 ell2))

(define ($map/3 func ell1 ell2 ell3)
  ($fold-right/3 (lambda (item1 item2 item3 nil)
			(cons (func item1 item2 item3) nil))
		      '() ell1 ell2 ell3))

(define ($map/any func ell1 ell2 ell3 ell*)
  ($fold-right/any (lambda (item1 item2 item3 . rest)
			  (receive (item* nil)
			      (butlast-and-last rest)
			    (cons (apply func item1 item2 item3 item*)
				  nil)))
			'() ell1 ell2 ell3 ell*))

;;; --------------------------------------------------------------------

(define ($for-each/1 func ell)
  ($fold-left/1 (lambda (nil item)
		       (func item)
		       nil)
		     (void) ell))

(define ($for-each/2 func ell1 ell2)
  ($fold-left/2 (lambda (nil item1 item2)
		       (func item1 item2)
		       nil)
		     (void) ell1 ell2))

(define ($for-each/3 func ell1 ell2 ell3)
  ($fold-left/3 (lambda (nil item1 item2 item3)
		       (func item1 item2 item3)
		       nil)
		     (void) ell1 ell2 ell3))

(define ($for-each/any func ell1 ell2 ell3 ell*)
  ($fold-left/any (lambda (nil item1 item2 item3 . item*)
			 (apply func item1 item2 item3 item*)
			 nil)
		       (void) ell1 ell2 ell3 ell*))

;;; --------------------------------------------------------------------

(define $for-each-in-order/1	$for-each/1)
(define $for-each-in-order/2	$for-each/2)
(define $for-each-in-order/3	$for-each/3)
(define $for-each-in-order/any	$for-each/any)

;;; --------------------------------------------------------------------

(define ($map-in-order/1 func ell)
  (reverse ($fold-left/1 func '() ell)))

(define ($map-in-order/2 func ell1 ell2)
  (reverse ($fold-left/2 func '() ell1 ell2)))

(define ($map-in-order/3 func ell1 ell2 ell3)
  (reverse ($fold-left/3 func '() ell1 ell2 ell3)))

(define ($map-in-order/any func ell1 ell2 ell3 ell*)
  (reverse ($fold-left/any func '() ell1 ell2 ell3 ell*)))

;;; --------------------------------------------------------------------

(define-list-mapper map-in-order
  $map-in-order/1
  $map-in-order/2
  $map-in-order/3
  $map-in-order/any)

(define-list-mapper for-each-in-order
  $for-each-in-order/1
  $for-each-in-order/2
  $for-each-in-order/3
  $for-each-in-order/any)


;;;; search functions

(define ($for-all/1 pred ell)
  (or (null? ell)
      (and (pair? ell)
	   (pred (car ell))
	   ($for-all/1 pred (cdr ell)))))

(define ($for-all/2 pred ell1 ell2)
  (or (null? ell1)
      (and (pair? ell1)
	   (pred (car ell1) (car ell2))
	   ($for-all/2 pred (cdr ell1) (cdr ell2)))))

(define ($for-all/3 pred ell1 ell2 ell3)
  (or (null? ell1)
      (and (pair? ell1)
	   (pred (car ell1) (car ell2) (car ell3))
	   ($for-all/3 pred (cdr ell1) (cdr ell2) (cdr ell3)))))

(define ($for-all/any pred ell1 ell2 ell3 ell*)
  (or (null? ell1)
      (and (pair? ell1)
	   (receive (car* cdr*)
	       (cars-and-cdrs ell*)
	     (and (apply pred (car ell1) (car ell2) (car ell3) car*)
		  ($for-all/any  pred (cdr ell1) (cdr ell2) (cdr ell3) cdr*))))))

;;; --------------------------------------------------------------------

(define ($exists/1 pred ell)
  (or (null? ell)
      (and (pair? ell)
	   (or (pred (car ell))
	       ($exists/1 pred (cdr ell))))))

(define ($exists/2 pred ell1 ell2)
  (or (null? ell1)
      (and (pair? ell1)
	   (or (pred (car ell1) (car ell2))
	       ($exists/2 pred (cdr ell1) (cdr ell2))))))

(define ($exists/3 pred ell1 ell2 ell3)
  (or (null? ell1)
      (and (pair? ell1)
	   (or (pred (car ell1) (car ell2) (car ell3))
	       ($exists/3 pred (cdr ell1) (cdr ell2) (cdr ell3))))))

(define ($exists/any pred ell1 ell2 ell3 ell*)
  (or (null? ell1)
      (and (pair? ell1)
	   (receive (car* cdr*)
	       (cars-and-cdrs ell*)
	     (or (apply pred (car ell1) (car ell2) (car ell3) car*)
		 ($exists/any pred (cdr ell1) (cdr ell2) (cdr ell3) cdr*))))))

;;; --------------------------------------------------------------------

(define ($find pred ell)
  (and (pair? ell)
       (let ((item (car ell)))
	 (if (pred item)
	     item
	   ($find pred (cdr ell))))))

;;; --------------------------------------------------------------------

(define* (find pred ell)
  (begin-checks
    (assert-argument-type (__who__) "procedure" procedure? pred 1)
    (assert-argument-type (__who__) "list"      list?      ell  2))
  ($find pred ell))

(define-list-searcher for-all
  $for-all/1
  $for-all/2
  $for-all/3
  $for-all/any)

(define-list-searcher exists
  $exists/1
  $exists/2
  $exists/3
  $exists/any)


;;;; done

#| end of module |# )

;;; end of file
