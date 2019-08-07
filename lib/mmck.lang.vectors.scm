;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: vectors module
;;;Date: Jul 29, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines more facilities to handle vectors.
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

(declare (unit mmck.lang.vectors)
	 (uses mmck.lang.debug)
	 (uses mmck.lang.core)
	 (uses mmck.lang.assertions)
	 (uses mmck.lang.lists)
	 (emit-import-library mmck.lang.vectors))

(module (mmck.lang.vectors)
    (
     ;; unsafe operations
     (syntax: $vector-ref)
     (syntax: $vector-set!)
     (syntax: $vector-set-immediate!)
     (syntax: $vector-length)

     ;; constructors
     vector-append

     ;; predicates
     vector-empty?
     vector-not-empty?
     list-of-vectors?
     vectors-of-equal-length?
     list-of-vectors-of-equal-length?

     ;; iteration and searching
     vector-fold-left vector-fold-right
     vector-find vector-exists vector-for-all
     vector-map vector-for-each
     vector-map-in-order vector-for-each-in-order
     vector-map-index vector-for-each-index

     ;; unsafe iteration and searching
     $vector-fold-left/1
     $vector-fold-left/2
     $vector-fold-left/3
     $vector-fold-left/list
     ;;
     $vector-fold-right/1
     $vector-fold-right/2
     $vector-fold-right/3
     $vector-fold-right/list
     ;;
     $vector-map/1
     $vector-map/2
     $vector-map/3
     $vector-map/list
     ;;
     $vector-for-each/1
     $vector-for-each/2
     $vector-for-each/3
     $vector-for-each/list
     ;;
     $vector-map-in-order/1
     $vector-map-in-order/2
     $vector-map-in-order/3
     $vector-map-in-order/list
     ;;
     $vector-for-each-in-order/1
     $vector-for-each-in-order/2
     $vector-for-each-in-order/3
     $vector-for-each-in-order/list
     ;;
     $vector-map-index/1
     $vector-map-index/2
     $vector-map-index/3
     $vector-map-index/list
     ;;
     $vector-for-each-index/1
     $vector-for-each-index/2
     $vector-for-each-index/3
     $vector-for-each-index/list
     ;;
     $vector-for-all/1
     $vector-for-all/2
     $vector-for-all/3
     $vector-for-all/list
     ;;
     $vector-exists/1
     $vector-exists/2
     $vector-exists/3
     $vector-exists/list
     ;;
     $vector-find

     ;; copying
     vector-copy
     $vector-copy

     ;; exceptional-condition object-types
     &vector-is-empty
     make-vector-is-empty-condition
     condition-vector-is-empty?
     raise-exception-vector-is-empty
     ;;
     &vectors-are-of-different-length
     make-vectors-are-of-different-length-condition
     condition-vectors-are-of-different-length?
     raise-exception-vectors-are-of-different-length
     ;;
     &vectors-are-empty-or-of-different-length
     make-vectors-are-empty-or-of-different-length-condition
     condition-vectors-are-empty-or-of-different-length?
     raise-exception-vectors-are-empty-or-of-different-length
     )
  (import (scheme)
	  (only (chicken base)
		add1
		sub1
		call/cc
		fixnum?
		void
		when)
	  (mmck lang debug)
	  (mmck lang core)
	  (mmck lang assertions)
	  (only (mmck lang lists)
		cons*
		#;fold-left
		$fold-left/1
		$map/1)
	  (mmck exceptional-conditions))


;;;; unsafe operations

(define-syntax-rule ($vector-ref ?vector ?slot-index)
  ;;Unsafe implementation of VECTOR-REF.  To be used when  we know that: ?VECTOR is a vector object;
  ;;?SLOT-INDEX is a valid slot index for ?VECTOR.
  ;;
  (##sys#slot ?vector ?slot-index))

(define-syntax-rule ($vector-set! ?vector ?slot-index ?new-value)
  ;;Unsafe implementation of VECTOR-REF.  To be used when  we know that: ?VECTOR is a vector object;
  ;;?SLOT-INDEX is a valid slot index for ?VECTOR.
  ;;
  (##sys#setslot ?vector ?slot-index ?new-value))

(define-syntax-rule ($vector-set-immediate! ?vector ?slot-index ?new-immediate-value)
  ;;Unsafe implementation of VECTOR-REF.  To be used when  we know that: ?VECTOR is a vector object;
  ;;?SLOT-INDEX  is a  valid slot  index for  ?VECTOR; ?NEW-IMMEDIATE-VALUE  is an  immediate Scheme
  ;;value.
  ;;
  (##sys#setislot ?vector ?slot-index ?new-immediate-value))

(define-syntax-rule ($vector-length ?vector)
  ;;Unsafe implementation  of VECTOR-LENGTH.   To be  used when we  know that:  ?VECTOR is  a vector
  ;;object.
  ;;
  (##sys#size ?vector))


;;;; helpers

(define-syntax define-vector-folder
  (syntax-rules ()
    ((_ ?who ?vector-folder/1 ?vector-folder/2 ?vector-folder/3 ?vector-folder/list)
     (case-define ?who
       ((combine nil vec)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec     2))
	(?vector-folder/1 combine nil vec))

       ((combine nil vec1 vec2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec1    2)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec2    3)
	  (assert-vectors-of-equal-length (quote ?who) vec1 vec2))
	(?vector-folder/2 combine nil vec1 vec2))

       ((combine nil vec1 vec2 vec3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec1    2)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec2    3)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec3    4)
	  (assert-vectors-of-equal-length (quote ?who) vec1 vec2 vec3))
	(?vector-folder/3 combine nil vec1 vec2 vec3))

       ((combine nil vec1 vec2 vec3 . vec*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec1    2)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec2    3)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec3    4)
	  (assert-argument-type/rest (quote ?who) "vector of vectors" list-of-vectors? vec*)
	  (assert-vectors-of-equal-length (quote ?who) vec1 vec2 vec3 vec*))
	(?vector-folder/list combine nil (cons* vec1 vec2 vec3 vec*)))))
    ))

(define-syntax define-vector-mapper
  (syntax-rules ()
    ((_ ?who ?vector-mapper/1 ?vector-mapper/2 ?vector-mapper/3 ?vector-mapper/list)
     (case-define ?who
       ((func vec)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec  2))
	(?vector-mapper/1 func vec))

       ((func vec1 vec2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec1 2)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec2 3)
	  (assert-vectors-of-equal-length (quote ?who) vec1 vec2))
	(?vector-mapper/2 func vec1 vec2))

       ((func vec1 vec2 vec3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec1 2)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec2 3)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec3 4)
	  (assert-vectors-of-equal-length (quote ?who) vec1 vec2 vec3))
	(?vector-mapper/3 func vec1 vec2 vec3))

       ((func vec1 vec2 vec3 . vec*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec1 2)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec2 3)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec3 4)
	  (assert-argument-type/rest (quote ?who) "vector of vectors" list-of-vectors? vec*)
	  (assert-vectors-of-equal-length (quote ?who) vec1 vec2 vec3 vec*))
	(?vector-mapper/list func (cons* vec1 vec2 vec3 vec*)))))
    ))

(define-syntax define-vector-searcher
  (syntax-rules ()
    ((_ ?who ?vector-searcher/1 ?vector-searcher/2 ?vector-searcher/3 ?vector-searcher/list)
     (case-define ?who
       ((pred vec)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec  2))
	(?vector-searcher/1 pred vec))

       ((pred vec1 vec2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec1 2)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec2 3)
	  (assert-vectors-of-equal-length (quote ?who) vec1 vec2))
	(?vector-searcher/2 pred vec1 vec2))

       ((pred vec1 vec2 vec3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec1 2)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec2 3)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec3 4)
	  (assert-vectors-of-equal-length (quote ?who) vec1 vec2 vec3))
	(?vector-searcher/3 pred vec1 vec2 vec3))

       ((pred vec1 vec2 vec3 . vec*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec1 2)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec2 3)
	  (assert-argument-type (quote ?who) "vector"      vector?      vec3 4)
	  (assert-argument-type/rest (quote ?who) "vector of vectors" list-of-vectors? vec*)
	  (assert-vectors-of-equal-length (quote ?who) vec1 vec2 vec3 vec*))
	(?vector-searcher/list pred (cons* vec1 vec2 vec3 vec*)))))
    ))


;;;; exceptional-condition object-types

(define-condition-type &vectors-are-of-different-length
    &assertion
  make-vectors-are-of-different-length-condition
  condition-vectors-are-of-different-length?)

(define (raise-exception-vectors-are-of-different-length who vector-of-vectors)
  (raise
   (condition (make-vectors-are-of-different-length-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid arguments, vectors are of different length")
	      (make-irritants-condition (vector vector-of-vectors)))))

;;; --------------------------------------------------------------------

(define-condition-type &vector-is-empty
    &assertion
  make-vector-is-empty-condition
  condition-vector-is-empty?)

(define (raise-exception-vector-is-empty who obj)
  (raise
   (condition (make-vector-is-empty-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid operand, expected non-empty vector")
	      (make-irritants-condition (vector obj)))))

;;; --------------------------------------------------------------------

(define-condition-type &vectors-are-empty-or-of-different-length
    &assertion
  make-vectors-are-empty-or-of-different-length-condition
  condition-vectors-are-empty-or-of-different-length?)

(define (raise-exception-vectors-are-empty-or-of-different-length who vector-of-vectors)
  (raise
   (condition (make-vectors-are-empty-or-of-different-length-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid arguments, vectors are empty or of different length")
	      (make-irritants-condition (vector vector-of-vectors)))))


;;;; constructors

(define (vector-append . vec*)
  (receive-and-return (dst.vec)
      (make-vector ($fold-left/1 (lambda (nil vec)
				   (+ nil ($vector-length vec)))
		     0 vec*))
    ($fold-left/1 (lambda (dst.idx src.vec)
		    (let ((src.len ($vector-length src.vec)))
		      (vector-copy dst.vec dst.idx src.vec 0 src.len)
		      (+ dst.idx src.len)))
      0 vec*)))


;;;; predicates

(define (vector-empty? obj)
  (and (vector? obj)
       (zero? (vector-length obj))))

(define (vector-not-empty? obj)
  (and (vector? obj)
       (positive? (vector-length obj))))

(define (list-of-vectors? objs)
  ;;Return true if OBJS is a (possibly empty)  list of vectors; otherwise return false.  Notice that
  ;;this function returns false if OBJS is not null or a proper list of pairs.
  ;;
  (or (null? objs)
      (and (pair? objs)
	   (vector? (car objs))
	   (list-of-vectors? (cdr objs)))))

(define (list-of-vectors-of-equal-length? vec*)
  ;;Return true if VEC*  is a list of vectors of equal length;  otherwise return false.  Notice that
  ;;this function returns false if VEC* is not null or a proper list of pairs.
  ;;
  (or (null? vec*)
      (and (pair? vec*)
	   (vector? (car vec*))
	   (let loop ((len1 (vector-length (car vec*)))
		      (vec* (cdr vec*)))
	     (or (null? vec*)
		 (and (pair? vec*)
		      (vector? (car vec*))
		      (= len1 (vector-length (car vec*)))
		      (loop len1 (cdr vec*))))))))

(case-define vectors-of-equal-length?
  ;;Return true if  all the arguments are  vectors of equal length; otherwise  return false.  Notice
  ;;that this function returns false if one of the arguments is not a vector.
  ;;
  ((vec1)
   (vector? vec1))

  ((vec1 vec2)
   (and (vector? vec1)
	(vector? vec2)
	(= (vector-length vec1)
	   (vector-length vec2))))

  ((vec1 vec2 vec3)
   (and (vector? vec1)
	(vector? vec2)
	(vector? vec3)
	(= (vector-length vec1)
	   (vector-length vec2)
	   (vector-length vec3))))


  ((vec1 vec2 vec3 . vec*)
   (and (vector? vec1)
	(vector? vec2)
	(vector? vec3)
	(= (vector-length vec1)
	   (vector-length vec2)
	   (vector-length vec3))
	($vector-for-all/1 (lambda (knil vec)
			     (= knil (vector-length vec)))
			   (vector-length vec1)
			   vec*)))
  #| end of CASE-DEFINE |# )


;;;; special exceptional-condition raisers

(case-define assert-vectors-of-equal-length
  ((who vec1 vec2)
   (unless (vectors-of-equal-length? vec1 vec2)
     (raise-exception-vectors-are-of-different-length who (vector vec1 vec2))))
  ((who vec1 vec2 vec3)
   (unless (vectors-of-equal-length? vec1 vec2 vec3)
     (raise-exception-vectors-are-of-different-length who (vector vec1 vec2 vec3))))
  ((who vec1 vec2 vec3 vec*)
   (unless (list-of-vectors-of-equal-length? (cons* vec1 vec2 vec3 vec*))
     (raise-exception-vectors-are-of-different-length who (cons* vec1 vec2 vec3 vec*)))))


;;;; folding functions

(define ($vector-fold-left/1 combine knil vec)
  (let ((vec.len (vector-length vec)))
    (if (zero? vec.len)
	knil
      (let loop ((i	0)
		 (i+1	1)
		 (knil	knil))
	(if (= i+1 vec.len)
	    ;;Last call to COMBINE is in tail position.
	    (combine knil (vector-ref vec i))
	  (loop i+1 (+ 1 i+1)
		(combine knil (vector-ref vec i))))))))

(define ($vector-fold-left/2 combine knil vec1 vec2)
  (let ((vec.len (vector-length vec1)))
    (if (zero? vec.len)
	knil
      (let loop ((i	0)
		 (i+1	1)
		 (knil	knil))
	(if (= i+1 vec.len)
	    ;;Last call to COMBINE is in tail position.
	    (combine knil (vector-ref vec1 i) (vector-ref vec2 i))
	  (loop i+1 (+ 1 i+1)
		(combine knil (vector-ref vec1 i) (vector-ref vec2 i))))))))

(define ($vector-fold-left/3 combine knil vec1 vec2 vec3)
  (let ((vec.len (vector-length vec1)))
    (if (zero? vec.len)
	knil
      (let loop ((i	0)
		 (i+1	1)
		 (knil	knil))
	(if (= i+1 vec.len)
	    ;;Last call to COMBINE is in tail position.
	    (combine knil (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i))
	  (loop i+1 (+ 1 i+1)
		(combine knil (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i))))))))

(define ($vector-fold-left/list combine knil vec*)
  (let ((vec.len (vector-length (car vec*))))
    (if (zero? vec.len)
	knil
      (let loop ((i	0)
		 (i+1	1)
		 (knil	knil))
	(if (= i+1 vec.len)
	    ;;Last call to COMBINE is in tail position.
	    (apply combine knil ($map/1 (lambda (vec)
					  (vector-ref vec i))
				  vec*))
	  (loop i+1 (+ 1 i+1)
		(apply combine knil ($map/1 (lambda (vec)
					      (vector-ref vec i))
				      vec*))))))))

;;; --------------------------------------------------------------------

(define ($vector-fold-right/1 combine knil vec)
  (let ((vec.len (vector-length vec)))
    (if (zero? vec.len)
	knil
      (let loop ((i	(sub1 vec.len))
		 (knil	knil))
	(if (zero? i)
	    ;;Last call to COMBINE is in tail position.
	    (combine (vector-ref vec i) knil)
	  (loop (sub1 i) (combine (vector-ref vec i) knil)))))))

(define ($vector-fold-right/2 combine knil vec1 vec2)
  (let ((vec.len (vector-length vec1)))
    (if (zero? vec.len)
	knil
      (let loop ((i	(sub1 vec.len))
		 (knil	knil))
	(if (zero? i)
	    ;;Last call to COMBINE is in tail position.
	    (combine (vector-ref vec1 i) (vector-ref vec2 i) knil)
	  (loop (sub1 i) (combine (vector-ref vec1 i) (vector-ref vec2 i) knil)))))))

(define ($vector-fold-right/3 combine knil vec1 vec2 vec3)
  (let ((vec.len (vector-length vec1)))
    (if (zero? vec.len)
	knil
      (let loop ((i	(sub1 vec.len))
		 (knil	knil))
	(if (zero? i)
	    ;;Last call to COMBINE is in tail position.
	    (combine (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i) knil)
	  (loop (sub1 i)
		(combine (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i) knil)))))))

(define ($vector-fold-right/list combine knil vec*)
  (let ((vec.len (vector-length (car vec*))))
    (if (zero? vec.len)
	knil
      (let loop ((i	(sub1 vec.len))
		 (knil	knil))
	(if (zero? i)
	    ;;Last call to COMBINE is in tail position.
	    (apply combine (append ($map/1 (lambda (vec)
					     (vector-ref vec i))
				     vec*)
				   (list knil)))
	  (loop (sub1 i)
		(apply combine (append ($map/1 (lambda (vec)
						 (vector-ref vec i))
					 vec*)
				       (list knil)))))))))

;;; --------------------------------------------------------------------

(define-vector-folder vector-fold-left
  $vector-fold-left/1
  $vector-fold-left/2
  $vector-fold-left/3
  $vector-fold-left/list)

(define-vector-folder vector-fold-right
  $vector-fold-right/1
  $vector-fold-right/2
  $vector-fold-right/3
  $vector-fold-right/list)


;;;; vector-mapping functions

(define ($vector-map/1 func vec.in)
  (receive-and-return (vec.out)
      (make-vector (vector-length vec.in))
    ($vector-fold-left/1 (lambda (idx item)
			   (vector-set! vec.out idx (func item))
			   (add1 idx))
      0 vec.in)))

(define ($vector-map/2 func vec1 vec2)
  (receive-and-return (vec.out)
      (make-vector (vector-length vec1))
    ($vector-fold-left/2 (lambda (idx item1 item2)
			   (vector-set! vec.out idx (func item1 item2))
			   (add1 idx))
      0 vec1 vec2)))

(define ($vector-map/3 func vec1 vec2 vec3)
  (receive-and-return (vec.out)
      (make-vector (vector-length vec1))
    ($vector-fold-left/3 (lambda (idx item1 item2 item3)
			   (vector-set! vec.out idx (func item1 item2 item3))
			   (add1 idx))
      0 vec1 vec2 vec3)))

(define ($vector-map/list func vec*)
  (receive-and-return (vec.out)
      (make-vector (vector-length (car vec*)))
    ($vector-fold-left/list (lambda (idx . item*)
			      (vector-set! vec.out idx (apply func item*))
			      (add1 idx))
      0 vec*)))

;;; --------------------------------------------------------------------

(define ($vector-for-each/1 func vec)
  ($vector-fold-left/1 (lambda (idx item)
			 (func item)
			 (add1 idx))
    0 vec))

(define ($vector-for-each/2 func vec1 vec2)
  ($vector-fold-left/2 (lambda (idx item1 item2)
			 (func item1 item2)
			 (add1 idx))
    0 vec1 vec2))

(define ($vector-for-each/3 func vec1 vec2 vec3)
  ($vector-fold-left/3 (lambda (idx item1 item2 item3)
			 (func item1 item2 item3)
			 (add1 idx))
    0 vec1 vec2 vec3))

(define ($vector-for-each/list func vec*)
  ($vector-fold-left/list (lambda (idx . item*)
			    (apply func item*)
			    (add1 idx))
    0 vec*))

;;; --------------------------------------------------------------------

(define $vector-map-in-order/1		$vector-map/1)
(define $vector-map-in-order/2		$vector-map/2)
(define $vector-map-in-order/3		$vector-map/3)
(define $vector-map-in-order/list	$vector-map/list)

;;; --------------------------------------------------------------------

(define $vector-for-each-in-order/1	$vector-for-each/1)
(define $vector-for-each-in-order/2	$vector-for-each/2)
(define $vector-for-each-in-order/3	$vector-for-each/3)
(define $vector-for-each-in-order/list	$vector-for-each/list)

;;; --------------------------------------------------------------------

(define-vector-mapper vector-map
  $vector-map/1
  $vector-map/2
  $vector-map/3
  $vector-map/list)

(define-vector-mapper vector-for-each
  $vector-for-each/1
  $vector-for-each/2
  $vector-for-each/3
  $vector-for-each/list)

(define-vector-mapper vector-map-in-order
  $vector-map-in-order/1
  $vector-map-in-order/2
  $vector-map-in-order/3
  $vector-map-in-order/list)

(define-vector-mapper vector-for-each-in-order
  $vector-for-each-in-order/1
  $vector-for-each-in-order/2
  $vector-for-each-in-order/3
  $vector-for-each-in-order/list)


;;;; vector-map-indexping functions

(define ($vector-map-index/1 func vec.in)
  (receive-and-return (vec.out)
      (make-vector (vector-length vec.in))
    ($vector-fold-left/1 (lambda (idx item)
			   (vector-set! vec.out idx (func idx item))
			   (add1 idx))
      0 vec.in)))

(define ($vector-map-index/2 func vec1 vec2)
  (receive-and-return (vec.out)
      (make-vector (vector-length vec1))
    ($vector-fold-left/2 (lambda (idx item1 item2)
			   (vector-set! vec.out idx (func idx item1 item2))
			   (add1 idx))
      0 vec1 vec2)))

(define ($vector-map-index/3 func vec1 vec2 vec3)
  (receive-and-return (vec.out)
      (make-vector (vector-length vec1))
    ($vector-fold-left/3 (lambda (idx item1 item2 item3)
			   (vector-set! vec.out idx (func idx item1 item2 item3))
			   (add1 idx))
      0 vec1 vec2 vec3)))

(define ($vector-map-index/list func vec*)
  (receive-and-return (vec.out)
      (make-vector (vector-length (car vec*)))
    ($vector-fold-left/list (lambda (idx . item*)
			      (vector-set! vec.out idx (apply func idx item*))
			      (add1 idx))
      0 vec*)))

;;; --------------------------------------------------------------------

(define ($vector-for-each-index/1 func vec)
  ($vector-fold-left/1 (lambda (idx item)
			 (func idx item)
			 (add1 idx))
    0 vec))

(define ($vector-for-each-index/2 func vec1 vec2)
  ($vector-fold-left/2 (lambda (idx item1 item2)
			 (func idx item1 item2)
			 (add1 idx))
    0 vec1 vec2))

(define ($vector-for-each-index/3 func vec1 vec2 vec3)
  ($vector-fold-left/3 (lambda (idx item1 item2 item3)
			 (func idx item1 item2 item3)
			 (add1 idx))
    0 vec1 vec2 vec3))

(define ($vector-for-each-index/list func vec*)
  ($vector-fold-left/list (lambda (idx . item*)
			    (apply func idx item*)
			    (add1 idx))
    0 vec*))

;;; --------------------------------------------------------------------

(define-vector-mapper vector-map-index
  $vector-map-index/1
  $vector-map-index/2
  $vector-map-index/3
  $vector-map-index/list)

(define-vector-mapper vector-for-each-index
  $vector-for-each-index/1
  $vector-for-each-index/2
  $vector-for-each-index/3
  $vector-for-each-index/list)


;;;; search functions

(define ($vector-for-all/1 pred vec)
  (call/cc
      (lambda (escape)
	($vector-fold-left/1 (lambda (knil item)
			       (if (pred item)
				   knil
				 (escape #f)))
	  #t vec))))

(define ($vector-for-all/2 pred vec1 vec2)
  (call/cc
      (lambda (escape)
	($vector-fold-left/2 (lambda (knil item1 item2)
			       (if (pred item1 item2)
				   knil
				 (escape #f)))
	  #t vec1 vec2))))

(define ($vector-for-all/3 pred vec1 vec2 vec3)
  (call/cc
      (lambda (escape)
	($vector-fold-left/3 (lambda (knil item1 item2 item3)
			       (if (pred item1 item2 item3)
				   knil
				 (escape #f)))
	  #t vec1 vec2 vec3))))

(define ($vector-for-all/list pred vec*)
  (call/cc
      (lambda (escape)
	($vector-fold-left/list (lambda (knil . item*)
				  (if (apply pred item*)
				      knil
				    (escape #f)))
	  #t vec*))))

;;; --------------------------------------------------------------------

(define ($vector-exists/1 pred vec)
  (call/cc
      (lambda (escape)
	($vector-fold-left/1 (lambda (knil item)
			       (cond ((pred item)
				      => escape)
				     (else
				      knil)))
	  #f vec))))

(define ($vector-exists/2 pred vec1 vec2)
  (call/cc
      (lambda (escape)
	($vector-fold-left/2 (lambda (knil item1 item2)
			       (cond ((pred item1 item2)
				      => escape)
				     (else
				      knil)))
	  #f vec1 vec2))))

(define ($vector-exists/3 pred vec1 vec2 vec3)
  (call/cc
      (lambda (escape)
	($vector-fold-left/3 (lambda (knil item1 item2 item3)
			       (cond ((pred item1 item2 item3)
				      => escape)
				     (else
				      knil)))
	  #f vec1 vec2 vec3))))

(define ($vector-exists/list pred vec*)
  (call/cc
      (lambda (escape)
	($vector-fold-left/list (lambda (knil . item*)
				  (cond ((apply pred item*)
					 => escape)
					(else
					 knil)))
	  #f vec*))))

;;; --------------------------------------------------------------------

(case-define $vector-find
  ((pred vec)
   ($vector-find pred vec #f))
  ((pred vec default)
   (call/cc
       (lambda (escape)
	 ($vector-fold-left/1 (lambda (knil item)
				(if (pred item)
				    (escape item)
				  knil))
	   default vec)))))

;;; --------------------------------------------------------------------

(case-define* vector-find
  ((pred vec)
   ($vector-find pred vec #f))
  ((pred vec default)
   (begin-checks
     (assert-argument-type (__who__) "procedure" procedure? pred 1)
     (assert-argument-type (__who__) "list"      vector?    vec  2))
   ($vector-find pred vec default)))

(define-vector-searcher vector-for-all
  $vector-for-all/1
  $vector-for-all/2
  $vector-for-all/3
  $vector-for-all/list)

(define-vector-searcher vector-exists
  $vector-exists/1
  $vector-exists/2
  $vector-exists/3
  $vector-exists/list)


;;;; copying

(define* (vector-copy dst.vec dst.start src.vec src.start src.end)
  (begin-checks
    (assert-argument-type (__who__) "vector"      vector?      dst.vec     1)
    (assert-argument-type (__who__) "fixnum"      fixnum?      dst.start   2)
    (assert-argument-type (__who__) "vector"      vector?      src.vec     3)
    (assert-argument-type (__who__) "fixnum"      fixnum?      src.start   4)
    (assert-argument-type (__who__) "fixnum"      fixnum?      src.end     5)
    (unless (and (<= 0 dst.start)
		 (<= dst.start (vector-length dst.vec)))
      (assertion-violation (__who__)
	"invalid start index for destination vector" dst.vec dst.start))
    (unless (and (<= 0 src.start)
		 (<= src.start (vector-length src.vec)))
      (assertion-violation (__who__)
	"invalid start index for source vector" src.vec src.start))
    (unless (and (<= 0 src.end)
		 (<= src.end (vector-length src.vec)))
      (assertion-violation (__who__)
	"invalid end index for source vector" src.vec src.end))
    (unless (<= (- src.end src.start)
		(- (vector-length dst.vec) dst.start))
      (assertion-violation (__who__)
	"invalid range in source vector for selected range in destination vector"
	dst.vec dst.start src.vec src.start src.end))
    #| end of BEGIN-CHECKS |# )
  ($vector-copy dst.vec dst.start src.vec src.start src.end))

(define ($vector-copy dst.vec dst.start src.vec src.start src.end)
  (do ((i dst.start (add1 i))
       (j src.start (add1 j)))
      ((= j src.end)
       dst.vec)
    ($vector-set! dst.vec i ($vector-ref src.vec j))))


;;;; done

#| end of module |# )

;;; end of file
