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
     )
  (import (scheme)
	  (only (chicken base)
		when)
	  (only (chicken fixnum)
		fx=
		fx<
		fx<=
		fx+
		fx-)
	  (mmck lang debug)
	  (mmck lang core)
	  (mmck lang assertions)
	  (only (mmck lang lists)
		fold-left))


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


;;;; operations

(define (vector-copy dst.vec dst.start src.vec src.start src.end)
  (do ((i dst.start (fx+ 1 i))
       (j src.start (fx+ 1 j)))
      ((fx= j src.end))
    ($vector-set! dst.vec i ($vector-ref src.vec j))))

(define (vector-append . vecs)
  (receive-and-return (dst.vec)
      (make-vector (fold-left (lambda (nil vec)
				(+ nil ($vector-length vec)))
		     0 vecs))
    (fold-left (lambda (dst.idx src.vec)
		 (let ((src.len ($vector-length src.vec)))
		   (vector-copy dst.vec dst.idx src.vec 0 src.len)
		   (fx+ dst.idx src.len)))
      0 vecs)))

(case-define vector-map-to-list
  ((func vec)
   (let loop ((i	(fx- ($vector-length vec) 1))
	      (result	'()))
     (if (fx<= 0 i)
	 (loop (fx- i 1) (cons (func ($vector-ref vec i)) result))
       result)))

  ((func vec1 vec2)
   (let loop ((i	(fx- ($vector-length vec1) 1))
	      (result	'()))
     (if (<= 0 i)
	 (loop (fx- i 1) (cons (func ($vector-ref vec1 i)
				     ($vector-ref vec2 i))
			       result))
       result))))

(case-define vector-for-each
  ((func vec)
   (do ((i 0 (fx+ 1 i)))
       ((fx= i ($vector-length vec)))
     (func i ($vector-ref vec i))))

  ((func vec1 vec2)
   (do ((i 0 (fx+ 1 i)))
       ((fx= i ($vector-length vec1)))
     (func i
	   ($vector-ref vec1 i)
	   ($vector-ref vec2 i)))))

(define (vector-for-all pred vec)
  (let loop ((i 0))
    (cond ((fx= i ($vector-length vec))
	   #t)
	  ((pred ($vector-ref vec i))
	   (loop (fx+ 1 i)))
	  (else
	   #f))))

(define (vector-for-each-index func vec)
  (let loop ((idx 0))
    (when (fx< idx ($vector-length vec))
      (func idx ($vector-ref vec idx))
      (loop (fx+ 1 idx)))))


;;;; done

#| end of module |# )

;;; end of file
