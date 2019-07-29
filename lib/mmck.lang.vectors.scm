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
	 (uses mmck.lang.core)
	 (uses mmck.lang.debug)
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
	  (mmck lang core)
	  (mmck lang debug))


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


;;;; done

#| end of module |# )

;;; end of file
