;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: booleans module
;;;Date: Jul 29, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines more facilities to handle booleans.
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

(declare (unit mmck.lang.booleans)
	 (uses mmck.lang.core)
	 (uses mmck.lang.debug)
	 (emit-import-library mmck.lang.booleans))

(module (mmck.lang.booleans)
    (boolean-true?
     boolean-false?)
  (import (scheme)
	  (mmck lang core)
	  (mmck lang debug))


;;;; predicates

(define-syntax-rule (boolean-false? obj)
  (eq? #f obj))

(define-syntax-rule (boolean-true? obj)
  (eq? #t obj))


;;;; done

#| end of module |# )

;;; end of file
