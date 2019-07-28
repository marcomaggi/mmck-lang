;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: debug module
;;;Date: Jul 28, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the debug module: implementation of basic debugging facilities.
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

(declare (unit mmck.lang.debug)
	 (emit-import-library mmck.lang.debug))

(module (mmck.lang.debug)
    (debug-print)
  (import (scheme)
	  (only (chicken base)
		current-error-port
		parameterize)
	  (only (chicken pretty-print)
		pretty-print
		pretty-print-width))


;;;; printing

(define (debug-print . args)
  (parameterize ((pretty-print-width 150))
    (pretty-print args (current-error-port))))


;;;; done

#| end of module |# )

;;; end of file
