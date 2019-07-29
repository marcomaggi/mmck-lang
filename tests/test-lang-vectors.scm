;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: test program vector functions
;;;Date: Jul 29, 2019
;;;
;;;Abstract
;;;
;;;	This program tests vector functions.
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

(module (test-lang-vectors)
    ()
  (import (scheme)
	  (mmck lang)
	  (mmck exceptional-conditions)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing language extensions: vector functions\n")


(parameterise ((check-test-name		'unsafe))

  (check
      (let ((vec	'#(1 2 3)))
	(values ($vector-length vec)
		($vector-ref vec 0)
		($vector-ref vec 1)
		($vector-ref vec 2)))
    => 3 1 2 3)

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file