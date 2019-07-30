;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: test program boolean functions
;;;Date: Jul 29, 2019
;;;
;;;Abstract
;;;
;;;	This program tests boolean functions.
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

(module (test-lang-booleans)
    ()
  (import (scheme)
	  (mmck lang)
	  (mmck exceptional-conditions)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing language extensions: boolean functions\n")


(parameterise ((check-test-name		'predicates))

  (check-for-true	(boolean-true? #t))
  (check-for-false	(boolean-true? #f))
  (check-for-false	(boolean-true? 123))

;;; --------------------------------------------------------------------

  (check-for-true	(boolean-false? #f))
  (check-for-false	(boolean-false? #t))
  (check-for-false	(boolean-false? 123))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
