;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: test program for core syntaxes
;;;Date: Jul 28, 2019
;;;
;;;Abstract
;;;
;;;	This program tests core syntaxes.
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms of the GNU  Lesser General Public License as published  by the Free Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
;;;
;;;You should  have received a  copy of the GNU  Lesser General Public  License along
;;;with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(module (test-core)
    ()
  (import (scheme)
	  (mmck lang)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing language extensions: core syntaxes\n")


(parameterise ((check-test-name		'alpha))

  (check
      (the-func)
    => #t)

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
