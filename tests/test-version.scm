;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: test program for version functions
;;;Date: Jul 28, 2019
;;;
;;;Abstract
;;;
;;;	This program tests version functions.
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

(require-library (mmck lang))

(module (test-version)
    ()
  (import (scheme)
	  (mmck lang)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing version functions\n")


(parameterise ((check-test-name		'versions))

  (debug-print (list 'mmck-lang-package-major-version	(mmck-lang-package-major-version)))
  (debug-print (list 'mmck-lang-package-minor-version	(mmck-lang-package-minor-version)))
  (debug-print (list 'mmck-lang-package-patch-level	(mmck-lang-package-patch-level)))
  (debug-print (list 'mmck-lang-package-prerelease-tag	(mmck-lang-package-prerelease-tag)))
  (debug-print (list 'mmck-lang-package-build-metadata	(mmck-lang-package-build-metadata)))
  (debug-print (list 'mmck-lang-package-version		(mmck-lang-package-version)))
  (debug-print (list 'mmck-lang-package-semantic-version	(mmck-lang-package-semantic-version)))

  (check-for-true		(number? (mmck-lang-package-major-version)))
  (check-for-true		(number? (mmck-lang-package-minor-version)))
  (check-for-true		(number? (mmck-lang-package-patch-level)))
  (check-for-true		(string? (mmck-lang-package-prerelease-tag)))
  (check-for-true		(string? (mmck-lang-package-build-metadata)))
  (check-for-true		(string? (mmck-lang-package-version)))
  (check-for-true		(string? (mmck-lang-package-semantic-version)))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
