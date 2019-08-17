;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: version functions
;;;Date: Jul 28, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines version functions.
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

(declare (unit mmck.lang.version)
	 (emit-import-library mmck.lang.version))

(module (mmck.lang.version)
    (mmck-lang-package-major-version
     mmck-lang-package-minor-version
     mmck-lang-package-patch-level
     mmck-lang-package-prerelease-tag
     mmck-lang-package-build-metadata
     mmck-lang-package-version
     mmck-lang-package-semantic-version)
  (import (scheme)
	  (only (chicken base)
		declare)
	  (prefix mmck.lang.config config::))


;;;; version functions

(declare (type (mmck-lang-package-major-version		(-> fixnum)))
	 (type (mmck-lang-package-minor-version		(-> fixnum)))
	 (type (mmck-lang-package-patch-level		(-> fixnum)))
	 (type (mmck-lang-package-prerelease-tag	(-> string)))
	 (type (mmck-lang-package-build-metadata	(-> string)))
	 (type (mmck-lang-package-version		(-> string)))
	 (type (mmck-lang-package-semantic-version	(-> string))))

(define (mmck-lang-package-major-version)	config::MMUX_PKG_MAJOR_VERSION)
(define (mmck-lang-package-minor-version)	config::MMUX_PKG_MINOR_VERSION)
(define (mmck-lang-package-patch-level)		config::MMUX_PKG_PATCH_LEVEL)
(define (mmck-lang-package-prerelease-tag)	config::MMUX_PKG_PRERELEASE_TAG)
(define (mmck-lang-package-build-metadata)	config::MMUX_PKG_BUILD_METADATA)
(define (mmck-lang-package-version)		config::MMUX_PKG_VERSION)
(define (mmck-lang-package-semantic-version)	config::MMUX_PKG_SEMANTIC_VERSION)


;;;; done

#| end of module |# )

;;; end of file
