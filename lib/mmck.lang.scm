;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: main compilation unit
;;;Date: Jul 28, 2019
;;;
;;;Abstract
;;;
;;;	This is the main compilation unit; it USES all the other compilation units.
;;;
;;;	This compilation  units defines the main  module: it imports all  the modules
;;;	exporting  public syntactic  bindings  and it  reexports  all such  syntactic
;;;	bindings.
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

(declare (unit mmck.lang)
	 (uses mmck.lang.debug)
	 (uses mmck.lang.core)
	 (uses mmck.lang.lists)
	 (uses mmck.lang.vectors)
	 (uses mmck.lang.strings)
	 (uses mmck.lang.version)
	 (emit-import-library mmck.lang))

(module (mmck.lang)
    ()
  (import (only (chicken module) reexport))
  (reexport (mmck.lang.debug))
  (reexport (mmck.lang.core))
  (reexport (mmck.lang.lists))
  (reexport (mmck.lang.vectors))
  (reexport (mmck.lang.strings))
  (reexport (mmck.lang.version))
  #| end of module |# )

;;; end of file
