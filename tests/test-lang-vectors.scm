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


(parameterise ((check-test-name		'predicates))

  (check-for-true	(list-of-vectors? '()))
  (check-for-true	(list-of-vectors? '(#(a))))
  (check-for-true	(list-of-vectors? '(#(a) #(b))))
  ;;
  (check-for-false	(list-of-vectors? '#()))
  (check-for-false	(list-of-vectors? '(#(a) 123)))
  (check-for-false	(list-of-vectors? 123))

;;; --------------------------------------------------------------------

  (check-for-true	(list-of-vectors-of-equal-length? '()))
  (check-for-true	(list-of-vectors-of-equal-length? '(#(a))))
  (check-for-true	(list-of-vectors-of-equal-length? '(#(a) #(b))))

  (check-for-false	(list-of-vectors-of-equal-length? '(#(a) (b))))
  (check-for-false	(list-of-vectors-of-equal-length? '(#(a) #(a b))))

;;; --------------------------------------------------------------------

  (check-for-true	(vectors-of-equal-length? '#()))
  (check-for-true	(vectors-of-equal-length? '#(a)))
  (check-for-true	(vectors-of-equal-length? '#(a) '#(b)))

  (check-for-false	(vectors-of-equal-length? '#(a) '(b)))
  (check-for-false	(vectors-of-equal-length? '#(a) '#(a b)))

  (values))


(parameterise ((check-test-name		'fold-left))

  (check
      (vector-fold-left
	  (lambda (knil item)
	    (cons item knil))
	123
	'#())
    => 123)

  (check
      (vector-fold-left
	  (lambda (knil item)
	    (cons item knil))
	'(0)
	'#(a b c))
    => '(c b a 0))

;;; --------------------------------------------------------------------

  (check
      (vector-fold-left
	  (lambda (knil item1 item2)
	    (cons (list item1 item2) knil))
	123
	'#()
	'#())
    => 123)

  (check
      (vector-fold-left
	  (lambda (knil item1 item2)
	    (cons (list item1 item2) knil))
	'(0)
	'#(a b c)
	'#(d e f))
    => '((c f) (b e) (a d) 0))

;;; --------------------------------------------------------------------

  (check
      (vector-fold-left
	  (lambda (knil item1 item2 item3)
	    (cons (list item1 item2 item3) knil))
	123
	'#()
	'#()
	'#())
    => 123)

  (check
      (vector-fold-left
	  (lambda (knil item1 item2 item3)
	    (cons (list item1 item2 item3) knil))
	'(0)
	'#(a b c)
	'#(d e f)
	'#(g h i))
    => '((c f i) (b e h) (a d g) 0))

;;; --------------------------------------------------------------------

  (check
      (vector-fold-left
	  (lambda (knil item1 item2 item3 item4)
	    (cons (list item1 item2 item3 item4) knil))
	123
	'#()
	'#()
	'#()
	'#())
    => 123)

  (check
      (vector-fold-left
	  (lambda (knil item1 item2 item3 item4)
	    (cons (list item1 item2 item3 item4) knil))
	'(0)
	'#(a b c)
	'#(d e f)
	'#(g h i)
	'#(l m n))
    => '((c f i n) (b e h m) (a d g l) 0))

  (values))


(parameterise ((check-test-name		'unsafe-fold-left))

  (check
      ($vector-fold-left/1
	  (lambda (knil item)
	    (cons item knil))
	123
	'#())
    => 123)

  (check
      ($vector-fold-left/1
	  (lambda (knil item)
	    (cons item knil))
	'(0)
	'#(a b c))
    => '(c b a 0))

;;; --------------------------------------------------------------------

  (check
      ($vector-fold-left/2
	  (lambda (knil item1 item2)
	    (cons (list item1 item2) knil))
	123
	'#()
	'#())
    => 123)

  (check
      ($vector-fold-left/2
	  (lambda (knil item1 item2)
	    (cons (list item1 item2) knil))
	'(0)
	'#(a b c)
	'#(d e f))
    => '((c f) (b e) (a d) 0))

;;; --------------------------------------------------------------------

  (check
      ($vector-fold-left/3
	  (lambda (knil item1 item2 item3)
	    (cons (list item1 item2 item3) knil))
	123
	'#()
	'#()
	'#())
    => 123)

  (check
      ($vector-fold-left/3
	  (lambda (knil item1 item2 item3)
	    (cons (list item1 item2 item3) knil))
	'(0)
	'#(a b c)
	'#(d e f)
	'#(g h i))
    => '((c f i) (b e h) (a d g) 0))

;;; --------------------------------------------------------------------

  (check
      ($vector-fold-left/list
	  (lambda (knil item1 item2 item3 item4)
	    (cons (list item1 item2 item3 item4) knil))
	123
	'(#()
	  #()
	  #()
	  #()))
    => 123)

  (check
      ($vector-fold-left/list
	  (lambda (knil item1 item2 item3 item4)
	    (cons (list item1 item2 item3 item4) knil))
	'(0)
	'(#(a b c)
	  #(d e f)
	  #(g h i)
	  #(l m n)))
    => '((c f i n) (b e h m) (a d g l) 0))

  (values))


(parameterise ((check-test-name		'fold-right))

  (check
      (vector-fold-right
	  (lambda (item knil)
	    (cons item knil))
	123
	'#())
    => 123)

  (check
      (vector-fold-right
	  (lambda (item knil)
	    (cons item knil))
	'(0)
	'#(a b c))
    => '(a b c 0))

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right
	  (lambda (item1 item2 knil)
	    (cons (list item1 item2) knil))
	123
	'#()
	'#())
    => 123)

  (check
      (vector-fold-right
	  (lambda (item1 item2 knil)
	    (cons (list item1 item2) knil))
	'(0)
	'#(a b c)
	'#(d e f))
    => '((a d) (b e) (c f) 0))

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right
	  (lambda (item1 item2 item3 knil)
	    (cons (list item1 item2 item3) knil))
	123
	'#()
	'#()
	'#())
    => 123)

  (check
      (vector-fold-right
	  (lambda (item1 item2 item3 knil)
	    (cons (list item1 item2 item3) knil))
	'(0)
	'#(a b c)
	'#(d e f)
	'#(g h i))
    => '((a d g) (b e h) (c f i) 0))

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right
	  (lambda (item1 item2 item3 item4 knil)
	    (cons (list item1 item2 item3 item4) knil))
	123
	'#()
	'#()
	'#()
	'#())
    => 123)

  (check
      (vector-fold-right
	  (lambda (item1 item2 item3 item4 knil)
	    (cons (list item1 item2 item3 item4) knil))
	'(0)
	'#(a b c)
	'#(d e f)
	'#(g h i)
	'#(l m n))
    => '((a d g l)
	 (b e h m)
	 (c f i n)
	 0))

  (values))


(parameterise ((check-test-name		'unsafe-fold-right))

  (check
      ($vector-fold-right/1
	  (lambda (item knil)
	    (cons item knil))
	123
	'#())
    => 123)

  (check
      ($vector-fold-right/1
	  (lambda (item knil)
	    (cons item knil))
	'(0)
	'#(a b c))
    => '(a b c 0))

;;; --------------------------------------------------------------------

  (check
      ($vector-fold-right/2
	  (lambda (item1 item2 knil)
	    (cons (list item1 item2) knil))
	123
	'#()
	'#())
    => 123)

  (check
      ($vector-fold-right/2
	  (lambda (item1 item2 knil)
	    (cons (list item1 item2) knil))
	'(0)
	'#(a b c)
	'#(d e f))
    => '((a d) (b e) (c f) 0))

;;; --------------------------------------------------------------------

  (check
      ($vector-fold-right/3
	  (lambda (item1 item2 item3 knil)
	    (cons (list item1 item2 item3) knil))
	123
	'#()
	'#()
	'#())
    => 123)

  (check
      ($vector-fold-right/3
	  (lambda (item1 item2 item3 knil)
	    (cons (list item1 item2 item3) knil))
	'(0)
	'#(a b c)
	'#(d e f)
	'#(g h i))
    => '((a d g) (b e h) (c f i) 0))

;;; --------------------------------------------------------------------

  (check
      ($vector-fold-right/list
	  (lambda (item1 item2 item3 item4 knil)
	    (cons (list item1 item2 item3 item4) knil))
	123
	'(#()
	  #()
	  #()
	  #()))
    => 123)

  (check
      ($vector-fold-right/list
	  (lambda (item1 item2 item3 item4 knil)
	    (cons (list item1 item2 item3 item4) knil))
	'(0)
	'(#(a b c)
	  #(d e f)
	  #(g h i)
	  #(l m n)))
    => '((a d g l)
	 (b e h m)
	 (c f i n)
	 0))

  (values))


(parameterise ((check-test-name		'map))

  (check
      (vector-map
	  (lambda (item)
	    (list item))
	'#())
    => '#())

  (check
      (vector-map
	  (lambda (item)
	    (list item))
	'#(a b c))
    => '#((a) (b) (c)))

;;; --------------------------------------------------------------------

  (check
      (vector-map
	  (lambda (item1 item2)
	    (list item1 item2))
	'#()
	'#())
    => '#())

  (check
      (vector-map
	  (lambda (item1 item2)
	    (list item1 item2))
	'#(a b c)
	'#(d e f))
    => '#((a d) (b e) (c f)))

;;; --------------------------------------------------------------------

  (check
      (vector-map
	  (lambda (item1 item2 item3)
	    (list item1 item2 item3))
	'#()
	'#()
	'#())
    => '#())

  (check
      (vector-map
	  (lambda (item1 item2 item3)
	    (list item1 item2 item3))
	'#(a b c)
	'#(d e f)
	'#(g h i))
    => '#((a d g) (b e h) (c f i)))

;;; --------------------------------------------------------------------

  (check
      (vector-map
	  (lambda (item1 item2 item3 item4)
	    (list item1 item2 item3 item4))
	'#()
	'#()
	'#()
	'#())
    => '#())

  (check
      (vector-map
	  (lambda (item1 item2 item3 item4)
	    (list item1 item2 item3 item4))
	'#(a b c)
	'#(d e f)
	'#(g h i)
	'#(l m n))
    => '#((a d g l)
	  (b e h m)
	  (c f i n)))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
