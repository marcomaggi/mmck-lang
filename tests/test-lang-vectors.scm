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
	  (only (chicken base)
		add1
		void)
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


(parameterise ((check-test-name		'constructors))

  (check
      (vector-append '#())
    => '#())

  (check
      (vector-append '#() '#() '#())
    => '#())

  (check
      (vector-append '#(a b c) '#() '#())
    => '#(a b c))

  (check
      (vector-append '#() '#(a b c) '#())
    => '#(a b c))

  (check
      (vector-append '#() '#() '#(a b c))
    => '#(a b c))

  (check
      (vector-append '#(a b c) '#(d e) '#(f g h i))
    => '#(a b c d e f g h i))

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


(parameterise ((check-test-name		'unsafe-map))

  (check
      ($vector-map/1
	  (lambda (item)
	    (list item))
	'#())
    => '#())

  (check
      ($vector-map/1
	  (lambda (item)
	    (list item))
	'#(a b c))
    => '#((a) (b) (c)))

;;; --------------------------------------------------------------------

  (check
      ($vector-map/2
	  (lambda (item1 item2)
	    (list item1 item2))
	'#()
	'#())
    => '#())

  (check
      ($vector-map/2
	  (lambda (item1 item2)
	    (list item1 item2))
	'#(a b c)
	'#(d e f))
    => '#((a d) (b e) (c f)))

;;; --------------------------------------------------------------------

  (check
      ($vector-map/3
	  (lambda (item1 item2 item3)
	    (list item1 item2 item3))
	'#()
	'#()
	'#())
    => '#())

  (check
      ($vector-map/3
	  (lambda (item1 item2 item3)
	    (list item1 item2 item3))
	'#(a b c)
	'#(d e f)
	'#(g h i))
    => '#((a d g) (b e h) (c f i)))

;;; --------------------------------------------------------------------

  (check
      ($vector-map/list
	  (lambda (item1 item2 item3 item4)
	    (list item1 item2 item3 item4))
	'(#()
	  #()
	  #()
	  #()))
    => '#())

  (check
      ($vector-map/list
	  (lambda (item1 item2 item3 item4)
	    (list item1 item2 item3 item4))
	'(#(a b c)
	  #(d e f)
	  #(g h i)
	  #(l m n)))
    => '#((a d g l)
	  (b e h m)
	  (c f i n)))

  (values))


(parameterise ((check-test-name		'for-each))

  (check
      (with-result
	(vector-for-each
	    (lambda (item)
	      (add-result item))
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each
	    (lambda (item)
	      (add-result item))
	  '#(a b c)))
    => '(3 (a b c)))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(vector-for-each
	    (lambda (item1 item2)
	      (add-result (list item1 item2)))
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each
	    (lambda (item1 item2)
	      (add-result (list item1 item2)))
	  '#(a b c)
	  '#(d e f)))
    => '(3 ((a d) (b e) (c f))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(vector-for-each
	    (lambda (item1 item2 item3)
	      (add-result (list item1 item2 item3)))
	  '#()
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each
	    (lambda (item1 item2 item3)
	      (add-result (list item1 item2 item3)))
	  '#(a b c)
	  '#(d e f)
	  '#(g h i)))
    => '(3 ((a d g) (b e h) (c f i))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(vector-for-each
	    (lambda (item1 item2 item3 item4)
	      (add-result (list item1 item2 item3 item4)))
	  '#()
	  '#()
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each
	    (lambda (item1 item2 item3 item4)
	      (add-result (list item1 item2 item3 item4)))
	  '#(a b c)
	  '#(d e f)
	  '#(g h i)
	  '#(l m n)))
    => '(3 ((a d g l)
	    (b e h m)
	    (c f i n))))

  (values))


(parameterise ((check-test-name		'unsafe-for-each))

  (check
      (with-result
	($vector-for-each/1
	    (lambda (item)
	      (add-result item))
	  '#()))
    => '(0 ()))

  (check
      (with-result
	($vector-for-each/1
	    (lambda (item)
	      (add-result item))
	  '#(a b c)))
    => '(3 (a b c)))

;;; --------------------------------------------------------------------

  (check
      (with-result
	($vector-for-each/2
	    (lambda (item1 item2)
	      (add-result (list item1 item2)))
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	($vector-for-each/2
	    (lambda (item1 item2)
	      (add-result (list item1 item2)))
	  '#(a b c)
	  '#(d e f)))
    => '(3 ((a d) (b e) (c f))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	($vector-for-each/3
	    (lambda (item1 item2 item3)
	      (add-result (list item1 item2 item3)))
	  '#()
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	($vector-for-each/3
	    (lambda (item1 item2 item3)
	      (add-result (list item1 item2 item3)))
	  '#(a b c)
	  '#(d e f)
	  '#(g h i)))
    => '(3 ((a d g) (b e h) (c f i))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	($vector-for-each/list
	    (lambda (item1 item2 item3 item4)
	      (add-result (list item1 item2 item3 item4)))
	  '(#()
	    #()
	    #()
	    #())))
    => '(0 ()))

  (check
      (with-result
	($vector-for-each/list
	    (lambda (item1 item2 item3 item4)
	      (add-result (list item1 item2 item3 item4)))
	  '(#(a b c)
	    #(d e f)
	    #(g h i)
	    #(l m n))))
    => '(3 ((a d g l)
	    (b e h m)
	    (c f i n))))

  (values))


(parameterise ((check-test-name		'map-in-order))

  (check
      (vector-map-in-order
	  (lambda (item)
	    (list item))
	'#())
    => '#())

  (check
      (vector-map-in-order
	  (lambda (item)
	    (list item))
	'#(a b c))
    => '#((a) (b) (c)))

;;; --------------------------------------------------------------------

  (check
      (vector-map-in-order
	  (lambda (item1 item2)
	    (list item1 item2))
	'#()
	'#())
    => '#())

  (check
      (vector-map-in-order
	  (lambda (item1 item2)
	    (list item1 item2))
	'#(a b c)
	'#(d e f))
    => '#((a d) (b e) (c f)))

;;; --------------------------------------------------------------------

  (check
      (vector-map-in-order
	  (lambda (item1 item2 item3)
	    (list item1 item2 item3))
	'#()
	'#()
	'#())
    => '#())

  (check
      (vector-map-in-order
	  (lambda (item1 item2 item3)
	    (list item1 item2 item3))
	'#(a b c)
	'#(d e f)
	'#(g h i))
    => '#((a d g) (b e h) (c f i)))

;;; --------------------------------------------------------------------

  (check
      (vector-map-in-order
	  (lambda (item1 item2 item3 item4)
	    (list item1 item2 item3 item4))
	'#()
	'#()
	'#()
	'#())
    => '#())

  (check
      (vector-map-in-order
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


(parameterise ((check-test-name		'unsafe-map-in-order))

  (check
      ($vector-map-in-order/1
	  (lambda (item)
	    (list item))
	'#())
    => '#())

  (check
      ($vector-map-in-order/1
	  (lambda (item)
	    (list item))
	'#(a b c))
    => '#((a) (b) (c)))

;;; --------------------------------------------------------------------

  (check
      ($vector-map-in-order/2
	  (lambda (item1 item2)
	    (list item1 item2))
	'#()
	'#())
    => '#())

  (check
      ($vector-map-in-order/2
	  (lambda (item1 item2)
	    (list item1 item2))
	'#(a b c)
	'#(d e f))
    => '#((a d) (b e) (c f)))

;;; --------------------------------------------------------------------

  (check
      ($vector-map-in-order/3
	  (lambda (item1 item2 item3)
	    (list item1 item2 item3))
	'#()
	'#()
	'#())
    => '#())

  (check
      ($vector-map-in-order/3
	  (lambda (item1 item2 item3)
	    (list item1 item2 item3))
	'#(a b c)
	'#(d e f)
	'#(g h i))
    => '#((a d g) (b e h) (c f i)))

;;; --------------------------------------------------------------------

  (check
      ($vector-map-in-order/list
	  (lambda (item1 item2 item3 item4)
	    (list item1 item2 item3 item4))
	'(#()
	  #()
	  #()
	  #()))
    => '#())

  (check
      ($vector-map-in-order/list
	  (lambda (item1 item2 item3 item4)
	    (list item1 item2 item3 item4))
	'(#(a b c)
	  #(d e f)
	  #(g h i)
	  #(l m n)))
    => '#((a d g l)
	  (b e h m)
	  (c f i n)))

  (values))


(parameterise ((check-test-name		'for-each-in-order))

  (check
      (with-result
	(vector-for-each-in-order
	    (lambda (item)
	      (add-result item))
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each-in-order
	    (lambda (item)
	      (add-result item))
	  '#(a b c)))
    => '(3 (a b c)))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(vector-for-each-in-order
	    (lambda (item1 item2)
	      (add-result (list item1 item2)))
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each-in-order
	    (lambda (item1 item2)
	      (add-result (list item1 item2)))
	  '#(a b c)
	  '#(d e f)))
    => '(3 ((a d) (b e) (c f))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(vector-for-each-in-order
	    (lambda (item1 item2 item3)
	      (add-result (list item1 item2 item3)))
	  '#()
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each-in-order
	    (lambda (item1 item2 item3)
	      (add-result (list item1 item2 item3)))
	  '#(a b c)
	  '#(d e f)
	  '#(g h i)))
    => '(3 ((a d g) (b e h) (c f i))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(vector-for-each-in-order
	    (lambda (item1 item2 item3 item4)
	      (add-result (list item1 item2 item3 item4)))
	  '#()
	  '#()
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each-in-order
	    (lambda (item1 item2 item3 item4)
	      (add-result (list item1 item2 item3 item4)))
	  '#(a b c)
	  '#(d e f)
	  '#(g h i)
	  '#(l m n)))
    => '(3 ((a d g l)
	    (b e h m)
	    (c f i n))))

  (values))


(parameterise ((check-test-name		'unsafe-for-each-in-order))

  (check
      (with-result
	($vector-for-each-in-order/1
	    (lambda (item)
	      (add-result item))
	  '#()))
    => '(0 ()))

  (check
      (with-result
	($vector-for-each-in-order/1
	    (lambda (item)
	      (add-result item))
	  '#(a b c)))
    => '(3 (a b c)))

;;; --------------------------------------------------------------------

  (check
      (with-result
	($vector-for-each-in-order/2
	    (lambda (item1 item2)
	      (add-result (list item1 item2)))
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	($vector-for-each-in-order/2
	    (lambda (item1 item2)
	      (add-result (list item1 item2)))
	  '#(a b c)
	  '#(d e f)))
    => '(3 ((a d) (b e) (c f))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	($vector-for-each-in-order/3
	    (lambda (item1 item2 item3)
	      (add-result (list item1 item2 item3)))
	  '#()
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	($vector-for-each-in-order/3
	    (lambda (item1 item2 item3)
	      (add-result (list item1 item2 item3)))
	  '#(a b c)
	  '#(d e f)
	  '#(g h i)))
    => '(3 ((a d g) (b e h) (c f i))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	($vector-for-each-in-order/list
	    (lambda (item1 item2 item3 item4)
	      (add-result (list item1 item2 item3 item4)))
	  '(#()
	    #()
	    #()
	    #())))
    => '(0 ()))

  (check
      (with-result
	($vector-for-each-in-order/list
	    (lambda (item1 item2 item3 item4)
	      (add-result (list item1 item2 item3 item4)))
	  '(#(a b c)
	    #(d e f)
	    #(g h i)
	    #(l m n))))
    => '(3 ((a d g l)
	    (b e h m)
	    (c f i n))))

  (values))


(parameterise ((check-test-name		'map-index))

  (check
      (vector-map-index
	  (lambda (idx item)
	    (list idx item))
	'#())
    => '#())

  (check
      (vector-map-index
	  (lambda (idx item)
	    (list idx item))
	'#(a b c))
    => '#((0 a) (1 b) (2 c)))

;;; --------------------------------------------------------------------

  (check
      (vector-map-index
	  (lambda (idx item1 item2)
	    (list idx item1 item2))
	'#()
	'#())
    => '#())

  (check
      (vector-map-index
	  (lambda (idx item1 item2)
	    (list idx item1 item2))
	'#(a b c)
	'#(d e f))
    => '#((0 a d) (1 b e) (2 c f)))

;;; --------------------------------------------------------------------

  (check
      (vector-map-index
	  (lambda (idx item1 item2 item3)
	    (list idx item1 item2 item3))
	'#()
	'#()
	'#())
    => '#())

  (check
      (vector-map-index
	  (lambda (idx item1 item2 item3)
	    (list idx item1 item2 item3))
	'#(a b c)
	'#(d e f)
	'#(g h i))
    => '#((0 a d g) (1 b e h) (2 c f i)))

;;; --------------------------------------------------------------------

  (check
      (vector-map-index
	  (lambda (idx item1 item2 item3 item4)
	    (list idx item1 item2 item3 item4))
	'#()
	'#()
	'#()
	'#())
    => '#())

  (check
      (vector-map-index
	  (lambda (idx item1 item2 item3 item4)
	    (list idx item1 item2 item3 item4))
	'#(a b c)
	'#(d e f)
	'#(g h i)
	'#(l m n))
    => '#((0 a d g l)
	  (1 b e h m)
	  (2 c f i n)))

  (values))


(parameterise ((check-test-name		'for-each-index))

  (check
      (with-result
	(vector-for-each-index
	    (lambda (idx item)
	      (add-result (list idx item)))
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each-index
	    (lambda (idx item)
	      (add-result (list idx item)))
	  '#(a b c)))
    => '(3 ((0 a) (1 b) (2 c))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(vector-for-each-index
	    (lambda (idx item1 item2)
	      (add-result (list idx item1 item2)))
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each-index
	    (lambda (idx item1 item2)
	      (add-result (list idx item1 item2)))
	  '#(a b c)
	  '#(d e f)))
    => '(3 ((0 a d) (1 b e) (2 c f))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(vector-for-each-index
	    (lambda (idx item1 item2 item3)
	      (add-result (list idx item1 item2 item3)))
	  '#()
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each-index
	    (lambda (idx item1 item2 item3)
	      (add-result (list idx item1 item2 item3)))
	  '#(a b c)
	  '#(d e f)
	  '#(g h i)))
    => '(3 ((0 a d g) (1 b e h) (2 c f i))))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(vector-for-each-index
	    (lambda (idx item1 item2 item3 item4)
	      (add-result (list idx item1 item2 item3 item4)))
	  '#()
	  '#()
	  '#()
	  '#()))
    => '(0 ()))

  (check
      (with-result
	(vector-for-each-index
	    (lambda (idx item1 item2 item3 item4)
	      (add-result (list idx item1 item2 item3 item4)))
	  '#(a b c)
	  '#(d e f)
	  '#(g h i)
	  '#(l m n)))
    => '(3 ((0 a d g l)
	    (1 b e h m)
	    (2 c f i n))))

  (values))


(parameterise ((check-test-name		'for-all))

  (check
      (vector-for-all (lambda (item)
			(even? item))
	'#())
    => #t)

  (check
      (vector-for-all (lambda (item)
			(number? item))
	'#(1 3 5 6 8 10))
    => #t)

  (check
      (vector-for-all (lambda (item)
			(even? item))
	'#(1 3 5 6 8 10))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-for-all (lambda (item1 item2)
			(< 10 (+ item1 item2)))
	'#()
	'#())
    => #t)

  (check
      (vector-for-all (lambda (item1 item2)
			(< 1 (+ item1 item2)))
	'#(1 3 5 7)
	'#(2 4 6 8))
    => #t)

  (check
      (vector-for-all (lambda (item1 item2)
			(if (< 10 (+ item1 item2))
			    (vector item1 item2)
			  #f))
	'#(1 2 3)
	'#(4 5 6))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-for-all (lambda (item1 item2 item3)
			(if (< 10 (+ item1 item2 item3))
			    (vector item1 item2 item3)
			  #f))
	'#()
	'#()
	'#())
    => #t)

  (check
      (vector-for-all (lambda (item1 item2 item3)
			(if (< 1 (+ item1 item2 item3))
			    (vector item1 item2 item3)
			  #f))
	'#(1 3 5 7)
	'#(2 4 6 8)
	'#(3 5 7 9))
    => #t)

  (check
      (vector-for-all (lambda (item1 item2 item3)
			(if (< 10 (+ item1 item2 item3))
			    (vector item1 item2 item3)
			  #f))
	'#(1 2 3)
	'#(1.1 2.2 3.3)
	'#(1.11 2.22 3.33))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-for-all (lambda (item1 item2 item3 item4)
			(if (< 10 (+ item1 item2 item3 item4))
			    (vector item1 item2 item3 item4)
			  #f))
	'#()
	'#()
	'#()
	'#())
    => #t)

  (check
      (vector-for-all (lambda (item1 item2 item3 item4)
			(if (< 3 (+ item1 item2 item3 item4))
			    (vector item1 item2 item3 item4)
			  #f))
	'#(1 3 5 7)
	'#(2 4 6 8)
	'#(3 5 7 9)
	'#(4 6 8 10))
    => #t)

  (check
      (vector-for-all (lambda (item1 item2 item3 item4)
			(if (< 100 (+ item1 item2 item3 item4))
			    (vector item1 item2 item3 item4)
			  #f))
	'#(1 2 3)
	'#(1.1 2.2 3.3)
	'#(1.11 2.22 3.33)
	'#(1.111 2.222 3.333))
    => #f)

  (values))


(parameterise ((check-test-name		'unsafe-for-all))

  (check
      ($vector-for-all/1 (lambda (item)
			   (even? item))
	'#())
    => #t)

  (check
      ($vector-for-all/1 (lambda (item)
			   (number? item))
	'#(1 3 5 6 8 10))
    => #t)

  (check
      ($vector-for-all/1 (lambda (item)
			   (even? item))
	'#(1 3 5 6 8 10))
    => #f)

;;; --------------------------------------------------------------------

  (check
      ($vector-for-all/2 (lambda (item1 item2)
			   (< 10 (+ item1 item2)))
	'#()
	'#())
    => #t)

  (check
      ($vector-for-all/2 (lambda (item1 item2)
			   (< 1 (+ item1 item2)))
	'#(1 3 5 7)
	'#(2 4 6 8))
    => #t)

  (check
      ($vector-for-all/2 (lambda (item1 item2)
			   (if (< 10 (+ item1 item2))
			       (vector item1 item2)
			     #f))
	'#(1 2 3)
	'#(4 5 6))
    => #f)

;;; --------------------------------------------------------------------

  (check
      ($vector-for-all/3 (lambda (item1 item2 item3)
			   (if (< 10 (+ item1 item2 item3))
			       (vector item1 item2 item3)
			     #f))
	'#()
	'#()
	'#())
    => #t)

  (check
      ($vector-for-all/3 (lambda (item1 item2 item3)
			   (if (< 1 (+ item1 item2 item3))
			       (vector item1 item2 item3)
			     #f))
	'#(1 3 5 7)
	'#(2 4 6 8)
	'#(3 5 7 9))
    => #t)

  (check
      ($vector-for-all/3 (lambda (item1 item2 item3)
			   (if (< 10 (+ item1 item2 item3))
			       (vector item1 item2 item3)
			     #f))
	'#(1 2 3)
	'#(1.1 2.2 3.3)
	'#(1.11 2.22 3.33))
    => #f)

;;; --------------------------------------------------------------------

  (check
      ($vector-for-all/list (lambda (item1 item2 item3 item4)
			      (if (< 10 (+ item1 item2 item3 item4))
				  (vector item1 item2 item3 item4)
				#f))
	'(#()
	  #()
	  #()
	  #()))
    => #t)

  (check
      ($vector-for-all/list (lambda (item1 item2 item3 item4)
			      (if (< 3 (+ item1 item2 item3 item4))
				  (vector item1 item2 item3 item4)
				#f))
	'(#(1 3 5 7)
	  #(2 4 6 8)
	  #(3 5 7 9)
	  #(4 6 8 10)))
    => #t)

  (check
      ($vector-for-all/list (lambda (item1 item2 item3 item4)
			      (if (< 100 (+ item1 item2 item3 item4))
				  (vector item1 item2 item3 item4)
				#f))
	'(#(1 2 3)
	  #(1.1 2.2 3.3)
	  #(1.11 2.22 3.33)
	  #(1.111 2.222 3.333)))
    => #f)

  (values))


(parameterise ((check-test-name		'exists))

  (check
      (vector-exists (lambda (item)
		(even? item))
	'#())
    => #f)

  (check
      (vector-exists (lambda (item)
		(if (even? item)
		    (vector item)
		  #f))
	'#(1 3 5 6 8 10))
    => '#(6))

;;; --------------------------------------------------------------------

  (check
      (vector-exists (lambda (item1 item2)
		(if (< 10 (+ item1 item2))
		    (vector item1 item2)
		  #f))
	'#()
	'#())
    => #f)

  (check
      (vector-exists (lambda (item1 item2)
		(if (< 10 (+ item1 item2))
		    (vector item1 item2)
		  #f))
	'#(1 3 5 7)
	'#(2 4 6 8))
    => '#(5 6))

  (check
      (vector-exists (lambda (item1 item2)
		(if (< 10 (+ item1 item2))
		    (vector item1 item2)
		  #f))
	'#(1 2 3)
	'#(4 5 6))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-exists (lambda (item1 item2 item3)
		(if (< 10 (+ item1 item2 item3))
		    (vector item1 item2 item3)
		  #f))
	'#()
	'#()
	'#())
    => #f)

  (check
      (vector-exists (lambda (item1 item2 item3)
		(if (< 10 (+ item1 item2 item3))
		    (vector item1 item2 item3)
		  #f))
	'#(1 3 5 7)
	'#(2 4 6 8)
	'#(3 5 7 9))
    => '#(3 4 5))

  (check
      (vector-exists (lambda (item1 item2 item3)
		(if (< 10 (+ item1 item2 item3))
		    (vector item1 item2 item3)
		  #f))
	'#(1 2 3)
	'#(1.1 2.2 3.3)
	'#(1.11 2.22 3.33))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-exists (lambda (item1 item2 item3 item4)
		(if (< 10 (+ item1 item2 item3 item4))
		    (vector item1 item2 item3 item4)
		  #f))
	'#()
	'#()
	'#()
	'#())
    => #f)

  (check
      (vector-exists (lambda (item1 item2 item3 item4)
		(if (< 10 (+ item1 item2 item3 item4))
		    (vector item1 item2 item3 item4)
		  #f))
	'#(1 3 5 7)
	'#(2 4 6 8)
	'#(3 5 7 9)
	'#(4 6 8 10))
    => '#(3 4 5 6))

  (check
      (vector-exists (lambda (item1 item2 item3 item4)
		(if (< 100 (+ item1 item2 item3 item4))
		    (vector item1 item2 item3 item4)
		  #f))
	'#(1 2 3)
	'#(1.1 2.2 3.3)
	'#(1.11 2.22 3.33)
	'#(1.111 2.222 3.333))
    => #f)

  (values))


(parameterise ((check-test-name		'unsafe-exists))

  (check
      ($vector-exists/1 (lambda (item)
			  (even? item))
	'#())
    => #f)

  (check
      ($vector-exists/1 (lambda (item)
			  (if (even? item)
			      (vector item)
			    #f))
	'#(1 3 5 6 8 10))
    => '#(6))

;;; --------------------------------------------------------------------

  (check
      ($vector-exists/2 (lambda (item1 item2)
			  (if (< 10 (+ item1 item2))
			      (vector item1 item2)
			    #f))
	'#()
	'#())
    => #f)

  (check
      ($vector-exists/2 (lambda (item1 item2)
			  (if (< 10 (+ item1 item2))
			      (vector item1 item2)
			    #f))
	'#(1 3 5 7)
	'#(2 4 6 8))
    => '#(5 6))

  (check
      ($vector-exists/2 (lambda (item1 item2)
			  (if (< 10 (+ item1 item2))
			      (vector item1 item2)
			    #f))
	'#(1 2 3)
	'#(4 5 6))
    => #f)

;;; --------------------------------------------------------------------

  (check
      ($vector-exists/3 (lambda (item1 item2 item3)
			  (if (< 10 (+ item1 item2 item3))
			      (vector item1 item2 item3)
			    #f))
	'#()
	'#()
	'#())
    => #f)

  (check
      ($vector-exists/3 (lambda (item1 item2 item3)
			  (if (< 10 (+ item1 item2 item3))
			      (vector item1 item2 item3)
			    #f))
	'#(1 3 5 7)
	'#(2 4 6 8)
	'#(3 5 7 9))
    => '#(3 4 5))

  (check
      ($vector-exists/3 (lambda (item1 item2 item3)
			  (if (< 10 (+ item1 item2 item3))
			      (vector item1 item2 item3)
			    #f))
	'#(1 2 3)
	'#(1.1 2.2 3.3)
	'#(1.11 2.22 3.33))
    => #f)

;;; --------------------------------------------------------------------

  (check
      ($vector-exists/list (lambda (item1 item2 item3 item4)
			     (if (< 10 (+ item1 item2 item3 item4))
				 (vector item1 item2 item3 item4)
			       #f))
	'(#()
	  #()
	  #()
	  #()))
    => #f)

  (check
      ($vector-exists/list (lambda (item1 item2 item3 item4)
			     (if (< 10 (+ item1 item2 item3 item4))
				 (vector item1 item2 item3 item4)
			       #f))
	'(#(1 3 5 7)
	  #(2 4 6 8)
	  #(3 5 7 9)
	  #(4 6 8 10)))
    => '#(3 4 5 6))

  (check
      ($vector-exists/list (lambda (item1 item2 item3 item4)
			     (if (< 100 (+ item1 item2 item3 item4))
				 (vector item1 item2 item3 item4)
			       #f))
	'(#(1 2 3)
	  #(1.1 2.2 3.3)
	  #(1.11 2.22 3.33)
	  #(1.111 2.222 3.333)))
    => #f)

  (values))


(parameterise ((check-test-name		'find))

  (check
      (vector-find even? '#())
    => #f)

  (check
      (vector-find even? '#(1))
    => #f)

  (check
      (vector-find even? '#(2))
    => 2)

  (check
      (vector-find even? '#(1 2 3))
    => 2)

  (values))


(parameterise ((check-test-name		'unsafe-find))

  (check
      ($vector-find even? '#())
    => #f)

  (check
      ($vector-find even? '#(1))
    => #f)

  (check
      ($vector-find even? '#(2))
    => 2)

  (check
      ($vector-find even? '#(1 2 3))
    => 2)

  (values))


(parameterise ((check-test-name		'copying))

  (check
      (let ((src.vec	'#(a b c d e))
	    (dst.vec	(make-vector 5 #f)))
	(vector-copy dst.vec 0
		     src.vec 0 (vector-length src.vec)))
    => '#(a b c d e))

  (check
      (let ((src.vec	'#(a b c d e))
	    (dst.vec	(make-vector 5 #f)))
	(vector-copy dst.vec 0
		     src.vec 3 3))
    => '#(#f #f #f #f #f))

  (check
      (let ((src.vec	'#(a b c d e))
	    (dst.vec	(make-vector 5 #f)))
	(vector-copy dst.vec 1
		     src.vec 1 4))
    => '#(#f b c d #f))

  (values))


(parameterise ((check-test-name		'sorted))

  (define (symbol<? a b)
    (string<? (symbol->string a)
	      (symbol->string b)))

  (check
      (sorted-vector-binary-search < '#() 1)
    => #f)

  (check
      (sorted-vector-binary-search < '#(0 1 2 3 4 5 6 7 8 9) 999)
    => #f)

  (check
      (sorted-vector-binary-search < '#(0 1 2 3 4 5 6 7 8 9) -999)
    => #f)

  (do ((i 0 (add1 i)))
      ((= i 10))
    (check
	(sorted-vector-binary-search < '#(0 1 2 3 4 5 6 7 8 9) i)
      => i))

  (do ((i 0 (add1 i)))
      ((= i 10))
    (let* ((vec '#(a b c d e f g h i l m))
	   (sym (vector-ref vec i)))
      (check
	  (sorted-vector-binary-search symbol<? vec sym)
	=> i)))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
