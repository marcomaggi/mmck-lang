;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lang
;;;Contents: test program list functions
;;;Date: Jul 29, 2019
;;;
;;;Abstract
;;;
;;;	This program tests list functions.
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

(module (test-lang-lists)
    ()
  (import (scheme)
	  (only (chicken base)
		void)
	  (mmck lang)
	  (mmck exceptional-conditions)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing language extensions: list functions\n")


;;;; helpers

(define-syntax check-lists-are-of-different-length
  (syntax-rules ()
    ((_ ?expr)
     (check-for-true
      (guard (E ((condition-lists-are-of-different-length? E)
		 #t)
		(else #f))
	?expr)))
    ))

(define-syntax check-lists-are-empty-or-of-different-length
  (syntax-rules ()
    ((_ ?expr)
     (check-for-true
      (guard (E ((condition-lists-are-empty-or-of-different-length? E)
		 #t)
		(else #f))
	?expr)))
    ))

(define (xcons a b)
  (cons b a))


(parameterise ((check-test-name	'constructors))

  (begin
    (check (cons* 1)		=> 1)
    (check (cons* 1 2)		=> '(1 . 2))
    (check (cons* 1 2 3)	=> '(1 2 . 3))
    (check (cons* 1 '(2 3 4))	=> '(1 2 3 4))
    (check (cons* 1 2 '(3 4))	=> '(1 2 3 4))
    (check (cons* 1 2 3 '(4))	=> '(1 2 3 4))
    #| end of BEGIN |# )

;;; --------------------------------------------------------------------

  (check (make-list 0)		=> '())
  (check (make-list 0 'a)	=> '())

  (check (make-list 3)		=> (list (void) (void) (void)))
  (check (make-list 3 'a)	=> '(a a a))

  (values))


(parameterise ((check-test-name	'predicates))

  (begin
    (check-for-true (list-of-lists? '()))
    (check-for-true (list-of-lists? '(())))
    (check-for-true (list-of-lists? '(() ())))
    (check-for-true (list-of-lists? '(() () ())))
    (check-for-true (list-of-lists? '((1))))
    (check-for-true (list-of-lists? '((1) (2))))
    (check-for-true (list-of-lists? '((1) (2) (3))))
    ;;
    (check-for-false (list-of-lists? '(1 (2) (3))))
    (check-for-false (list-of-lists? '((1) 2 (3))))
    (check-for-false (list-of-lists? '((1) (2) 3)))
    ;;
    (check-for-false (list-of-lists? '#()))
    #| end of BEGIN |# )

;;; --------------------------------------------------------------------

  (begin
    (check-for-true  (list-of-pairs? '()))
    (check-for-false (list-of-pairs? '(())))
    (check-for-false (list-of-pairs? '(() ())))
    (check-for-false (list-of-pairs? '(() () ())))
    (check-for-true  (list-of-pairs? '((1))))
    (check-for-true  (list-of-pairs? '((1) (2))))
    (check-for-true  (list-of-pairs? '((1) (2) (3))))
    ;;
    (check-for-false (list-of-pairs? '(1 (2) (3))))
    (check-for-false (list-of-pairs? '((1) 2 (3))))
    (check-for-false (list-of-pairs? '((1) (2) 3)))
    ;;
    (check-for-false (list-of-pairs? '#()))
    #| end of BEGIN |# )

;;; --------------------------------------------------------------------

  (begin
    (check-for-true (list-of-nulls? '()))
    (check-for-true (list-of-nulls? '(())))
    (check-for-true (list-of-nulls? '(() ())))
    (check-for-true (list-of-nulls? '(() () ())))
    (check-for-false (list-of-nulls? '((1))))
    (check-for-false (list-of-nulls? '((1) (2))))
    (check-for-false (list-of-nulls? '((1) (2) (3))))
    ;;
    (check-for-false (list-of-nulls? '(1 (2) (3))))
    (check-for-false (list-of-nulls? '((1) 2 (3))))
    (check-for-false (list-of-nulls? '((1) (2) 3)))
    ;;
    (check-for-false (list-of-nulls? '#()))
    #| end of BEGIN |# )

;;; --------------------------------------------------------------------

  (begin
    (check-for-true (lists-of-lists-of-equal-length? '()))
    (check-for-true (lists-of-lists-of-equal-length? '(())))
    (check-for-true (lists-of-lists-of-equal-length? '(() ())))
    (check-for-true (lists-of-lists-of-equal-length? '(() () ())))
    (check-for-true (lists-of-lists-of-equal-length? '((1))))
    (check-for-true (lists-of-lists-of-equal-length? '((1) (2))))
    (check-for-true (lists-of-lists-of-equal-length? '((1) (2) (3))))
    ;;
    (check-for-false (lists-of-lists-of-equal-length? '(() (2) (3))))
    (check-for-false (lists-of-lists-of-equal-length? '((1) () (3))))
    (check-for-false (lists-of-lists-of-equal-length? '((1) (2) ())))
    (check-for-false (lists-of-lists-of-equal-length? '((1 0) (2) (3))))
    (check-for-false (lists-of-lists-of-equal-length? '((1) (2 0) (3))))
    (check-for-false (lists-of-lists-of-equal-length? '((1) (2) (3 0))))
    ;;
    (check-for-true (lists-of-lists-of-equal-length? '((1 2 3 4 5))))
    (check-for-true (lists-of-lists-of-equal-length? '((1 2 3 4 5)
						       (6 7 8 9 0))))
    (check-for-true (lists-of-lists-of-equal-length? '((1 2 3 4 5)
						       (6 7 8 9 0)
						       (9 8 7 6 5))))
    ;;
    (check-for-false (lists-of-lists-of-equal-length? 123))
    #| end of BEGIN |# )

;;; --------------------------------------------------------------------

  (begin
    (check-for-true (lists-of-equal-length? '()))
    (check-for-true (lists-of-equal-length? '() '()))
    (check-for-true (lists-of-equal-length? '() '() '()))
    (check-for-true (lists-of-equal-length? '(1)))
    (check-for-true (lists-of-equal-length? '(1) '(2)))
    (check-for-true (lists-of-equal-length? '(1) '(2) '(3)))
    ;;
    (check-for-false (lists-of-equal-length? '() '(2) '(3)))
    (check-for-false (lists-of-equal-length? '(1) '() '(3)))
    (check-for-false (lists-of-equal-length? '(1) '(2) '()))
    (check-for-false (lists-of-equal-length? '(1 0) '(2) '(3)))
    (check-for-false (lists-of-equal-length? '(1) '(2 0) '(3)))
    (check-for-false (lists-of-equal-length? '(1) '(2) '(3 0)))
    ;;
    (check-for-true (lists-of-equal-length? '(1 2 3 4 5)))
    (check-for-true (lists-of-equal-length? '(1 2 3 4 5)
					    '(6 7 8 9 0)))
    (check-for-true (lists-of-equal-length? '(1 2 3 4 5)
					    '(6 7 8 9 0)
					    '(9 8 7 6 5)))
    (check-for-true (lists-of-equal-length? '(1 2 3 4 5)
					    '(6 7 8 9 0)
					    '(9 8 7 6 5)
					    '(#\1 #\2 #\3 #\4 #\5)))
    (check-for-true (lists-of-equal-length? '(1 2 3 4 5)
					    '(6 7 8 9 0)
					    '(9 8 7 6 5)
					    '(#\1 #\2 #\3 #\4 #\5)
					    '(#\6 #\7 #\8 #\9 #\0)))

    (check-for-false (lists-of-equal-length? '(1 2 3 4)
					     '(6 7 8 9 0)
					     '(9 8 7 6 5)
					     '(#\1 #\2 #\3 #\4 #\5)
					     '(#\6 #\7 #\8 #\9 #\0)))
    (check-for-false (lists-of-equal-length? '(1 2 3 4 5)
					     '(6 7 8 9)
					     '(9 8 7 6 5)
					     '(#\1 #\2 #\3 #\4 #\5)
					     '(#\6 #\7 #\8 #\9 #\0)))
    (check-for-false (lists-of-equal-length? '(1 2 3 4 5)
					     '(6 7 8 9 0)
					     '(9 8 7 6)
					     '(#\1 #\2 #\3 #\4 #\5)
					     '(#\6 #\7 #\8 #\9 #\0)))
    (check-for-false (lists-of-equal-length? '(1 2 3 4 5)
					     '(6 7 8 9 0)
					     '(9 8 7 6 5)
					     '(#\1 #\2 #\3 #\4)
					     '(#\6 #\7 #\8 #\9 #\0)))
    (check-for-false (lists-of-equal-length? '(1 2 3 4 5)
					     '(6 7 8 9 0)
					     '(9 8 7 6 5)
					     '(#\1 #\2 #\3 #\4 #\5)
					     '(#\6 #\7 #\8 #\9)))
    ;;
    (check-for-false (lists-of-equal-length? 123))
    #| end of BEGIN |# )

  (values))


(parameterise ((check-test-name	'getters))

  ;;No cars tail.
  ;;
  (begin
    (check-lists-are-empty-or-of-different-length (cars-and-cdrs '(())))
    (check-lists-are-empty-or-of-different-length (cars-and-cdrs '(() ())))
    (check-lists-are-empty-or-of-different-length (cars-and-cdrs '(() () ())))
    ;;
    (check (cars-and-cdrs '((1 2 3) (4 5 6)))		=> '(1 4) '((2 3) (5 6)))
    (check (cars-and-cdrs '((1 2 3) (4 5 6) (7 8 9)))	=> '(1 4 7) '((2 3) (5 6) (8 9)))
    ;;
    (check-lists-are-empty-or-of-different-length (cars-and-cdrs '(() (2) (3))))
    (check-lists-are-empty-or-of-different-length (cars-and-cdrs '((1) () (3))))
    (check-lists-are-empty-or-of-different-length (cars-and-cdrs '((1) (2) ())))
    #| end of BEGIN |# )

  ;;Select a cars tail.
  ;;
  (begin
    (check
	(try
	    (cars-and-cdrs '()
			   '(a b))
	  (catch E
	    ((&lists-are-empty-or-of-different-length)
	     (values (condition-who E)
		     (condition-irritants E)))
	    (else
	     (values #f #f))))
      => 'cars-and-cdrs '(()))

    (check
	(try
	    (cars-and-cdrs '(() () ())
			   '(a b))
	  (catch E
	    ((&lists-are-empty-or-of-different-length)
	     (values (condition-who E)
		     (condition-irritants E)))
	    (else
	     (values #f #f))))
      => 'cars-and-cdrs '((() () ())))

    (check
	(cars-and-cdrs '((1 2 3) (4 5 6))
		       '(a b))
      => '(1 4 a b) '((2 3) (5 6)))

    (check
	(cars-and-cdrs '((1 2 3) (4 5 6) (7 8 9))
		       '(a b))
      => '(1 4 7 a b) '((2 3) (5 6) (8 9)))
    #| end of BEGIN |# )

;;; --------------------------------------------------------------------

  ;;No tail.
  ;;
  (begin
    (check (gather-cars-in-reverse '())			=> '())
    (check (gather-cars-in-reverse '(()))		=> '())
    (check (gather-cars-in-reverse '(() ()))		=> '())
    (check (gather-cars-in-reverse '(() () ()))		=> '())
    (check (gather-cars-in-reverse '((1) (4) (7)))	=> '((1 4 7)))
    (check
	(gather-cars-in-reverse '((1 2 3)
				  (4 5 6)
				  (7 8 9)))
      => '((3 6 9)
	   (2 5 8)
	   (1 4 7)))
    #| end of BEGIN |# )

  ;;Explicitly select a tail.
  ;;
  (let ((tail '(a b)))
    (check
	(gather-cars-in-reverse '()		tail)
      => '())
    (check
	(gather-cars-in-reverse '(())		tail)
      => '())
    (check
	(gather-cars-in-reverse '(() ())	tail)
      => '())
    (check
	(gather-cars-in-reverse '(() () ())	tail)
      => '())
    (check
	(gather-cars-in-reverse '((1) (4) (7))	tail)
      => '((1 4 7 a b)))
    (check
	(gather-cars-in-reverse '((1 2 3)
				  (4 5 6)
				  (7 8 9))
				tail)
      => '((3 6 9 a b)
	   (2 5 8 a b)
	   (1 4 7 a b)))
    #| end of BEGIN |# )

;;; --------------------------------------------------------------------

  (begin
    (check (butlast-and-last '())		=> '() (void))
    (check (butlast-and-last '(0))		=> '() 0)
    (check (butlast-and-last '(0 1))		=> '(0) 1)
    (check (butlast-and-last '(0 1 2))		=> '(0 1) 2)
    (check (butlast-and-last '(0 1 2 3))	=> '(0 1 2) 3)
    (check (butlast-and-last '(0 1 2 3 4))	=> '(0 1 2 3) 4)
    (check (butlast-and-last '(0 1 2 3 4 5))	=> '(0 1 2 3 4) 5)
    (check (butlast-and-last '(0 1 2 3 4 5 6))	=> '(0 1 2 3 4 5) 6)
    #| end of BEGIN |# )

  (values))


(parameterise ((check-test-name		'fold-left))

  (check
      (fold-left
	  (lambda (knil item)
	    (+ knil item))
	0
	'())
    => 0)


  (check
      (fold-left
	  (lambda (knil item)
	    (+ knil item))
	0
	'(1 2 3 4))
    => (+ 1 2 3 4))

  (check
      (fold-left
	  cons
	0
	'(1 2 3 4))
    => '((((0 . 1) . 2) . 3) . 4))

  (check
      (fold-left
	  xcons
	'()
	'(1 2 3 4))
    => '(4 3 2 1))

;;; --------------------------------------------------------------------

  ;;2 lists
  ;;
  (check
      (fold-left
	  (lambda (knil item1 item2)
	    (+ knil item1 item2))
	0
	'(1 2 3 4)
	'(5 6 7 8))
    => (+ 1 2 3 4 5 6 7 8))

  (check
      (fold-left
	  list
	0
	'(1 2 3 4)
	'(5 6 7 8))
    => '((((0 1 5) 2 6) 3 7) 4 8))

;;; --------------------------------------------------------------------

  ;;3 lists
  ;;
  (check
      (fold-left
	  (lambda (knil item1 item2 item3)
	    (+ knil item1 item2 item3))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3))
    => (+ 1 2 3 4 5 6 7 8 9 1 2 3))

  ;;3 lists
  ;;
  (check
      (fold-left
	  list
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3))
    => '((((0 1 5 9) 2 6 1) 3 7 2) 4 8 3))

;;; --------------------------------------------------------------------

  ;;more lists
  ;;
  (check
      (fold-left
	  (lambda (knil item1 item2 item3 item4)
	    (+ knil item1 item2 item3 item4))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3)
	'(4 5 6 7))
    => (+ 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7))

  (check
      (fold-left
	  (lambda (knil item1 item2 item3 item4)
	    (list knil item1 item2 item3 item4))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3)
	'(4 5 6 7))
    => '((((0 1 5 9 4) 2 6 1 5) 3 7 2 6) 4 8 3 7))

  (values))


(parameterise ((check-test-name		'unsafe-fold-left))

  (check
      ($fold-left/1
	  (lambda (knil item)
	    (+ knil item))
	0
	'())
    => 0)

  (check
      ($fold-left/1
	  (lambda (knil item)
	    (+ knil item))
	0
	'(1 2 3 4))
    => (+ 1 2 3 4))

  (check
      ($fold-left/1
	  cons
	0
	'(1 2 3 4))
    => '((((0 . 1) . 2) . 3) . 4))

  (check
      ($fold-left/1
	  xcons
	'()
	'(1 2 3 4))
    => '(4 3 2 1))

;;; --------------------------------------------------------------------

  ;;2 lists
  ;;
  (check
      ($fold-left/2
	  (lambda (knil item1 item2)
	    (+ knil item1 item2))
	0
	'(1 2 3 4)
	'(5 6 7 8))
    => (+ 1 2 3 4 5 6 7 8))

  (check
      ($fold-left/2
	  list
	0
	'(1 2 3 4)
	'(5 6 7 8))
    => '((((0 1 5) 2 6) 3 7) 4 8))

;;; --------------------------------------------------------------------

  ;;3 lists
  ;;
  (check
      ($fold-left/3
	  (lambda (knil item1 item2 item3)
	    (+ knil item1 item2 item3))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3))
    => (+ 1 2 3 4 5 6 7 8 9 1 2 3))

  ;;3 lists
  ;;
  (check
      ($fold-left/3
	  list
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3))
    => '((((0 1 5 9) 2 6 1) 3 7 2) 4 8 3))

;;; --------------------------------------------------------------------

  ;;more lists
  ;;
  (check
      ($fold-left/any
	  (lambda (knil item1 item2 item3 item4)
	    (+ knil item1 item2 item3 item4))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3)
	'((4 5 6 7)))
    => (+ 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7))

  (check
      ($fold-left/any
	  (lambda (knil item1 item2 item3 item4)
	    (list knil item1 item2 item3 item4))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3)
	'((4 5 6 7)))
    => '((((0 1 5 9 4) 2 6 1 5) 3 7 2 6) 4 8 3 7))

  (check
      ($fold-left/any
	  (lambda (knil item1 item2 item3 item4 item5)
	    (cons knil (list item1 item2 item3 item4 item5)))
	'(0)
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3)
	'((4 5 6 7)
	  (a b c d)))
    => '(((((0) 1 5 9 4 a) 2 6 1 5 b) 3 7 2 6 c) 4 8 3 7 d))

  (values))


(parameterise ((check-test-name		'fold-right))

  (check
      (fold-right
	  (lambda (item knil)
	    (+ item knil))
	0
	'())
    => 0)


  (check
      (fold-right
	  (lambda (item knil)
	    (+ item knil))
	0
	'(1 2 3 4))
    => (+ 1 2 3 4))

  (check
      (fold-right
	  (lambda (item knil)
	    (cons item knil))
	0
	'(1 2 3 4))
    => '(1 2 3 4 . 0))

;;; --------------------------------------------------------------------

  ;;2 lists
  ;;
  (check
      (fold-right
	  (lambda (item1 item2 knil)
	    (+ knil item1 item2))
	0
	'(1 2 3 4)
	'(5 6 7 8))
    => (+ 1 2 3 4 5 6 7 8))

  (check
      (fold-right
	  (lambda (item1 item2 knil)
	    (cons (list item1 item2) knil))
	0
	'(1 2 3 4)
	'(5 6 7 8))
    => '((1 5) (2 6) (3 7) (4 8) . 0))

;;; --------------------------------------------------------------------

  ;;3 lists
  ;;
  (check
      (fold-right
	  (lambda (item1 item2 item3 knil)
	    (+ knil item1 item2 item3))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3))
    => (+ 1 2 3 4 5 6 7 8 9 1 2 3))

  ;;3 lists
  ;;
  (check
      (fold-right
	  (lambda (item1 item2 item3 knil)
	    (cons (list item1 item2 item3) knil))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3))
    => '((1 5 9) (2 6 1) (3 7 2) (4 8 3) . 0))

;;; --------------------------------------------------------------------

  ;;more lists
  ;;
  (check
      (fold-right
	  (lambda (item1 item2 item3 item4 knil)
	    (+ knil item1 item2 item3 item4))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3)
	'(4 5 6 7))
    => (+ 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7))

  (check
      (fold-right
	  (lambda (item1 item2 item3 item4 knil)
	    (cons (list item1 item2 item3 item4) knil))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3)
	'(4 5 6 7))
    => '((1 5 9 4) (2 6 1 5) (3 7 2 6) (4 8 3 7) . 0))

  (values))


(parameterise ((check-test-name		'unsafe-fold-right))

  (check
      ($fold-right/1
	  (lambda (item knil)
	    (+ item knil))
	0
	'())
    => 0)


  (check
      ($fold-right/1
	  (lambda (item knil)
	    (+ item knil))
	0
	'(1 2 3 4))
    => (+ 1 2 3 4))

  (check
      ($fold-right/1
	  (lambda (item knil)
	    (cons item knil))
	0
	'(1 2 3 4))
    => '(1 2 3 4 . 0))

;;; --------------------------------------------------------------------

  ;;2 lists
  ;;
  (check
      ($fold-right/2
	  (lambda (item1 item2 knil)
	    (+ knil item1 item2))
	0
	'(1 2 3 4)
	'(5 6 7 8))
    => (+ 1 2 3 4 5 6 7 8))

  (check
      ($fold-right/2
	  (lambda (item1 item2 knil)
	    (cons (list item1 item2) knil))
	0
	'(1 2 3 4)
	'(5 6 7 8))
    => '((1 5) (2 6) (3 7) (4 8) . 0))

;;; --------------------------------------------------------------------

  ;;3 lists
  ;;
  (check
      ($fold-right/3
	  (lambda (item1 item2 item3 knil)
	    (+ knil item1 item2 item3))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3))
    => (+ 1 2 3 4 5 6 7 8 9 1 2 3))

  ;;3 lists
  ;;
  (check
      ($fold-right/3
	  (lambda (item1 item2 item3 knil)
	    (cons (list item1 item2 item3) knil))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3))
    => '((1 5 9) (2 6 1) (3 7 2) (4 8 3) . 0))

;;; --------------------------------------------------------------------

  ;;more lists
  ;;
  (check
      ($fold-right/any
	  (lambda (item1 item2 item3 item4 knil)
	    (+ knil item1 item2 item3 item4))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3)
	'((4 5 6 7)))
    => (+ 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7))

  (check
      ($fold-right/any
	  (lambda (item1 item2 item3 item4 item5 knil)
	    (cons (list item1 item2 item3 item4 item5) knil))
	0
	'(1 2 3 4)
	'(5 6 7 8)
	'(9 1 2 3)
	'((4 5 6 7)
	  (a b c d)))
    => '((1 5 9 4 a) (2 6 1 5 b) (3 7 2 6 c) (4 8 3 7 d) . 0))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
