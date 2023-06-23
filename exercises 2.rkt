#lang racket

; Practices data manipulation using higher-order functions in a functional programming language.


; only-capitals - takes a list of strings and returns a list of strings that start with a capital 
; letter. 
; example: (only-capitals ’("I" "go" "to" "Rhodes" "College" "in" "Memphis"))
; ==> ’("I" "Rhodes" "College" "Memphis")
(define (only-capitals lst)
  (filter (lambda (x) (char-upper-case? (string-ref x 0))) lst))

; longest-string1 - takes a list of strings and returns the longest string in the list. If list is 
; empty returns empty string. If there is a tie return first one. 
; example: (longest-string1 ’("I" "go" "to" "Rhodes" "College" "in" "Memphis")) ==> "College"
(define (longest-string1 lst)
  (if (null? lst)
      ""
      (car (foldr (lambda (str acc)
                    (let ((len (string-length str))
                          (longest (car acc)))
                      (cond ((> len (string-length longest))
                             (list str))
                            ((= len (string-length longest))
                             (cons str acc))
                            (else acc))))
                  '("")
                  lst))))

; longest-string2 - works exactly the same as above but returns the later string if there's a tie.
; example: (longest-string1 ’("I" "go" "to" "Rhodes" "College" "in" "Memphis")) ==> "Memphis"
(define (longest-string2 lst)
  (foldr (lambda (str longest)
           (if (> (string-length str) (string-length longest))
               str
               longest))
         ""
         lst))

; map2 - works just like the function map but takes two lists of equal length  and a function of two
; arguments. 
; example: (map2 + ’(1 2 3) ’(4 5 6)) ==> ’(5 7 9)
(define (map2 func lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (func (car lst1) (car lst2))
            (map2 func (cdr lst1) (cdr lst2)))))

; map-any - maps any function with any lst-of-lsts. 
; examples:
; (map-any + ’((1 2 3) (4 5 6) (7 8 9))) ==> ’(12 15 18)
; (map-any (lambda (a b c d) (if (> a b) c d)) ’((1 4) (2 3) (10 20) (30 40))) ==> ’(30 20)
(define (map-any func lst-of-lsts)
  (define (get-cars lst-of-lsts)
    (if (null? lst-of-lsts)
        '()
        (cons (car (car lst-of-lsts))
              (get-cars (cdr lst-of-lsts)))))
  (define (get-cdrs lst-of-lsts)
    (if (null? lst-of-lsts)
        '()
        (cons (cdr (car lst-of-lsts))
              (get-cdrs (cdr lst-of-lsts)))))
  (if (null? (car lst-of-lsts))
      '()
      (cons (apply func (get-cars lst-of-lsts))
            (map-any func (get-cdrs lst-of-lsts)))))
     
; least - takes any function and returns the 'least' element. 
; examples:
; (least < ’(2 4 3 5 1 7 6)) ==> 1
; (least string<? ’("I" "go" "to" "Rhodes" "College" "in" "Memphis")) ==> "College"
(define (least f lst)
  (define (helper smallest lst)
    (cond ((null? lst) smallest)
          ((f (car lst) smallest) (helper (car lst) (cdr lst)))
          (#t (helper smallest (cdr lst)))))
  (helper (car lst) (cdr lst)))

; lookup - searches for the first element in the lst that satisfies the function (predicate).
; returns false if none found.
; example: (lookup (lambda (x) (< x 3)) ’((4 "Alice") (2 "Bob") (1 "Carl"))) ==> "Bob"
(define (lookup func lst)
  (define (helper lst)
    (cond ((null? lst) #f)
          ((func (car (car lst))) (cdr (car lst)))
          (#t (helper (cdr lst)))))
  (helper lst))

; power-set - returns power-set of lst.
; example: (power-set ’(1 2 3)) ==> ’(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(define (power-set lst)
  (foldr (lambda (x acc)
           (append acc (map (lambda (y) (cons x y)) acc)))
         (list empty)
         lst))

; dot-product - takes dot product of values. 
; example: (dot-product ’(1 2 3) ’(4 5 6)) ==> 32
(define (dot-product v w)
  (foldr + 0 (map2 * v w)))

; matrix-*-vector - takes a lst of lsts and multiplies it by the vector.
; example: (matrix-*-vector ’((1 2 3) (4 5 6)) ’(-1 1 2)) ==> ’(7 13)
(define (matrix-*-vector lst-of-lsts lst)
  (map (lambda (x) (foldr + 0 (map * x lst))) lst-of-lsts))

; transpose - transposes the matrix. 
; example: (transpose ’((1 2 3) (4 5 6))) ==> ’((1 4) (2 5) (3 6))
(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m) (transpose (map cdr m)))))

; matrix-*-matrix - multiplies two matrices. 
; example:
; (define A ’((1 2 3) (4 5 6)))
; (define B (transpose A))
; (matrix-*-matrix A B) ==> ’((14 32) (32 77))
; (matrix-*-matrix B A) ==> ’((17 22 27) (22 29 36) (27 36 45))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (map (lambda (y) (dot-product x y)) cols)) m)))