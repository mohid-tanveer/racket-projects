#lang racket

; Set of functions related to dates and lists. Helps practice data manipulation within a functional
; programming language.


; 1. before? - returns which date comes first. 
; example: (before? '(2017 4 2) '(2017 5 1)) ==> #t
(define (before? lst1 lst2)
  (if (null? lst1)
      #f
      (if (< (car lst1) (car lst2))
          #t
          (if (= (car lst1) (car lst2))
              (before? (cdr lst1) (cdr lst2))
              #f))))
; 2. number-in-month - takes a lst of dates and returns how many dates are in the month specified
; by value. 
; example: (number-in-month '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1)) 2) ==> 2
(define (number-in-month lst value)
  (if (null? lst)
      0
      (if (= (car (cdr (car lst))) value)
          (+ (number-in-month (cdr lst) value) 1)
          (number-in-month (cdr lst) value))))
; 3. number-in-months - same as above, however now takes a list of months. 
; example: '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1)) '(12 2)) ==> 3
(define (number-in-months lst1 lst2)
  (+ (number-in-month lst1 (car lst2)) (number-in-month lst1 (car (cdr lst2)))))
; 4. dates-in-month - returns lst of dates that are in the months specified by value.
; example: (dates-in-month '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1)) 2) ==> 
; '((2016 2 1) (2019 2 3))
(define (dates-in-month lst value)
  (if (null? lst)
      '()
      (if (= (car (cdr (car lst))) value)
          (cons (car lst) (dates-in-month (cdr lst) value))
          (dates-in-month (cdr lst) value))))
; 5. dates-in-months - same as above but with lst of months. 
; example: (dates-in-months '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1)) '(12 2)) 
; ==> '((2017 12 1) (2016 2 1) (2019 2 3))
(define (dates-in-months lst1 lst2)
  (append (dates-in-month lst1 (car lst2)) (dates-in-month lst1 (car (cdr lst2)))))
; 6. get-nth - returns nth element. 
; example: (get-nth '(7 5 3 8 1) 2) ==> 5
(define (get-nth lst int)
  (if (= int 1)
      (car lst)
      (get-nth (cdr lst) (- int 1))))
; 7. date->string - returns string representation of date. 
; example: (date->string '(2016 7 29)) ==> "July 29, 2016"
(define (date->string lst)
  (string-append (get-nth '("January" "February" "March" "April" "May" "June" "July" "August" "September" "November" "December") (car (cdr lst))) " " (number->string (car (cdr (cdr lst)))) ", " (number->string (car lst))))
; 8. number-before-reaching-sum - returns how many numbers in the list you must sum up to either equal
; or surpass the sum given. 
; examples: 
; (number-before-reaching-sum 2 '(3 2 4 2 5)) ==> 1
; (number-before-reaching-sum 3 '(3 2 4 2 5)) ==> 1
; (number-before-reaching-sum 4 '(3 2 4 2 5)) ==> 2
; (number-before-reaching-sum 5 '(3 2 4 2 5)) ==> 2
; (number-before-reaching-sum 6 '(3 2 4 2 5)) ==> 3
(define (number-before-reaching-sum sum lst)
  (if (<= sum (car lst))
      1
      (+ 1 (number-before-reaching-sum (- sum (car lst)) (cdr lst)))))
; 9. what-month - returns what month you would be in based on what day of the year is specified.
; example: (what-month 32) ==> 2. or (what-month 31) ==> 1.
(define (what-month int)
  (number-before-reaching-sum int '(31 28 31 30 31 30 31 31 30 31 30 31)))
; 10. month-range - takes two days and returns a list of months that each day between the two days
; land on. 
; example: (month-range 30 34) ==> '(1 1 2 2 2)
(define (month-range day1 day2)
  (if (> day1 day2)
      '()
      (cons (what-month day1) (month-range (+ day1 1) day2))))
; 11. earliest - returns earliest date of all specified dates. 
; example: (earliest '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1))) ==> '(2016 2 1)
(define (earliest lst)
  (earliest-helper (cdr lst) (car lst)))
(define (earliest-helper lst curr)
  (cond
    ((null? lst) curr)
    ((before? (car lst) curr)
     (earliest-helper (cdr lst) (car lst)))
    (else
     (earliest-helper (cdr lst) curr))))