#lang racket
(require rsound)

; Practices using streams and music library within racket.

; stream library
(define-syntax-rule (stream-cons first rest)
  (cons first (delay rest)))
(define the-empty-stream '())
(define stream-null? null?)
(define stream-car car)
(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-to-sound stream)
  (define (next-value)
    (let ((v (stream-car stream)))
      (set! stream (stream-cdr stream))
      v))
  next-value)

; Play a stream as a sound.
(define (play-stream s)
  (signal-play (stream-to-sound s)))

; Turn a finite stream into a list.
(define (stream->list stream)
  (if (stream-null? stream) the-empty-stream
      (cons (stream-car stream) (stream->list stream))))

; Get a specific element of a stream (zero-based indexing).
(define (stream-ref stream index)
  (if (= index 0) (stream-car stream)
      (stream-ref (stream-cdr stream) (- index 1))))

; Get a list of the first n elements of a stream.
(define (stream-enumerate stream n)
  (if (= n 0) '()
      (cons (stream-car stream)
            (stream-enumerate (stream-cdr stream) (- n 1)))))

; Construct a new stream by transforming an existing one.
; f must be a function of one argument.
(define (stream-map f s)
  (if (stream-null? s) the-empty-stream
      (stream-cons (f (stream-car s)) (stream-map f (stream-cdr s)))))

; Construct a new stream by transforming an existing one.
; f must be a function of two arguments.
(define (stream-map2 f s1 s2)
  (if (or (stream-null? s1) (stream-null? s2)) the-empty-stream
      (stream-cons (f (stream-car s1) (stream-car s2)) (stream-map2 f (stream-cdr s1) (stream-cdr 
s2)))))

; Construct a new stream by filtering an existing one.
(define (stream-filter f s)
  (if (stream-null? s) the-empty-stream
      (if (f (stream-car s)) (stream-cons (stream-car s) (stream-filter f (stream-cdr s)))
          (stream-filter f (stream-cdr s)))))

;PART A!

; Recursively creates an infinite stream.
(define (make-recursive-stream f init)
  (stream-cons init (make-recursive-stream f (f init))))

; Creates a pascal triangle with lists.
(define pascal
  (make-recursive-stream (lambda (row) (map + (append row '(0)) (cons 0 row))) '(1)))

; If a stream is made up of lists, this function will make a stream without the individual lists internally.
(define (stream-flatten s)
  (append (car s) (delay (stream-flatten (stream-cdr s)))))

; Merges two in order streams into one in order stream.
(define (stream-merge s1 s2)
  (if (> (car s1) (car s2))
      (stream-cons (car s2) (stream-merge s1 (stream-cdr s2)))
      (stream-cons (car s1) (stream-merge (stream-cdr s1) s2))))

;PART B!

; Sine function!
(define (make-sine-function freq)
  (lambda (t) (* 0.2 (sin (* pi freq t)))))

; Creates an infinite stream of numbers from zero and increasing by 1/44100ths.
(define sampling-stream
  (make-recursive-stream (lambda (x) (+ x (/ 1 44100))) 0))

; Creates sine-stream.
(define (make-sine-stream freq)
  (stream-map (make-sine-function freq) sampling-stream))

; Creates the sine stream circularly.
(define (make-circular-stream lst)
  (define (helper lst first)
    (if (null? (cdr lst))
        (stream-cons (car lst) (force first))
        (stream-cons (car lst) (helper (cdr lst) first))))
  (define s (helper lst (delay s)))
  s)