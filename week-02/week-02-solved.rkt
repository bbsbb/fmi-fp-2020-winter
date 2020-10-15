#lang racket

;; This is still a very basic but let's fast recap.

;; How do you define a procedure? Show?

;; What is scope? What is the scope of your procedure?

;; Ok, in a functional language, how are we going to loop for a start?

;; Exercise 1:
;; Let's write a predicate that returns true if 3 numeric parameters are larger in order
;; (in-order? 3 5 11) => #t
;; (in-order? 3 7 5) => #f
(define (in-order? m n y)
  (< m n y))


;; Exercise 2:
;; Let's do a factorial, but do it fast.
;; (factorial 8) -> 40320
(define (factorial n)
  (cond
   [(= n 1) 1]
   [else (* n (factorial (- n 1)))]))


;; Exercise 3:
;; Same shit, but for fibonacci
;; (fib 27) -> 196418
(define (fib n)
  (cond
   [(= 0 n) 0] ;; Or do 1 case???
   [(= 1 n) 1]
   [else (+ (fib (- n 1)) (fib (- n 2)))]))


;; Exercise 4:
;; Write a predicate that checks if a number is divisible by another
;; (divisible? 4 2) => #t
;; (divisible? 4 3) => #
(define (divisible? n k) (= (remainder n k) 0))


;; Exercise 5:
;; Ok, can we write a function that reverse a number? What would the algo look like?
;; (reverse-digits 1234) -> 4321
(define (reverse-digits n)
  (define (reverse-digits-helper n acc)
    (cond
     [(= n 0) acc]
     [else (reverse-digits-helper (quotient n 10)
                                  (+ (* 10 acc) (remainder n 10)))]))
  (reverse-digits-helper n 0))


;; Exercise 6:
;; We do this one because you may need it. What are primes? Write the f
;; (prime? 17) -> #t
;; (prime 725) -> #f
(define (prime? n)
  (define max-prime-factor-n (floor (sqrt n)))
  ;; This isn't an fn ^^^
  (define (prime-helper? i)
    (cond
     [(> i max-prime-factor-n) #t]
     [(divisible? n i) #f]
     [else (prime-helper? (+ i 1))]))
  (and (> n 1) (prime-helper? 2)))




























(define (prime? n)
  (define max-prime-factor-n (floor (sqrt n)))
  ;; This isn't an fn ^^^
  (define (prime-helper? i)
    (cond
     [(> i max-prime-factor-n) #t]
     [(divisible? n i) #f]
     [else (prime-helper? (+ i 1))]))
  (and (> n 1) (prime-helper? 2)))
