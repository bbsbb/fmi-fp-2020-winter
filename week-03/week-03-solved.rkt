#lang racket

;; Hello, survivors!
;; Ok, cool, let's play a bit with let forms.
;; What are thsoe? How are they different to define?

;; Can you give me an example of a letrec form?
;; (if you can, what are you doing with your life)

(let* ([a 3]
      [b (+ 2 a)])
  b)

(define c 3)

;; What are lambdas?
;; What are higher order functions


;; Exercise 1: So, we want to figure out if the numbers in order.
;; Bonus points for mutual recursion:
;;(ordered? 137889) => #t
;;(ordered? 137798) => #f

(define (ordered? n)
  (define (current-ordered? n current)
    (and (>= current (remainder n 10))
         (ordered? n)))
  (cond
   [(< n 10) #t]
   [else (current-ordered? (quotient n 10) (remainder n 10))]))

(define (ordered-simple? n)
  (cond
   [(< n 10) #t]
   [(>= (remainder n 10) (quotient (remainder n 100) 10))
    (ordered-simple? (quotient n 100))]
   [else #f]))

;;;;;;;Exercise 2:
;; What do we order by in exercise 1? => predicate
;; Let's write the same function, but ordering by a random predicate.
;; Attention: Up to 5 mins.
(define (ordered-by? pred n)
  (define (current-ordered? n current)
    (and (pred current (remainder n 10))
         (ordered? n)))
  (cond
   [(< n 10) #t]
   [else (current-ordered? (quotient n 10) (remainder n 10))]))

;; How do you solve exercise 1 if you already had 2?
;; SHOW?
;;Exercise 3:
;; Fun with fucking functions ...inc and shit.

;; You can assign functions
(define (identity x) x)
(define f identity)
(define g identity)


;; This increments
(define shitty-inc (lambda (n) (+ 1 n)))


;; You better learn this well.
(define (compose-two f g)
  (lambda (x) (g (f x))))

;; Usages
(let ([add-5 (compose-two (lambda (x) x)
                          (curry + 5))])
  (add-5 7))

(define increment-by-TWO (compose-two shitty-inc shitty-inc))

(increment-by-TWO 5)

;; Exercise 4: Google what a narcissistic number is. Implement a predicate

(define (narcissistic? n)
  (define (count-digits n)
    (cond
     [(= n 0) 0]
     [else (+ 1 (count-digits (quotient n 10)))]))
  (define (total m)
    (cond [(= (count-digits m) 1) m]
          [else (+ (expt (remainder m 10) (count-digits n))
                   (total (quotient m 10)))]))
  (= n (total n)))
