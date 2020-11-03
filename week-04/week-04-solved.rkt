#lang racket

;; Alright.....what were lambda functions again?
;; Let's do a quick fast one
(define (make-divisible m)
  (lambda (n) (= 0 (remainder n m))))

(define divisible-by-FIVE (make-divisible 5))


(divisible-by-FIVE 10)
;; Wtf is a cons?
;; What is a list, then?
;; Playing a bit.
(let ([a (list 1 2 4)])
  (list (car a) (cdr a)))

(null? '())

(length '(1 2 3))

(append '(1) '(2))

(cons 1 (cons 2 3))
(list 12 3 4)

'(1 2 3)

(list length 2 3 "poopy7face")


;; Exercise 1:
;; Let's right a function that gives the length of a list.

(define (omg-ze-length xs)
  (cond
   [(null? xs) 0]
   [else (+ 1 (omg-ze-length (cdr xs)))]))

(omg-ze-length '(1 2 3))

;; Exercise 2:
;; Can we prepend to a list? HOW?

(cons 1 (cons 2 '()))

;; Exercise 3:
;; Define a function that accepts two arguments - a one argument fn and a list.
;; The return value is a new list constructed by the application of the passed fn to all list elements.
;; function to all elements

(define (apply-to=list fn xs)
  (cond
   [(null? xs) xs]
   [else (cons (fn (car xs))
               (apply-to=list fn (cdr xs)))]))

(apply-to=list (lambda (x) (list "butt" x)) (list 0 1))

(apply-to=list '())

;; Exercise 4: Let's write a function that accepts a list of numbers xs
;; and a single number n and returns the number of occurences
;; of n within xs.


(define (count-poop x xs)
  (cond
   [(null? xs) 0]
   [(= x (car xs)) (+ 1 (count-poop x (cdr xs)))]
   [else (count-poop x (cdr xs))]))

(count-poop 5 '(1 2 4 5 5 5))


;;Exercise 5: Write a function that takes a list as an argument and removes all duplicate items.
;; Bonus: Is this tail recursion?

(define (member? x xs)
  (and (member x xs) #t))

(define (remove-duplicates xs)
  (cond
   [(null? xs) xs]
   [(member? (car xs) (cdr xs)) (remove-duplicates (cdr xs))]
   [else (cons (car xs) (remove-duplicates (cdr xs)))]))

(remove-duplicates '(1 3 2 3 3 "wut" 5 5))
