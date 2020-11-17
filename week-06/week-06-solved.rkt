#lang racket

;; Ok, wtf did we do last time?
;; apply/map/filter. Show me what you got!

(map identity '(1 2 3))

(filter identity '(#t #t #f))

(apply + '(1 2 3))

;; Write a function that accepts a list of integers and returns a list of all integers with 5 added to them.
;; No lambda.
;;

(define (i-am-with-stupid xs)
  (map (curry + 5 3 2 1) xs))


;; Exercise:
;; Write a predicate that returns true or false if all elements in a list are equal.
;; (all? '(1 1 1)) => '(#t #t #t)
(define (all? xs)
  (andmap (curry equal? (car xs)) xs))


;; Exercise: Write a predicate if a nested list is a matrix.
(define (matrix? xss)
  (all? (map length xss)))

;; (matrix? (list '(1 2)
;;                '(3 4)))




;; Exercise:
;; What is the transpose of a matrice? How can you transform a matrix to its transposed version?
(map list (list '(1 2) '(3 4)))

(map car (list '(1 2)
               '(3 4)
               '(5 6)))

(define (transpose xss)
  (apply map list xss))

(transpose (list '(1 2 3)
                 '(4 5 6)
                 '(7 8 9)))



;; Exercise:
;; Let's write a function that we are going to call `mapcat`.
;; We would like to receive two arguments - an fn and a nested list.
;; We are going to execute the fn on all first level elements of the list
;; and then flatten the results 1 level. You have 5 minutes


;;(mapcat reverse (list '(2 1 0) '(5 4 3))) => '(0 1 2 3 4 5)
(define (mapcat f xss)
  (apply append (map f xss)))

;; flatMap == mapcat
;;(mapcat reverse (list '(2 1 0) '(5 4 3)))




;; Exercise:
;; Let's write a function that accepts two lists of words and
;; returns true only if all words in one list are also in the other

;;(all-are-in? '("cat" "dog") '("dog" "cat")) => #t
;;(all-are-in? '("cat" "dog") '("dog" "cat" "pussy")) => #f

;; Which higher order fn are we going to use?
;; You don't need another higher order fn.
(define (all-are-in? xs ys)
  (andmap (lambda (x y)
            (and (list? (member x ys))
                 (list? (member y xs))))
          xs
          ys))


;; Let's write a function that accepts as an argument an "alphabet" (list of characters)
;; and returns a predicate that verifies if all words in a list are constructed only with
;; letters from the given alphabet
;;(equal? #t '())

(define (generate-alphabet-checker alphabet)
  (define (in-alphabet? word)
    (andmap (lambda (c)
              (and (member c (string->list alphabet)) #t))
            (string->list word)))
  (lambda (words)
    (andmap in-alphabet? words)))


;; Ormap exists.
;;(ormap identity '(#t #t #f))


;; Spoilers: Retun a lambda
;; How many arguments does our lambda accepts?  => 1.

;;((generate-alphabet-checker "abcdef") '("cae" "dae" "bae"))

;;((generate-alphabet-checker "abcdef") '("cat" "dog" "bae"))
