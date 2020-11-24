#lang racket

;; Last week of racket,
;; Get ready for test time!





;; Exercise 1: What is a narcissistic number? Write a predicate. https://en.wikipedia.org/wiki/Narcissistic_number (we do AGAIN)

;; 153 is narcissistic beacuse 1^3 + 5^3 + 3^3 = 153

;; NB:
;; How do we traverse numbers? - From right to left
;; Get the digit - remainder 10
;; Get the rest - quotient 10

(define (count-digits n)
  (cond [(< n 10) 1]
        [else (+ 1 (count-digits (quotient n 10)))]))

(define (count-digits-lol-n n)
  (length (string->list (number->string n))))

(define (sum-of-digits n p)
  (cond [(< n 10) n]
        [else (+ (expt (remainder n 10) p)
                 (sum-of-digits (quotient n 10) p))]))

(define (narcissistic? n)
  (= n (sum-of-digits n (count-digits n))))

;; Another crazy thing:
;; (define (narcissistic-a? n)
;;   (let ([digit-count (length (string->list (number->string n)))])
;;     (= n (apply + (map (lambda (current)
;;                          (expt (string->number (string current)) digit-count))
;;                        (string->list (number->string n)))))))



;; Exercise 2: We want to write a function that receives a predicate and a collection
;; and returns a collection of two items - all values for which the predicate was true
;; and all values for which the predicate was false.

;;(split-me-baby string? '(1 "a" "b" "c" 5 7)) => '((list "a" "b" "c") (list 1 5 7))


;; Oh btw, we can do composition.
;;((compose not odd?) 12)

(define (split-me-baby xs p)
  (list (filter p xs)
        (filter (compose not p) xs)))





;; Exercise 3: Write a function that accepts a number
;; and returns another function which will accept a LIST OF DIGITS
;; and return a list of tuples with how many times each digits occurs
;; in the origuinal number.
;; (define (count-occurences-filter n digit)
;;   (length (filter (curry = digit)
;;                   (map (compose string->number string)
;;                        (string->list (number->string n))))))

(define (count-occurences n digit)
  (cond [(= n 0) 0]
        [(= (remainder n 10) digit) (+ 1 (count-occurences (quotient n 10) digit))]
        [else (count-occurences (quotient n 10) digit)]))

(define (counts-for-number n)
  (lambda (xs)
    (map (lambda (digit)
           (cons digit (count-occurences n digit)))
         xs)))

;;((counts-for-number 1123322) '(1 4 2))

;; Map arguments are:
;;;;;;;Arg 1: Function - "a" => '("a" "a")....
;;;;;;;Arg 2: Collection 1
.......
;;;;;;;Arg n: Collection n


;; Exercise Last:  Write a function that accepts a matrix and returns it's diagonal. You have 54 mins

;; (extract-diagonal  '((1 2 3)
;;                      (4 5 6)
;;                      (7 8 9))) => '(1 5 9)

;;Recursive
(define (extract-diagonal-rec xss)
  (define (helper idx)
    (cond [(= idx (length xss)) null]
          [else (cons (list-ref (list-ref xss idx) idx)
                      (helper (+ 1 idx)))]))
  (helper 0))


;; Fold
(define (extract-diagonal xss)
  (reverse (foldl (lambda (line diagonal)
                    (cons (list-ref line (length diagonal))
                          diagonal))
                  '()
                  xss)))


;; Map
(define (extract-diagonal-map xss)
  (map (lambda (line idx)
         (list-ref line idx))
       xss
       (range 0 (length xss))))

(extract-diagonal-map '((1 2 3)
                        (4 5 6)
                        (7 8 9)))
