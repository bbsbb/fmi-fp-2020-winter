#lang racket

;;FP EXERCISE Group 4 wtfbbq.
;; Write a function that accepts two lists
;; and returns the longer of them

(define (longer-list xs ys)
  (cond [(> (length xs) (length ys)) xs]
        [else ys]))

(longer-list '(1) '(1 2))


;; Strings?? Wtf are they?
;; C -> String - list of chars

(let ([a "butts"])
  a)

;; Error
;;(length "butts")


;; The thing to remember is that Margarita Mollova is WRONG!
(length "butts")

;; Racket -> Strings ARE NOT lists of chars
(list->string (string->list "butts"))

(string->list "butts?")

;; Write a predicate that accepts a character and returns
;; TRUE if the character is an empty space or punctuation
(define (break-char? c)
  (or (char-whitespace? c) (char-punctuation? c)))

;; Find the longest word in a phrase

(define (longest-word phrase)
  (define (longest-word-helper phrase-rest longest current)
    (cond [(null? phrase-rest) (list->string (reverse (longer-list longest current)))]
          [(break-char? (car phrase-rest)) (longest-word-helper (cdr phrase-rest)  ;; "dog"
                                                                (longer-list longest current)
                                                                null)]
          [else (longest-word-helper (cdr phrase-rest)
                                     longest
                                     (cons (car phrase-rest) current))]))
  (longest-word-helper (string->list phrase) null null))

(longest-word "I am just a pretty face that likes brocolli and poop!")


;;;;
;;;; higher order functions.
;;;; - Functions that accept other functions.
;;;; Spoilers: The only useful higher order functions are those that work on collections.

;;;; map, filter, apply.
(define (demo-fn x)
  (+ 1 x))

(demo-fn 1)

(map demo-fn '(1 2 "no butts!"))

;; args:  fn + collection
;; output: collection of the same number of elements



(filter demo-fn '(1 2 3))

;; args: Fn + collection
;; output: Collection up to N of the exact same elements
(filter (lambda (x) (and (odd? x) (even? x))) '(1 2 3 4 5 0))


;; Apply
(+ 1 2 3 4)


;; Error
;;(+ 1 '(1 2 3))

(apply + '(1 2 3 4))

(apply + 1 '(2 3 4))


;; Exercise: Define a function that accepts something X
;; and a number N and returns a list of N X's. Use map

(define (list-of-n x n)
  (map (lambda (_) x) (range 0 n)))

(list-of-n "cat" 3)


;; Write an FN that calculates the dot product of two vectors
;;


;; Oh, btw, about map....
(map (lambda (x y) (+ x y))
     '(1 2 3 1 1 1 1 1 1)
     '(3 2 1 1 1 1 1 1 1))

;; It's one line.
(define (dot-product xs ys)
  (apply + (map (lambda (x y) (* y x)) xs ys)))


;; What is ASCII?
;; Encoding table for chars, where chars have an associated VALUE.

;; Write a function that accepts a list of words
;; - Sums of the value of chars in a word
;; - Returns the highest VALUE of a found word.

;; Use map + apply.
(define (highest-value-word xs)
  (apply max (map (lambda (word)
                    (apply + (map char->integer (string->list word)))) xs)))

(highest-value-word '("cat" "DOG"))
