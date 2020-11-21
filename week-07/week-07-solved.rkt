#lang racket


;; Last time we discussed higher order functions

;; Exercise:
;; Let's assume we have the graph of a function expressed
;; as a collection of integers in a range. Let's write a function
;; accepting two parameters: the graph with the definition above,
;; and a value on the y axis. We would like to return how many
;; times the function crossed that value at minimum.

;;(min-crosses '(0 1 2 3 4) 0)
;;(min-crosses '(1 3 4 1 5) 2)

(define (min-crosses graph v)
  (apply + (map (lambda (p1 p2)
                  (cond
                   [(< p1 v p2) 1]
                   [(> p1 v p2) 1]
                   [(= v p1)    1]
                   [else 0])) graph (append (cdr graph) (list (car (reverse graph)))))))



;;Exercise: people fucking dying.
;; We are writing an fn that accepts two fns - a predicate and a transformer
;; and returns a function that will accept a string and execute the transrformer for each character
;; for which the predicate is true, returning a modified string.

(define (people-fking-dying p t)
  (lambda (phrase)
    (list->string (map (lambda (c)
                         (cond
                          [(p c) (t c)]
                          [else c])) (string->list phrase)))))

((people-fking-dying (lambda (c) (list? (member c (string->list "oatvdaye"))))
                     char-upcase) "Look, ma! I am a cat!")



;; Playin around with fold.
;; This is when we find out it's too hard to read half a page on the internet.

(foldl + 1 '(1 1 1))

(foldr + 1 '(1 1 1))

(foldl cons '() '(0 1 2 3))
(cons 3 (cons 2 (cons 1 (cons 0 '()))))

;; Also
(foldr cons '() '(0 1 2 3))

;;Exercise: Write a function that accepts a collection and returns the sum of--all even numbers.
(define (sum-all-even xs)
  (foldl (lambda (current result)
           (+ result (cond [(even? current) current]
                           [else 0])))
         0 xs))

;;(sum-all-even '(1 2 3 4))


;; Exercise: Solve with fold - all the same from prev exercise
;; Are all elemetns from one in the other
;;(all-the-same? '("cat" "dog") '("dog" "cat")) => #t
(define (all-the-same? xs ys)
  (foldl (lambda (x y same?)
           (and same?
                (and (member x ys) #t)
                (and (member y xs) #t)))
         #t
         xs ys))

(all-the-same? '("cat" "dog" "larger cat") '("dog" "cat" "smaller cat"))


;;;;;;; Exercise:
;;;;;;; Create a function a list of numbers xs and a count n and returning
;;;;;;; the first number to be found n times in the list.
;;;;;;;;;;;;;;;;;;;;;;;;

;;Transform each number in ys into the number of times
;;it occurs in xs.

(define (count-poop xs x)
  (cond
   [(null? xs) 0]
   [(= x (car xs)) (+ 1 (count-poop (cdr xs) x))]
   [else (count-poop (cdr xs) x)]))


(define (count-all-occurences xs ys)
  (map (curry count-poop xs) ys))

(first-n-occurences-index '(1 7 3 1 7) 3) =>  nil

(define (first-n-occurences-index xs n)
  (cond [(> n (apply max (count-all-occurences xs xs))) "No element exists that many times!"]
        [else "we're fucked"]))

;; That's as far as we got.
