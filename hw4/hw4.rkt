#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (n)
              (cond [(< n 0) (cons n (lambda () (f (+ (* n -1) 1))))]
                    [(= (remainder (+ n 1) 5) 0) (cons n (lambda () (f (* (+ n 1) -1))))]
                    [else (cons n (lambda () (f (+ n 1))))]))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" (lambda () (dog))))]
           [dog (lambda () (cons "dog.jpg" (lambda () (dan))))])
    (lambda () (dan))))

(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons 
                           (cons 0 (car (s))) 
                           (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons
                           (cons (list-nth-mod xs n) (list-nth-mod ys n))
                           (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (if (= n (vector-length vec))
                              #f
                              (let ([element (vector-ref vec n)])
                                (cond [(not (pair? element)) (f (+ n 1))]
                                      [(equal? (car element) v) element]
                                      [else (f (+ n 1))]))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([index 0]
           [cache (make-vector n #f)]
           [f (lambda(v)
              (let ([element (vector-assoc v cache)])
                (if element
                    element
                    (let ([value (assoc v xs)])
                      (vector-set! cache index value)
                      (set! index (remainder (+ index 1) n))
                      value))))])
    f))