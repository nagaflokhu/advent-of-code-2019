#lang racket

(define-values (MIN-VAL MAX-VAL) (values 108457 562041))

(define (solve MIN-VAL MAX-VAL)
  (for/fold ([n-solutions 0])
            ([val (in-range MIN-VAL (+ MAX-VAL 1))])
    (if (valid-solution? (number->digit-list val))
        (+ n-solutions 1)
        n-solutions)))

(define (number->digit-list num)
  (foldr
   (lambda (digit digit-list)
     (if (string=? digit "")
         digit-list
         (cons (string->number digit) digit-list)))
   '()
   (string-split (format "~a" num) "")))

(define (valid-solution? num/list)
  (and (has-repeated-digits? num/list)
       (ltr-digits-nondecreasing? num/list)))
  
(define (has-repeated-digits? num/list)
  (cond
    [(= (length num/list) 1) #f]
    [(= (first num/list) (second num/list)) #t]
    [else (has-repeated-digits? (rest num/list))]))

(define (ltr-digits-nondecreasing? num/list)
  (cond
    [(= (length num/list) 1) #t]
    [(>= (second num/list) (first num/list))
     (ltr-digits-nondecreasing? (rest num/list))]
    [else #f]))

(module+ main
  (displayln (solve MIN-VAL MAX-VAL)))