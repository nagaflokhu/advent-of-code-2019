#lang racket

(define-values (MIN-VAL MAX-VAL) (values 108457 562041))

(define (solve min-val max-val valid-solution?)
  (for/fold ([n-solutions 0])
            ([val (in-range min-val (+ max-val 1))])
    (if (valid-solution? (number->digit-list val))
        (+ n-solutions 1)
        n-solutions)))

(define (part1)
  (solve MIN-VAL
         MAX-VAL
         (lambda (num/list)
           (and (has-repeated-digits? num/list)
                (ltr-digits-nondecreasing? num/list)))))

(define (part2)
  (solve MIN-VAL
         MAX-VAL
         (lambda (num/list)
           (and (has-1+pair-repeated-digits? num/list)
                (ltr-digits-nondecreasing? num/list)))))

(define (number->digit-list num)
  (foldr
   (lambda (digit digit-list)
     (if (string=? digit "")
         digit-list
         (cons (string->number digit) digit-list)))
   '()
   (string-split (format "~a" num) "")))
  
(define (has-repeated-digits? num/list)
  (cond
    [(= (length num/list) 1) #f]
    [(= (first num/list) (second num/list)) #t]
    [else (has-repeated-digits? (rest num/list))]))

(define (has-1+pair-repeated-digits? num/list)
  (cond
    [(<= (length num/list) 1) #f]
    [(and (= (length num/list) 2) (= (first num/list) (second num/list)))
     #t]
    [(= (length num/list) 2) #f]
    [(= (first num/list) (second num/list) (third num/list))
     (has-1+pair-repeated-digits?
      (dropf num/list
             (lambda (num) (= num (first num/list)))))]
    [(= (first num/list) (second num/list)) #t]
    [else (has-1+pair-repeated-digits? (rest num/list))]))

(define (ltr-digits-nondecreasing? num/list)
  (cond
    [(= (length num/list) 1) #t]
    [(>= (second num/list) (first num/list))
     (ltr-digits-nondecreasing? (rest num/list))]
    [else #f]))

(module+ main
  (displayln (part1))
  (displayln (part2)))