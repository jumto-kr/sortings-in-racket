#lang racket/base

(require racket/list)
(require rackunit)


(define (merge-sort list-a)
  (define (merge-sorted list-a list-b)
    (if (or (empty? list-a) (empty? list-b))
        (append list-a list-b)
        (if (< (first list-a) (first list-b))
            (cons (first list-a) (merge-sorted (rest list-a) list-b))
            (cons (first list-b) (merge-sorted list-a (rest list-b))))
        ) 
    )
  (check-equal? (merge-sorted `(1 2 5) `(2 4 5)) `(1 2 2 4 5 5))
  (check-equal? (merge-sorted `(1 1 3 5) `(1 2 4 6)) `(1 1 1 2 3 4 5 6))
  (check-equal? (merge-sorted `() `()) `())
  (check-equal? (merge-sorted `() `(4)) `(4))
  (check-equal? (merge-sorted `(3) `()) `(3))
  
  (if (<= (length list-a) 1)
      list-a
      (let ([left-length (quotient (length list-a) 2)])
        (merge-sorted (merge-sort (take list-a left-length))
                      (merge-sort (drop list-a left-length))))))

(check-equal? (merge-sort `(5 2 4 2 0 2 2 0 6 2 3)) `(0 0 2 2 2 2 2 3 4 5 6))
