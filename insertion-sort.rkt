#lang racket/base

(require racket/list)
(require rackunit)

(define (insertion-sort list-a)
  (define (insert list-a item)
    (if (empty? list-a)
        (list item)
        (if (< (first list-a) item)
            (cons (first list-a) (insert (rest list-a) item))
            (cons item list-a)
            )
        )
    )
  (check-equal? (insert `(1 3) 2) `(1 2 3))
  (check-equal? (insert `() 1) `(1))

  (define (insertion-sort-helper list-sorted list-to-be-sorted)
    (if (empty? list-to-be-sorted)
        list-sorted
        (insertion-sort-helper (insert list-sorted (first list-to-be-sorted)) (rest list-to-be-sorted))
        )
    )

  (insertion-sort-helper `() list-a)
  )
(check-equal? (insertion-sort `(5 2 4 2 0 2 2 0 6 2 3)) `(0 0 2 2 2 2 2 3 4 5 6))
