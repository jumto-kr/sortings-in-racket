#lang racket/base

(require racket/list)
(require racket/match)
(require rackunit)

(define (selection-sort list-a)
  (define (pop-min list-a)
    (case (length list-a)
      [(0) (raise "Expected at least one element.")]
      [(1) (cons (first list-a) `())]
      [else (let ([rest-pop-min (pop-min (rest list-a))])
              (let ([rest-min (car rest-pop-min)]
                    [rest-popped (cdr rest-pop-min)])
                (if (< (first list-a) rest-min)
                    (cons (first list-a) (rest list-a))
                    (cons rest-min (cons (first list-a) rest-popped))
                    )
                )
              )]
      )
    )
  (check-equal? (pop-min `(1 2 3)) `(1 2 3))
  (check-equal? (pop-min `(3 1 2)) `(1 3 2))
  
  (define (selection-sort-helper list-sorted list-to-be-sorted)
    (if (empty? list-to-be-sorted)
        list-sorted
        (let* ([popped (pop-min list-to-be-sorted)]
               [min (car popped)]
               [rest (cdr popped)])
          (selection-sort-helper (append list-sorted (list min)) rest)
          )
        )
    )
  (selection-sort-helper `() list-a)
  )
(check-equal? (selection-sort `(1 2 3)) `(1 2 3))
(check-equal? (selection-sort `(3 1 2)) `(1 2 3))
(check-equal? (selection-sort `(5 2 4 2 0 2 2 0 6 2 3)) `(0 0 2 2 2 2 2 3 4 5 6))
