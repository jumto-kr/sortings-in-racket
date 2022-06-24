#lang racket

(require racket/list)
(require rackunit)

(define (parent index) (if (= index 0) (raise "Already root") (quotient (- index 1) 2)))
(check-exn (lambda (message) (eq? message "Already root")) (lambda () (parent 0)))
(check-equal? (parent 1) 0)
(check-equal? (parent 2) 0)
(check-equal? (parent 3) 1)

(define (left index) (+ (* index 2) 1))
(define (right index) (+ (* index 2) 2))
(check-equal? (left 0) 1)
(check-equal? (right 0) 2)
(check-equal? (left 1) 3)
(check-equal? (right 1) 4)

(define (heap? my-heap)
  (define (check-heap-helper? my-heap root)
    (cond [(>= root (length my-heap)) #t]
          [(>= (left root) (length my-heap)) #t]
          [(< (list-ref my-heap (left root)) (list-ref my-heap root)) #f]
          [(>= (right root) (length my-heap)) #t]
          [(< (list-ref my-heap (right root)) (list-ref my-heap root)) #f]
          [else (and (check-heap-helper? my-heap (left root))
                     (check-heap-helper? my-heap (right root)))]))
  (check-heap-helper? my-heap 0))
(check-equal? (heap? `(1 2 3)) #t)
(check-equal? (heap? `(1 2)) #t)
(check-equal? (heap? `(1)) #t)
(check-equal? (heap? `(2 1 3)) #f)
(check-equal? (heap? `(1 2 3 1)) #f)

(define (swap my-heap index-a index-b)
  (cond [(= index-a index-b) my-heap]
        [(> index-a index-b) (swap my-heap index-b index-a)]
        [else (let* ([head (take my-heap index-a)]
                     [body (take (drop my-heap index-a) (- index-b index-a))]
                     [leg (drop my-heap index-b)])
                (append head (take leg 1) (rest body) (take body 1) (rest leg)))]))
(check-equal? (swap `(0 1 2 3 4) 1 3) `(0 3 2 1 4))
(check-equal? (swap `(0 1 2 3 4) 3 1) `(0 3 2 1 4))
(check-equal? (swap `(0 1 2 3 4) 0 1) `(1 0 2 3 4))
(check-equal? (swap `(0 1 2 3 4) 1 1) `(0 1 2 3 4))

(define (push-to-heap item my-heap)
  (define (bubble-up my-heap index)
    (cond [(= index 0) my-heap]
          [(< (list-ref my-heap (parent index)) (list-ref my-heap index)) my-heap]
          [else (bubble-up (swap my-heap (parent index) index) (parent index))])
    )
  (let ([new-heap (append my-heap (list item))])
    (let ([repaired-heap (bubble-up new-heap (- (length new-heap) 1))])
      (if (heap? repaired-heap)
          repaired-heap
          (raise "Heap broken")))))

(define (check-push my-heap new-value)
  (check-true (heap? my-heap))
  (let ([new-heap (push-to-heap new-value my-heap)])
    (check-true (heap? new-heap))
    (check-equal? (sort (cons new-value my-heap) <) (sort new-heap <))))
(check-push `(1 5 3 7 9) 0)
(check-push `(1 5 3 7 9) 2)
(check-push `(1 5 3 7 9) 4)
(check-push `(1 5 3 7 9) 6)
(check-push `(1 5 3 7) 6)
(check-push `(3 4 5 7) 2)

(define (pop-from-heap my-heap)
  (define (children my-heap parent)
    (define (children-indexes heap-length parent)
      (filter (lambda (each) (< each heap-length)) (list (left parent) (right parent))))
    (map (lambda (child-index)
           (cons child-index (list-ref my-heap child-index)))
         (children-indexes (length my-heap) parent)))
        
  (define (bubble-down my-heap index)
    (let ([my-children (children my-heap index)])
      (if (empty? my-children)
          my-heap
          (let ([min-child (argmin cdr my-children)])
            (if (< (list-ref my-heap index) (cdr min-child))
                my-heap
                (bubble-down (swap my-heap index (car min-child)) (car min-child)))))))
  (let ([head (first my-heap)]
        [new-heap (if (empty? (rest my-heap))
                      `()
                      (cons (last my-heap) (drop-right (rest my-heap) 1)))])
    (let ([repaired-heap (bubble-down new-heap 0)])
      (if (heap? repaired-heap)
          (cons head repaired-heap)
          (raise "Heap broken")))))
(define (check-pop my-heap)
  (check-true (heap? my-heap))
  (let ([popped (pop-from-heap my-heap)])
    (let ([head (car popped)]
          [new-heap (cdr popped)])
      (check-true (heap? new-heap))
      (check-equal? head (first (sort my-heap <)))
      (check-equal? (sort (cons head new-heap) <) (sort my-heap <))
      popped)))
(check-equal? (check-pop `(1 2 3 4 5)) (cons 1 `(2 4 3 5)))
(check-equal? (check-pop `(1 3 2 4 5)) (cons 1 `(2 3 5 4)))
(let ([sample-heap (push-to-heap 6 (push-to-heap 2 (push-to-heap 0 (push-to-heap 4 `()))))])
  (check-equal? (check-pop sample-heap) (cons 0 `(2 4 6)))
  (check-equal? (check-pop (cdr (check-pop sample-heap))) (cons 2 `(4 6))))

(define (heap-sort list-a)
  (define (heap-sort-helper sorted rest-heap)
    (if (empty? rest-heap)
        sorted
        (let ([popped (pop-from-heap rest-heap)])
          (heap-sort-helper (append sorted (list (car popped))) (cdr popped)))))
  
  (let ([heap (foldl (lambda (item heap) (push-to-heap item heap)) `() list-a)])
    (heap-sort-helper `() heap)))
    
(heap-sort `(2 2 9 0 6 2 4))
