#lang racket
(struct node (key moves left right))

;; sqr -> lst of sqrs


;;Builds a tree form a list
(define (build-tree lst)
  (cond [(empty? lst) empty]
        [else (let ((n (ceiling (/ (length lst) 2)))) (let ((x (mid-list lst n))) (node (first x) (second x) (build-tree (left-list lst n)) (build-tree (right-list lst n)))))]))

(define (mid-list lst n)
  (cond [(= n 1) (first lst)]
        [else (mid-list (rest lst) (- n 1))]))

(define (left-list lst n)
  (cond [(= n 1) empty]
        [else (cons (first lst) (left-list (rest lst) (sub1 n)))]))

(define (right-list lst n)
  (cond [(= n 1) (rest lst)]
        [else (right-list (rest lst) (sub1 n))]))

(define (bst-search k bst)
  (cond [(empty? bst) empty]
        [(false? bst) empty]
        [(equal? k (node-key bst)) (node-moves bst)]
        [(< k (node-key bst)) (bst-search k (node-left bst))]
        [(> k (node-key bst)) (bst-search k (node-right bst))]
        [else (error "F U")]))


(define N `(-10 -14 -23 -25 10 14 23 25))
(define K `(1 -1 12 -12 -11 11 13 -13))
(define border (string->number "111111111111111111111111110000000011110000000011110000000011110000000011110000000011110000000011110000000011110000000011111111111111111111111111" 2))

(define (!! x m) (filter (lambda (y) (zero? (bitwise-and x y))) m))
(define (->> x m) (!! border (map (lambda (u) (* (expt 2 u) x)) m)))
(define (->>> x m) (map (lambda (u) (* (expt 2 u) x)) m))
(define (?? s) (length (filter (lambda (x) (equal? x #\1)) (string->list (number->string s 2)))))
(define mm 0)
(define (num->move num)
  (let* ((x (modulo (floor (/ (log num) (log 2))) 12)) (y (* (floor (/ (/ (log num) (log 2)) 12)))))
    (cond [(= x 2) (cons 'N (cons 'a (cons (- y 1) empty)))]
          [(= x 3) (cons 'N (cons 'b (cons (- y 1) empty)))]
          [(= x 4) (cons 'N (cons 'c (cons (- y 1) empty)))]
          [(= x 5) (cons 'N (cons 'd (cons (- y 1) empty)))]
          [(= x 6) (cons 'N (cons 'e (cons (- y 1) empty)))]
          [(= x 7) (cons 'N (cons 'f (cons (- y 1) empty)))]
          [(= x 8) (cons 'N (cons 'g (cons (- y 1) empty)))]
          [(= x 9) (cons 'N (cons 'h (cons (- y 1) empty)))])))

(define (>-> x s mt ans)
  (define u (- (?? s) 80))
  (cond [(= u 63) (printf "~a\n" (cons (num->move x) ans))]
       ; [(and (> 62 u) (false? (falser mt mt))) empty]
        [else (map (lambda (u) (>-> (second u) (+ s x) mt (cons (num->move x) ans))) (sort (map (lambda (n) `(,(?? (bitwise-and (foldr + 0 (->>> n K)) s)) ,n)) (!! s (bst-search x mt))) (lambda (x y) (> (first x) (first y)))))]))

(define (falser mt mto)
  (cond 
        [(empty? mt) true]
        [(empty? (node-moves mt)) false]
        [(and (empty? (rest (node-moves mt))) (equal? (list (node-key mt)) (bst-search (first (node-moves mt)) mto))) false]
        [else (and (falser (node-left mt) mto) (falser (node-right mt) mto))]))

(define (del val lst tree)
  (cond [(empty? lst) tree]
        [else (delone val (first lst) (del val (rest lst) tree))]))

(define (delone val k tree)
  (cond [(empty? tree) tree]
        [(false? tree) false]
        [(= k (node-key tree)) (node k (remove val (node-moves tree)) (node-left tree) (node-right tree))]
        [(> k (node-key tree)) (node (node-key tree) (node-moves tree) (node-left tree) (delone val k (node-right tree)))]
        [(< k (node-key tree)) (node (node-key tree) (node-moves tree) (delone val k (node-left tree)) (node-right tree))]
        [else tree]))

(define (make-pairs n)
  (cond [(= n 25) empty]
        [(or (= (modulo n 12) 1) (= (modulo n 12) 10) (= (modulo n 12) 11) (= (modulo n 12) 0))(make-pairs (- n 1))]
        [else (cons (expt 2 n) (make-pairs (- n 1)))]))

(define (graph-maker lst)
  (build-tree (reverse (map (lambda (x) (cons x (cons (->> x N) empty))) lst))))

(define moves-tree (graph-maker (make-pairs 120)))
;(falser (foldr (lambda (f r) (del f (bst-search f moves-tree) r)) moves-tree (bst-search (expt 2 29) moves-tree)))
;(>-> (expt 2 50) border moves-tree empty)
