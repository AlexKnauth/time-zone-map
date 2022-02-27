#lang racket/base

(provide voronoi-diagram2 plot-voronoi-diagram2 metric)

(require racket/function racket/list plot "common.rkt")
(module+ main
  (require pict))

;; Based on:
;; https://rosettacode.org/wiki/Voronoi_diagram#Racket

;; ---------------------------------------------------------

(define metric (make-parameter manhattan-distance))

;; ---------------------------------------------------------

;; voronoi-diagram2 : (Hashof (Listof Point) Pict) -> (Treeof Renderer2D)
(define (voronoi-diagram2 #:x-min [x-min #f]
                          #:x-max [x-max #f]
                          #:y-min [y-min #f]
                          #:y-max [y-max #f]
                          #:samples [samples 300]
                          #:point-sym [point-sym 'none]
                          point-groups)
  (define n (hash-count point-groups))
  (define F (classification-function (hash-keys point-groups)))
  (list
   (contour-intervals (compose F v) x-min x-max y-min y-max
                      #:samples samples
                      #:levels n
                      #:colors (range 1 (+ n 2))
                      #:contour-styles '(solid)
                      #:alphas '(1))
   (for/list ([(group label) (in-hash point-groups)])
     (point-pict (first group) label #:anchor 'center #:point-sym point-sym))))

;; plot-voronoi-diagram2 : (Hashof (Listof Point) Pict) -> Plot
;; Plots the Voronoi diagram as a contour plot of
;; the classification function built for a set of points
(define (plot-voronoi-diagram2 #:x-min [x-min #f]
                               #:x-max [x-max #f]
                               #:y-min [y-min #f]
                               #:y-max [y-max #f]
                               #:width [width (plot-width)]
                               #:height [height (plot-height)]
                               #:title [title (plot-title)]
                               #:x-label [x-label (plot-x-label)]
                               #:y-label [y-label (plot-y-label)]
                               #:aspect-ratio [aspect-ratio (plot-aspect-ratio)]
                               #:samples [samples 300]
                               #:point-sym [point-sym 'none]
                               point-groups)
  (plot
   #:x-min x-min
   #:x-max x-max
   #:y-min y-min
   #:y-max y-max
   #:width width
   #:height height
   #:title title
   #:x-label x-label
   #:y-label y-label
   #:aspect-ratio aspect-ratio
   (voronoi-diagram2 #:x-min x-min
                     #:x-max x-max
                     #:y-min y-min
                     #:y-max y-max
                     #:samples samples
                     #:point-sym point-sym
                     point-groups)))

;; For a set of centroids returns a function
;; which finds the index of the centroid nearest
;; to a given point
(define (classification-function groups)
  (define sorted-groups (sort groups < #:key (compose v-x first)))
  (define points (append* sorted-groups))
  (define tbl
    (for*/hash ([(g i) (in-indexed (in-list sorted-groups))]
                [p (in-list g)])
      (values p i)))
  (λ (x)
    (hash-ref tbl (argmin (curry (metric) x) points))))

;; ---------------------------------------------------------

(module+ main
  (define pgs
    (for/hash ([i 50])
      (values (list (v (random) (random)))
              (text (number->string i)))))

  (display (plot-voronoi-diagram2 pgs #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1)))
