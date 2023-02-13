#lang racket
(require racket/trace)

(define (readlist filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))
(define (import)
  (let ((p65 (readlist "partition65.scm"))
        (p74 (readlist "partition74.scm")) 
        (p75 (readlist "partition74.scm")))
    (append p65 p74 p75)))

(define (saveList filename L)
  (call-with-output-file filename
    (lambda (out)
      (write L out))))


(define testpoint1 '(74 230390 40.749987 -73.94406 74000003))
(define testpoint2 '(74 144526 40.749987 -73.94406 74000003))
(define testpoint3 '(74 208234 40.749987 -73.94406 74000002))
(define testset1 '((74 211266 40.74502 -73.949245 74000001)
                  (74 230390 40.749987 -73.94406 74000003)))

(define testset2 '((74 211266 40.74502 -73.949245 74000001)
                  (74 230390 40.749987 -73.94406 74000003)
                  (74 230390 40.749987 -73.94406 74000009)
                  (74 230390 40.749987 -73.94406 74000007)
                  (74 230390 40.749987 -73.94406 7400000)
                  (74 230390 40.749987 -73.94406 74000001)
                  (74 230390 40.749987 -73.94406 74000002)))


;creates new list, excluding all points that have the same pointID but different clusterID as the point at the head of the list

;(mergeClusters testset2)
;'((74 211266 40.74502 -73.949245 74000001) (74 230390 40.749987 -73.94406 74000003))
(define (mergeClusters allpoints)
  (if (null? allpoints) '()
      (cons (car allpoints) (mergeClusters (filter
                                        (lambda (point)
                                          ;checks if the point has a different point ID
                                          (or (not (equal? (list-ref point 1) (list-ref (car allpoints) 1)))
                                              ;checks if the point has the same point ID and cluster ID
                                              (and (equal? (list-ref point 1) (list-ref (car allpoints) 1)) (equal? (list-ref point 4) (list-ref (car allpoints) 4)))))
                                        (cdr allpoints))))))


;writes list to file (saveList didn't work, had to change the name)
(define (writeToFile L)
  (call-with-output-file "output.txt"
  (lambda (output-port)
    (display L output-port)))) 

 ;(writeToFile (mergeClusters (import)))