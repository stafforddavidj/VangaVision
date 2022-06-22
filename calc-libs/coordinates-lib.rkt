#lang racket
(require "./time-lib.rkt")
(provide ecliptic->equatorial equatorial->horizon get-rise/set)

; Note that this returns equatorial longitude in hours rather than degrees
(define (ecliptic->equatorial in-lat in-long)
  (letrec ([ec-lat (degrees->radians in-lat)]
           [ec-long (degrees->radians in-long)]
           [obliquity (degrees->radians 23.439292)]
           [eq-lat (radians->degrees (asin
                                      (+ (* (sin ec-lat) (cos obliquity))
                                         (* (cos ec-lat) (sin obliquity) (sin ec-long)))))]
           [eq-long (letrec ([atan-y (- (* (sin ec-long) (cos obliquity))
                                        (* (tan ec-lat) (sin obliquity)))]
                             [atan-x (cos ec-long)]
                             [atan-adjust (cond [(and (positive? atan-y) (positive? atan-x)) 0]
                                                [(and (positive? atan-y) (negative? atan-x)) 180]
                                                [(and (negative? atan-y) (positive? atan-x)) 360]
                                                [(and (negative? atan-y) (negative? atan-x)) 180])])
                            (+ (radians->degrees (atan (/ atan-y atan-x))) atan-adjust))])
          (cons eq-lat (/ eq-long 15))))

; Note that this expects equatorial longitude in hours rather than degrees
(define (equatorial->horizon in-dec in-hour latitude)
  (letrec ([eq-dec (degrees->radians in-dec)]
           [eq-hour (degrees->radians (* in-hour 15))]
           [eq-lat (degrees->radians latitude)]
           [altitude (asin (+ (* (sin eq-dec) (sin eq-lat))
                              (* (cos eq-dec) (cos eq-lat) (cos eq-hour))))]
           [azimuth (acos (/ (- (sin eq-dec) (* (sin eq-lat) (sin altitude)))
                             (* (cos eq-lat) (cos altitude))))])
          (cons (radians->degrees altitude)
                (if (< (sin eq-hour) 0)
                    (radians->degrees azimuth)
                    (- 360 (radians->degrees azimuth))))))

; Returned results are in LST
(define (get-rise/set in-dec in-hour latitude)
  (letrec ([eq-dec (degrees->radians in-dec)]
           [eq-lat (degrees->radians latitude)]
           [Ar (/ (sin eq-dec) (cos eq-lat))]
           [H1 (* (tan eq-dec) (tan eq-lat))])
          (if (or (> (abs Ar) 1) (> (abs H1) 1))
              (cons #f #f)
              (letrec ([H2 (/ (radians->degrees (acos (- H1))) 15)]
                       [R-lst (range-normalize (- (+ 24 in-hour) H2) 24)]
                       [S-lst (range-normalize (+ in-hour H2) 24)]
                       [R-angle (radians->degrees (acos Ar))]
                       [S-angle (- 360 R-angle)])
                      (cons R-lst S-lst)))))
