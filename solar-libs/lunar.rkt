#lang racket
(require gregor
         "./calc-libs/time-lib.rkt"
         "./calc-libs/orbit-lib.rkt"
         "./calc-libs/coordinates-lib.rkt")
(provide get-lunar-data)

; These constants are with respect to the epoch of J2000
(define orbital-elements
  (hash
    'e 0.0549 ; eccentricity of orbit
    'a0 384400 ; length of orbital semi-major axis (km)
    'theta0 0.05181 ; angular diameter when a distance of a0 from earth (degrees)
    'i 5.1453964 ; inclination of the orbit with respect to ecliptic
    'epsilon0 218.316433 ; ecliptic longitude at epoch (degrees)
    'w0 83.353451 ; ecliptic longitude at perigee at epoch (degrees)
    'Tp 0.074804 ; orbit time in tropical years (1 because it's the earth-sun orbit)
    'omega0 125.044522)) ; ecliptic longitude of the ascending node at epoch

; Done once to calculate position and again to calculate first interpolation set
; for rise/set functions
(define (get-ecliptic-coords localtime solar-data latitude longitude)
  (letrec ([tt-time (+milliseconds (->datetime/utc localtime) 63800)]
           [Julian-time (date->jdn (->year tt-time)
                                   (->month tt-time)
                                   (+ (->day tt-time)
                                      (time->day% (->hours tt-time)
                                                  (->minutes tt-time)
                                                  (->seconds tt-time))))]
           [De (- Julian-time J2000)]
           [mean-long (range-normalize (+ (* 13.176339686 De)
                                          (hash-ref orbital-elements 'epsilon0)) 360)]
           [mean-asc-node (range-normalize (- (hash-ref orbital-elements 'omega0)
                                              (* 0.0529539 De)) 360)]
           [M-anomaly (range-normalize (- mean-long
                                          (* 0.1114041 De)
                                          (hash-ref orbital-elements 'w0)) 360)]
           [solar-M (degrees->radians (hash-ref solar-data 'M))]
           [solar-ecliptic (degrees->radians (cdr (hash-ref solar-data 'Ecliptic)))]
           [Ae (* 0.1858 (sin solar-M))]
           [Ev (* 1.2739 (sin (- (* 2 (- (degrees->radians mean-long) solar-ecliptic))
                                 (degrees->radians M-anomaly))))]
           [Ca (- (+  M-anomaly Ev) Ae (* 0.37 (sin solar-M)))]
           [vm (+ (* 6.2886 (sin (degrees->radians Ca))) (* 0.214 (sin (* 2 (degrees->radians Ca)))))]
           [long-fix1 (- (+ mean-long Ev vm) Ae)]
           [V (* 0.6583 (sin (* 2 (- (degrees->radians long-fix1) solar-ecliptic))))]
           [long-fix2 (+ long-fix1 V)]
           [fix-asc-node (- mean-asc-node (* 0.16 (sin solar-M)))]
           [ecliptic-long (letrec ([atan-y (* (sin (- (degrees->radians long-fix2)
                                                      (degrees->radians fix-asc-node)))
                                              (cos (degrees->radians (hash-ref orbital-elements 'i))))]
                                   [atan-x (cos (- (degrees->radians long-fix2)
                                                   (degrees->radians fix-asc-node)))]
                                   [atan-adjust (cond [(and (positive? atan-y) (positive? atan-x)) 0]
                                                      [(and (positive? atan-y) (negative? atan-x)) 180]
                                                      [(and (negative? atan-y) (positive? atan-x)) 360]
                                                      [(and (negative? atan-y) (negative? atan-x)) 180])])
                                  (range-normalize (+ fix-asc-node
                                                      (+ (radians->degrees (atan (/ (degrees->radians atan-y)
                                                                                    (degrees->radians atan-x))))
                                                         atan-adjust))
                                                   360))]
           [ecliptic-lat (let ([asin-x (sin (- (degrees->radians long-fix2)
                                               (degrees->radians fix-asc-node)))]
                               [asin-y (sin (degrees->radians (hash-ref orbital-elements 'i)))])
                              (radians->degrees (asin (* asin-x asin-y))))])
          (values
            (cons ecliptic-lat ecliptic-long) fix-asc-node Ca)))

; Lunar calculations are unique due to corrections being based on
; translating from a geocentric model rather than a heliocentric one
(define (get-lunar-data localtime solar-data latitude longitude)
  (letrec ([ecliptic-coords (let-values
                             ([(coords node Ca)
                               (get-ecliptic-coords localtime solar-data latitude longitude)]) coords)]
           [equatorial-coords (ecliptic->equatorial (car ecliptic-coords) (cdr ecliptic-coords))]
           [eq-hour (- (utc->lst (->datetime/utc localtime) longitude) (cdr equatorial-coords))]
           [rise/set (letrec-values
                       ([(ecliptic1 node Ca)
                         (get-ecliptic-coords (->date localtime) solar-data latitude longitude)]
                        [(equatorial1) (ecliptic->equatorial (car ecliptic1) (cdr ecliptic1))]
                        [(S/R-set1) (get-rise/set (car equatorial1) (cdr equatorial1) latitude)]
                        [(ecliptic-lat2) (+ (car ecliptic1)
                                            (* 0.05 12 (cos (- (degrees->radians (cdr ecliptic1))
                                                               (degrees->radians node)))))]
                        [(ecliptic-long2) (+ (cdr ecliptic1)
                                             (* 12 (+ 0.55 (* 0.06 (cos (degrees->radians Ca))))))]
                        [(equatorial2) (ecliptic->equatorial ecliptic-lat2 ecliptic-long2)]
                        [(S/R-set2) (get-rise/set (car equatorial2) (cdr equatorial2) latitude)]
                        [(S/R-interpolate) (lambda (ST1 ST2) (/ (* 12.03 ST1)
                                                                (- (+ 12.03 ST1) ST2)))])
                       (cons (lst->lct (S/R-interpolate (car S/R-set1) (car S/R-set2))
                              localtime longitude)
                             (lst->lct (S/R-interpolate (cdr S/R-set1) (cdr S/R-set2))
                              localtime longitude)))])
          (hash
            'Ecliptic ecliptic-coords
            'Equatorial equatorial-coords
            'Horizon (equatorial->horizon (car equatorial-coords) eq-hour latitude)
            'Moonrise (car rise/set)
            'Moonset (cdr rise/set)
            )))
