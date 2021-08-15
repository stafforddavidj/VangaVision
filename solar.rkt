#lang racket/base
(require gregor
         "./lib/time-lib.rkt"
         "./lib/orbit-lib.rkt"
         "./lib/coordinates-lib.rkt")
(provide get-solar-data)

; These constants are with respect to the epoch of J2000
(define orbital-elements
  (hash
   'e 0.016708 ; eccentricity of orbit
   'a0 1.495985e08 ; length of orbital semi-major axis (km)
   'theta0 0.0533128 ; angular diameter when a distance of a0 from earth (degrees)
   'epsilon0 280.466069 ; ecliptic longitude at epoch (degrees)
   'w0 282.938346 ; ecliptic longitude at perigee at epoch (degrees)
   'Tp 1)) ; orbit time in tropical years (1 because it's the earth-sun orbit)

; First get position at input time. The calculation for orbit is used to find
; the position of the sun in the sky. Even though the sun revolves around the
; Earth, the Sun's path through the sky follows elliptical properties observed
; in satellite orbits.
; Ch. 6.2, pp.136-137
(define (get-solar-data localtime latitude longitude)
  (letrec ([utc-time (->datetime/utc localtime)]
           [m-anomaly (get-mean-anomaly utc-time orbital-elements)]
           [v (get-orbit-anomaly utc-time orbital-elements m-anomaly)]
           [ecliptic-long (range-normalize (+ v (hash-ref orbital-elements 'w0)) 360)]
           [equatorial-coords (ecliptic->equatorial 0 ecliptic-long)]
           ; Calculations for sun rise/set
           [day1-0hour (datetime (->year localtime) (->month localtime) (->day localtime))]
           [v1 (get-orbit-anomaly day1-0hour orbital-elements)]
           [ecliptic1 (range-normalize (+ v1 (hash-ref orbital-elements 'w0)) 360)]
           [equatorial1 (ecliptic->equatorial 0 ecliptic1)]
           [S/R-set1 (get-rise/set (car equatorial1) (cdr equatorial1) latitude)]
           [ecliptic2 (+ ecliptic1 0.985647)]
           [equatorial2 (ecliptic->equatorial 0 ecliptic2)]
           [S/R-set2 (get-rise/set (car equatorial2) (cdr equatorial2) latitude)]
           [S/R-interpolate (lambda (ST1 ST2) (/ (* 24.07 ST1)
                                                 (- (+ 24.07 ST1) ST2)))])
    (hash
      'Ecliptic (cons 0 ecliptic-long)
      'Equatorial equatorial-coords
      'Horizon (equatorial->horizon (car equatorial-coords)
                                    (- (utc->lst utc-time longitude)
                                       (cdr equatorial-coords))
                                    latitude)
      'M m-anomaly
      'Sunrise (lst->lct (S/R-interpolate (car S/R-set1) (car S/R-set2))
                         localtime longitude)
      'Sunset (lst->lct (S/R-interpolate (cdr S/R-set1) (cdr S/R-set2))
                        localtime longitude))))
