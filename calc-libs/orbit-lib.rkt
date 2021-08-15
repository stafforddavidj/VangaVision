#lang racket
(require gregor
         "./time-lib.rkt")
(provide J2000 get-mean-anomaly get-orbit-anomaly)

(define J2000 (date->jdn 2000 1 1.5))

(define (true-estimate Md init-est orbit-e)
  (letrec ([Mr (degrees->radians Md)]
           [true-est (lambda (first-est)
                       (define (Kepler-est prev-est [delta-goal 0.000002] [max-iter 12] [_iter 0])
                         (let ([this-est (- prev-est
                                            (/ (- prev-est (* orbit-e (sin prev-est)) Mr)
                                               (- 1 (* orbit-e (cos prev-est)))))])
                           (cond [(< (abs (- prev-est this-est)) delta-goal) this-est]
                                 [(= _iter max-iter) this-est]
                                 [else (Kepler-est this-est delta-goal max-iter (+ _iter 1))])))
                       (Kepler-est (degrees->radians first-est)))])
    (radians->degrees (* (atan
                           (* (sqrt (/ (+ 1 orbit-e) (- 1 orbit-e)))
                              (tan (/ (true-est init-est) 2))))
                       2))))

; This is separate so it can be retrieved separately for solar calculations
(define (get-mean-anomaly utc-time element-table)
  (letrec ([Julian-time (date->jdn (->year utc-time)
                                   (->month utc-time)
                                   (+ (->day utc-time)
                                      (time->day% (->hours utc-time)
                                                  (->minutes utc-time)
                                                  (->seconds utc-time))))]
           [De (- Julian-time J2000)]
           [M  (- (+ (/ (* 360 De)
                        (* 365.242191 (hash-ref element-table 'Tp)))
                     (hash-ref element-table 'epsilon0))
                  (hash-ref element-table 'w0))])
    (range-normalize M 360)))

(define (get-orbit-anomaly utc-time element-table [m-anomaly #f])
  (letrec ([M (if (eq? m-anomaly #f)
                  (get-mean-anomaly utc-time element-table)
                  m-anomaly)]
           [init-est (if (< M 0.75) M pi)])
          (range-normalize (true-estimate M init-est (hash-ref element-table 'e)) 360)))
