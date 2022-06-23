#lang racket/base
(require gregor)
(provide range-normalize time->day% date->jdn
         utc->gst gst->utc gst->lst lst->gst
         utc->lst lst->utc utc->lct lst->lct)

; Used to normalize times to range of [0, mod] (racket modulo only supports ints base)
(define (range-normalize x mod)
  (if (< 0 x mod)
      x
      (- x (* (floor (/ x mod)) mod))))

; Converts between time and fractions of a day
(define (time->day% hours minutes seconds)
  (/ (+ hours (/ (+ minutes (/ seconds 60)) 60)) 24))

; Calculate Julian day from date (use decimal days for times other than 0h)
; Ch. 3.6, pp. 41-42
(define (date->jdn year [month 1] [day 0])
  ; Correct year/month for equation
  (define year-month
    (let ([y year] [m month])
      (if (> m 2)
          (cons y m)
          (cons (- y 1) (+ m 12)))))
  (define T (if (< year 0) .75 0))
  ; If date is a Gregorian one (any after 15 Oct 1582), use leap year adjustments
  (define A-B
    (let ([input-date (date year
                            month
                            (cond [(= day 0) 1]
                                  [(integer? day) day] 
                                  [else (inexact->exact (truncate day))]))])
      (if (date<? (date 1582 10 15) input-date)
          (let ([A (truncate (/ (car year-month) 100))])
            (cons A (+ (- 2 A) (truncate (/ A 4)))))
          (cons 0 0))))
  (+ (cdr A-B)
     (truncate (- (* 365.25 (car year-month)) T))
     (truncate (* 30.6001 (+ (cdr year-month) 1)))
     day
     1720994.5))

; Conversions between UTC/GST are the step between for all other time conversions
; Chs. 3.8-3.12, pp. 46-50
(define (utc->gst utc-time)
  (letrec ([T0 (jdn-T0 utc-time)]
           [GST (+ T0 (* 1.002738 (+ (->hours utc-time)
                                     (/ (+ (->minutes utc-time)
                                           (/ (->seconds utc-time) 60)) 60))))])
          (range-normalize GST 24)))

; Conversions between GST/UTC are just meant to calculate time, as they require
; a known date. Helpful to calculate when objects cross the horizon.
(define (gst->utc gst-time utc-time)
  (letrec ([T0 (range-normalize (jdn-T0 utc-time) 24)]
           [A (let ([time-diff (- gst-time T0)])
                   (if (< time-diff 0) (+ time-diff 24) time-diff))]
           [UT  (* 0.997270 A)]
           [hours (floor UT)]
           [minutes (floor (* (- UT hours) 60))]
           [seconds (floor (* (- (* (- UT hours) 60) minutes) 60))])
          (datetime (->year utc-time)
                    (->month utc-time)
                    (->day utc-time)
                    (inexact->exact hours)
                    (inexact->exact minutes)
                    (inexact->exact seconds))))

; Used to convert between UTC and GST in either direction
(define (jdn-T0 utc-time)
  (letrec ([year (->year utc-time)]
           [month (->month utc-time)]
           [day (->day utc-time)]
           [JD0 (date->jdn year)]
           [jdn-days (- (date->jdn year month day) JD0)]
           [T (/ (- JD0 2415020.0) 36525.0)]
           [R (+ 6.6460656 (* 2400.051262 T) (* 0.00002581 (expt T 2)))]
           [B (+ (- 24 R) (* 24 (- (->year utc-time) 1900)))])
          (- (* 0.0657098 jdn-days) B)))

(define (gst->lst gst-time longitude)
  (letrec ([long-adjust (/ longitude 15)]
           [LST (+ gst-time long-adjust)])
          (range-normalize LST 24)))

(define (lst->gst lst-time longitude)
  (letrec ([long-adjust (/ longitude 15)]
           [GST (- lst-time long-adjust)])
          (range-normalize GST 24)))

(define (lst->utc lst-time utc-time longitude)
  (let ([GST (lst->gst lst-time longitude)])
    (gst->utc GST utc-time)))

(define (utc->lst utc-time longitude)
  (let ([GST (utc->gst utc-time)])
    (gst->lst GST longitude)))

; Helpful for rise/set to go from LST to LCT without
; converting everything to moments
(define (utc->lct utc-time localtime)
  (+seconds utc-time (->utc-offset localtime)))

; This passes the localtime as UTC for the lst->utc conversion
; because that is only used for the date. Since local and UTC date
; can differ, this prevents instances of the correct times being shown
; for the wrong day.
(define (lst->lct lst-time localtime longitude)
  (utc->lct (lst->utc lst-time localtime longitude) localtime))
