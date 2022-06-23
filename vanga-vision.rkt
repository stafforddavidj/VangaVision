#lang racket
(require racket/pretty
         gregor
         "./solar-libs/solar.rkt"
         "./solar-libs/lunar.rkt"
         "./solar-libs/test-json.rkt"
         "./ffi-ZoneDetect/zonedetect-wrapper.rkt")
(provide vanga-capture)

(define (get-arg index)
  (vector-ref (current-command-line-arguments) index))

; Parse latitude, and longitude from input
; Coerce values to decimals if lat/long parse as ints
(define (parse-number var-name input-num)
  (cond [(not (number? input-num))
         (raise-argument-error var-name "number?" input-num)]
        [(exact? input-num) (exact->inexact input-num)]
        [else input-num]))

; Lookup timezone from geolocation and parse ISO-8601 datetime
(define (parse-datetime input-datetime latitude longitude)
  (let ([timezone (ZD-timezone-lookup latitude longitude)])
    (with-handlers
      ([exn:fail? (lambda (err)
        (raise-argument-error
          'local-time
          "format: \"yyyy-MM-ddTHH:mm:ss\""
          input-datetime))])
      (parameterize ([current-timezone timezone])
        (parse-moment input-datetime
          "yyyy-MM-dd'T'HH:mm:ss"
          #:resolve-offset resolve-offset/post)))))

(define (vanga-capture #:local-time input-datetime
                       #:latitude input-lat
                       #:longitude input-long)
  (letrec (
    [latitude (parse-number 'latitude input-lat)]
    [longitude (parse-number 'longitude input-long)]
    [localtime (parse-datetime input-datetime latitude longitude)]
    [solar-data (get-solar-data localtime latitude longitude)])
      (hash
        'Geolocation (cons latitude longitude)
        'Timezone (->timezone localtime)
        'LocalTime (->datetime/local localtime)
        'SolarData solar-data
        'LunarData (get-lunar-data localtime solar-data latitude longitude)
        ; solar system planet locations
        ; observer's meridian star chart
      )))

(module+ main (cond
  [(equal? (vector-length (current-command-line-arguments)) 3)
   (pretty-display
    (vanga-capture
      #:local-time (get-arg 0)
      #:latitude (string->number (get-arg 1))
      #:longitude (string->number (get-arg 2))))]
  [(equal? (vector-length (current-command-line-arguments)) 0)
   (pretty-display
    (vanga-capture
      #:local-time (hash-ref test-json 'LocalTime)
      #:latitude (hash-ref test-json 'Latitude)
      #:longitude (hash-ref test-json 'Longitude)))]
  [else (raise-user-error
    (format "~a~n~n~a"
            "Incorrect number of arguments supplied."
            "  Usage: racket VangaVision.rkt <ISO8601 DateTime> <Latitude> <Longitude>"))]))
