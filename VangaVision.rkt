#lang racket
(require gregor
         "solar.rkt"
         "lunar.rkt"
         "./ffi-ZoneDetect/zonedetect-wrapper.rkt")

(define (get-arg index)
  (vector-ref (current-command-line-arguments) index))

(define (hash-ref* table key-list)
  (let ([current-key (car key-list)])
    (cond [(empty? (cdr key-list)) (hash-ref table current-key)]
          [(hash? (hash-ref table current-key))
           (let ([next-table (hash-ref table current-key)])
             (if (hash-has-key? next-table (cadr key-list))
                 (hash-ref* next-table (cdr key-list))
                 (raise-argument-error 'hash-ref* "hash-has-key?" current-key)))]
          [else (raise-argument-error 'hash-ref* "hash?" table)])))

(define (vanga-capture local-time latitude longitude)
  (let ([solar-data (get-solar-data local-time latitude longitude)])
    (hash
      'Geolocation (cons latitude longitude)
      'Timezone (->timezone local-time)
      'LocalTime (->datetime/local local-time)
      'SolarData solar-data
      'LunarData (get-lunar-data local-time solar-data latitude longitude)
      ; solar system planet locations
      ; observer's meridian star chart
    )))

(module+ main (cond 
  [(equal? (vector-length (current-command-line-arguments)) 3)
   (let ([input-datetime (get-arg 0)]
         [input-lat (string->number (get-arg 1))]
         [input-long (string->number (get-arg 2))])

        ; Parse latitude, and longitude from input
        ; Coerce values to decimals if lat/long parse as ints
        (define latitude (cond [(not (number? input-lat))
                                (raise-argument-error 'latitude "number?" (get-arg 1))]
                               [(exact? input-lat) (exact->inexact input-lat)]
                               [else input-lat]))
        (define longitude (cond [(not (number? input-long))
                                 (raise-argument-error 'longitude "number?" (get-arg 2))]
                                [(exact? input-long) (exact->inexact input-long)]
                                [else input-long]))

        ; Lookup timezone from geolocation and parse ISO-8601 datetime
        (define timezone (ZD-timezone-lookup latitude longitude))
        (define local-time (with-handlers
          ([exn:fail? (lambda (err) 
            (raise-argument-error 'local-time 
                                  "format: \"yyyy-MM-ddTHH:mm:ss\"" 
                                  input-datetime))])
          (parameterize ([current-timezone timezone])
            (parse-moment input-datetime 
                          "yyyy-MM-dd'T'HH:mm:ss"
                          #:resolve-offset resolve-offset/post))))

        (define test-moment (vanga-capture local-time latitude longitude))
        (define (debug-output ref-input)
          (let ([ref-list (if (list? ref-input) ref-input (cons ref-input '()))])
               (display (string-append 
                          (symbol->string (last ref-list)) ": "))
               (displayln (hash-ref* test-moment ref-list))))
        ; Print values of times derived from calculations
        (debug-output 'Geolocation)
        (debug-output 'Timezone)
        (debug-output 'LocalTime)
        (displayln "Solar Data: ")
        (debug-output (list 'SolarData 'Horizon))
        (debug-output (list 'SolarData 'Sunrise))
        (debug-output (list 'SolarData 'Sunset))
        (displayln "Lunar Data: ")
        (debug-output (list 'LunarData 'Horizon))
        (debug-output (list 'LunarData 'Moonrise))
        (debug-output (list 'LunarData 'Moonset)))]

  [else (raise-user-error
    (format "~a~n~n~a"
            "Incorrect number of arguments supplied."
            "  Usage: racket VangaVision.rkt <ISO8601 DateTime> <Latitude> <Longitude>"))]))
