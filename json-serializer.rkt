#lang racket
(require json
         gregor
         racket/pretty
         "vanga-vision.rkt"
         "./solar-libs/test-json.rkt")
(provide vanga-json)

; Gregor datetimes and cons pairs need to be changed to 
; datatypes that are valid JSON
(define (serialize output-object)
  (for/hash ([key (in-hash-keys output-object)])
    (let ([val (hash-ref output-object key)])
      (cond [(hash? val)
             (values key (serialize (hash-ref output-object key)))]
            [(pair? val)
             (values key (hash 'X (car val) 'Y (cdr val)))]
            [(datetime? val)
             (values key (datetime->iso8601 val))]
            [else (values key val)]))))

(define (vanga-json input-block)
  (letrec (
    [json-block (bytes->jsexpr input-block)]
    [output-block (vanga-capture
      #:local-time (hash-ref json-block 'LocalTime)
      #:latitude (hash-ref json-block 'Latitude)
      #:longitude (hash-ref json-block 'Longitude))])
    (serialize output-block)))

(module+ main 
  (pretty-display (vanga-json (jsexpr->bytes test-json))))
