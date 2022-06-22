#lang web-server
(require json
         web-server/servlet/web
         web-server/servlet-env
         "json-serializer.rkt")
 
(define (start req)
  (let ([req-body (vanga-json (request-post-data/raw req))])
    (response/jsexpr req-body)))
 
(serve/servlet start #:command-line? #t
                     #:servlet-current-directory (current-directory))
