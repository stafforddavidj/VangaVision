#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
; This is the only part of this I actually use
(provide ZD-timezone-lookup)

(define-ffi-definer define-zone (ffi-lib "./ffi-ZoneDetect/ZoneDetect"))

; Objects and data structures
(define _ZD-pointer (_cpointer `ZoneDetect))
(define-cstruct _ZoneDetectResult ([ZDLookupResult _int]
                                   [polygonId _int32]
                                   [metaId _int32]
                                   [numFields _int8]
                                   [fieldNames (_array/list _string 1)]
                                   [data (_array/list _string 1)]))

; Database operations
;; open/close
(define-zone ZDOpenDatabase (_fun _path -> _ZD-pointer))
(define-zone ZDCloseDatabase (_fun _ZD-pointer -> _void))
;; lookup/free
(define-zone ZDLookup (_fun _ZD-pointer _float _float _float -> (_array/list _ZoneDetectResult 1)))
(define-zone ZDFreeResults (_fun (_array/list _ZoneDetectResult 1) -> _void))
;; return string of timezone given location is in
(define-zone ZDHelperSimpleLookupString (_fun _ZD-pointer _float _float -> _string))
;; error handling
(define-zone ZDSetErrorHandler (_fun (_fun _int _int -> _void) -> _void))
(define-zone ZDGetErrorString (_fun _int -> _string))

; Function that performs all the operations necessary for a timezone lookup
(define (ZD-timezone-lookup lat long)
  (ZDSetErrorHandler
    (lambda (error-ZD error-native)
      (printf "ZD error: ~a ~x" (ZDGetErrorString error-ZD) error-native)))

  ; Build path to database and open it
  (define lib-path (path->complete-path (string->path "./ffi-ZoneDetect/timezone21.bin")))
  (define zone-detect (ZDOpenDatabase lib-path))
  ; Perform quick lookup of timezone
  (define timezone (ZDHelperSimpleLookupString zone-detect lat long))
  ; Close database
  (ZDCloseDatabase zone-detect)
  ; Return results of lookup
  timezone)
