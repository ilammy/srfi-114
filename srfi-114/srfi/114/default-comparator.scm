;; default-comparator.scm -- default comparison and comparator
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE


;; Default comparison and hashing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax type-ordered-comparison-procedure
  (syntax-rules (else)
    ((_ (obj1 obj2) (type? compare) ... (else . body))
     (lambda (obj1 obj2)
       (cond ((type? obj1) (if (type? obj2) (compare obj1 obj2) -1)) ...
             (else . body))))))

(define (fallback-comparison obj1 obj2) 0)

(define default-comparison
  (type-ordered-comparison-procedure (obj1 obj2)
    (null?       null-comparison)
    (pair?       pair-comparison)
    (boolean?    boolean-comparison)
    (char?       char-comparison)
    (string?     string-comparison)
    (symbol?     symbol-comparison)
    (number?     complex-number-comparison)
    (vector?     vector-comparison)
    (bytevector? bytevector-comparison)
    (else
      (fallback-comparison obj1 obj2))))

(define (default-hash obj)
  (cond ((null? obj)       (hash-by-identity obj))
        ((pair? obj)       (hash obj))
        ((boolean? obj)    (hash-by-identity obj))
        ((char? obj)       (hash obj))
        ((string? obj)     (string-hash obj))
        ((symbol? obj)     (hash-by-identity obj))
        ((number? obj)     (hash obj))
        ((vector? obj)     (hash obj))
        ((bytevector? obj) (hash obj))
        (else              (hash obj))))


;; The default comparator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-comparator
  (make-comparator #t #t default-comparison default-hash))
