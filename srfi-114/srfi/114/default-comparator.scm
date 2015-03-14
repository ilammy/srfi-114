;; default-comparator.scm -- default comparison and comparator
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE


;; Default comparison and hashing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fallback-comparator
  (make-comparator #t
    (lambda (a b) #t)
    (lambda (a b) 0)
    hash))

; This procedure defines the default ordering of objects by their types. Objects
; of known primitive types have strict order between them. All other objects are
; treated as equal. Primitive objects are less than any non-primitive object.
(define (choose-default-comparator obj)
  (cond ((null? obj)       (values 0 null-comparator))
        ((pair? obj)       (values 1 pair-comparator))
        ((boolean? obj)    (values 2 boolean-comparator))
        ((char? obj)       (values 3 char-comparator))
        ((string? obj)     (values 4 string-comparator))
        ((symbol? obj)     (values 5 symbol-comparator))
        ((number? obj)     (values 6 number-comparator))
        ((vector? obj)     (values 7 vector-comparator))
        ((bytevector? obj) (values 8 bytevector-comparator))
        (else              (values 9 fallback-comparator))))

(define (default-equality obj1 obj2)
  (let-values (((obj1-order obj1-comparator) (choose-default-comparator obj1))
               ((obj2-order obj2-comparator) (choose-default-comparator obj2)))
    (if (= obj1-order obj2-order)
        (comparator-equal? obj1-comparator obj1 obj2)
        #f)))

(define (default-comparison obj1 obj2)
  (let-values (((obj1-order obj1-comparator) (choose-default-comparator obj1))
               ((obj2-order obj2-comparator) (choose-default-comparator obj2)))
    (cond ((< obj1-order obj2-order) -1)
          ((> obj1-order obj2-order) +1)
          (else
            (comparator-compare obj1-comparator obj1 obj2)))))

(define (default-hash obj)
  (let-values (((order comparator) (choose-default-comparator obj)))
    (comparator-hash comparator obj)))


;; The default comparator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-comparator
  (make-comparator #t default-equality default-comparison default-hash))
