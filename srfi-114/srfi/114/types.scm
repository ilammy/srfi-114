;; types.scm -- the comparator record type, accessors, simple constructor
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE


;; Type definition and accessors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Comparator
  (%make-comparator type-test-fn equality-fn comparison-fn hash-fn)
  comparator?
  (type-test-fn  comparator-type-test-procedure)
  (equality-fn   comparator-equality-predicate)
  (comparison-fn comparator-comparison-procedure)
  (hash-fn       comparator-hash-function))


;; Primitive applicators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (comparator-test-type comparator obj)
  ((comparator-type-test-procedure comparator) obj))

(define (comparator-check-type comparator obj)
  (let ((result ((comparator-type-test-procedure comparator) obj)))
    (if result result
        (error "comparator-check-type: incorrect object type" result))))

(define (comparator-equal? comparator obj1 obj2)
  ((comparator-equality-predicate comparator) obj1 obj2))

(define (comparator-compare comparator obj1 obj2)
  ((comparator-comparison-procedure comparator) obj1 obj2))

(define (comparator-hash comparator obj)
  ((comparator-hash-function comparator) obj))


;; Default procedures and predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-type-test obj) #t)

(define (failure-compare obj1 obj2)
  (error "Comparator: comparison procedure not specified"))

(define (failure-hash obj)
  (error "Comparator: hash function not specified"))

(define (make-default-equality compare)
  (lambda (obj1 obj2)
    (= 0 (compare obj1 obj2))))

(define (comparator-comparison-procedure? comparator)
  (not (eq? failure-compare (comparator-comparison-procedure comparator))))

(define (comparator-hash-function? comparator)
  (not (eq? failure-hash (comparator-hash-function comparator))))


;; Principal constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax if-eq
  (syntax-rules ()
    ((_ indicator value default)
     (if (eq? indicator value) default value))))

(define (make-comparator type-test equality compare hash)
  (let* ((type-test (if-eq #t type-test default-type-test))
         (compare   (if-eq #f compare   failure-compare))
         (equality  (if-eq #t equality (make-default-equality compare)))
         (hash      (if-eq #f hash      failure-hash)))
    (%make-comparator type-test equality compare hash)))
