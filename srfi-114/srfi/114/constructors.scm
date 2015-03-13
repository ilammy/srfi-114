;; constructors.scm -- advanced comparator constructors and combinators
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE


;; Real comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-inexact-real-comparison epsilon rounding nan-handling)
  (lambda (num1 num2)
    (if (or (nan? num1) (nan? num2))
        (compare-inexact-nans num1 num2 epsilon rounding nan-handling)
        (compare-inexact-real num1 num2 epsilon rounding))))

(define (compare-inexact-nans num1 num2 epsilon rounding nan-handling)
  (if (and (nan? num1) (nan? num2)) 0 ; because this is what the SRFI says
    (case nan-handling
      ((min) (if (nan? num1) -1 +1))
      ((max) (if (nan? num1) +1 -1))
      ((error) (error "inexact-real-comparison: cannot compare NaNs"))
      (else
        (if (nan? num1)
            (compare-inexact-real (nan-handling num2) num2 epsilon rounding)
            (compare-inexact-real num1 (nan-handling num1) epsilon rounding))))))

(define (compare-inexact-real num1 num2 epsilon rounding)
  (real-number-comparison
    (round-number num1 epsilon rounding)
    (round-number num2 epsilon rounding)))

(define (round-number num epsilon rounding)
  (case rounding
    ((floor)    (floor    (/ num epsilon))) ; don't bother multiplying these
    ((ceiling)  (ceiling  (/ num epsilon))) ; back by epsilon for a proper round
    ((truncate) (truncate (/ num epsilon))) ; as it has no effect on comparison
    ((round)    (round    (/ num epsilon)))
    (else
      (rounding num epsilon))))

(define (make-inexact-real-comparator epsilon rounding nan-handling)
  (make-comparator number? #t
    (make-inexact-real-comparison epsilon rounding nan-handling)
    hash))


;; Collection comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-list-comparator element-comparator)
  (make-comparator list? #t
    (make-list-comparison
      (comparator-comparison-procedure element-comparator))
    hash))

(define (make-vector-comparator element-comparator)
  (make-comparator vector? #t
    (make-vector-comparison
      (comparator-comparison-procedure element-comparator))
    hash))

(define (make-bytevector-comparator element-comparator)
  (make-comparator bytevector? #t
    (make-bytevector-comparison
      (comparator-comparison-procedure element-comparator))
    hash))

(define (make-listwise-comparator type-test element-comparator empty? head tail)
  (make-comparator type-test #t
    (make-listwise-comparison
      (comparator-comparison-procedure element-comparator)
      empty? head tail)
    hash))

(define (make-vectorwise-comparator type-test element-comparator length ref)
  (make-comparator type-test #t
    (make-vectorwise-comparison
      (comparator-comparison-procedure element-comparator)
      length ref)
    hash))


;; Pair comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-car-comparison compare)
  (lambda (pair1 pair2)
    (compare (car pair1) (car pair2))))

(define (make-cdr-comparison compare)
  (lambda (pair1 pair2)
    (compare (cdr pair1) (cdr pair2))))

(define (make-car-comparator comparator)
  (make-comparator pair? #t
    (make-car-comparison (comparator-comparison-procedure comparator))
    (lambda (pair) (hash (car pair)))))

(define (make-cdr-comparator comparator)
  (make-comparator pair? #t
    (make-cdr-comparison (comparator-comparison-procedure comparator))
    (lambda (pair) (hash (cdr pair)))))

(define (make-pair-comparator car-comparator cdr-comparator)
  (make-comparator pair? #t
    (make-pair-comparison
      (comparator-comparison-procedure car-comparator)
      (comparator-comparison-procedure cdr-comparator))
    hash))

(define (make-improper-list-comparator element-comparator)
  (make-comparator #t #t
    (make-improper-list-comparison
      (comparator-comparison-procedure element-comparator))
    default-hash))


;; Selecting & refining comparators: type refining ;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-type-refining-combination
  (syntax-rules (then else)
    ; Allow the (next) clause to be omitted
    ((_ binding then-clause else-clause)
     (define-type-refining-combination binding (next) then-clause else-clause))
    ; Bind the combinator to `binding`, with `args ...` as combination arguments.
    ; Combinator takes a list of type-checking procedures and additional lists
    ; of `procedures ...` and combines them in a type-refining manner. If a type
    ; match is successful, `then-body` is evaluated. You can skip to more refined
    ; type with `next`, and you can take `vars ...` with you (which have default
    ; values of `default ...`). If no type match is found, `else-body` is evaluated.
    ((_ (binding args ...)
        (next (var default) ...)
        (then (procedures ...) . then-body)
        (else . else-body))
     (define (binding type-tests procedures ...)
       (lambda (args ...)
         (let loop ((type-tests type-tests) (procedures procedures) ...
                    (var default) ...)
           (if (null? type-tests)
               (begin . else-body)
               (let-syntax
                 ((next (syntax-rules ()
                          ((next var ...)
                           (loop (cdr type-tests) (cdr procedures) ... var ...)))))
                 (if (and ((car type-tests) args) ...)
                     (let ((procedures (car procedures)) ...)
                       . then-body)
                     (next var ...))))))))))


;; Selecting & refining comparators: procedure combinators ;;;;;;;;;;;;;

(define-type-refining-combination (make-selecting-type-test-procedure obj)
  (then () #t)
  (else #f))

(define-type-refining-combination (make-selecting-equality-predicate obj1 obj2)
  (then (equal?) (equal? obj1 obj2))
  (else (error "selecting-comparator: unsupported object type" obj1 obj2)))

(define-type-refining-combination (make-selecting-comparison-procedure obj1 obj2)
  (then (compare) (compare obj1 obj2))
  (else (error "selecting-comparator: unsupported object type" obj1 obj2)))

(define-type-refining-combination (make-selecting-hash-function obj)
  (then (hash) (hash obj))
  (else (error "selecting-comparator: unsupported object type" obj)))

(define-type-refining-combination (make-refining-equality-predicate obj1 obj2)
  (next (equality-met? #f))
  (then (equal?)
    (if (equal? obj1 obj2) #t (next #t)))
  (else
    (if equality-met? #t
        (error "refining-comparator: unsupported object type" obj1 obj2))))

(define-type-refining-combination (make-refining-comparison-procedure obj1 obj2)
  (next (equality-met? #f))
  (then (compare)
    (or= (compare obj1 obj2) (next #t)))
  (else
    (if equality-met? 0
        (error "refining-comparator: unsupported object type" obj1 obj2))))

(define-type-refining-combination (make-refining-hash-function obj)
  (then (hash) (hash obj))
  (else (error "refining-comparator: unsupported object type" obj)))


;; Selecting & refining comparators: interface ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-decomposed-comparators comparators handler)
  (handler
    (map comparator-type-test-procedure  comparators)
    (map comparator-equality-predicate   comparators)
    (map comparator-comparison-procedure comparators)
    (map comparator-hash-function        comparators)))

(define (make-selecting-comparator comparator . comparators)
  (if (null? comparators)
      comparator
      (with-decomposed-comparators (cons comparator comparators)
        (lambda (type-tests equalities comparisons hashes)
          (make-comparator
            (make-selecting-type-test-procedure  type-tests)
            (make-selecting-equality-predicate   type-tests equalities)
            (make-selecting-comparison-procedure type-tests comparisons)
            (make-selecting-hash-function        type-tests hashes))))))

(define (make-refining-comparator comparator . comparators)
  (if (null? comparators)
      comparator
      (with-decomposed-comparators (cons comparator comparators)
        (lambda (type-tests equalities comparisons hashes)
          (make-comparator
            (make-selecting-type-test-procedure type-tests)
            (make-refining-equality-predicate   type-tests equalities)
            (make-refining-comparison-procedure type-tests comparisons)
            (make-selecting-hash-function       type-tests hashes))))))


;; Reverse comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inverse-comparison compare)
  (lambda (obj1 obj2)
    (- (compare obj1 obj2))))

(define (make-reverse-comparator comparator)
  (make-comparator
    (comparator-type-test-procedure comparator)
    (comparator-equality-predicate  comparator)
    (inverse-comparison (comparator-comparison-procedure comparator))
    (comparator-hash-function comparator)))
