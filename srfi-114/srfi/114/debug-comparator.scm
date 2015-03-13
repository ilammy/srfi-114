;; debug-comparator.scm -- comparator with invariant checks for debugging
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE


;; Relation definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (implies p q) (not (and p (not q))))

(define (relation-reflexive? relation obj)
  (relation obj obj))

(define (relation-symmetric? relation obj1 obj2)
  (eq? (relation obj1 obj2)
       (relation obj2 obj1)))

(define (relation-antisymmetric? relation equality obj1 obj2)
  (implies (and (relation obj1 obj2) (relation obj2 obj1))
           (equality obj1 obj2)))

(define (relation-transitive? relation obj1 obj2 obj3)
  (define (holds-for obj1 obj2 obj3)
    (implies (and (relation obj1 obj2) (relation obj2 obj3))
             (relation obj1 obj3)))
  (and (holds-for obj1 obj2 obj3)
       (holds-for obj1 obj3 obj2)
       (holds-for obj3 obj1 obj2)
       (holds-for obj2 obj1 obj3)
       (holds-for obj2 obj3 obj1)))


;; Expected types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type-check-result? value) (boolean? value))

(define (equality-result? value) (boolean? value))

(define (comparison-result? value) (case value ((-1 0 1) #t) (else #f)))

(define (hash-result? value) (and (exact-integer? value) (positive? value)))


;; Property verifiers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (verify-type-check-type type-check obj)
  (unless (type-check-result? (type-check obj))
    (error "debug-comparator: invalid type-check value" obj (type-check obj))))

(define (verify-equality-type equal? obj1 obj2)
  (unless (equality-result? (equal? obj1 obj2))
    (error "debug-comparator: invalid equality value" obj1 obj2 (equal? obj1 obj2))))

(define (verify-comparison-type compare obj1 obj2)
  (unless (comparison-result? (compare obj1 obj2))
    (error "debug-comparator: invalid comparison value" obj1 obj2 (compare obj1 obj2))))

(define (verify-hash-function-type hash obj)
  (unless (hash-result? (hash obj))
    (error "debug-comparator: invalid hash function value" obj (hash obj))))

(define (verify-strict-type-operation type-check obj)
  (unless (type-check obj)
    (error "debug-comparator: comparator applied to an object of unexpected type" obj)))

(define (verify-equality-reflexivity equal? obj)
  (unless (relation-reflexive? equal? obj)
    (error "debug-comparator: equality predicate is not reflexive" obj)))

(define (verify-equality-symmetry equal? obj1 obj2)
  (unless (relation-symmetric? equal? obj1 obj2)
    (error "debug-comparator: equality predicate is not symmetric" obj1 obj2)))

(define (verify-equality-transitivity equal? obj1 obj2 obj3)
  (unless (relation-transitive? equal? obj1 obj2 obj3)
    (error "debug-comparator: equality predicate is not transitive" obj1 obj2 obj3)))

(define (comparison->partial-order compare)
  (lambda (obj1 obj2)
    (if<=? (compare obj1 obj2) #t #f)))

(define (comparison->equality compare)
  (lambda (obj1 obj2)
    (if=? (compare obj1 obj2) #t #f)))

(define (verify-comparison-reflexivity compare obj)
  (unless (relation-reflexive? (comparison->partial-order compare) obj)
    (error "debug-comparator: comparison procedure is not reflexive" obj)))

(define (verify-comparison-antisymmetry compare obj1 obj2)
  (unless (relation-antisymmetric? (comparison->partial-order compare)
           (comparison->equality compare) obj1 obj2)
    (error "debug-comparator: comparison procedure is not antisymmetric" obj1 obj2)))

(define (verify-comparison-transitivity compare obj1 obj2 obj3)
  (unless (relation-transitive? (comparison->partial-order compare) obj1 obj2 obj3)
    (error "debug-comparator: comparison procedure is not transitive" obj1 obj2 obj3)))


;; Verifying decorators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-verifying-unary-procedure comparator proc)
  (let ((type-check (comparator-type-test-procedure  comparator))
        (equal?     (comparator-equality-predicate   comparator))
        (compare    (comparator-comparison-procedure comparator))
        (hash       (comparator-hash-function        comparator)))
    (lambda (obj)
      (verify-type-check-type        type-check obj)
      (verify-strict-type-operation  type-check obj)
      (verify-equality-type          equal? obj obj)
      (verify-equality-reflexivity   equal? obj)
      (verify-comparison-type        compare obj obj)
      (verify-comparison-reflexivity compare obj)
      (verify-hash-function-type     hash obj)
      (proc obj))))

; The SRFI explicitly allows the implementations to keep a global third object
; to be used for verification of transitivity of the equivalence and comparison.
(define (make-obj3) (cons #f #f))

(define (make-verifying-binary-procedure comparator proc obj3)
  (let ((type-check (comparator-type-test-procedure  comparator))
        (equal?     (comparator-equality-predicate   comparator))
        (compare    (comparator-comparison-procedure comparator))
        (hash       (comparator-hash-function        comparator)))
    (lambda (obj1 obj2)
      (verify-type-check-type       type-check obj1)
      (verify-type-check-type       type-check obj2)
      (verify-strict-type-operation type-check obj1)
      (verify-strict-type-operation type-check obj2)
      (unless (car obj3)
        (set-car! obj3 #t)    ; after checking object types
        (set-cdr! obj3 obj1))
      (verify-equality-type           equal? obj1 obj2)
      (verify-equality-reflexivity    equal? obj1)
      (verify-equality-reflexivity    equal? obj2)
      (verify-equality-symmetry       equal? obj1 obj2)
      (verify-equality-transitivity   equal? obj1 obj2 (cdr obj3))
      (verify-comparison-type         compare obj1 obj2)
      (verify-comparison-reflexivity  compare obj1)
      (verify-comparison-reflexivity  compare obj2)
      (verify-comparison-antisymmetry compare obj1 obj2)
      (verify-comparison-transitivity compare obj1 obj2 (cdr obj3))
      (verify-hash-function-type      hash obj1)
      (verify-hash-function-type      hash obj2)
      (proc obj1 obj2))))

(define (make-verifying-type-check comparator)
  (make-verifying-unary-procedure comparator
    (comparator-type-test-procedure comparator)))

(define (make-verifying-equality-predicate comparator obj3)
  (make-verifying-binary-procedure comparator
    (comparator-equality-predicate comparator) obj3))

(define (make-verifying-comparison-procedure comparator obj3)
  (make-verifying-binary-procedure comparator
    (comparator-comparison-procedure comparator) obj3))

(define (make-verifying-hash-function comparator)
  (make-verifying-unary-procedure comparator
    (comparator-hash-function comparator)))


;; Comparator combination ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-debug-comparator comparator)
  (let ((obj3 (make-obj3)))
    (make-comparator
      (make-verifying-type-check comparator)
      (make-verifying-equality-predicate comparator obj3)
      (make-verifying-comparison-procedure comparator obj3)
      (make-verifying-hash-function comparator))))
