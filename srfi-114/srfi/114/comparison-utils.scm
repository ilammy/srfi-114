;; comparison-utils.scm -- comparison constructors, combinators, syntax
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE


;; Comparison syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax if3
  (syntax-rules ()
    ((_ expr less equal greater)
     (case expr
       ((-1) less)
       (( 0) equal)
       ((+1) greater)
       (else (error "if3: invalid comparison result" expr))))))

(define-syntax define-comparison-syntax
  (syntax-rules ()
    ((_ binding yes no error-message)
     (define-syntax binding
       (syntax-rules ()
         ((_ expr consequent) (binding expr consequent (when #f #f)))
         ((_ expr consequent alternate)
          (case expr
            (yes consequent)
            (no   alternate)
            (else (error error-message expr)))))))))

(define-comparison-syntax if=?     (0) (-1 +1) "if=?: invalid comparison result")
(define-comparison-syntax if<?     (-1) (0 +1) "if<?: invalid comparison result")
(define-comparison-syntax if>?     (+1) (0 -1) "if>?: invalid comparison result")
(define-comparison-syntax if<=?    (-1 0) (+1) "if<=?: invalid comparison result")
(define-comparison-syntax if>=?    (0 +1) (-1) "if>=?: invalid comparison result")
(define-comparison-syntax if-not=? (-1 +1) (0) "if-not=?: invalid comparison result")


;; Comparison predicate constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make= comparator)
  (comparator-equality-predicate comparator))

(define-syntax define-comparison-predicate-constructor
  (syntax-rules ()
    ((_ binding if-in-order?)
     (define (binding comparator)
       (let ((compare (comparator-comparison-procedure comparator)))
         (lambda (obj1 obj2)
           (if-in-order? (compare obj1 obj2) #t #f)))))))

(define-comparison-predicate-constructor make<  if<?)
(define-comparison-predicate-constructor make>  if>?)
(define-comparison-predicate-constructor make<= if<=?)
(define-comparison-predicate-constructor make>= if>=?)


;; Comparison procedure constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-comparison< <)
  (lambda (obj1 obj2)
    (cond ((< obj1 obj2) -1)
          ((< obj2 obj1) +1)
          (else           0))))

(define (make-comparison> >)
  (lambda (obj1 obj2)
    (cond ((> obj1 obj2) +1)
          ((> obj2 obj1) -1)
          (else           0))))

(define (make-comparison<= <=)
  (lambda (obj1 obj2)
    (if (<= obj1 obj2)
        (if (<= obj2 obj1) 0 -1) +1)))

(define (make-comparison>= >=)
  (lambda (obj1 obj2)
    (if (>= obj1 obj2)
        (if (>= obj2 obj1) 0 +1) -1)))

(define (make-comparison=/< = <)
  (lambda (obj1 obj2)
    (cond ((= obj1 obj2)  0)
          ((< obj2 obj1) +1)
          (else          -1))))

(define (make-comparison=/> = >)
  (lambda (obj1 obj2)
    (cond ((= obj1 obj2)  0)
          ((> obj2 obj1) -1)
          (else          +1))))


;; Comparison predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-comparison-predicate make-relation)
  (lambda (comparator obj1 obj2 . objs)
    (let ((relation (make-relation comparator)))
      (if (null? objs)
          (relation obj1 obj2)
          (let loop ((objs (cons obj1 (cons obj2 objs))))
            (if (null? (cdr objs)) #t
                (if (relation (car objs) (cadr objs))
                    (loop (cdr objs))
                    #f)))))))

(define =?  (make-comparison-predicate make=))
(define <?  (make-comparison-predicate make<))
(define >?  (make-comparison-predicate make>))
(define <=? (make-comparison-predicate make<=))
(define >=? (make-comparison-predicate make>=))


;; Interval comparison predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-interval-comparison-predicate left-relation right-relation)
  (define predicate
    (case-lambda
      ((obj1 obj2 obj3) (predicate default-comparator obj1 obj2 obj3))
      ((comparator obj1 obj2 obj3)
       (and (left-relation comparator obj1 obj2)
            (right-relation comparator obj2 obj3)))))
  predicate)

(define in-open-interval?        (make-interval-comparison-predicate <?   <?))
(define in-open-closed-interval? (make-interval-comparison-predicate <?  <=?))
(define in-closed-open-interval? (make-interval-comparison-predicate <=?  <?))
(define in-closed-interval?      (make-interval-comparison-predicate <=? <=?))


;; Min/max comparison procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (comparator-min comparator obj . objs)
  (let ((< (make< comparator)))
    (let loop ((min obj) (objs objs))
      (if (null? objs) min
          (loop (if (< (car objs) min) (car objs) min)
                (cdr objs))))))

(define (comparator-max comparator obj . objs)
  (let ((> (make> comparator)))
    (let loop ((max obj) (objs objs))
      (if (null? objs) max
          (loop (if (> (car objs) max) (car objs) max)
                (cdr objs))))))
