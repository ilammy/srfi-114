;; standard-comparisons.scm -- standard comparison definitions and constructors
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE

(define-syntax or=
  (syntax-rules ()
    ((_ comparison consequent)
     (let ((value comparison)) (if=? value consequent value)))))


;; Primitive types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (null-comparison null1 null2) 0)

(define (boolean-comparison bool1 bool2)
  (if (eq? bool1 bool2) 0
      (if (eq? bool1 #f) -1 +1)))

(define (char-comparison char1 char2)
  (if (char=? char1 char2) 0
      (if (char<? char1 char2) -1 +1)))

(define (char-ci-comparison char1 char2)
  (if (char-ci=? char1 char2) 0
      (if (char-ci<? char1 char2) -1 +1)))

(define (string-comparison str1 str2)
  (if (string=? str1 str2) 0
      (if (string<? str1 str2) -1 +1)))

(define (string-ci-comparison str1 str2)
  (if (string-ci=? str1 str2) 0
      (if (string-ci<? str1 str2) -1 +1)))

(define (symbol-comparison sym1 sym2)
  (if (symbol=? sym1 sym2) 0
      (if (string<? (symbol->string sym1) (symbol->string sym2)) -1 +1)))

(define (real-number-comparison num1 num2)
  (if (= num1 num2) 0
      (if (< num1 num2) -1 +1)))

(define (complex-number-comparison num1 num2)
  (or= (real-number-comparison (real-part num1) (real-part num2))
       (real-number-comparison (imag-part num1) (imag-part num2))))


;; Pairs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-pair-comparison compare-cars compare-cdrs)
  (lambda (pair1 pair2)
    (or= (compare-cars (car pair1) (car pair2))
         (compare-cdrs (cdr pair1) (cdr pair2)))))

(define pair-comparison
  (make-pair-comparison default-comparison default-comparison))

(define (make-improper-list-comparison compare)
  (define (choose-improper-comparison obj)
    (cond ((null? obj) (values 0 null-comparison))
          ((pair? obj) (values 1 pair-comparison))
          (else        (values 2 compare))))
  (lambda (obj1 obj2)
    (let-values (((obj1-order obj1-compare) (choose-improper-comparison obj1))
                 ((obj2-order obj2-compare) (choose-improper-comparison obj2)))
      (cond ((< obj1-order obj2-order) -1)
            ((> obj1-order obj2-order) +1)
            (else (obj1-compare obj1 obj2))))))


;; Lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-listwise-comparison compare-heads empty? head tail)
  (define (compare-lists list1 list2)
    (if (empty? list1)
        (if (empty? list2) 0 -1)
        (if (empty? list2) 1
            (or= (compare-heads (head list1) (head list2))
                 (compare-lists (tail list1) (tail list2))))))
  compare-lists)

(define (make-list-comparison compare-heads)
  (make-listwise-comparison compare-heads null? car cdr))

(define list-comparison
  (make-list-comparison default-comparison))


;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-vectorwise-comparison compare length ref)
  (lambda (vec1 vec2)
    (or= (real-number-comparison (length vec1) (length vec2))
         (let ((length (length vec1)))
           (let loop ((i 0))
             (if (= i length) 0
               (or= (compare (ref vec1 i) (ref vec2 i))
                    (loop (+ 1 i)))))))))

(define (make-vector-comparison compare)
  (make-vectorwise-comparison compare vector-length vector-ref))

(define (make-bytevector-comparison compare)
  (make-vectorwise-comparison compare bytevector-length bytevector-u8-ref))

(define vector-comparison
  (make-vector-comparison default-comparison))

(define bytevector-comparison
  (make-bytevector-comparison real-number-comparison))
