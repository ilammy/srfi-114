;; standard-procedures.scm -- standard comparator procedures and constructors
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE

(define-syntax or=
  (syntax-rules ()
    ((_ comparison consequent)
     (let ((value comparison)) (if=? value consequent value)))))


;; Primitive types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (null-equality null1 null2) #t)

(define (null-comparison null1 null2) 0)

(define (boolean-equality bool1 bool2)
  (eq? bool1 bool2))

(define (boolean-comparison bool1 bool2)
  (if (eq? bool1 bool2) 0
      (if (eq? bool1 #f) -1 +1)))

(define (char-equality char1 char2)
  (char=? char1 char2))

(define (char-comparison char1 char2)
  (if (char=? char1 char2) 0
      (if (char<? char1 char2) -1 +1)))

(define (char-ci-equality char1 char2)
  (char-ci=? char1 char2))

(define (char-ci-comparison char1 char2)
  (if (char-ci=? char1 char2) 0
      (if (char-ci<? char1 char2) -1 +1)))

(define (string-equality str1 str2)
  (string=? str1 str2))

(define (string-comparison str1 str2)
  (if (string=? str1 str2) 0
      (if (string<? str1 str2) -1 +1)))

(define (string-ci-equality str1 str2)
  (string-ci=? str1 str2))

(define (string-ci-comparison str1 str2)
  (if (string-ci=? str1 str2) 0
      (if (string-ci<? str1 str2) -1 +1)))

(define (symbol-equality sym1 sym2)
  (symbol=? sym1 sym2))

(define (symbol-comparison sym1 sym2)
  (if (symbol=? sym1 sym2) 0
      (if (string<? (symbol->string sym1) (symbol->string sym2)) -1 +1)))

(define (real-number-equality num1 num2)
  (= num1 num2))

(define (real-number-comparison num1 num2)
  (if (= num1 num2) 0
      (if (< num1 num2) -1 +1)))

(define (complex-number-equality num1 num2)
  (= num1 num2))

(define (complex-number-comparison num1 num2)
  (or= (real-number-comparison (real-part num1) (real-part num2))
       (real-number-comparison (imag-part num1) (imag-part num2))))


;; Pair cars and cdrs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-car-equality equal?)
  (lambda (pair1 pair2)
    (equal? (car pair1) (car pair2))))

(define (make-cdr-equality equal?)
  (lambda (pair1 pair2)
    (equal? (cdr pair1) (cdr pair2))))

(define (make-car-comparison compare)
  (lambda (pair1 pair2)
    (compare (car pair1) (car pair2))))

(define (make-cdr-comparison compare)
  (lambda (pair1 pair2)
    (compare (cdr pair1) (cdr pair2))))


;; Pairs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-pair-equality equal-cars? equal-cdrs?)
  (lambda (pair1 pair2)
    (and (equal-cars? (car pair1) (car pair2))
         (equal-cdrs? (cdr pair1) (cdr pair2)))))

(define (make-pair-comparison compare-cars compare-cdrs)
  (lambda (pair1 pair2)
    (or= (compare-cars (car pair1) (car pair2))
         (compare-cdrs (cdr pair1) (cdr pair2)))))

(define pair-equality
  (make-pair-equality default-equality default-equality))

(define pair-comparison
  (make-pair-comparison default-comparison default-comparison))

(define (make-improper-list-equality equal?)
  (define pair-equality (make-pair-equality equal? equal?))
  (define (choose-improper-equality obj)
    (cond ((null? obj) (values 0 null-equality))
          ((pair? obj) (values 1 pair-equality))
          (else        (values 2 equal?))))
  (lambda (obj1 obj2)
    (let-values (((obj1-type obj1-equal?) (choose-improper-equality obj1))
                 ((obj2-type obj2-equal?) (choose-improper-equality obj2)))
      (if (= obj1-type obj2-type)
          (obj1-equal? obj1 obj2)
          #f))))

(define (make-improper-list-comparison compare)
  (define pair-comparison (make-pair-comparison compare compare))
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

(define (make-listwise-equality equal-heads? empty? head tail)
  (define (equal-lists? list1 list2)
    (if (empty? list1)
        (if (empty? list2) #t #f)
        (if (empty? list2) #f
            (and (equal-heads? (head list1) (head list2))
                 (equal-lists? (tail list1) (tail list2))))))
  equal-lists?)

(define (make-listwise-comparison compare-heads empty? head tail)
  (define (compare-lists list1 list2)
    (if (empty? list1)
        (if (empty? list2) 0 -1)
        (if (empty? list2) 1
            (or= (compare-heads (head list1) (head list2))
                 (compare-lists (tail list1) (tail list2))))))
  compare-lists)

(define (make-list-equality equal-heads?)
  (make-listwise-equality equal-heads? null? car cdr))

(define (make-list-comparison compare-heads)
  (make-listwise-comparison compare-heads null? car cdr))

(define list-equality
  (make-list-equality default-equality))

(define list-comparison
  (make-list-comparison default-comparison))


;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-vectorwise-equality equal? length ref)
  (lambda (vec1 vec2)
    (and (real-number-equality (length vec1) (length vec2))
         (let ((length (length vec1)))
           (let loop ((i 0))
             (if (= i length) #t
                 (and (equal? (ref vec1 i) (ref vec2 i))
                      (loop (+ 1 i)))))))))

(define (make-vectorwise-comparison compare length ref)
  (lambda (vec1 vec2)
    (or= (real-number-comparison (length vec1) (length vec2))
         (let ((length (length vec1)))
           (let loop ((i 0))
             (if (= i length) 0
               (or= (compare (ref vec1 i) (ref vec2 i))
                    (loop (+ 1 i)))))))))

(define (make-vector-equality equal?)
  (make-vectorwise-equality equal? vector-length vector-ref))

(define (make-vector-comparison compare)
  (make-vectorwise-comparison compare vector-length vector-ref))

(define vector-equality
  (make-vector-equality default-equality))

(define vector-comparison
  (make-vector-comparison default-comparison))

(define (make-bytevector-equality equal?)
  (make-vectorwise-equality equal? bytevector-length bytevector-u8-ref))

(define (make-bytevector-comparison compare)
  (make-vectorwise-comparison compare bytevector-length bytevector-u8-ref))

(define bytevector-equality
  (make-bytevector-equality real-number-equality))

(define bytevector-comparison
  (make-bytevector-comparison real-number-comparison))
