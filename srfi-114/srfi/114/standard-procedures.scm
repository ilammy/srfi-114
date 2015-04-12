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

(define (null-hash null)
  (srfi-69:hash-by-identity null))

(define (boolean-equality bool1 bool2)
  (eq? bool1 bool2))

(define (boolean-comparison bool1 bool2)
  (if (eq? bool1 bool2) 0
      (if (eq? bool1 #f) -1 +1)))

(define (boolean-hash bool)
  (srfi-69:hash-by-identity bool))

(define (char-equality char1 char2)
  (char=? char1 char2))

(define (char-comparison char1 char2)
  (if (char=? char1 char2) 0
      (if (char<? char1 char2) -1 +1)))

(define (char-hash char)
  (srfi-69:hash char))

(define (char-ci-equality char1 char2)
  (char-ci=? char1 char2))

(define (char-ci-comparison char1 char2)
  (if (char-ci=? char1 char2) 0
      (if (char-ci<? char1 char2) -1 +1)))

(define (char-ci-hash char)
  (srfi-69:hash (char-foldcase char)))

(define (string-equality str1 str2)
  (string=? str1 str2))

(define (string-comparison str1 str2)
  (if (string=? str1 str2) 0
      (if (string<? str1 str2) -1 +1)))

(define (string-hash str)
  (srfi-69:string-hash str))

(define (string-ci-equality str1 str2)
  (string-ci=? str1 str2))

(define (string-ci-comparison str1 str2)
  (if (string-ci=? str1 str2) 0
      (if (string-ci<? str1 str2) -1 +1)))

(define (string-ci-hash str)
  (srfi-69:string-ci-hash str))

(define (symbol-equality sym1 sym2)
  (symbol=? sym1 sym2))

(define (symbol-comparison sym1 sym2)
  (if (symbol=? sym1 sym2) 0
      (if (string<? (symbol->string sym1) (symbol->string sym2)) -1 +1)))

(define (symbol-hash sym)
  (srfi-69:hash-by-identity sym))

(define (real-number-equality num1 num2)
  (= num1 num2))

(define (real-number-comparison num1 num2)
  (if (= num1 num2) 0
      (if (< num1 num2) -1 +1)))

(define (real-number-hash num)
  (srfi-69:hash num))

(define (complex-number-equality num1 num2)
  (= num1 num2))

(define (complex-number-comparison num1 num2)
  (or= (real-number-comparison (real-part num1) (real-part num2))
       (real-number-comparison (imag-part num1) (imag-part num2))))

(define (complex-number-hash num)
  (srfi-69:hash num))


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

(define (make-car-hash hash)
  (lambda (pair)
    (hash (car pair))))

(define (make-cdr-hash hash)
  (lambda (pair)
    (hash (cdr pair))))


;; Pairs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-pair-equality equal-cars? equal-cdrs?)
  (lambda (pair1 pair2)
    (and (equal-cars? (car pair1) (car pair2))
         (equal-cdrs? (cdr pair1) (cdr pair2)))))

(define (make-pair-comparison compare-cars compare-cdrs)
  (lambda (pair1 pair2)
    (or= (compare-cars (car pair1) (car pair2))
         (compare-cdrs (cdr pair1) (cdr pair2)))))

(define (make-pair-hash hash-car hash-cdr)
  (lambda (pair)
    (bitwise-xor (hash-car (car pair)) (hash-cdr (cdr pair)))))

(define pair-equality
  (make-pair-equality default-equality default-equality))

(define pair-comparison
  (make-pair-comparison default-comparison default-comparison))

(define pair-hash
  (make-pair-hash default-hash default-hash))

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

(define (make-improper-list-hash hash)
  (define pair-hash (make-pair-hash hash hash))
  (lambda (obj)
    (cond ((null? obj) (null-hash obj))
          ((pair? obj) (pair-hash obj))
          (else        (hash obj)))))


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

(define (make-listwise-hash hash-head empty? head tail)
  (define max-length 32)
  (define initial-value 42)
  (define (hash-list list)
    (let loop ((list list) (i 0)
               (result initial-value))
      (if (or (empty? list) (= i max-length))
          result
          (loop (tail list) (+ 1 i)
                (bitwise-xor result (hash-head (head list)))))))
  hash-list)

(define (make-list-equality equal-heads?)
  (make-listwise-equality equal-heads? null? car cdr))

(define (make-list-comparison compare-heads)
  (make-listwise-comparison compare-heads null? car cdr))

(define (make-list-hash hash-head)
  (make-listwise-hash hash-head null? car cdr))

(define list-equality
  (make-list-equality default-equality))

(define list-comparison
  (make-list-comparison default-comparison))

(define list-hash
  (make-list-hash default-hash))


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

(define (make-vectorwise-hash hash length ref)
  (define max-length 32)
  (define initial-value 9)
  (lambda (vec)
    (let ((max-length (min (length vec) max-length)))
      (let loop ((i 0)
                 (result (+ initial-value (length vec))))
        (if (= i max-length)
            result
            (loop (+ 1 i) (bitwise-xor result (hash (ref vec i)))))))))

(define (make-vector-equality equal?)
  (make-vectorwise-equality equal? vector-length vector-ref))

(define (make-vector-comparison compare)
  (make-vectorwise-comparison compare vector-length vector-ref))

(define (make-vector-hash hash)
  (make-vectorwise-hash hash vector-length vector-ref))

(define vector-equality
  (make-vector-equality default-equality))

(define vector-comparison
  (make-vector-comparison default-comparison))

(define vector-hash
  (make-vector-hash default-hash))

(define (make-bytevector-equality equal?)
  (make-vectorwise-equality equal? bytevector-length bytevector-u8-ref))

(define (make-bytevector-comparison compare)
  (make-vectorwise-comparison compare bytevector-length bytevector-u8-ref))

(define (make-bytevector-hash hash)
  (make-vectorwise-hash hash bytevector-length bytevector-u8-ref))

(define bytevector-equality
  (make-bytevector-equality real-number-equality))

(define bytevector-comparison
  (make-bytevector-comparison real-number-comparison))

(define bytevector-hash
  (make-bytevector-hash real-number-hash))
