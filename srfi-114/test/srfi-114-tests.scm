(import (chibi) (chibi test) (scheme inexact) (srfi 114) (srfi 114 default-comparator))

(test-begin "srfi-114")

(test-group "Predicates"
  (test #t (comparator? boolean-comparator))
  (test #f (comparator? '(1 2 3)))
  (test #f (comparator? equal?))

  (define ff (make-comparator #t #f #f #f))
  (define ft (make-comparator #t #f #f (lambda (x) 42)))
  (define tf (make-comparator #t #f eq? #f))
  (define tt (make-comparator #t #f eq? (lambda (x) 42)))

  (test #t (comparator-comparison-procedure? tf))
  (test #t (comparator-comparison-procedure? tt))
  (test #t (comparator-hash-function? ft))
  (test #t (comparator-hash-function? tt))
  (test #f (comparator-comparison-procedure? ff))
  (test #f (comparator-comparison-procedure? ft))
  (test #f (comparator-hash-function? ff))
  (test #f (comparator-hash-function? tf)))

(test-group "Standard comparators"
  (define-syntax for-each-standard-comparator
    (syntax-rules (list)
      ((_ proc (list comparators ...))
       (for-each proc (list 'comparators ...) (list comparators ...)))))

  (for-each-standard-comparator
    (lambda (name standard-comparator)
      (test name #t (comparator? standard-comparator))
      (test name #t (comparator-hash-function? standard-comparator))
      (test name #t (comparator-comparison-procedure? standard-comparator)))
    (list boolean-comparator char-comparator char-ci-comparator string-comparator
      string-ci-comparator symbol-comparator exact-integer-comparator integer-comparator
      rational-comparator real-comparator complex-comparator number-comparator
      pair-comparator list-comparator vector-comparator bytevector-comparator))

  (test #t (<? boolean-comparator #f #t))
  (test #f (<? boolean-comparator #t #t))
  (test #t (<? char-comparator #\a #\z))
  (test #t (<? char-comparator #\A #\a))
  (test #f (<? char-comparator #\3 #\1))
  (test #t (<? char-ci-comparator #\a #\Z))
  (test #t (<? char-ci-comparator #\A #\z))
  (test #t (=? char-ci-comparator #\a #\A))
  (test #t (<? string-comparator "123" "456"))
  (test #f (=? string-comparator "foo" "bar"))
  (test #t (<? string-ci-comparator "haha" "Haha!"))
  (test #t (=? string-ci-comparator "Foo" "foo"))
  (test #t (<? symbol-comparator '|123| '|456|))
  (test #f (=? symbol-comparator 'foo 'bar))
  (test #t (<? exact-integer-comparator 1 2))
  (test #t (<? integer-comparator 10 20))
  (test #t (<? rational-comparator 1/2 4/5))
  (test #t (<? real-comparator 1.2 3.4))
  (test #t (<? real-comparator 0 +inf.0))
  (test #t (<? complex-comparator -2-3i 1+4i))
  (test #t (<? complex-comparator -i +i))
  (test #t (<? number-comparator -3/4 5.4e+3))
  (test #t (<? pair-comparator '(a . 1) '(a . 2)))
  (test #f (<? pair-comparator '(a . 1) '(a . 1)))
  (test #t (<? pair-comparator '(a . 20) '(b . 3)))
  (test #t (=? list-comparator '() '()))
  (test #t (<? list-comparator '() '(1 2 3)))
  (test #t (<? list-comparator '(1 2 3) '(4 5 6)))
  (test #t (<? list-comparator '(1 2 3) '(1 2 3 4)))
  (test #t (<? list-comparator '(1 2 3) '(1 2 6)))
  (test #t (=? vector-comparator #()      #()))
  (test #t (<? vector-comparator #()      #(1 2 3)))
  (test #t (<? vector-comparator #(6 6)   #(1 1 1)))
  (test #t (=? vector-comparator #(1 2 3) #(1 2 3)))
  (test #t (<? vector-comparator #(1 2 3) #(1 2 4)))
  (test #t (=? bytevector-comparator #u8()      #u8()))
  (test #t (<? bytevector-comparator #u8()      #u8(1 2 3)))
  (test #t (<? bytevector-comparator #u8(6 6)   #u8(1 1 1)))
  (test #t (=? bytevector-comparator #u8(1 2 3) #u8(1 2 3)))
  (test #t (<? bytevector-comparator #u8(1 2 3) #u8(1 2 4))))

(test-group "The default comparator"
  (test #t (<? default-comparator '() '(a . d) '#false #\space "string" 'zorro 42 #(9) #u8() cons))
  (test #t (>? default-comparator cons #u8() #(9) 42 'zorro "string" #\space '#false '(a . d) '()))
  (test #t (<? default-comparator '(a . 1) '(a . 2)))
  (test #f (<? default-comparator '(a . 1) '(a . 1)))
  (test #t (<? default-comparator '(a . 20) '(b . 3)))
  (test #t (<? default-comparator #f #t))
  (test #f (<? default-comparator #t #t))
  (test #t (<? default-comparator #\a #\z))
  (test #t (<? default-comparator #\A #\a))
  (test #f (<? default-comparator #\3 #\1))
  (test #t (<? default-comparator "123" "456"))
  (test #f (=? default-comparator "foo" "bar"))
  (test #t (<? default-comparator '|123| '|456|))
  (test #f (=? default-comparator 'foo 'bar))
  (test #t (<? default-comparator 1 2))
  (test #t (<? default-comparator 10 20))
  (test #t (<? default-comparator 1/2 4/5))
  (test #t (<? default-comparator 1.2 3.4))
  (test #t (<? default-comparator 0 +inf.0))
  (test #t (<? default-comparator -2-3i 1+4i))
  (test #t (<? default-comparator -i +i))
  (test #t (<? default-comparator -3/4 5.4e+3))
  (test #t (=? default-comparator #()      #()))
  (test #t (<? default-comparator #()      #(1 2 3)))
  (test #t (<? default-comparator #(6 6)   #(1 1 1)))
  (test #t (=? default-comparator #(1 2 3) #(1 2 3)))
  (test #t (<? default-comparator #(1 2 3) #(1 2 4)))
  (test #t (=? default-comparator #u8()      #u8()))
  (test #t (<? default-comparator #u8()      #u8(1 2 3)))
  (test #t (<? default-comparator #u8(6 6)   #u8(1 1 1)))
  (test #t (=? default-comparator #u8(1 2 3) #u8(1 2 3)))
  (test #t (<? default-comparator #u8(1 2 3) #u8(1 2 4)))
  (test #t (<? default-comparator 'symbol symbol?))
  (test #t (=? default-comparator symbol? cons +))

  (define Foo (register-simple-type "Foo" #f 0))
  (define make-foo (make-constructor "make-foo" Foo))
  (define foo? (make-type-predicate "foo?" Foo))

  (define Bar (register-simple-type "Bar" #f 0))
  (define make-bar (make-constructor "make-bar" Bar))
  (define bar? (make-type-predicate "bar?" Bar))

  (test #t (<? default-comparator #u8() (make-foo)))
  (test #t (<? default-comparator #u8() (make-bar)))
  (test #t (=? default-comparator (make-foo) (make-foo)))
  (test #t (=? default-comparator (make-foo) (make-bar)))
  (test #t (=? default-comparator (make-bar) (make-bar)))
  (test #t (=? default-comparator (make-foo) cons))
  (test #t (=? default-comparator (make-bar) cons))
  (test #f (<? default-comparator (make-foo) (make-bar)))
  (test #f (<? default-comparator (make-bar) (make-foo)))
  (test #f (<? default-comparator (make-foo) cons))
  (test #f (<? default-comparator (make-bar) cons))

  (define Foo-comparator (make-comparator foo? #t (lambda (a b) 0) #f))
  (define Bar-comparator (make-comparator bar? #t (lambda (a b) 0) #f))

  (register-default-comparator! Foo-comparator)
  (register-default-comparator! Bar-comparator)

  (test #t (<? default-comparator #u8() (make-foo)))
  (test #t (<? default-comparator #u8() (make-bar)))
  (test #t (=? default-comparator (make-foo) (make-foo)))
  (test #f (=? default-comparator (make-foo) (make-bar)))
  (test #t (=? default-comparator (make-bar) (make-bar)))
  (test #f (=? default-comparator (make-foo) cons))
  (test #f (=? default-comparator (make-bar) cons))
  (test #t (<? default-comparator (make-foo) (make-bar))) ; implementation-specific ordering
  (test #f (<? default-comparator (make-bar) (make-foo))) ; between registered types
  (test #t (<? default-comparator (make-foo) cons))
  (test #t (<? default-comparator (make-bar) cons))

  (define procedure-comparator
    (make-comparator procedure?
      (lambda (proc-a proc-b) (eqv? proc-a proc-b))
      #f #f))

  (register-default-comparator! procedure-comparator)

  (test #t (=? default-comparator cons cons))
  (test #f (=? default-comparator car cdr))
  (test #t (=? default-comparator (cons cons cons) (cons cons cons)))
  (test #f (=? default-comparator (cons car cdr) (cons cdr car)))
  (test #t (=? default-comparator (list cons car cdr) (list cons car cdr)))
  (test #f (=? default-comparator (list cons car cdr) (list list + -)))
  (test #t (=? default-comparator (vector cons car cdr) (vector cons car cdr)))
  (test #f (=? default-comparator (vector cons car cdr) (vector list + -))))

(test-group "Comparator constructors"
  (test-group "make-comparator"
    (define (type-test  x)   (number? x))
    (define (equality   a b) (= a b))
    (define (comparison a b) (cond ((< a b) -1) ((= a b) 0) ((> a b) +1)))
    (define (hash       x)   0)

    (define comparator-full       (make-comparator type-test equality comparison hash))
    (define comparator-type-test  (make-comparator #t equality comparison hash))
    (define comparator-equality   (make-comparator type-test #t comparison hash))
    (define comparator-comparison (make-comparator type-test equality #f hash))
    (define comparator-hash       (make-comparator type-test equality comparison #f))

    (test #t (comparator? comparator-full))
    (test #t (comparator? comparator-type-test))
    (test #t (comparator? comparator-equality))
    (test #t (comparator? comparator-comparison))
    (test #t (comparator? comparator-hash))

    (test type-test  (comparator-type-test-procedure  comparator-full))
    (test equality   (comparator-equality-predicate   comparator-full))
    (test comparison (comparator-comparison-procedure comparator-full))
    (test hash       (comparator-hash-function        comparator-full))

    (test #t (comparator-test-type comparator-type-test #f))
    (test #t (comparator-test-type comparator-type-test 42))
    (test #t (comparator-test-type comparator-type-test 'foo))

    (test #t (comparator-equal? comparator-equality 0 0))
    (test #f (comparator-equal? comparator-equality 2 1))
    (test #f (comparator-equal? comparator-equality 1 2))

    (test #f (comparator-comparison-procedure? comparator-comparison))
    (test-error (comparator-compare comparator-comparison 1 2))

    (test #f (comparator-hash-function? comparator-hash))
    (test-error (comparator-hash comparator-comparison 0)))

  (test-group "make-inexact-real-comparator"
    ; Beware of the numerical floating-point magic

    (define compare-0.1   (make-inexact-real-comparator 0.1   'round 'error))
    (define compare-0.01  (make-inexact-real-comparator 0.01  'round 'error))
    (define compare-0.001 (make-inexact-real-comparator 0.001 'round 'error))
    (test #t (<? compare-0.1   0.1111 0.2222))
    (test #t (=? compare-0.1   0.1111 0.1222))
    (test #t (<? compare-0.01  0.1111 0.1222))
    (test #t (=? compare-0.01  0.1111 0.1122))
    (test #t (<? compare-0.001 0.1111 0.1122))
    (test #t (=? compare-0.001 0.1111 0.1112))

    (define compare-round    (make-inexact-real-comparator 0.01 'round 'error))
    (define compare-floor    (make-inexact-real-comparator 0.01 'floor 'error))
    (define compare-ceiling  (make-inexact-real-comparator 0.01 'ceiling 'error))
    (define compare-truncate (make-inexact-real-comparator 0.01 'truncate 'error))
    (test #t (=? compare-round    +0.111 +0.112)) ; +0.11  +0.11
    (test #f (=? compare-round    +0.111 +0.116)) ; +0.11  +0.12
    (test #t (=? compare-round    -0.111 -0.112)) ; -0.11  -0.11
    (test #f (=? compare-round    -0.111 -0.116)) ; -0.11  -0.11
    (test #t (=? compare-floor    +0.111 +0.110)) ; +0.11  +0.11
    (test #t (=? compare-floor    +0.111 +0.119)) ; +0.11  +0.11
    (test #f (=? compare-floor    -0.111 -0.110)) ; -0.12  -0.11
    (test #t (=? compare-floor    -0.111 -0.119)) ; -0.12  -0.12
    (test #f (=? compare-ceiling  +0.111 +0.109)) ; +0.12  +0.11
    (test #t (=? compare-ceiling  +0.111 +0.120)) ; +0.12  +0.12
    (test #t (=? compare-ceiling  -0.111 -0.110)) ; -0.11  -0.11
    (test #f (=? compare-ceiling  -0.111 -0.121)) ; -0.11  -0.12
    (test #t (=? compare-truncate +0.111 +0.112)) ; +0.11  +0.11
    (test #t (=? compare-truncate +0.111 +0.119)) ; +0.11  +0.11
    (test #t (=? compare-truncate -0.111 -0.112)) ; -0.11  -0.11
    (test #t (=? compare-truncate -0.111 -0.119)) ; -0.11  -0.11

    ;; Checking rounding lambda argument semantics
    (define (custom-round number epsilon)
      (test-assert (or (= 1 number) (= 2 number)))
      (test 0.123 epsilon)
      5)
    (define compare-custom-round (make-inexact-real-comparator 0.123 custom-round 'error))
    (test #t (=? compare-custom-round 1 2))

    (define compare-min   (make-inexact-real-comparator 0.01 'round 'min))
    (define compare-max   (make-inexact-real-comparator 0.01 'round 'max))
    (define compare-error (make-inexact-real-comparator 0.01 'round 'error))
    (test #t (<? compare-min +nan.0 -inf.0 0.0 +inf.0))
    (test #t (<? compare-max -inf.0 0.0 +inf.0 +nan.0))
    (test-error (=? compare-error 0.0 +nan.0))
    (test #t (=? compare-min   +nan.0 +nan.0))
    (test #t (=? compare-max   +nan.0 +nan.0))
    (test #t (=? compare-error +nan.0 +nan.0))

    ;; Checking unnanning lambda argument semantics
    (define (custom-unnan nan)
      (test-assert (not (nan? nan)))
      0.0)
    (define compare-custom-unnan (make-inexact-real-comparator 0.01 'round custom-unnan))
    (test #t (=? compare-custom-unnan +nan.0 +nan.0))
    (test #t (=? compare-custom-unnan 0.0 +nan.0))
    (test #t (<=? compare-custom-unnan -inf.0 0.0 +nan.0 +inf.0)))

  (test-group "make-{list{,wise},vector{,wise},bytevector}-comparator"
    (define (inverse-number-comparison a b)
      (cond ((< a b) +1)
            ((= a b)  0)
            ((> a b) -1)))
    (define inverse-number-comparator     (make-comparator number? = inverse-number-comparison #f))
    (define inverse-list-comparator       (make-list-comparator       inverse-number-comparator))
    (define inverse-vector-comparator     (make-vector-comparator     inverse-number-comparator))
    (define inverse-bytevector-comparator (make-bytevector-comparator inverse-number-comparator))
    (test #t (=? inverse-list-comparator '() '()))
    (test #t (<? inverse-list-comparator '() '(1 2 3)))
    (test #f (<? inverse-list-comparator '(1 2 3) '(4 5 6)))
    (test #t (<? inverse-list-comparator '(1 2 3) '(1 2 3 4)))
    (test #f (<? inverse-list-comparator '(1 2 3) '(1 2 6)))
    (test #t (=? inverse-vector-comparator #()      #()))
    (test #t (<? inverse-vector-comparator #()      #(1 2 3)))
    (test #t (<? inverse-vector-comparator #(6 6)   #(1 1 1)))
    (test #t (=? inverse-vector-comparator #(1 2 3) #(1 2 3)))
    (test #f (<? inverse-vector-comparator #(1 2 3) #(1 2 4)))
    (test #t (=? inverse-bytevector-comparator #u8()      #u8()))
    (test #t (<? inverse-bytevector-comparator #u8()      #u8(1 2 3)))
    (test #t (<? inverse-bytevector-comparator #u8(6 6)   #u8(1 1 1)))
    (test #t (=? inverse-bytevector-comparator #u8(1 2 3) #u8(1 2 3)))
    (test #f (<? inverse-bytevector-comparator #u8(1 2 3) #u8(1 2 4)))

    (define equal-only-number-comparator     (make-comparator number? = #f #f))
    (define equal-only-list-comparator       (make-list-comparator       equal-only-number-comparator))
    (define equal-only-vector-comparator     (make-vector-comparator     equal-only-number-comparator))
    (define equal-only-bytevector-comparator (make-bytevector-comparator equal-only-number-comparator))
    (test #t (=? equal-only-list-comparator '() '()))
    (test #f (=? equal-only-list-comparator '() '(1 2 3)))
    (test #f (=? equal-only-list-comparator '(1 2 3) '(4 5 6)))
    (test #t (=? equal-only-list-comparator '(1 2 3) '(1 2 3)))
    (test #f (=? equal-only-list-comparator '(1 2 3) '(1 2 3 4)))
    (test #f (=? equal-only-list-comparator '(1 2 3) '(1 2 6)))
    (test #t (=? equal-only-vector-comparator #()      #()))
    (test #f (=? equal-only-vector-comparator #()      #(1 2 3)))
    (test #f (=? equal-only-vector-comparator #(1 2 3) #(4 5 6)))
    (test #t (=? equal-only-vector-comparator #(1 2 3) #(1 2 3)))
    (test #f (=? equal-only-vector-comparator #(1 2 3) #(1 2 3 4)))
    (test #f (=? equal-only-vector-comparator #(1 2 3) #(1 2 6)))
    (test #t (=? equal-only-bytevector-comparator #u8()      #u8()))
    (test #f (=? equal-only-bytevector-comparator #u8()      #u8(1 2 3)))
    (test #f (=? equal-only-bytevector-comparator #u8(1 2 3) #u8(4 5 6)))
    (test #t (=? equal-only-bytevector-comparator #u8(1 2 3) #u8(1 2 3)))
    (test #f (=? equal-only-bytevector-comparator #u8(1 2 3) #u8(1 2 3 4)))
    (test #f (=? equal-only-bytevector-comparator #u8(1 2 3) #u8(1 2 6)))

    (define element-comparator default-comparator)

    (define (vector-type?  vec) (vector? vec))
    (define (vector-empty? vec) (= 0 (vector-length vec)))
    (define (vector-head   vec) (vector-ref vec 0))
    (define (vector-tail   vec) (vector-copy vec 1))
    (define vector-as-list-comparator (make-listwise-comparator vector-type? element-comparator vector-empty? vector-head vector-tail))
    (test #t (=? vector-as-list-comparator #() #()))
    (test #t (<? vector-as-list-comparator #() #(1 2 3)))
    (test #t (<? vector-as-list-comparator #(1 2 3) #(4 5 6)))
    (test #t (<? vector-as-list-comparator #(1 2 3) #(1 2 3 4)))
    (test #t (<? vector-as-list-comparator #(1 2 3) #(1 2 6)))

    (define (list-type? list)  (list? list))
    (define (list-length list) (length list))
    (define list-as-vector-comparator (make-vectorwise-comparator list-type? element-comparator list-length list-ref))
    (test #t (=? list-as-vector-comparator '()      '()))
    (test #t (<? list-as-vector-comparator '()      '(1 2 3)))
    (test #t (<? list-as-vector-comparator '(6 6)   '(1 1 1)))
    (test #t (=? list-as-vector-comparator '(1 2 3) '(1 2 3)))
    (test #t (<? list-as-vector-comparator '(1 2 3) '(1 2 4)))

    (define equal-only-vector-as-list-comparator (make-listwise-comparator vector-type? equal-only-number-comparator vector-empty? vector-head vector-tail))
    (test #t (=? equal-only-vector-as-list-comparator #() #()))
    (test #f (=? equal-only-vector-as-list-comparator #() #(1 2 3)))
    (test #f (=? equal-only-vector-as-list-comparator #(1 2 3) #(4 5 6)))
    (test #t (=? equal-only-vector-as-list-comparator #(1 2 3) #(1 2 3)))
    (test #f (=? equal-only-vector-as-list-comparator #(1 2 3) #(1 2 3 4)))
    (test #f (=? equal-only-vector-as-list-comparator #(1 2 3) #(1 2 6)))

    (define equal-only-list-as-vector-comparator (make-vectorwise-comparator list-type? equal-only-number-comparator list-length list-ref))
    (test #t (=? equal-only-list-as-vector-comparator '()      '()))
    (test #f (=? equal-only-list-as-vector-comparator '()      '(1 2 3)))
    (test #f (=? equal-only-list-as-vector-comparator '(1 2 3) '(4 5 6)))
    (test #t (=? equal-only-list-as-vector-comparator '(1 2 3) '(1 2 3)))
    (test #f (=? equal-only-list-as-vector-comparator '(1 2 3) '(1 2 3 4)))
    (test #f (=? equal-only-list-as-vector-comparator '(1 2 3) '(1 2 6))))

  (test-group "make-{car,cdr,pair,improper-list}-comparator"
    (define symbol-car-comparator (make-car-comparator symbol-comparator))
    (test #f (<? symbol-car-comparator '(a . 1) '(a . 2)))
    (test #f (<? symbol-car-comparator '(a . 1) '(a . 1)))
    (test #t (<? symbol-car-comparator '(a . 20) '(b . 3)))

    (define number-cdr-comparator (make-cdr-comparator number-comparator))
    (test #t (<? number-cdr-comparator '(x . 1) '(a . 2)))
    (test #f (<? number-cdr-comparator '(a . 1) '(a . 1)))
    (test #f (<? number-cdr-comparator '(a . 20) '(b . 3)))

    (define equal-only-symbol-comparator (make-comparator symbol? eq? #f #f))
    (define equal-only-symbol-car-comparator (make-car-comparator equal-only-symbol-comparator))
    (test #t (=? equal-only-symbol-car-comparator '(a . 1) '(a . 2)))
    (test #t (=? equal-only-symbol-car-comparator '(a . 1) '(a . 1)))
    (test #f (=? equal-only-symbol-car-comparator '(a . 20) '(b . 3)))

    (define equal-only-number-comparator (make-comparator number? = #f #f))
    (define equal-only-number-cdr-comparator (make-cdr-comparator equal-only-number-comparator))
    (test #f (=? equal-only-number-cdr-comparator '(x . 1) '(a . 2)))
    (test #t (=? equal-only-number-cdr-comparator '(a . 1) '(a . 1)))
    (test #t (=? equal-only-number-cdr-comparator '(a . 1) '(b . 1)))
    (test #f (=? equal-only-number-cdr-comparator '(a . 20) '(b . 3)))

    (define (obersymbol-comparator obersymbol)
      (define (total-order s1 s2)
        (cond ((eq? s1 s2) 0)
              ((eq? s1 obersymbol) +1)
              ((eq? s2 obersymbol) -1)
              (else (comparator-compare symbol-comparator s1 s2))))
      (make-comparator symbol? eq? total-order #f) )
    (define special-pair-comparator (make-pair-comparator (obersymbol-comparator 'x) (obersymbol-comparator 'y)))
    (test #t (=? special-pair-comparator '(a . b) '(a . b)))
    (test #t (>? special-pair-comparator '(x . b) '(a . b)))
    (test #t (<? special-pair-comparator '(a . b) '(x . b)))
    (test #t (<? special-pair-comparator '(x . b) '(x . y)))
    (test #t (>? special-pair-comparator '(a . y) '(a . x)))

    (define equal-only-symbol/number-pair-comparator (make-pair-comparator equal-only-symbol-comparator equal-only-number-comparator))
    (test #t (=? equal-only-symbol/number-pair-comparator '(a . 1) '(a . 1)))
    (test #f (=? equal-only-symbol/number-pair-comparator '(a . 1) '(a . 2)))
    (test #f (=? equal-only-symbol/number-pair-comparator '(a . 1) '(b . 2)))
    (test #f (=? equal-only-symbol/number-pair-comparator '(a . 1) '(b . 1)))

    (define improper-comparator (make-improper-list-comparator default-comparator))
    (test #t (<? improper-comparator '() '(a . b) 1 2 3))
    (test #t (<? improper-comparator '(a . ()) '(a . b)))
    (test #t (<? improper-comparator '(a b c) '(a b . c)))
    (test #t (>? improper-comparator 3 2 1 '(a . b) '()))
    (test #t (>? improper-comparator '(a . b) '(a . ())))
    (test #t (>? improper-comparator '(a b . c) '(a b c)))

    ; Make sure that the wrapped comparator can compare both elements and pairs
    (define equal-only-improper-comparator (make-improper-list-comparator equal-comparator))
    (test #t (=? equal-only-improper-comparator '() '()))
    (test #t (=? equal-only-improper-comparator 'foo 'foo))
    (test #t (=? equal-only-improper-comparator '(foo bar) '(foo bar)))
    (test #t (=? equal-only-improper-comparator '(foo . bar) '(foo . bar)))
    (test #f (=? equal-only-improper-comparator 'foo 'bar))
    (test #f (=? equal-only-improper-comparator '(foo bar) '(foo zog)))
    (test #f (=? equal-only-improper-comparator '(oops . bar) '(foo . bar))))

  (test-group "make-{selecting,refining,reverse}-comparator"
    ;; make-selecting-comparator
    (define number-symbol-comparator (make-selecting-comparator number-comparator symbol-comparator))
    (test #t (comparator-test-type number-symbol-comparator 1))
    (test #t (comparator-test-type number-symbol-comparator 'foo))
    (test #f (comparator-test-type number-symbol-comparator null?))
    (test #t (<? number-symbol-comparator 1 2))
    (test #t (<? number-symbol-comparator 'x 'y))
    (test-error (<? number-symbol-comparator 1 'x))
    (test-error (=? number-symbol-comparator null? "bar"))

    ;; make-refining-comparator
    (define (op-type? op) (or (eq? op '+) (eq? op '-)))
    (define (op-comparison a b)
      (cond ((and (eq? a '+) (eq? b '+))  0)
            ((and (eq? a '-) (eq? b '+)) +1)
            ((and (eq? a '+) (eq? b '-)) -1)
            ((and (eq? a '-) (eq? b '-))  0)))
    (define (number-comparison a b)
      (cond ((< a b) -1)
            ((= a b)  0)
            ((> a b) +1)))
    (define (for-list-element n proc)
      (lambda args
        (apply proc (map (lambda (list) (list-ref list n)) args))))
    (define refined-comparator
      (make-refining-comparator
        (make-comparator (for-list-element 0 op-type?) #t (for-list-element 0 op-comparison) #f)
        (make-comparator (for-list-element 1 number?)  #t (for-list-element 1 number-comparison) #f)
        (make-comparator (for-list-element 2 number?)  #t (for-list-element 2 number-comparison) #f)))

    (test #t (comparator-test-type refined-comparator '(+ 1 2)))
    (test #t (comparator-test-type refined-comparator '(- 1/2 +inf.0)))
    (test #f (comparator-test-type refined-comparator '(fo fo fo)))
    (test #t (comparator-test-type refined-comparator '(+ a b)))
    (test #t (<? refined-comparator '(+ 1 2) '(- 1 2)))
    (test #f (<? refined-comparator '(+ 1 2) '(+ 0 1)))
    (test #t (>? refined-comparator '(- 2 1) '(- 2 0)))
    (test #t (=? refined-comparator '(+ 1 2) '(+ 1 2 3)))
    (test-error (<? refined-comparator '(+ 1 2) '(ha ha ha)))

    ;; type test evaluation order
    (define eval-order '())
    (define (check-1 x) (set! eval-order (cons 1 eval-order)) #f)
    (define (check-2 x) (set! eval-order (cons 2 eval-order)) #f)
    (define comparator-1 (make-comparator check-1 #t #f #f))
    (define comparator-2 (make-comparator check-2 #t #f #f))

    (set! eval-order '())
    (define selecting-ordered-comparator (make-selecting-comparator comparator-1 comparator-2))
    (test #f (comparator-test-type selecting-ordered-comparator 'example))
    (test '(2 1) eval-order)

    (set! eval-order '())
    (define refining-ordered-comparator (make-refining-comparator comparator-1 comparator-2))
    (test #f (comparator-test-type refining-ordered-comparator 'example))
    (test '(2 1) eval-order)

    ;; make-reverse-comparator
    (define descending-comparator (make-reverse-comparator number-comparator))
    (test -1 (comparator-compare descending-comparator 2 1))
    (test  0 (comparator-compare descending-comparator 1 1))
    (test +1 (comparator-compare descending-comparator 1 2)))

  (test-group "make-debug-comparator"
    (define-syntax define-debug-comparator
      (syntax-rules ()
        ((_ binding (args ...))
         (define binding (make-debug-comparator (make-comparator args ...))))))

    (define (id x) x)

    (define (number-comparison a b)
      (cond ((< a b) -1)
            ((= a b)  0)
            ((> a b) +1)))

    ;; Strict type checking
    (define debug-exact-integer-comparator
      (make-debug-comparator exact-integer-comparator))
    (test-error (=? debug-exact-integer-comparator 1 1.0))

    ;; Throw in a success test so that they were not all failures
    (test #t (=? debug-exact-integer-comparator (+ 2 2) 4))

    ;; Equality predicate requirements
    (define (not-reflexive-=  a b) #f)
    (define (not-symmetric-=  a b) (not (< a b)))
    (define (not-transitive-= a b) (not (or (and (= a 1) (= b 3))
                                            (and (= a 3) (= b 1)))))

    (define-debug-comparator not-reflexive-eq  (#t not-reflexive-=  number-comparison id))
    (define-debug-comparator not-symmetric-eq  (#t not-symmetric-=  number-comparison id))
    (define-debug-comparator not-transitive-eq (#t not-transitive-= number-comparison id))

    (test-error (comparator-test-type not-reflexive-eq 5))
    (test-error (=? not-reflexive-eq 1 5))
    (test-error (=? not-reflexive-eq 2 2))
    (test-error (<? not-symmetric-eq 1 2))
    (test-error (=? not-symmetric-eq 2 1))
    (test #t    (=? not-transitive-eq 1 2)) ; implementation specifics as noted in the SRFI
    (test-error (<? not-transitive-eq 2 3))
    (test-error (comparator-hash not-reflexive-eq 1))

    ;; Comparison procedure requirements
    (define (not-total-comparison         a b) -1)
    (define (not-antisymmetric-comparison a b) (if (= a b) 0 -1))
    (define (not-transitive-comparison    a b) (if (and (= a 1) (= b 3)) +1 (number-comparison a b)))
    (define (invalid-result-comparison    a b) (* 5 (number-comparison a b)))
    (define (contradictory-to-equality    a b) (if (< a b) -1 +1))

    (define-debug-comparator not-total-cmp         (#t #t not-total-comparison         id))
    (define-debug-comparator not-antisymmetric-cmp (#t #t not-antisymmetric-comparison id))
    (define-debug-comparator not-transitive-cmp    (#t #t not-transitive-comparison    id))
    (define-debug-comparator invalid-result-cmp    (#t #t invalid-result-comparison    id))
    (define-debug-comparator contradictory-cmp     (#t #t contradictory-to-equality    id))

    (test-error (comparator-test-type not-total-cmp 5))
    (test-error (<?  not-total-cmp 1 1))
    (test-error (=?  not-total-cmp 1 2))
    (test-error (<=? not-antisymmetric-cmp 1 2))
    (test-error (=?  not-antisymmetric-cmp 2 1))
    (test #t    (<=? not-transitive-cmp 1 2))  ; implementation specifics as noted in the SRFI
    (test-error (<=? not-transitive-cmp 2 3))
    (test-error (<=? invalid-result-cmp 5 1))
    (test-error (<?  invalid-result-cmp 1 2))
    (test-error (=?  contradictory-cmp  1 1))
    (test-error (<=? contradictory-cmp  5 4))
    (test-error (comparator-hash not-total-cmp 3))

    ;; Hash function requirements
    (define (negative-hash-function      x) -1)
    (define (not-exact-hash-function     x) +inf.0)
    (define (not-numerical-hash-function x) "haha")

    (define-debug-comparator negative-hash      (#t = number-comparison negative-hash-function))
    (define-debug-comparator not-exact-hash     (#t = number-comparison not-exact-hash-function))
    (define-debug-comparator not-numerical-hash (#t = number-comparison not-numerical-hash-function))

    (test-error (comparator-hash negative-hash      5))
    (test-error (comparator-hash not-exact-hash     5))
    (test-error (comparator-hash not-numerical-hash 5))))

(test-group "Wrapped equality predicates"
  (test #t (=? eq-comparator '() '()))
  (test #t (=? eq-comparator 'foo 'foo))
  (test #f (=? eq-comparator 'foo 'bar))

  (test #t (=? eqv-comparator 45 45))
  (test #t (=? eqv-comparator number? number?))
  (test #f (=? eqv-comparator number? string?))

  (test #t (=? equal-comparator '(1 2 3) '(1 2 3)))
  (test #f (=? equal-comparator #(5 6 7) #(5 6 8))))

(test-group "Accessors"
  (define type-test number?)
  (define equality equal?)
  (define comparison <)
  (define hash (lambda (x) 42))

  (define comparator (make-comparator type-test equality comparison hash))
  (test type-test  (comparator-type-test-procedure  comparator))
  (test equality   (comparator-equality-predicate   comparator))
  (test comparison (comparator-comparison-procedure comparator))
  (test hash       (comparator-hash-function        comparator))

  (define empty-comparator (make-comparator #t #t #f #f))
  (test-assert (procedure? (comparator-type-test-procedure  empty-comparator)))
  (test-assert (procedure? (comparator-equality-predicate   empty-comparator)))
  (test-assert (procedure? (comparator-comparison-procedure empty-comparator)))
  (test-assert (procedure? (comparator-hash-function        empty-comparator))))

(test-group "Primitive applicators"
  (test #t (comparator-test-type boolean-comparator #f))
  (test #f (comparator-test-type boolean-comparator 42))

  (test #t    (comparator-check-type boolean-comparator #t))
  (test-error (comparator-check-type boolean-comparator 'foo))

  (test #f (comparator-equal? number-comparator 1 2))
  (test #t (comparator-equal? number-comparator 10 10))

  (test -1 (comparator-compare number-comparator 1 2))
  (test  0 (comparator-compare number-comparator 1 1))
  (test +1 (comparator-compare number-comparator 2 1))

  (define (hash-value? x) (and (exact-integer? x) (<= 0 x)))
  (test #t (hash-value? (comparator-hash symbol-comparator 'foo)))
  (test #t (= (comparator-hash list-comparator (list 1 2 3))
              (comparator-hash list-comparator (list 1 2 3)))))

(test-group "Comparison procedure constructors"
  (define-syntax for-each-comparison-constructor
    (syntax-rules (list)
      ((_ proc (list (constructor predicates ...) ...))
       (for-each proc
         (list 'constructor ...) (list (constructor predicates ...) ...)))))

  (for-each-comparison-constructor
    (lambda (name comparison)
      (test name -1 (comparison 1 2))
      (test name  0 (comparison 1 1))
      (test name +1 (comparison 2 1)))
    (list (make-comparison< <) (make-comparison> >) (make-comparison<= <=)
      (make-comparison>= >=) (make-comparison=/< = <) (make-comparison=/> = >))))

(test-group "Comparison syntax"
  (test 'less    (if3 -1 'less 'equal 'greater))
  (test 'equal   (if3  0 'less 'equal 'greater))
  (test 'greater (if3 +1 'less 'equal 'greater))

  (test 'nope (if=?     -1 'yep 'nope))
  (test 'yep  (if=?      0 'yep 'nope))
  (test 'nope (if=?     +1 'yep 'nope))
  (test 'yep  (if<?     -1 'yep 'nope))
  (test 'nope (if<?      0 'yep 'nope))
  (test 'nope (if<?     +1 'yep 'nope))
  (test 'nope (if>?     -1 'yep 'nope))
  (test 'nope (if>?      0 'yep 'nope))
  (test 'yep  (if>?     +1 'yep 'nope))
  (test 'yep  (if<=?    -1 'yep 'nope))
  (test 'yep  (if<=?     0 'yep 'nope))
  (test 'nope (if<=?    +1 'yep 'nope))
  (test 'nope (if>=?    -1 'yep 'nope))
  (test 'yep  (if>=?     0 'yep 'nope))
  (test 'yep  (if>=?    +1 'yep 'nope))
  (test 'yep  (if-not=? -1 'yep 'nope))
  (test 'nope (if-not=?  0 'yep 'nope))
  (test 'yep  (if-not=? +1 'yep 'nope))

  (test-error (if3 9 'less 'equal 'greater))
  (test-error (if=?     9 'yep 'nope))
  (test-error (if<?     9 'yep 'nope))
  (test-error (if>?     9 'yep 'nope))
  (test-error (if<=?    9 'yep 'nope))
  (test-error (if>=?    9 'yep 'nope))
  (test-error (if-not=? 9 'yep 'nope))

  (test 'da (if=?      0 'da))
  (test 'da (if<?     -1 'da))
  (test 'da (if>?     +1 'da))
  (test 'da (if<=?     0 'da))
  (test 'da (if<=?    -1 'da))
  (test 'da (if>=?     0 'da))
  (test 'da (if>=?    +1 'da))
  (test 'da (if-not=? -1 'da))
  (test 'da (if-not=? +1 'da)))

(test-group "Comparison predicates"
  (test #t (=? string-comparator "fo" "fo" "fo"))
  (test #t (<? number-comparator 1 2 3 4))
  (test #t (>? number-comparator 5 2))
  (test #f (>=? number-comparator 5 3 3 6))
  (test #t (<=? number-comparator 3 3 6)))

(test-group "Comparison predicate constructors"
  (test #t ((make= number-comparator) 1 1))
  (test #t ((make< number-comparator) 1 3))
  (test #t ((make> number-comparator) 9 0))
  (test #t ((make<= symbol-comparator) 'a 'b))
  (test #f ((make>= symbol-comparator) 'x 'y)))

(test-group "Interval comparison predicates"
  (test #f (in-open-interval?        1 1 3))
  (test #t (in-open-interval?        1 2 3))
  (test #f (in-open-interval?        1 3 3))
  (test #f (in-open-interval?        1 4 3))

  (test #f (in-open-closed-interval? 1 1 3))
  (test #t (in-open-closed-interval? 1 2 3))
  (test #t (in-open-closed-interval? 1 3 3))
  (test #f (in-open-closed-interval? 1 4 3))

  (test #t (in-closed-open-interval? 1 1 3))
  (test #t (in-closed-open-interval? 1 2 3))
  (test #f (in-closed-open-interval? 1 3 3))
  (test #f (in-closed-open-interval? 1 4 3))

  (test #t (in-closed-interval?      1 1 3))
  (test #t (in-closed-interval?      1 2 3))
  (test #t (in-closed-interval?      1 3 3))
  (test #f (in-closed-interval?      1 4 3))

  (test #t (in-open-interval? '(1 . x) '(2 . y) '(3 . z)))
  (test #f (in-closed-interval? string-comparator "1" "4" "3")))

(test-group "Min/max comparison predicates"
  (test 'x (comparator-min symbol-comparator 'x 'y 'z))
  (test 'y (comparator-min symbol-comparator 'y))
  (test 'y (comparator-max symbol-comparator 'y))
  (test 'z (comparator-max symbol-comparator 'x 'y 'z)))

(test-end)

(test-exit)
