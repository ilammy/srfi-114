(package
  (name (srfi 114))
  (version "1.0.0")
  (description "SRFI 114 'Comparators'")
  (homepage "https://github.com/ilammy/srfi-114")
  (authors "ilammy")
  (depends
    (scheme base)
    (scheme case-lambda)
    (scheme char)
    (scheme complex)
    (scheme inexact)
    (srfi 60)
    (srfi 69))
  (library
    (name (srfi 114))
    (path "srfi/114.sld")
    (depends
      (srfi 114 exported))
    (test-program "test/srfi-114-tests.scm"))
  (library
    (name (srfi 114 exported))
    (path "srfi/114/exported.sld"))
  (library
    (name (srfi 114 default-comparator))
    (path "srfi/114/default-comparator.sld")
    (depends
      (srfi 114 exported)))
  (program
    (use-for test)
    (path "test/srfi-114-tests.scm")
    (depends
      (chibi)
      (chibi test)
      (scheme inexact)
      (srfi 114)
      (srfi 114 default-comparator))))
