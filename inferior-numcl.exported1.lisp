(defpackage :inferior-numcl
  (:nicknames :in)
  (:export :asarray
           :assert-size
           :amax
           :amin
           :arange
           :aref
           :argwhere
           :concatenate
           :dot
           :length
           :map-outer
           :mean
           :ones
           :size
           :sum
           :zeros
           ))

(defpackage :inferior-numcl.exported
  (:use :inferior-numcl.impl :cl :iterate :alexandria)
  (:export :*unary-broadcast-functions*
           :*binary-broadcast-functions*))

(in-package :inferior-numcl.exported)

(defmacro define-unary-broadcast-function (name)
  (let ((lambda-list '(x))
        (docstring (documentation name 'function)))
    (assert (= 1 (length lambda-list)))
    `(defun ,(intern (symbol-name name) :inferior-numcl) ,lambda-list
       ,docstring
       (unary-broadcast ',name ,@lambda-list))))

(defmacro define-unary-broadcast-functions (&rest names)
  `(progn
     (defparameter *unary-broadcast-functions*
       (mapcar (lambda (name) (intern (symbol-name name) :inferior-numcl)) ',names))     
     (export *unary-broadcast-functions* :inferior-numcl)
     ,@(loop for name in names
          collect `(define-unary-broadcast-function ,name))))

(defmacro define-binary-broadcast-function
    (name lambda-list
     &optional (initial-value nil initial-value-supplied-p)
       (body nil body-supplied-p))
  (let* ((docstring (documentation name 'function))
         (nu-symbol (intern (symbol-name name) :inferior-numcl))
         (body (if body-supplied-p
                   body
                   (cond ((and (equalp lambda-list '(arg &rest args))
                               initial-value-supplied-p)
                          `(if args
                               (reduce (lambda (x y) (binary-broadcast ',name x y))
                                       args
                                       :initial-value arg)
                               (binary-broadcast ',name ,initial-value arg)))
                         ((equalp lambda-list '(arg &rest args))
                          `(reduce (lambda (x y) (binary-broadcast ',name x y))
                                   args :initial-value arg))
                         ((and (equalp lambda-list '(&rest args))
                               initial-value-supplied-p)
                          `(reduce (lambda (x y) (binary-broadcast ',name x y))
                                   args :initial-value ,initial-value))
                         ((equalp lambda-list '(number &optional (divisor 1)))
                          `(binary-broadcast ',name number divisor))
                         (t
                          (warn "Using default body for ~D" nu-symbol)
                          `(binary-broadcast ',name ,@lambda-list))))))
    `(progn
       (defvar *binary-broadcast-functions* nil)
       (push ',nu-symbol *binary-broadcast-functions*)
       (export ',nu-symbol :inferior-numcl)
       (defun ,nu-symbol ,lambda-list
         ,docstring
         ,body))))

(defmacro define-binary-broadcast-functions (&body body)
  `(progn
     ,@(loop for list in body
          collect `(define-binary-broadcast-function ,@list))))

(define-unary-broadcast-functions
    minusp plusp zerop sin cos tan asin acos sinh cosh tanh asinh acosh atanh 1+ 1- abs
    evenp oddp exp signum sqrt isqrt numberp cis complexp phase realpart imagpart
    realp numerator denominator rational rationalize rationalp integer-length
    integerp lognot logcount byte-size byte-position float-radix float-digits float-precision
    floatp)

(define-binary-broadcast-functions
  (= (arg &rest args) t)
  (/= (arg &rest args) t)
  (< (arg &rest args) t)
  (> (arg &rest args) t)
  (<= (arg &rest args) t)
  (>= (arg &rest args) t)
  (max (arg &rest args))
  (min (arg &rest args))
  (floor (number &optional (divisor 1)))
  (ffloor (number &optional (divisor 1)))
  (ceiling (number &optional (divisor 1)))
  (fceiling (number &optional (divisor 1)))
  (truncate (number &optional (divisor 1)))
  (ftruncate (number &optional (divisor 1)))
  (round (number &optional (divisor 1)))
  (fround (number &optional (divisor 1)))
  (atan (number &optional (real nil real-supplied-p))
        nil
        (if real-supplied-p
            (binary-broadcast 'atan number real)
            (unary-broadcast 'atan number)))
  (- (arg &rest args) 0)
  (/ (arg &rest args) 1)
  (+ (&rest args) 0)
  (* (&rest args) 1)
  (expt (base power))
  (gcd (&rest args) 0)
  (lcm (&rest args) 1)
  (mod (number &optional (divisor 1)))
  (rem (number &optional (divisor 1)))
  (complex (realpart &optional (imagpart 0))
           nil
           (binary-broadcast 'complex realpart imagpart))
  (ash (integer count))
  (boole (op arg1 arg2)
         nil
         (binary-broadcast (lambda (x y) (boole op x y))
                           arg1
                           arg2))
  (logand (&rest args) -1)
  (logandc1 (arg1 arg2))
  (logandc2 (arg1 arg2))
  (logeqv (&rest args) -1)
  (logior (&rest args) 0)
  (lognand (arg1 arg2))
  (lognor (arg1 arg2))
  (logorc1 (arg1 arg2))
  (logorc2 (arg1 arg2))
  (logxor (&rest args) 0)
  (logbitp (index arg))
  (logtest (arg1 arg2))
  (byte (size position))
  (float (arg &optional (other nil other-supplied-p)) nil
         (if other-supplied-p
             (binary-broadcast 'float arg other)
             (unary-broadcast 'float arg)))
  (float-sign (arg1 &optional (arg2 (float 1 arg1) arg2-supplied-p)) nil
              (if arg2-supplied-p
                  (binary-broadcast 'float-sign arg1 arg2)
                  (unary-broadcast 'float-sign arg1))))

(defparameter *broadcast-blacklisted*
  '(upgraded-complex-part-type parse-integer deposit-field dpb ldb ldb-test mask-field
    decode-float scale-float incf decf integer-decode-float))

