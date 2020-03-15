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

(defun in:zeros (&rest dimensions) (make-array dimensions :initial-element 0))
(defun in:ones (&rest dimensions) (make-array dimensions :initial-element 1))

(defmacro define-reduction-function (name reduction-function)
  `(defun ,name (array &key (axes (iota (length (array-dimensions array)))))
     (unless (listp axes) (setq axes (list axes)))
     (reduce (lambda (array axis) (reduce-array ,reduction-function array axis))
             (nreverse axes) :initial-value array)))
(defmacro define-reduction-functions (&body lists)
  `(progn
     ,@(loop for list in lists
          collect `(define-reduction-function ,@list))))

(define-reduction-functions
  (in:amax 'max)
  (in:amin 'min)
  (in:sum '+))

(defun in:arange (&rest args)
  "ARGS can be (START) (START STOP) or (START STOP STEP)."
  (cond ((= (length args) 1) (apply #'vector (iota (first args))))
        ((= (length args) 2)
         (apply #'vector (iota (- (second args) (first args))
                               :start (first args))))
        ((= (length args) 3)
         (apply #'vector (iota (- (second args) (first args))
                               :start (first args)
                               :step (third args))))
        (t (error "Unknown signature"))))

(defun in:aref (array &rest subscripts)
  (apply #'select:select array
         (append subscripts
                 (make-list (- (length (array-dimensions array))
                               (length subscripts))
                            :initial-element t))))

(defun (setf in:aref) (new-value array &rest subscripts)
  (setf (apply #'select:select array
               (append subscripts
                       (make-list (- (length (array-dimensions array))
                                     (length subscripts))
                                  :initial-element t)))
        new-value))

(defun in:concatenate (axis &rest arrays)
  (when arrays
    (if (not (< -1 axis (length (array-dimensions (first arrays)))))
        (error "AXIS can only take a value from 0 to ~D"
               (1- (length (array-dimensions (first arrays))))))
    (flet ((reduced-array-dimensions (array)
             (iter (for dimension in (array-dimensions array))
                   (for i from 0)
                   (when (/= i axis) (collect i)))))
      (let ((reduced-array-dimensions (reduced-array-dimensions (first arrays))))
        (iter (for array in (cdr arrays))
              (if (not (equalp reduced-array-dimensions
                               (reduced-array-dimensions array)))
                  (error "Expected ~D to have reduced size ~D but has ~D"
                         array
                         reduced-array-dimensions
                         (reduced-array-dimensions array))))
        (let* ((result-array-dimensions
                (iter (for dimension in (array-dimensions (first arrays)))
                      (for i from 0)
                      (collect (if (= i axis)
                                   (reduce '+
                                           (mapcar (lambda (array)
                                                     (array-dimension array axis))
                                                   arrays)
                                           :initial-value 0)
                                   dimension))))
               (result-array (make-array result-array-dimensions))
               (idx-array (make-array (length arrays) :initial-element 0))
               (stride-array (map 'vector
                                  (lambda (array)
                                    (array-stride array (1- axis)))
                                  arrays)))
          (iter outer
                (generate result-idx below (array-total-size result-array))
                (iter (for array in arrays)
                      (for i from 0)
                      (iter (for idx
                                 from (aref idx-array i)
                                 below (+ (aref idx-array i)
                                          (aref stride-array i)))
                            (setf (row-major-aref result-array
                                                  (in outer (next result-idx)))
                                  (row-major-aref array idx))
                            (finally (setf (aref idx-array i) idx))))
                (finally (return-from outer result-array))))))))

(defun in:map-outer (function &rest arrays)
  "MAP FUNCTION over the outer dimensions of ARRAYS.
For example, (in:map-outer '+ #(1 2 3) #(4 5 6)) ;=> (1 2 3)."
  (unless (apply #'= (mapcar (lambda (array) (array-dimension array 0))
                             arrays))
    (error "Outer dimensions of arrays must be the same for mapping"))
  (in:asarray (iter (for i below (array-dimension (first arrays) 0))
                    (collect (apply function
                                    (iter (for array in arrays)
                                          (collect (in:aref array i))))))))

(defun in:length (object) (if (arrayp object)
                              (array-dimension object 0)
                              (length object)))

(defun in:dot (array1 array2) (reduce-array '+ (in::* array1 array2) 0))

(defmacro in:assert-size ((&rest sizes) &body body)
  (with-gensyms (result)
    `(let ((,result (progn
                      ,@body)))
       (if (not (arrayp ,result))
           (error "BODY starting with form
  ~D~% expected to return an array, but returned a non-array."
                  ',(first body)))
       (iter (for dimension in (array-dimensions ,result))
             (for expected-dimension in (list ,@sizes))
             (for i from 0)
             (if (/= dimension expected-dimension)
                 (error "Dimension ~D of BODY starting with form 
    ~D~% has size ~D expected to have size ~D"
                        i (quote ,(first body)) dimension expected-dimension)))
       ,result)))

(defun dimensions (array-like)
  (cond ((arrayp array-like) (array-dimensions array-like))
        ((listp array-like)
         (cons (length array-like)
               (dimensions (car array-like))))
        (t ())))

(defun in:asarray (array-like)
  (make-array (dimensions array-like) :initial-contents array-like))
