(in-package :inferior-numcl.exported)

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

(defun in:mean (array &key (axes (iota (length (array-dimensions array)))))
  (unless (listp axes) (setq axes (list axes)))
  (let ((reduction-factor (apply #'*
                                 (mapcar (lambda (axis) (array-dimension array axis))
                                         axes))))
    (in:/ (in:sum array :axes axes) reduction-factor)))

