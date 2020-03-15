(defpackage :inferior-numcl.impl
  (:use :cl :alexandria :iterate)
  (:export
   :binary-broadcast
   :unary-broadcast
   :array-stride
   :reduce-array))

(in-package :inferior-numcl.impl)
;; (+ 2 3) ;=> 5
;; (+ #[2 3] 3) ;=> #[5 6]
;; (+ #[1 2 3
;;      4 5 6]
;;    2)

;; (2 3)

(defun cartesian-product (&rest lists)
  (if lists
      (iter outer
            (for elt in (car lists))
            (iter (for list in (apply #'cartesian-product (cdr lists)))
                  (in outer (collect (cons elt list)))))
      '(())))

(defun cartesian-product-append (&rest lists)
  (if lists
      (let* ((remaining-lists (cdr lists))
             (list (car lists)))
        (iter outer
              (for elt in list)
              (iter (for list in (apply #'cartesian-product-append remaining-lists))
                    (in outer (collect (nconc elt list))))))
      '(())))

(defun generate-broadcast-indices (dimensions)
  (apply #'cartesian-product
         (mapcar #'iota dimensions)))

(defun binary-broadcast (function x y &optional result-array)
  (cond
    ((and (not (arrayp x)) (not (arrayp y)))
     (funcall function x y))
    ((not (arrayp x))
     (unary-broadcast (lambda (elt) (funcall function x elt)) y))
    ((not (arrayp y))
     (unary-broadcast (lambda (elt) (funcall function elt y)) x))
    (t
     (let ((result
            (let* ((x-dim (array-dimensions x))
                   (y-dim (array-dimensions y))
                   (result-array (or result-array (make-array x-dim))))
              (if (/= (length x-dim) (length y-dim))
                  (error (format nil "Incompatible array dimensions: ~D and ~D" x-dim y-dim)))
              #+sbcl
              (sb-kernel:with-array-data ((x x) (s) (e) :check-fill-pointer nil)
                (declare (ignore s e))
                (sb-kernel:with-array-data ((y y) (s) (e) :check-fill-pointer nil)
                  (declare (ignore s e))
                  (sb-kernel:with-array-data ((result-array result-array) (s) (e)
                                              :check-fill-pointer nil)
                    (declare (ignore s e))
                    (iter (declare (declare-variables))
                          (for (the fixnum idx) below (array-total-size result-array))
                          (setf (row-major-aref result-array idx)
                                (funcall function
                                         (row-major-aref x idx)
                                         (row-major-aref y idx)))
                          (finally (return result-array))))))
              #-sbcl
              (iter (declare (declare-variables))
                    (for (the fixnum idx) below (array-total-size x))
                    (setf (row-major-aref result-array idx)
                          (funcall function
                                   (row-major-aref x idx)
                                   (row-major-aref y idx)))
                    (finally (return result-array))))))
       (if (= 1 (array-total-size result))
           (row-major-aref result 0)
           result)))))

(defun unary-broadcast (function x)
  (cond ((not (arrayp x))
         (funcall function x))
        (t
         (let ((result
                (let* ((dimensions (array-dimensions x))
                       (result-array (make-array dimensions)))
                  (iter (for idx below (array-total-size x))
                        (setf (row-major-aref result-array idx)
                              (funcall function (row-major-aref x idx)))
                        (finally (return result-array))))))
           (if (= 1 (array-total-size result))
               (row-major-aref result 0)
               result)))))

(defun array-stride (array axis)
  (reduce '* (iter (for dimension in (array-dimensions array))
                   (for i from 0)
                   (if (> i axis) (collect dimension)))))

(defun reduce-array (function array axis)
  (let* ((axis-size (array-dimension array axis))
         (stride (reduce '* (iter (for dim in (array-dimensions array))
                                  (for i from 0)
                                  (if (> i axis) (collect dim)))))
         (result-array (make-array (iter (for i from 0)
                                         (for dimension in (array-dimensions array))
                                         (if (/= axis i) (collect dimension)))))
         (result-size (array-total-size result-array)))
    (iter (for result-idx below result-size)
          (setf (row-major-aref result-array result-idx)
                (reduce function (iter (for i below axis-size)
                                       (for offset from 0 by stride)
                                       (collect (row-major-aref array
                                                                (+ array-idx offset))))))
          (for array-idx
               initially 0
               then (if (zerop (rem (1+ result-idx) stride))
                        (+ array-idx 1 (* (1- axis-size) stride))
                        (+ array-idx 1)))          
          (finally
           (if (array-dimensions result-array)
               (return result-array)
               (return (row-major-aref result-array 0)))))))

;; (defun broadcast (function x y)
;;   "For binary functions"
;;   (cond
;;     ((and (not (arrayp x)) (not (arrayp y)))
;;      (funcall function x y))
;;     (t
;;      (let* ((x (if (arrayp x) x (vector x)))
;;             (y (if (arrayp y) y (vector y)))
;;             (x-num-dim (length (array-dimensions x)))
;;             (y-num-dim (length (array-dimensions y)))
;;             (x (if (< x-num-dim y-num-dim) y x))
;;             (y (if (< x-num-dim y-num-dim) x y))
;;             (x-dim (array-dimensions x))
;;             (y-dim (array-dimensions y))
;;             ;; Ensured x has higher dimensions than y.
;;             (num-outer-iterations
;;              (iter (for x-d in x-dim)
;;                    (for y-d in y-dim)
;;                    (cond ((= y-d 1) (collect x-d))
;;                          ((and (not (first-iteration-p))
;;                                (= y-d x-d))
;;                           (collect nil))
;;                          (t (error "X and Y have incompatible dimensions: ~D and ~D"
;;                                    x-dim y-dim)))))
;;             (result-array (make-array x-dim)))
;;        ;; (break)
;;        (print num-outer-iterations)
;;        ;; (iter (for (x-idx . y-idx) = )
;;        ;;       (setf (apply #'aref result-array x-idx)
;;        ;;             (funcall function
;;        ;;                      (apply #'aref x x-idx)
;;        ;;                      (apply #'aref y y-idx))))
;;        ))))

;; #[2 3] #[3]
;; (0) (1)
;; (0)

;; #[1 2 3
;;   4 5 6]
;; #[[-1 0 1]]

;; (0 0) (0 0)
;; (0 1) (0 1)
;; (0 2) (0 2)
;; (1 0) (0 0)
;; (1 1) (0 1)
;; (1 2) (0 2)

