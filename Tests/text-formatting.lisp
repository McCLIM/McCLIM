(cl:in-package #:clim-tests)

(def-suite* :mcclim.text-formatting
  :in :mcclim)

(test normalize-margin-spec
  (flet ((equals (spec-1 spec-2)
           (and (= (length spec-1) (length spec-2))
                (loop for (edge value) on spec-1 by #'cddr
                      always (equal value (getf spec-2 edge))))))
    (is (equals '(:left (:relative 0)
                  :top (:relative 0)
                  :right (:relative 0)
                  :bottom (:relative 0))
                (climi::normalize-margin-spec nil
                                              '(:left (:relative 0)
                                                :top (:relative 0)
                                                :right (:relative 0)
                                                :bottom (:relative 0)))))
    (is (equals '(:left (:relative 42)
                  :top (:relative 42)
                  :right (:absolute 3)
                  :bottom (:relative 0))
                (climi::normalize-margin-spec '(:left 42
                                                :top 42
                                                :right (:absolute 3)
                                                :bottom 0)
                                              '(:left (:relative 1)
                                                :top (:relative 1)
                                                :right (:relative 1)
                                                :bottom (:relative 1)))))
    (is (equals '(:left (:absolute 1)
                  :top (:relative 1)
                  :right (:absolute 1)
                  :bottom (:relative 0))
                (climi::normalize-margin-spec '(:left (:absolute 1)
                                                :top (:relative 1)
                                                :bottom 0)
                                              '(:left (:absolute 1)
                                                :top (:absolute 1)
                                                :right (:absolute 1)
                                                :bottom (:absolute 1)))))))

(defun gen-invalid-margin-spec-element ()
  (let ((anchor (gen-one-element :relative :absolute)))
   (lambda ()
     (case (random 3)
       (0 :foo)
       (1 (list (funcall anchor) :foo))
       (2 (list (funcall anchor) 1 2))))))

(defun gen-invalid-margin-spec ()
  (let ((edge (gen-one-element :left :right :top :bottom))
        (element (gen-invalid-margin-spec-element)))
    (lambda ()
      (list (funcall edge) (funcall element)))))

(test normalize-margin-spec.validation

  (for-all ((spec (gen-invalid-margin-spec)))
    (let ((default '(:left (:relative 0)
                     :top (:relative 0)
                     :right (:relative 0)
                     :bottom (:relative 0))))
      (signals error (climi::normalize-margin-spec spec default)))))
