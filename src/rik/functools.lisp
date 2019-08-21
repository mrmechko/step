(in-package :FUNC)

(defun curry (func &rest initial-args)
  (lambda (&rest args)
    (apply func (append initial-args args))))
