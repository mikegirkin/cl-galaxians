(in-package :galaxians)

(defun all-positions-if (pred seq &key (start 0) (end nil))
  "Return a list of indices (0-based) where (funcall PRED element) is true.
Handles lists and vectors. Optional :start and :end limit the scan."
  (let ((end (or end (length seq))))
    (cond
      ((vectorp seq)
       (loop for i from start below end
             for x = (aref seq i)
             when (funcall pred x) collect i))
      ((listp seq)
       (loop for x in seq
             for i from 0
             when (and (>= i start) (< i end) (funcall pred x))
               collect i))
      (t
       (loop for i from start below end
             for x = (elt seq i)
             when (funcall pred x) collect i)))))
