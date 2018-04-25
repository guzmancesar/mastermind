(defvar *sequential-guess* '())

(defun Sequential (board colors SCSA last-response)
    (declare (ignore SCSA))
        (if (equal last-response nil)
            (setf *sequential-guess* (make-list board :initial-element 'A))
            (increment *sequential-guess* colors)))

(defun increment (x colors)
    (loop for i from (1- (length x)) downto 0
        if (equal (nth i x) (first (last colors)))
            do (setf (nth i x) (nth 0 colors))
        else 
            do (setf (nth i x) (nth (1+ (position (nth i x) colors)) colors)) 
            and do (return x)
        finally (return x)))