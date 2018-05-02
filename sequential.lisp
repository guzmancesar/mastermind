(defvar *sequential-guess* '())

(defun increment (guess colors)
    (loop for i from (1- (length guess)) downto 0
        if (equal (nth i guess) (first (last colors)))
            do (setf (nth i guess) (nth 0 colors))
        else 
            do (setf (nth i guess) (nth (1+ (position (nth i guess) colors)) colors)) 
            and do (return guess)
        finally (return guess)))

(defun Sequential (board colors SCSA last-response)
    (declare (ignore SCSA))
        (if (equal last-response nil)
            (setf *sequential-guess* (make-list board :initial-element 'A))
            (increment *sequential-guess* colors)))