(defvar *counter* 0)
(defvar *player-guess* '())

(defun Sequential (board colors SCSA last-response)
    (declare (ignore SCSA last-response))
        (if (= *counter* 100)
            (setf *counter* 1)
            (setf *counter* (1+ counter)))
        (cond 
            ((= counter 1) (setf *player-guess* (make-list board :initial-element '0))))

)

(defun list-to-int (x)
    (loop for i from (1- (length x)) downto 0
        collect (* (expt 10 (- (length x) (1+ i))) (nth i x)) into nums
        finally (return (apply '+ nums))))

(defun int-to-list (x)
    )