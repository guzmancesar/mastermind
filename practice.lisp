(defvar *counter* 0)
(defvar *player-guess* '())

(defun Sequential (board colors SCSA last-response)
    (declare (ignore SCSA last-response))
        (if (= *counter* 100)
            (setf *counter* 1)
            (setf *counter* (1+ counter)))
        (cond 
            ((= counter 1) (setf *player-guess* (make-list board :initial-element 'A))))