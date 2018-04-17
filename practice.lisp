(defvar *counter* 0)
(defvar *player-guess* '())

(defun Sequential (board colors SCSA last-response)
    (declare (ignore SCSA last-response))
        (if (= *counter* 100)
            (setf *counter* 1)
            (setf *counter* (1+ counter)))
        (if (= counter 1)
            (setf *player-guess* (make-list board :initial-element '0))
            (setf *player-guess* (increment *player-guest* colors)))
        ;; return list of letters converted from *player-guess*
)

(defun increment (x colors)
    (loop for i from (1- (length x)) downto 0
        if (= (nth i x) (1- (length colors)))
            do (setf (nth i x) 0)
        else 
            do (setf (nth i x) (1+ (nth i x))) 
            and do (return x)
        finally (return x)))