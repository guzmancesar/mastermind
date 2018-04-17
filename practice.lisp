(defvar *counter* 0)
(defvar *player-guess* '())

(defun Sequential (board colors SCSA last-response)
    (declare (ignore SCSA last-response))
        ; need to see last-response to see if its a win/
        (if (= *counter* 100)
            ; need to reset counter if its a win
            (setf *counter* 1)
            (setf *counter* (1+ *counter*)))
        (if (= *counter* 1)
            (setf *player-guess* (make-list board :initial-element '0))
            (setf *player-guess* (increment *player-guest* colors)))
        ; return list of letters converted from *player-guess*
)

(defun increment (x colors)
    (loop for i from (1- (length x)) downto 0
        if (= (nth i x) (first (last colors)))
            do (setf (nth i x) (nth 0 colors))
        else 
            do (setf (nth i x) (nth (1+ (position (nth i x) colors)) colors)) 
            and do (return x)
        finally (return x)))