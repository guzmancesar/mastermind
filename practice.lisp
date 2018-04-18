(defvar *player-guess* '())

(defun Sequential (board colors SCSA last-response)
    (declare (ignore SCSA last-response))
        (if (equal last-response nil)
            (setf *player-guess* (make-list board :initial-element '0))
            (increment *player-guest* colors)) ;do setf if this doesn't work
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