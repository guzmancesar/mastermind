;; code that should be added to FakeBrain function for ab-color
;; for initial guess
(if (eql SCSA 'ab-color) 
    (progn 
        (setf colors '(A B))
        (setf *player-guess* (loop for c from 1 to board
                                when (evenp c)
                                collect (first colors)
                                when (oddp c)
                                collect (second colors))))
	(setf *player-guess* (first-guess board)))
;;for subsequent guesses
(cond ((eql SCSA 'ab-color) (setf colors '(A B))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun two-color-scsa (current)
    ;; checks if current element has two colors
    (let ((color-number (length (remove-duplicates current))))
        (if (= color-number 2)
            (return-from two-color-alternating-scsa nil)
            (return-from two-color-alternating-scsa T))))

(defun only-once-scsa (current)
    ;; checks if the current element has duplicates or not
    (loop for e in current
        when (member e (rest (member e current)))
        do (return-from only-once-scsa nil)
        finally (return-from only-once-scsa T)))

(defun scsa-match (current SCSA)
    (cond ((eql SCSA 'two-color-alternating) (two-color-alternating-scsa current))
        ((eql SCSA 'only-once) (only-once-scsa current))
        ;; more scsa's to come
        (T T)))