(defun two-color-alternating-scsa (current)
    ;; this is really dumb (only checks number of colors, not for alternating), so can be updated
    (let ((color-number (length (remove-duplicates current))))
        (if (> color-number 2)
            ;; returns arbitrary bonus points (20) if current has no more than two colors
            (return-from two-color-alternating-scsa 0)
            (return-from two-color-alternating-scsa 20))))

(defun only-once-scsa (current)
    ;; checks if the current element has duplicates or not
    (loop for e in current
        when (member e (rest (member e current)))
        ;; returns arbitrary bonus points (20) if no duplicates
        do (return-from only-once-scsa 0)
        finally (return-from only-once-scsa 20)))

(defun scsa-match (current SCSA)
    (cond ((eql SCSA 'two-color-alternating) (two-color-alternating-scsa current))
        ((eql SCSA 'only-once) (only-once-scsa current))
        ;; more scsa's to come
        (T 0)))