(defun two-color-alternating-scsa (current)
    ;; this is really dumb (only checks number of colors, not for alternating), so can be updated
    (let ((color-number (length (remove-duplicates current))))
        (if (> color-number 2)
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