(defun two-color-alternating-scsa (current)
    (let ((color-number (length (remove-duplicates current))))
        (if (> color-number 2)
            ;; returns arbitrary bonus points (20) if current has no more than two colors
            (return-from two-color-alternating-scsa 0)
            (return-from two-color-alternating-scsa 20))))

(defun scsa-match (current SCSA)
    (cond ((eql SCSA 'two-color-alternating) (two-color-alternating-scsa current))
        ;; more scsa's to come
        (T 0)))