;; code that should be added to FakeBrain function for ab-color
;; for initial guess
(defvar *colors* nil)

;; this is super lazy, should do something better
(defun ab-first-guess (size); first version/ make more robust later
  (case size
    (2 '(A A))
    (3 '(A A B))
    (4 '(A A B B))
    (5 '(A A A B B))
    (6 '(A A A B B B))
    (7 '(A A A A B B B))
    (8 '(A A A A B B B B))
    (9 '(A A A A A B B B B))
    (10 '(A A A A A B B B B B))
    (11 '(A A A A A A B B B B B))
    (12 '(A A A A A A B B B B B B))
    (15 '(A A A A A A A A B B B B B B B))))

(if (eql SCSA 'ab-color)
    (setf *colors* '(A B))
    (setf *colors* colors))
(if (eql SCSA 'ab-color)
    (setf *player-guess* (ab-first-guess board))
    (setf *player-guess* (first-guess board)))    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun two-color-scsa (current)
    ;; checks if current element has two colors
    (let ((color-number (length (remove-duplicates current))))
        (if (= color-number 2)
            (return-from two-color-scsa nil)
            (return-from two-color-scsa T))))

(defun only-once-scsa (current)
    ;; checks if the current element has duplicates or not
    (loop for e in current
        when (member e (rest (member e current)))
        do (return-from only-once-scsa nil)
        finally (return-from only-once-scsa T)))

(defun scsa-match (current SCSA)
    (cond ((eql SCSA 'two-color) (two-color-scsa current))
        ((eql SCSA 'only-once) (only-once-scsa current))
        ;; more scsa's to come
        (T T)))