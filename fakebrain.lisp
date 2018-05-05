;; Main player using genetic algorithm

(defparameter *population-size* 150)
(defparameter *mutation-rate*  )
(defparameter *permutation-rate*  )
(defparameter *inversion-rate*  )
(defparameter *slick-value*  )
;; likely are more parameters to add

(defvar *guesses* nil)
(defvar *responses* nil)

(defun make-weighted-list (population)
    ;; population should be a list containing tuples (e f), where e is a given element
    ;; and f is its corresponding fitness score
    ;; fitness scores should be standardized (falling the range 0 to 1)
    (loop for element in population
        append (make-list (ceiling (* (second element) *weighting-constant*)) :initial-element (first element))))