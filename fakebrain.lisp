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

(defun random-pick (population)
    ;; population should be the WEIGHTED list of elements e (with NO fitness scores attached)
    (nth (random (length population)) population))

(defun one-point (parent-one parent-two)
    ;; one point crossover
    ;; TO DO: make sure crossover point is not first or last element!
    (let ((split-index (random (length parent-one))))
        (append (subseq parent-one 0 split-index) (subseq parent-two split-index (length parent-two)))))

(defun two-point (parent-one parent-two)
    ;; two point crossover
    ;; TO DO: make sure crossover points are not first or last element!
    )

(defun crossover (parent-one parent-two)
    ;; produces one child from parents one and two using either
    ;; one point crossover or two point crossover (0.5 probability for each)
    (if (> 0.5 (random 1.0))
        (one-point parent-one parent-two)
        (two-point parent-one parent-two)))

(defun make-new-generation (old-population colors)
    ;; population should be the WEIGHTED list of elements e (with NO fitness scores attached)
    loop for element in old-population
        ;; randomly choose two parents
        for parent-one = (random-pick old-population)
        for parent-two = (random-pick old-population)
        ;; create child via crossover
        for child = (crossover parent-one parent-two)
        ;; mutate, permutate, etc.
        ;; return new population
        )