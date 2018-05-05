;; Main player using genetic algorithm

(defparameter *population-size* 150)
(defparameter *mutation-rate* 0.03 )
(defparameter *permutation-rate* 0.03)
(defparameter *inversion-rate* 0.02 )
(defparameter *slick-value* 4 ); what should be the value?
(defparameter *maxgen* 100)
(defparameter *maxsize* 150)
;; likely are more parameters to add

(defvar *guesses* nil)
(defvar *responses* nil)
(defvar *player-guess* nil)
(defvar *population* nil)
(defvar *eligible-set* nil)

(defun make-weighted-list (population)
    ;; population should be a list containing tuples (e f), where e is a given element
    ;; and f is its corresponding fitness score
    ;; fitness scores should be standardized (falling the range 0 to 1)
    ;; reutrns eighted list where elements e have NO attached fitness scores
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
(defun inversion-GA (child-list size)
  ;;given a list it chooses two random random points (indexes)
  ;; it reverse the sublist between the points
  (let ((x (random size)))
    (let ((y (random size)))
      (if (< x y)
	  (append (append (subseq child-list 0 x) (reverse (subseq child-list x y))) (subseq child-list y size))
          (append (append (subseq child-list 0 y) (reverse (subseq child-list y x))) (subseq child-list x size))))))	  	    

(defun first-guess (size); first version/ make more robust later
  (case size
    (2 '(A A))
    (3 '(A A B))
    (4 '(A A B C))
    (5 '(A A B C D))
    (6 '(A A B B C D))
    (7 '(A A B B C C D))
    (8 '(A A B B C C D E))
    (9 '(A A B B C C D D E))
    (10 '(A A B B C C D D E F))
    (11 '(A A B B C C D D E E F))
    (12 '(A A B B C C D D E E F G))))

(defun initialize-population (size board colors)
  (loop for i from 1 to size
       collect (list (insert-colors board colors) (/ 1 size))))
  
(defun FakeBrain (board colors SCSA last-response)
  (if (equal last-response NIL)
      (progn; First round set up
	(setf *population* (initialize-population *population-size* board colors))
	(setf *player-guess* (first-guess board)))
      (progn; Other rounds
	;keep track of all previous guesses and responses
	(setf *guesses* (append *guesses* (list *player-guess*)))
	(setf *responses* (append *responses* (list last-response)))
	;;WHILE LOOP STARTS unitl *maxgen* or *maxsize*
	;;create new population-> calls crossover, mutation, inversion and permutation
	;;calculate fitness of all elements of the new population
	;;add eligible combinations to *eligible-set*
	;;END WHILE
	;; choose guess from *eligible-set*
	(setf *player-guess* (insert-colors board colors)))))
      
      
