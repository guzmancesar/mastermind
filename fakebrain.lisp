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
    (let* ((second-split (random (length parent-one)))
            (first-split (random (length (subseq parent-one 0 second-split)))))
            (append (subseq parent-one 0 first-split) 
                (subseq parent-two first-split second-split) 
                (subseq parent-one second-split (length parent-one)))))

(defun crossover (parent-one parent-two)
    ;; produces one child from parents one and two using either
    ;; one point crossover or two point crossover (0.5 probability for each)
    (if (> 0.5 (random 1.0))
        (one-point parent-one parent-two)
        (two-point parent-one parent-two)))

(defun mutation-GA (child colors)
    (let ((m-index (1+ (random (1- (length child))))))
        (append (subseq child 0  m-index) 
            (list (nth (random (length colors)) colors)) 
            (subseq child (1+ m-index) (length child)))))

(defun mutation (child rate colors)
    ;; mutation done with probability of rate
    (if (>= rate (random 1.0))
        (mutation-GA child colors)
        child))

(defun inversion-GA (child-list size)
  ;;given a list it chooses two random random points (indexes)
  ;; it reverse the sublist between the points
  (let ((x (random size)))
    (let ((y (random size)))
      (if (< x y)
	  (append (append (subseq child-list 0 x) (reverse (subseq child-list x y))) (subseq child-list y size))
          (append (append (subseq child-list 0 y) (reverse (subseq child-list y x))) (subseq child-list x size))))))

(defun inversion (child rate)
    ;; inversion done with probability of rate
    (if (> rate (random 1.0))
        (inversion-GA child (length child))
        child))	

(defun make-new-generation (old-population colors)
    ;; population should be the WEIGHTED list of elements e (with NO fitness scores attached)
    (loop for element in old-population
        ;; randomly choose two parents
        for parent-one = (random-pick old-population)
        for parent-two = (random-pick old-population)
        ;; create child via crossover
        for child = (crossover parent-one parent-two)
        ;; mutate
        for child-m = (mutation child *mutation-rate* colors)
        ;; permutate
        for child-mp = 
        ;; inversion
        for child-mpi = (inversion child-mpi *inversion-rate*)
        ;; return new population
        collect child-mpi))  	    

;counts the number of each color in a guess into an array and returns the array
(defun custom-color-counter (guess colors)
  (loop with tally = (make-array (length colors) :initial-element 0)
     for peg in guess
     for index = (spot peg)
     do (incf (aref tally index))
     finally (return tally)))

;scores a guess, returning a two-element list (#exact #other) where other means "right color, wrong location"
(defun custom-process-guess (answer guess colors)
  (loop with answer = answer
     with guess-color-count = (custom-color-counter guess colors)
     with true-color-count = (custom-color-counter answer colors)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     finally (return (list exact-counter (loop for i from 0 to (1- (length colors))
                        for guessed = (aref true-color-count i)
                        for true = (aref guess-color-count i)
                        when (<= true guessed)
                        sum true
                        else sum guessed)))))


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


(defun not-eligible (candidate guesses responses colors)
  ;; Takes an element from new population and checks if it can be added to *eligible-set*
  ;; returns T if candidate is NOT eligible for admission to *eligible-set*
  ;; ELIGIBILITY-> treat all previous guesses as if they were secret codes
  ;; get score for candidate aganist previous guess if the difference between their scores is zero 
  ;; then candidate is eligible
  (let ((score nil))
  (loop for guess in guesses for response in responses
     do (setf score ( custom-process-guess guess candidate colors))
       (print score)
       (print response)
     when (not (and (eq (first score) (first response)) (eq (nth 1 score) (nth 1 response))))
     do(return 'T))))



(defun fitness-score (current guesses responses colors val-a val-b turn-i pos-P)
  (let ((fitnessX 0))
    (let ((fitnessY 0))
      (let ((score 0))
	(loop for guess in guesses for response in responses
	   do (setf score ( custom-process-guess guess current colors))
	     (print score)
	     (setq fitnessX (+ fitnessX (abs (- (first score) (first response)))))
	     (setq fitnessY (+ fitnessY (abs (- (nth 1 score) (nth 1 response)))))
	     (print fitnessX)
	     (print fitnessY))
	(+  (* val-a fitnessX) fitnessY (* val-b pos-P (- turn-i 1))))))) 


  
(defun FakeBrain (board colors SCSA last-response)
  (if (null last-response)
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
      
      
