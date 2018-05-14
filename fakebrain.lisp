;; Main player using genetic algorithm (with min remaining eligible choice policy)

;; PARAMETER DEFINTIONS
(defparameter *population-size* 150) ; size of each generation
(defparameter *mutation-rate* 0.05) ; probability of mutation
(defparameter *permutation-rate* 0.05) ; probability of permutation
(defparameter *inversion-rate* 0.05) ; probability of inversion
(defparameter *slick-value* 0.5)
(defparameter *maxgen* 100) ; maximum number of generations to fill eligible-set
(defparameter *maxsize* 50) ; maximum size of eligible-set
(defparameter *a-weight* 1) ; weight for component of fitness function
(defparameter *b-weight* 2) ; weight for component of fitness function
(defparameter *weighting-constant* 150) ; weight for making the weighted population list
(defparameter *selection-size* 25) ; size of the random selection subset from eligible-set

;; GAME-SPECIFIC VARIABLE DEFINITIONS
(defvar *legal-colors* nil) ; list of colors possible in code
(defvar *guesses* nil) ; previous guesses
(defvar *responses* nil) ; previous responses
(defvar *player-guess* nil) ; guess to be made
(defvar *population* nil) ; population from current generation
(defvar *eligible-set* nil) ; guesses eligible from various generations
(defvar *2-choices* nil) ; list of known colors if only two distinct colors present in code
(defvar *3-choices* nil) ; list of known colors if only three distinct colors present in code

; MAKE-WEIGHTED-LIST DEFINTION
; takes population list of tuples as input
; tuple format (e f): e -> element, f -> corresponding fitness score
; PRECONDITION: fitness scores should be standardized (falling the range 0 to 1)
; returns weighted list where elements have no attached fitness scores
(defun make-weighted-list (population)
    (loop for element in population
        ;; if fitness is negative, set it to zero
        do (when (> 0 (second element))
                (setf (second element) 0))
        append (make-list (ceiling (* (second element) *weighting-constant*)) :initial-element (first element))))

; RANDOM-PICK helper function
; population should be the WEIGHTED list of elements e (with NO fitness scores attached)
(defun random-pick (population)
    (nth (random (length population)) population))

; ONE-POINT helper function
; returns new guess created from two given parents with one crossover point
(defun one-point (parent-one parent-two)
    ;; crossover made at split-index
    (let ((split-index (random (length parent-one))))
        (append (subseq parent-one 0 split-index) (subseq parent-two split-index (length parent-two)))))

; TWO-POINT helper function
; returns new guess created from two given parents with two crossover points
(defun two-point (parent-one parent-two)
    ;; crossover made at randomly chosen first-split and second-split indices
    (let* ((first-split (random (length parent-one)))
            (second-split (random (length parent-one))))
        (if (< first-split second-split)
            (append (subseq parent-one 0 first-split) ; if second split index is after first
                (subseq parent-two first-split second-split)
                (subseq parent-one second-split (length parent-one)))
            (append (subseq parent-one 0 second-split) ; if first split index is after second
                (subseq parent-two second-split first-split)
                (subseq parent-one first-split (length parent-one))))))

; CROSSOVER helper function
; 50/50 chance of using one-point or two-point crossover functions
; returns new guess produced from two parents by chosen crossover function
(defun crossover (parent-one parent-two)
    ; 50/50 chance of either crossover
    (if (>= 0.5 (random 1.0))
        (one-point parent-one parent-two)
        (two-point parent-one parent-two)))

; MUTATION-GA helper function
; replaces the color of one randomly chosen position by a random other color
; returns mutated child
(defun mutation-GA (child colors)
    (let ((m-index (1+ (random (1- (length child)))))) ; m-index is random index of mutation
        (append (subseq child 0  m-index)
            (list (nth (random (length colors)) colors))
            (subseq child (1+ m-index) (length child)))))

; MUTATION helper function
; mutates child with probability of RATE
; returns child
(defun mutation (child rate colors)
    (if (>= rate (random 1.0))
        (mutation-GA child colors)
        child))

; INVERSION-GA helper function
; two positions are randomly picked, and the sequence of colors between these positions is inverted
; returns child-list
(defun inversion-GA (child-list size)
  (let ((x (random size)))
    (let ((y (random size)))
      (if (< x y) ;; reverse the sublist between the points
	  (append (append (subseq child-list 0 x) (reverse (subseq child-list x y))) (subseq child-list y size))
          (append (append (subseq child-list 0 y) (reverse (subseq child-list y x))) (subseq child-list x size))))))

; INVERSION helper function
; inverts child with probability of RATE
; returns child
(defun inversion (child rate)
    (if (>= rate (random 1.0))
        (inversion-GA child (length child))
        child))

; PERMUTATE-GA helper function
; switches colors of two random positions in child
; returns child
(defun permutate-GA (child)
    (progn (rotatef (nth (random (length child)) child) (nth (random (length child)) child))
            child))

; PERMUTATION helper function
; permutes child with probability of RATE
; returns child
(defun permutation (child rate)
    (if (>= rate (random 1.0))
        (permutate-GA child)
        child))

; MAKE-NEW-GENERATION DEFINITION
; takes previous population and list of colors as input
; old-population should be WEIGHTED list of elements e (with NO fitness scores attached)
; returns new generation
(defun make-new-generation (old-population colors)
    (loop for element upto (1- *population-size*)
        ;; randomly choose two parents
        for parent-one = (random-pick old-population)
        for parent-two = (random-pick old-population)
        ;; create child via crossover
        for child = (crossover parent-one parent-two)
        ;; mutate child with probability of *mutation-rate*
        for child-m = (mutation child *mutation-rate* colors)
        ;; permute child with probability of *permutation-rate*
        for child-mp = (permutation child-m *permutation-rate*)
        ;; invert child with probability of *inversion-rate*
        for child-mpi = (inversion child-mp *inversion-rate*)
        ;; put child into new population
        collect child-mpi))

; CUSTOM-COLOR-COUNTER helper function
; counts the number of each color in a guess into an array and returns the array
(defun custom-color-counter (guess colors)
  (loop with tally = (make-array (length colors) :initial-element 0)
     for peg in guess
     for index = (spot peg)
     do (incf (aref tally index))
     finally (return tally)))

; CUSTOM-PROCESS-GUESS DEFINITION
; scores guess against answer as secret code
; returns a two-element list (#exact #other): other -> "right color, wrong location"
(defun custom-process-guess (answer guess colors)
  (loop with answer = answer
     with guess-color-count = (custom-color-counter guess colors)
     with true-color-count = (custom-color-counter answer colors)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact
     do (incf exact-counter) ; compute "exact"
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     ; compute "other"
     finally (return (list exact-counter (loop for i from 0 to (1- (length colors))
                        for guessed = (aref true-color-count i)
                        for true = (aref guess-color-count i)
                        when (<= true guessed)
                        sum true
                        else sum guessed)))))

; FIRST-GUESS helper function
; takes input size as number of pegs and colors as legal colors for game
; returns the first guess made by the algorithm for p=size, n=(length colors) game
(defun first-guess (size colors)
    (loop for i from 0 to (1- size)
        collect (nth (mod (floor i 2) (length colors)) colors)))

; FIRST-AND-LAST SCSA GUESS ELIGIBILITY
; takes candidate as potential guess
; returns T if guess fits given first-and-last SCSA (first and last pegs are of the same color)
; returns nil otherwise
(defun first-and-last-scsa (candidate)
	  (if (equal (nth 0 candidate)
	      (nth (- (length candidate) 1) candidate))
	       (return-from first-and-last-scsa t)
	       (return-from first-and-last-scsa nil)))

; ONLY-ONCE-SCSA GUESS ELIGIBILITY
; takes current as potential guess
; returns T if the guess has only unique elements
; returns nil otherwise
(defun only-once-scsa (current)
    ;; checks if each element has duplicates in current
    (loop for e in current
        when (member e (rest (member e current)))
        do (return-from only-once-scsa nil)
        finally (return-from only-once-scsa T)))

; SCSA-MATCH GUESS ELIGIBILITY
; delegates current to corresponding SCSA eligibility function
; IMPORTANT: only use this for first-and-last and only-once scsa's
; returns result
(defun scsa-match (current SCSA)
    (cond ((eql SCSA 'first-and-last) (first-and-last-scsa current))
        ((eql SCSA 'only-once) (only-once-scsa current))
        (T T)))

; NOT-ELIGIBLE DEFINITION
; takes candidate as an element from new population
; checks if it can be added to *eligible-set*
; ELIGIBILITY is determined by:
;   1. treating all previous guesses as if they were secret codes
;   2. get score for candidate aganist previous guess
;   3. if the difference between their scores is zero, then candidate is eligible
; returns T if candidate is NOT ELIGIBLE for admission to *eligible-set*
(defun not-eligible (candidate guesses responses colors SCSA)
  ;; Takes an element from new population and checks if it can be added to *eligible-set*
  ;; returns T if candidate is NOT eligible for admission to *eligible-set*
  ;; ELIGIBILITY-> treat all previous guesses as if they were secret codes
  ;; get score for candidate aganist previous guess if the difference between their scores is zero
  ;; then candidate is eligible
  (let ((score nil))
  (loop for guess in guesses for response in responses
     do (setf score ( custom-process-guess guess candidate colors))
       ;;(print score)
       ;;(print response)
     when (not (and (eq (first score) (first response)) (eq (nth 1 score) (nth 1 response))))
     do(return 'T)
     finally (not (scsa-match candidate SCSA)))))

; FITNESS-SCORE DEFINITION
; implements fitness function modified from K.U.Leuven paper for better results
; assuming response is in format (#exact #other)
; subtracts weighted difference of EXACT between current and previous guesses,
; and unweighted difference of OTHER between current and previous guesses,
; from the score weighted for this turn
; returns fitness score for current guess
(defun fitness-score (current guesses responses colors val-a val-b turn-i pos-P)
  (let ((fitnessX 0)
        (fitnessY 0)
        (score 0))
	(loop for guess in guesses
        for response in responses
	    do (setf score ( custom-process-guess guess current colors))
	     ;;(print score)
	     (setq fitnessX (+ fitnessX (abs (- (first score) (first response)))))
	     (setq fitnessY (+ fitnessY (abs (- (nth 1 score) (nth 1 response))))))
	     ;;(print fitnessX)
	     ;;(print fitnessY))
    ;; two versions of fitness function! first is from paper, second is our reordered, modified function
	;; (+  (* val-a fitnessX) fitnessY (* val-b pos-P (- turn-i 1)))))
    (-  (* val-b pos-P (- turn-i 1)) (* val-a fitnessX) fitnessY)))

; STANDARDIZE-FTINESS-SCORES helper function
; takes population as input
; population has elements (e f): e -> candidate, f -> candidate's fitness score
; standardizes the fitness scores (0 to 1)
; returns augmented population array
(defun standardize-fitness-scores (population)
    (let ((total (loop for element in population
                    sum (second element))))
        (loop for element in population
            collect (list (first element) (/ (second element) total)))))

; MYSTERY-1 SCSA GUESS GENERATION
; generates initial population for the mystery-1 SCSA using 3 colors given in colors list
; returns the population
(defun three-color-alternating (board colors)
  (let ((first-color (first colors)))
  (let ((second-color (second colors)))
  (let ((third-color (third colors)))
    (loop for i from 0 to (- board 1)
       collect (cond ((eq (mod i 3) 0) first-color)
	             ((eq (mod i 3) 1) second-color)
	             ((eq (mod i 3) 2) third-color)))))))

; TWO-COLOR-ALTERNATING SCSA GUESS GENERATION
; generates a population for the two-color-alternating using the 2 colors given in colors list
; returns the guess
(defun two-alternating-colors (board colors)
  (let ((first-color  (first colors)))
    (let ((second-color (second colors)))
      (loop for i from 0 to (- board 1)
	 when (oddp i) collect first-color
	   else collect second-color))))

; MYSTERY-2 SCSA GUESS GENERATION
; generates a population for the mystery-2 SCSA using colors given in colors list
; returns the population
(defun mystery-2-scsa (population-size colors len)
	   (let ((x ()) (y 0) (z 0)(k 0)(w ()) (f1 0) (f2 0) (f3 0)(f4 0))
	   (loop for q from 0 to (/ population-size 3)
	       collect (make-list len :initial-element (nth (mod z (length colors)) colors)) into newlist
		   do (incf z)
		   finally (setq x newlist)
		)
	   (if (= (mod len 2) 1)(setq f1 (+ (/ len 2) 1)) (setq f1(- len (/ len 2))))
	   (if (= (mod len 2) 1)(setq f4 (ceiling (/ len 3))) (setq f4 (ceiling (/ len 3))))
	   (if (= (mod len 2) 1)(setq f2 (+ (floor (/ len 3)) 1)) (setq f2 (ceiling (/ len 3))))
	   (if (= (mod len 2) 1)(setq f3 (- len (+ f2 f4))) (setq f3 (- LEN (+ F2 F4))))
	   (if (= len 4) (progn (setq f2 1) (setq f4 2) (setq f3 1)))

	   (loop for q4 from 0 to (/ population-size 3)
		do (setq y (random-chooser colors))
		do (setq z (random-chooser (remove y colors)))
		  do (loop for q1 from 1 to (/ len 2)
				collect y into nulist
				do (setq w nulist)
				finally (loop for q2 from 1 to f1
							collect z into nulist2
							finally (progn (setq w (append nulist nulist2))
							(setq x (append (list w) x))))))
		(loop for q5 from 0 to (/ population-size 3)
			do (setq y (random-chooser colors))
			do (setq z (random-chooser (remove y colors)))
			do (setq k (random-chooser (remove z (remove y colors))))
			do (loop for q1 from 1 to f4
				collect y into nulist
				do (if (= (length x) population-size)(return-from mystery-2-scsa x))
				do (setq w nulist)
				finally (loop for q2 from 1 to f2
							collect z into nulist2
							do (setq w (append nulist nulist2))
							finally (loop for q8 from 0 to (- f3 1)
									 collect k into nulist3
									 finally (progn
									 (setq w (append w nulist3))
									 (setq x (append (list w) x)))))))
		(return-from mystery-2-scsa x)))

; MYSTERY-5 SCSA GUESS GENERATION
; generates a population for the mystery-5 SCSA using colors given in colors list
; creates a list of lists where every sublist has a probability of:
;   1. containing three(or more) colors in a row,
;   2. two colors in a row,
;   3. or a random assortment of colors
; returns the population
(defun mystery-5-scsa (population-size colors len)
	(let ((x ())  (next 0) (randv 0) (choice ()))
			 (loop for q from 1 to population-size
				do (setq choice ())
				do (loop for q1 from 0 to len
						 do (setq next (random-chooser colors))
						; do (print "step: ") do (print q1)
						 do (setq randv (random 1.0)) ;choose a random float from 0 - 1
						; do (print randv)
						 do (progn
							(if (and (< randv .15) (< q1 (- len 3))) ;if less than .45 triple it
								 (progn (setq choice (append (list next) choice))(incf q1))) ;if .15 > randv .45 double it
							(if (and (< randv .45) (< q1 (- len 2)))
								 (progn (setq choice (append (list next) choice))
								 (setq choice (cons next choice)) (incf q1)  (incf q1)))
							(if (< q1 len) (setq choice (append (list (random-chooser colors)) choice))))
					;	do (print choice)
					finally (setq x (cons choice x)))
				;do (print x)
				finally (return-from mystery-5-scsa x))))

; INITIALIZE POPULATION DEFINITION
; PROCEDURE:
;   Elements of initial population are given an equal fitness score
;   This is so that they will have equal probability to be parents of next generation
;   SCSA input is used to call corresponding generation function
; returns smart population
(defun initialize-population (size board colors SCSA)
  (case SCSA
    (two-color
      (progn
	(loop for i from 1 to size
	   do(setf *2-choices* (loop for i from 1 to 2 for chosen = (random-chooser colors) collect chosen))
	   collect (list (insert-colors board *2-choices*) (/ 1 size)))))
      (usually-fewer
       (progn
	(loop for i from 1 to size
	   do(setf *3-choices* (loop for i from 1 to 3 for chosen = (random-chooser colors) collect chosen))
	   collect (list (insert-colors board *3-choices*) (/ 1 size)))))
      (mystery-1
       (progn (loop for i from 1 to size
		 do(setf *3-choices* (loop for i from 1 to 3 for chosen = (random-chooser colors) collect chosen))
		 collect(list (three-color-alternating board *3-choices*) (/ 1 size)))))
      (mystery-2 (mapcar (lambda (element) (list element (/ 1 size))) (mystery-2-scsa size colors board)))
      (mystery-3
       (progn (loop for i from 1 to size
		 do(setf *3-choices* (loop for i from 1 to 3 for chosen = (random-chooser colors) collect chosen))
		 collect(list (insert-colors board *3-choices*) (/ 1 size)))))
      (two-color-alternating
        (progn (loop for i from 1 to size
		  collect(list (two-color-alternating board colors) (/ 1 size)))))
      (mystery-4
        (progn (loop for i from 1 to size
		 do(setf *2-choices* (loop for i from 1 to 3 for chosen = (random-chooser colors) collect chosen))
		  collect(list (two-alternating-colors board *2-choices*) (/ 1 size)))))
      (mystery-5 (mapcar (lambda (element) (list element (/ 1 size))) (mystery-5-scsa size colors board)))
      (prefer-fewer
       (loop for i from 1 to size
	  collect (list (prefer-fewer board colors) (/ 1 size))))
      (T
       (loop for i from 1 to size
       collect (list (insert-colors board colors) (/ 1 size))))))

(defun most-similar (eligible-set colors)
    ;; returns the candidate in the set of eligible guesses that is the most
    ;; similar to the others, the logic being that playing this guess will shrink
    ;; the size of the eligible set the most upon the next guess
    (let ((best (list (first eligible-set) 0)))
        (loop for i in eligible-set
		    for l = (loop for j in eligible-set
			        sum (apply '+ (custom-process-guess i j colors)))
		    do (when (> l (second best))
                    (setf best (list i l))))
        (first best)))

;; CHOOSE-N-RANDOM-SELECTION (selection helper function)
;; returns random selection of size n, or length of list if n > length
(defun choose-n-random-selection (n list)
  (if (> n (length list))
        (loop for i from 1 to (length list)
            for choices = (copy-list list) then (set-difference choices (list chosen))
            for chosen = (random-chooser choices)
            collect chosen)
        (loop for i from 1 to n
            for choices = (copy-list list) then (set-difference choices (list chosen))
            for chosen = (random-chooser choices)
            collect chosen)))

;; GUESS-STILL-ELIGIBLE (selection helper function)
;; sees if givenguess is still eligble after candidate is the guess and code is the code
;; ELIGIBILITY is determinded by pretending that candidate was the last guess, and code is the answer
; uses not-eligible function with parameters (candidate guesses responses colors)
; returns T if still eligible, NIL otherwise
(defun guess-still-eligible (givenguess candidate code colors guesses responses SCSA)
    (let ((response (custom-process-guess code candidate colors))) ; response to c with code c*
    (let ((guessescopy (append guesses (list candidate)))) ; pretend that we made guess c
    (let ((responsescopy (append responses (list response)))) ; pretend that guess c got response
    (return-from guess-still-eligible (not (not-eligible givenguess guessescopy responsescopy colors SCSA)) )))))

;; SELECTION FUNCTION DEFINITION
;; chooses the most appropriate guess from the eligible guesses
;; PROCEDURE
; 1. take a random selection set S ⊆ Eligible
;    utilizes choose-n-random (n list)
; 2. for each guess c ⊆ S and each c* ⊆ S/{c} functioning as a secret code,
;    check how many codes in S/{c,c*} remain eligible
; 3. pick the guess with the minimum average number of remaining eligible codes
(defun select-guess-from-eligible (eligible colors guesses responses SCSA)
    (let ((random-selection (choose-n-random-selection *selection-size* eligible))) ; random selection
        (let ((minindex -1)) ; index of guess with current minimum
            (let ((minimum 150)) ; current minimum tracker for average remaining eligble codes
    (loop for candidate in random-selection for i from 0 ; pick every possible c
        do (let ((eligibleremainingsum 0)) ; track how many eligible guesses remain on average
            (let ((eligibleremainingaverage))
                (let ((count 0))
        (loop for functioningcode in random-selection unless (equal candidate functioningcode) ; pick every possible c* != c
            do (let ((eligibleremaining 0)) ; track with this particular code as functioning answer
            ; pick every currently eligible guess that is not c or c*
            (loop for givenguess in random-selection unless (or (eql givenguess candidate) (eql givenguess functioningcode) )
                when (guess-still-eligible givenguess candidate functioningcode colors guesses responses SCSA)
                    do (incf eligibleremaining)) ; add 1 to counter if currently eligible guess is still eligible
            (setf eligibleremainingsum (+ eligibleremainingsum eligibleremaining))) ; add total eligible guesses to sum
            do (incf count))
        ; calculate average after all functioning answers for given candidate have been processed
        (setf eligibleremainingaverage (/ eligibleremainingsum count))
        ; compare to minimum
        (when (< eligibleremainingaverage minimum)
            (progn (setf minindex i) (setf minimum eligibleremainingaverage))))))) ; set new minimum and index
    (return-from select-guess-from-eligible (nth minindex eligible))))))

; FAKEBRAIN FUNCTION DEFINITION
; MAIN ALGORITHM SEQUENCE
(defun FakeBrain (board colors SCSA last-response)
    (if (null last-response)
        (progn; First round set up -> initialize legal colors based on SCSA
            (setf *guesses* nil)
            (setf *responses* nil)
            (if (eql SCSA 'ab-color)
                (setf *legal-colors* '(A B))
                (setf *legal-colors* colors))
	        (setf *player-guess* (first-guess board *legal-colors*))
            *player-guess*)
        (progn; Other rounds
            ;; initialize population
	  (setf *population* (initialize-population *population-size* board *legal-colors* SCSA))
            ;; initialize eligible set (make empty)
            (setf *eligible-set* nil)
	        ;keep track of all previous guesses and responses
	        (setf *guesses* (append *guesses* (list *player-guess*)))
	        (setf *responses* (append *responses* (list last-response)))
	        ;;WHILE LOOP STARTS until *maxgen* or *maxsize*
            (loop for mgen upto *maxgen*
                for eligible-set-size = (length *eligible-set*)
                while (> *maxsize* eligible-set-size)
                ;; make weighted list according to fitness out of parent population
                for parent-pool = (make-weighted-list *population*)
	            ;;create new population-> calls crossover, mutation, inversion and permutation
                for new-pop = (make-new-generation parent-pool *legal-colors*)
	            ;;calculate fitness of all elements of the new population
                for new-pop-with-fitness = (mapcar
                                            (lambda (element)
                                                (list element
                                                (fitness-score element
                                                    *guesses*
                                                    *responses*
                                                    *legal-colors*
                                                    *a-weight*
                                                    *b-weight*
                                                    (1+ (third last-response)) ;; number of current guess
                                                    board)))
                                            new-pop)
	       ;; standardize fitness values
                for new-pop-with-standardized-fitness = (standardize-fitness-scores new-pop-with-fitness)
	            ;; add eligible combinations to *eligible-set*
                ;; eligible combinations do NOT have fitness scores attached
                for eligible-guesses = (loop for element-with-fitness in new-pop-with-standardized-fitness
                                            for element = (first element-with-fitness)
                                            when (not (not-eligible element *guesses* *responses* *legal-colors* SCSA))
                                            collect element)
                ;; set population to the new population and append new eligible combinations
                ;; to the previous set of eligible combinations
                do (progn
                        (setf *population* new-pop-with-standardized-fitness)
                        (setf *eligible-set* (append *eligible-set* eligible-guesses))))
	        ;;END WHILE
	        ;; choose guess from *eligible-set*
            ;; make sure eligible-set is not empty!!
	        (if (not *eligible-set*)
                (setf *player-guess* (insert-colors board *legal-colors*))
                ;; either do random pick (dumb) or most-similar
                ;; (setf *player-guess* (random-pick *eligible-set*)))
                (setf *player-guess* (select-guess-from-eligible *eligible-set* *legal-colors* *guesses* *responses* SCSA)))
            *player-guess*)))
