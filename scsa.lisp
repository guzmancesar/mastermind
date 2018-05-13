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

    ;;checks if current element has colors that are not equal to first 2 positions, i.e.e alternating colors
(defun alt-color-scsa (current)
    (loop for x from 0 to (- (length current) 1)
	do (if (equal (nth x current) (nth (mod x 2) current))
	 ()(return-from alt-color-scsa nil))
	finally (return-from alt-color-scsa t)))

(defun first-and-last (candidate)
	  (if (equal (nth 0 candidate) 
	      (nth (- (length candidate) 1) candidate))
	       (return-from first-and-last t)
	       (return-from first-and-last nil)))

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





;;;;;;;;;;;;;;;;;;;;;;;;;
;Note population maker for myster-2 uses random-chooser function that epstein wrote
;tested for lens from 4 (minimum peg size) to 10
(defun random-chooser (list)
  (nth (random (length list)) list))

;;to do, implement into player
;this function takes a pop size, a list of colors, (A B C D), and a length for each guess 

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


;;NOTE MYSTERY-5 USES RANDOM CHOOSER

(defun random-chooser (list)
  (nth (random (length list)) list))

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
