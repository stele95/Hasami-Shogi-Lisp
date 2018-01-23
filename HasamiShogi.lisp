(defun stampaj (n stanje)
  (format t "~%")
  (loop for a from 0 to n        
        do (loop for i from 0 to n
                 do (format t "~a " (nth i (nth a stanje))))
        do (format t "~%")))
  
(defun sendvic() ;;startuje igru
  (let* ((stanje '())         
         (n (progn
         (format t "~%Unesite n (mora da bude vece ili jednako 9): ")
              (read))))
    (loop while (< n 9)    	
    	do(setq n (progn
         (format t "~%n mora da bude vece ili jednako 9: ")
              (read))))
    (start n stanje)))



(defun testiraj (n stanje) ;;proverava da li ima pobednika
  (setq mx 1) ;; broji x u liniji
  (setq my 1) ;; broji y u liniji
  (setq ishod 0) ;; 0- nema pobednik / 1- pobednik X / 2- pobednik O
  ;;(setq stanje1 (reverse (cddr (reverse (cdddr stanje))))) 
  (setq brojx 0)	;; koliko X je ostalo na tabli
  (setq brojo 0)	;; koliko Y je ostalo na tabli

  ;; dole
  (loop for i from 3 to (- n 6)  
  ;; nema potrebe da gleda zadnjih 6 reda na tabeli zato sto ne moze da se zavrsi igra sa 4 figurice u liniji i jos 2 reda su pocetna mesta      
	  do (loop for j from 1 to n
               do (cond                                       
                ((equal (nth j (nth i stanje)) 'x) (loop for k from (+ i 1) to (+ i 4)
                                                        do (if (equal (nth j (nth k stanje)) 'x) (setq mx (+ mx 1)) '())
                                                        do (if (>= mx 5) (setq ishod 1) '())))
                ((equal (nth j (nth i stanje)) 'o) (loop for k from (+ i 1) to (+ i 4)
                                                        do (if (equal (nth j (nth k stanje)) 'o) (setq my (+ my 1)) '())
                                                          do (if (>= my 5) (setq ishod 2) '())))
                   )))


  ;; dijagonala desno
  (setq mx 1) 
  (setq my 1)
  (loop for i from 3 to (- n 6)
  	do (loop for j from 1 to (- n 4)
      do (cond                                       
                ((equal (nth j (nth i stanje)) 'x) (progn
                	(setq k (+ i 1))
                	(setq h (+ j 1))
                	(loop while (and (<= k (+ i 4)) (<= h (+ j 4)))                                  
                		do (if (equal (nth h (nth k stanje)) 'x) (setq mx (+ mx 1)) '())
                		do (if (>= mx 5) (setq ishod 1) '())
                		do (setq k (+ k 1))
                		do (setq h (+ h 1))
                		)))
                ((equal (nth j (nth i stanje)) 'o) (progn
                	(setq k (+ i 1))
                	(setq h (+ j 1))
                	(loop while (and (<= k (+ i 4)) (<= h (+ j 4)))                                     
                		do (if (equal (nth h (nth k stanje)) 'o) (setq my (+ my 1)) '())
                		do (if (>= my 5) (setq ishod 2) '())
                		do (setq k (+ k 1))
                		do (setq h (+ h 1))
                		)))
                )
          ))

  ;; dijagonala levo
  (setq mx 1) 
  (setq my 1)
  (loop for i from 3 to (- n 6)
        do (setq j n)
      do (loop while (>= j 5)
               do (cond

               ((equal (nth j (nth i stanje)) 'x) (progn
                	(setq k (+ i 1))
                	(setq h (- j 1))
                	(loop while (and (<= k (+ i 4)) (>= h (- j 4)))                                  
                		do (if (equal (nth h (nth k stanje)) 'x) (setq mx (+ mx 1)) '())
                		do (if (>= mx 5) (setq ishod 1) '())
                		do (setq k (+ k 1))
                		do (setq h (- h 1))
                		)))
                ((equal (nth j (nth i stanje)) 'o) (progn
                	(setq k (+ i 1))
                	(setq h (- j 1))
                	(loop while (and (<= k (+ i 4)) (>= h (- j 4)))                                     
                		do (if (equal (nth h (nth k stanje)) 'o) (setq my (+ my 1)) '())
                		do (if (>= my 5) (setq ishod 2) '())
                		do (setq k (+ k 1))
                		do (setq h (- h 1))
                		)))
                   
                   )
               do(setq j (- j 1))))


  ;; provera koliko figurica je ostalo igracima
  (loop for i from 1 to n
        do(loop for j from 1 to n
                do(if (equal (nth j (nth i stanje)) 'x) (setq brojx (+ brojx 1)) (if (equal (nth j (nth i stanje)) 'o)
                                                                                    (setq brojo (+ brojo 1)) '()))))
  (if (< brojx 5) (setq ishod 2) (if (< brojo 5) (setq ishod 1) '()))


  (return-from testiraj ishod)
  )
  


;;potez se unosi u sledecem formatu: npr. ((G 1)(E 1))
;; (G 1) pretstavlja figuricu koju pomeramo, (E 1) polje na koje zelimo da je pomerimo
(defun potez (n stanjee figurica potez igrac) ;;odigrava potez ako je validan
  (setq igraj (validacija n stanjee figurica potez))
  (setq lista '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  (setq index 0)
  (setq index1 0)
  (loop for i from 0 to n
        do (if (equal (nth 0 figurica) (nth i lista)) (setq index i) '()))
    (loop for i from 0 to n
        do (if (equal (nth 0 potez) (nth i lista)) (setq index1 i) '()))
  (if (not (equal igrac (nth (nth 1 figurica) (nth (+ index 1) stanjee))))(setq igraj 0) '())
  (if (equal igraj 1) (setf (nth (nth 1 potez) (nth (+ index1 1) stanjee)) (nth (nth 1 figurica) (nth (+ index 1) stanjee))) '()) 
  (if (equal igraj 1) (setf (nth (nth 1 figurica) (nth (+ index 1) stanjee)) '-) '())
  (if (equal igraj 1) (da_li_je_sendvic n stanjee potez (nth (nth 1 potez) (nth (+ index1 1) stanjee))) '())
  (return-from potez igraj))




(defun start (n stanje)
  (setq lista '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  (loop for i from 0 to n
        do (setq stanje1 '())
      do (cond
          ((equal i 0) (loop for j from 0 to n
                              do (setq stanje1 (append stanje1 (list j)))))
          ((equal i 1) (loop for j from 0 to n
                           do (if (equal j 0) (setq stanje1 (append stanje1 (list (nth (- i 1) lista)))) 
                                (setq stanje1 (append stanje1 '(x))))))
           ((equal i 2) (loop for j from 0 to n
                              do (if (equal j 0) (setq stanje1 (append stanje1 (list (nth (- i 1) lista)))) 
                                (setq stanje1 (append stanje1 '(x))))))
           ((equal i (- n 1)) (loop for j from 0 to n
                                  do (if (equal j 0) (setq stanje1 (append stanje1 (list (nth (- i 1) lista))))
                                       (setq stanje1 (append stanje1 '(o))))))
           ((equal i n) (loop for j from 0 to n
                            do (if (equal j 0) (setq stanje1 (append stanje1 (list (nth (- i 1) lista))))
                                 (setq stanje1 (append stanje1 '(o))))))
           (t (loop for j from 0 to n
                  do (if (equal j 0) (setq stanje1 (append stanje1 (list (nth (- i 1) lista))))
                       (setq stanje1 (append stanje1 (list '-)))))))
      do (setq stanje (append stanje (list stanje1))))
  (stampaj n stanje)
  (setq kraj 'f)
  (setq igrac 'x)
  (format t "~%Unesite potez za igraca ~a: " igrac)
  (setq potezz (read))
  (loop while (equal kraj 'f)
      do (if(not (equal potezz 'k))
             (progn 
               (if(not (equal (potez n stanje (nth 0 potezz) (nth 1 potezz) igrac) 1))
                   (format t "~%Potez nije validan")
                 (progn 
                   (stampaj n stanje)
                   (setq ishod (testiraj n stanje))
                   (if (equal ishod 1) (progn (format t "Pobedio je igrac x!!") (setq kraj 't)) 
                    (if (equal ishod 2) (progn (format t "Pobedio je igrac o!!")(setq kraj 't)) '()))
                   (if (equal igrac 'x)(setq igrac 'o)(setq igrac 'x))))
               (if(equal kraj 'f)
                   (progn (format t "~%Unesite potez za igraca ~a: " igrac)
               (setq potezz (read))) '()))
           (setq kraj 't))
        )
  
  )

(defun validacija (n stanje figurica potez)
  (setq lista '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  (setq index 0)
  (setq index1 0)
  (setq pozicija 0)
  (setq stop 0)
  (setq igraj 0)
  (cond
   ((equal (nth 0 figurica) (nth 0 potez))
    (loop for i from 0 to n
          do (if (equal (nth 0 figurica) (nth i lista)) (setq index i) '()))
    (if (> (nth 1 figurica) (nth 1 potez))        
        (progn 
          (setq brojac (1- (nth 1 figurica)))         
         (loop while (> brojac (- (nth 1 potez) 1))              
           do (if (equal stop 0) 
               (if (not (equal (nth brojac (nth (+ index 1) stanje)) '-))                   
                 (progn (setq stop 1)(setq pozicija (1+ brojac))) (setq pozicija brojac)) '())           
           do (setq brojac (1- brojac))           
           )
          (if (equal pozicija (nth 1 potez)) (setq igraj 1) 
            (if (equal pozicija (nth 1 figurica)) (if (equal (- (nth 1 figurica) 2) (nth 1 potez)) 
                                                         (if (equal (nth (nth 1 potez) (nth (1+ index) stanje)) '-)                                                            
                                                           (setq igraj 1)                                                           
                                                            '()) '()))
            )) 
      (progn 
          (setq brojac (+ (nth 1 figurica) 1))         
         (loop while (< brojac (+ (nth 1 potez) 1))
           do (if (equal stop 0) 
               (if (not (equal (nth brojac (nth (+ index 1) stanje)) '-))                   
                 (progn (setq stop 1) (setq pozicija (1- brojac))) (setq pozicija brojac)) '())           
           do (setq brojac (1+ brojac))           
           )
          (if (equal pozicija (nth 1 potez)) (setq igraj 1) 
            (if (equal pozicija (nth 1 figurica)) (if (equal (+ (nth 1 figurica) 2) (nth 1 potez)) 
                                                         (if (equal (nth (nth 1 potez) (nth (1+ index) stanje)) '-)                                                            
                                                           (setq igraj 1)                                                           
                                                            '()) '()))
                
            ))))
      
   ((equal (nth 1 figurica) (nth 1 potez))
    (loop for i from 0 to n
        do (if (equal (nth 0 figurica) (nth i lista)) (setq index (1+ i)) '()))
    (loop for i from 0 to n
        do (if (equal (nth 0 potez) (nth i lista)) (setq index1 (1+ i)) '()))
    (if (> index index1)        
        (progn 
          (setq brojac (1- index))         
         (loop while (> brojac (1- index1))
           do (if (equal stop 0) 
               (if (not (equal (nth (nth 1 figurica) (nth brojac stanje)) '-))                   
                 (progn (setq stop 1) (setq pozicija (1+ brojac))) (setq pozicija brojac)) '())           
           do (setq brojac (1- brojac))           
           )
          (if (equal pozicija index1) (setq igraj 1) 
            (if (equal pozicija index) (if (equal (- index 2) index1) 
                                                         (if (equal (nth (nth 1 potez) (nth index1 stanje)) '-)                                                            
                                                           (setq igraj 1)                                                           
                                                            '()) '()))
                
            )) 
      (progn 
          (setq brojac (1+ index))         
         (loop while (< brojac (1+ index1))
           do (if (equal stop 0) 
               (if (not (equal (nth (nth 1 figurica) (nth brojac stanje)) '-))                   
                 (progn (setq stop 1) (setq pozicija (1- brojac))) (setq pozicija brojac)) '())           
           do (setq brojac (1+ brojac))           
           )
          (if (equal pozicija index1) (setq igraj 1) 
            (if (equal pozicija index) (if (equal (+ index 2) index1) 
                                                         (if (equal (nth (nth 1 potez) (nth index1 stanje)) '-)                                                            
                                                           (setq igraj 1)                                                           
                                                            '()) '()))
                
            ))))    
   )
  (return-from validacija igraj)
  )


(defun da_li_je_sendvic (n stanje polje figurica)
	(setq y_axis (+ (position (car polje) lista :test #'string=) 1));;pretvara slovo iz polja na kojem se nalazi figurica nakon sto smo je pomerili u broj da se lakse porede indeksi
    (if (equal figurica 'x)
    	(setq suprotna_figurica 'o)
    	(setq suprotna_figurica 'x)
    	)    
    (setq x_axis (nth 1 polje))

    ;; gore
    (setq found 'f)
    (setq brojac (- y_axis 1))
    (loop while (and (equal (nth x_axis (nth brojac stanje)) suprotna_figurica) (> brojac 0))
    	do(setq brojac (- brojac 1))
    	)
    (if (and (equal (nth x_axis (nth brojac stanje)) figurica) (> brojac 0))
    	(setq found 't) '())
    (setq brojac (+ brojac 1))
    (if (and (not (equal brojac y_axis)) (equal found 't)) ;;ako je nasao sendvic u tom smeru
    	(loop while (<= brojac (- y_axis 1))
    		do(setf (nth x_axis (nth brojac stanje)) '-)
    		do(setq brojac (+ brojac 1)))
    	'() ;;else
    	)

    ;; dole
    (setq found 'f)
    (setq brojac (+ y_axis 1))
    (loop while (and (equal (nth x_axis (nth brojac stanje)) suprotna_figurica) (<= brojac n))
    	do(setq brojac (+ brojac 1))
    	)
    (if (and (equal (nth x_axis (nth brojac stanje)) figurica) (<= brojac n))
    	(setq found 't) '())
    (setq brojac (- brojac 1))
    (if (and (not (equal brojac y_axis)) (equal found 't)) ;;ako je nasao sendvic u tom smeru
    	(loop while (>= brojac (+ y_axis 1))
    		do(setf (nth x_axis (nth brojac stanje)) '-)
    		do(setq brojac (- brojac 1)))
    	'() ;;else
    	)

    ;; levo
	(setq found 'f)
    (setq brojac (- x_axis 1))
    (loop while (and (equal (nth brojac (nth y_axis stanje)) suprotna_figurica) (> brojac 0))
    	do(setq brojac (- brojac 1))
    	)
    (if (and (equal (nth brojac (nth y_axis stanje)) figurica) (> brojac 0))
    	(setq found 't) '())
    (setq brojac (+ brojac 1))
    (if (and (not (equal brojac x_axis)) (equal found 't)) ;;ako je nasao sendvic u tom smeru
    	(loop while (<= brojac (- x_axis 1))
    		do(setf (nth brojac (nth y_axis stanje)) '-)
    		do(setq brojac (+ brojac 1)))
    	'() ;;else
    	)

    ;; desno
    (setq found 'f)
    (setq brojac (+ x_axis 1))
    (loop while (and (equal (nth brojac (nth y_axis stanje)) suprotna_figurica) (<= brojac n))
    	do(setq brojac (+ brojac 1))
    	)
    (if (and (equal (nth brojac (nth y_axis stanje)) figurica) (<= brojac n))
    	(setq found 't) '())
    (setq brojac (- brojac 1))
    (if (and (not (equal brojac x_axis)) (equal found 't)) ;;ako je nasao sendvic u tom smeru
    	(loop while (>= brojac (+ x_axis 1))
    		do(setf (nth brojac (nth y_axis stanje)) '-)
    		do(setq brojac (- brojac 1)))
    	'() ;;else
    	)

	)