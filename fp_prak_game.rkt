#lang scheme/base
(require racket/list)
(require racket/mpair)
(require racket/class)

;;------------------------------------------------------------
;; Utility procedures

(define (random-number n)
  ;; Generate a random number between 1 and n
  (+ 1 (random n)))

(define (pick-random lst) (when (not (null? lst)) (list-ref lst (random (length lst))))) ; Выдает случайный эллемет списка, если список пуст, то #void

(define (find-all source type)
  (filter (lambda (x) (is-a? x type)) ;  #t если x является экземпляром класса type 
          (send source get-things))) ; возвращает список вещей объекта source

(define (all-people) ; возвращает список людей во всех комнатах
  (append-map (lambda (room) (find-all room person%)) all-rooms)) 

(define (get-list-with-random-num n) ; возвращает список длины [1, n] c различными числами от 0 до n-1
  (if (< n 1) '()
      (let loop ((m (random-number n)) (lst '()) (elem (random n)))
                (if (= m 0) lst
                    (if (member elem lst) (loop m lst (random n))
                        (loop (sub1 m) (cons elem lst) (random n)))))))
;;------------------------------------------------------------
;; Support for Objects in a Simulation World

;;--------------------
;; Clock
;;
;; A clock is an object with a notion of time, which it
;; imparts to all objects that have asked for it.  It does
;; this by invoking a list of CALLBACKs whenever the TICK
;; method is invoked on the clock.  A CALLBACK is an action to
;; invoke on each tick of the clock, by sending a message to an object

(define clock%
   (class object%
      (super-new)
      (field (the-time 0)
             (name 'the-clock)
			 (callbacks '())
			 (removed-callbacks '()))
      (define/public (print-tick) (send screen tell-world (list "---" (send this get-name) "Tick" (send this get-the-time) "---")))
      (define/public (install) (send this add-callback (new callback% (name 'tick-printer) (obj this) (message (lambda () (send this print-tick))))))
      (define/public (get-name) name)	  
      (define/public (get-the-time) the-time)
      (define/public (reset) (begin (set! the-time 0) (set! callbacks '()) (set! removed-callbacks '()))) ; сброс в начало
      (define/public (tick) (begin (set! removed-callbacks '())
	                               (for-each (lambda (x) (when (not (memq x removed-callbacks)) (send x activate)))
		                           (reverse callbacks))
					               (set! the-time (add1 the-time)))) 
      (define/public (add-callback cb) 
	      (when (null? (filter (lambda (x) (send x same-as? cb)) callbacks)) (set! callbacks (cons cb callbacks)))) ; добавляем cb в список callbacks, но если его еще нет
      (define/public (remove-callback obj cb-name)
                       (set! callbacks 
	                      (filter (lambda (x) (cond ((and (eq? (send x get-name) cb-name) (eq? (send x get-object) obj))
			                                         (set! removed-callbacks (cons x removed-callbacks)) #f)
			                                         (else #t)))
		                     callbacks)))
	  (send this install) ; инициализация
   ))

;; Clock callbacks
;;
;; A callback is an object that stores a target object, 
;; message.  When activated, it evaluate message (lambda in it).  It can be thought of as a button that executes an 
;; action at every tick of the clock.

(define callback%
   (class object%
      (super-new)
      (init-field name obj message)
      (define/public (get-name) name)
      (define/public (get-object) obj)
      (define/public (get-message) message) ; message -- (lambda () (send obj method args))
	  (define/public (install) 'installed)
      (define/public (activate) (message)) ;
	  (define/public (same-as? cb) (and (is-a? cb callback%) (eq? name (send cb get-name)) (eq? obj (send cb get-object)))) ; типа метода equal
   ))

;; Setup global clock object
(define global-clock (new clock%))

;; Get the current time
(define (current-time)
  (send global-clock get-the-time))

;; Advance the clock some number of ticks
(define (run-clock n)
  (if (<= n 0) 'DONE
        (begin (send global-clock tick) ;; remember that this activates each item in callback list
               (run-clock (sub1 n)))))

;; Using the global-clock:
;;
;; When you want the object to start being aware of the clock
;; (during initialization of autonomous-person, for example),
;; add a callback to the clock which activates a method on the
;; object:
;; (send global-clock add-callback (new callback% (name 'thingy) (obj self) (message do-thingy)))
;; The first argument is a name or descriptor of the callback.
;; The second argument is the object to which to send the message.
;; The third argument is the message (lambda) to eval.
;; In this case, the method do-thingy should be descriptive of
;; the behavior the object will exhibit when time passes.
;; When the object's lifetime expires (sometimes this is taken
;; literally!), it should remove its callback(s) from the clock.
;; This can be done with
;; (send global-clock remove-callback self 'thingy)
;;
;; An example of using callback names and additional arguments:
;; (send global-clock add-callback (new callback% (name 'whoopee) (obj me) (message (lambda () (send me say '("Whoopee!"))))))
;; (send global-clock add-callback (new callback% (name 'fun) (obj me) (message (lambda () (send me say '("I am having fun!"))))))
;; This causes the avatar to say two things every time the clock
;; ticks.

;;-----------
;; screen
;;
;; This is a singleton object (only one object of this type in 
;; existence at any time), which deals with outputting text to 
;; the user.
;;
;; If the screen is in deity-mode, the user will hear every message,
;; regardless of the location of the avatar.  If deity-mode is
;; false, only messages sent to the room which contains the avatar
;; will be heard.
;;
;; network-mode is something set only by the network code.
(define screen%
   (class object%
      (super-new)
      (field (deity-mode #t) (network-mode #f) (me #f))
      (define/public (get-name) 'the-screen)
      (define/public (set-me! new-me) (set! me new-me))
      (define/public (tell-room room msg) (when (or deity-mode (eq? room (send me get-location)))
			  (if network-mode (display-net-message msg) (display-message msg))))
      (define/public (tell-world msg) (if network-mode (display-net-message msg) (display-message msg)))
	  (define/public (set-deity-mode mode) (set! deity-mode mode))
	  (define/public (set-nework-mode mode) (set! network-mode mode))
	  (define/public (deity-mode?) deity-mode)
   ))

(define screen (new screen%))

;;--------------------
;; Utilities for our simulation world 
;;

(define (display-message list-of-stuff)
  (when (not (null? list-of-stuff)) (newline))
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff)
  'MESSAGE-DISPLAYED)

(define (display-net-message list-of-stuff)
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff)
  (display #\newline)
  (flush-output)
  'MESSAGE-DISPLAYED)

(define (display-list-message list-of-list) ; каждый список будет выведен в новой строке
  (for-each (lambda (s) (display-message s))
            list-of-list)
  'MESSAGE-DISPLAYED)

; Grab any kind of thing from avatar's location, 
; given its name.  The thing may be in the possession of
; the place, or in the possession of a person at the place.
; THING-NAMED SHOULD NEVER BE USED IN OBJTYPES OR ANY OBJECT
; YOU CREATE.
(define (thing-named name)
  (let* ((place (send me get-location))
         (things (send place get-things))
         (peek-stuff (send me peek-around))
         (my-stuff (send me get-things))
         (all-things (append things (append my-stuff peek-stuff)))
         (things-named (filter (lambda (x) (eq? name (send x get-name)))
                               all-things)))
    (cond ((null? things-named)
           (error "In here there is nothing named" name))
          ((null? (cdr things-named))   ; just one thing
           (car things-named))
          (else
           (display-message (list "There is more than one thing named"
                                  name "here. Picking one of them."))
           (pick-random things-named)))))

;;; This part defines object types for use in our simulation
;;; world.  The full world is created in next part.

;;--------------------
;; named-object
;; 
;; Named objects are the basic underlying object type in our
;; system. For example, persons, places, and things will all 
;; be kinds of (inherit from) named objects.
;;
;; Behavior (messages) supported by all named objects:
;;  - Has a NAME that it can return

(define named-object%
   (class object%
      (init-field name)
      (define/public (get-name) name)
      (define/public (set-name! newname) (set! name newname))
      (super-new)
     ))

(define (names-of objects)
  ; Given a list of objects, returns a list of their names.
  (map (lambda (x) (send x get-name)) objects))


;;--------------------
;; container
;;
;; A container holds THINGS.  
;; 
;; This class is not meant for "stand-alone" objects; rather, 
;; it is expected that other classes will inherit from the
;; container class in order to be able to contain things. 
;; For this reason, there is no create-container procedure.

(define container%
   (class object%
      (init-field (things '()))
      (define/public (get-things) things)
      (define/public (get-thing-by-name thing-name)
        (let loop ((things (send this get-things)))
          (if (null? things) #f
              (if (equal? (send (car things) get-name) thing-name) (car things)
                  (loop (cdr things))))))
      (define/public (have-thing? thing) (memq thing things))
      (define/public (add-thing! thing) (when (not (send this have-thing? thing)) (set! things (cons thing things))))
      (define/public (del-thing! thing) (set! things (remove thing things)))
      (super-new)
     ))
   
;;--------------------
;; thing
;;
;; A thing is a named-object that has a LOCATION
;;
;; Note that there is a non-trivial INSTALL here.  What does it do?

(define thing%
   (class named-object%
      (init name)
      (init-field origin)
      (inherit get-name set-name!)
      (define/public (get-location) origin)
      (define/public (install) (send origin add-thing! this))
      (define/public (destroy) (send origin del-thing! this))
      (define/public (emit text) (send screen tell-room origin (append (list "At" (send origin get-name)) text)))
      (super-new (name name))
      (install) ; initializations
     ))


;;--------------------
;; mobile-thing
;;
;; A mobile thing is a thing that has a LOCATION that can change.

(define mobile-thing%
   (class thing%
          (init name origin)
	  (field (location origin))
	  (inherit get-name set-name! install destroy emit)
	  (define/override (get-location) location)
	  (define/public (creation-site) (super get-location))
	  (define/public (change-location newlocation) (begin (send location del-thing! this) (send newlocation add-thing! this) (set! location newlocation)))
	  (define/public (enter-room) #t)
 	  (define/public (leave-room) #t)
          (super-new (name name) (origin origin))
     ))


;;--------------------
;; place
;;
;; A place is a container (so things may be in the place).
;;
;; A place has EXITS, which are passages from one place
;; to another.  One can retrieve all of the exits of a 
;; place, or an exit in a given direction from place. 

(define place%
   (class container%
      (init-field name)
      (field (named-part (new named-object% (name name))) (exits '()))
      (inherit get-things have-thing? add-thing! del-thing! get-thing-by-name)
      (define/public (get-name) (send named-part get-name))
      (define/public (set-name! newname) (send named-part set-name! newname))	  
      (define/public (get-exits) exits)
      (define/public (exit-towards direction) (find-exit-in-direction exits direction))
      (define/public (add-exit! exit) (when (not (send this exit-towards (send exit get-direction))) (set! exits (cons exit exits))))
      (super-new)
     ))


;;------------------------------------------------------------
;; exit
;;
;; An exit leads FROM one place TO another in some DIRECTION ('up 'down 'north 'south 'west 'east).

(define exit%
   (class named-object%
          (init direction)
          (init-field from to)
	  (inherit get-name set-name!)
          (define/public (get-direction) (get-name))
	  (define/public (get-from) from)
	  (define/public (get-to) to)
	  (define/public (install) 	(when (not (null? from)) (send from add-exit! this)))
	  (define/public (use whom) (begin (send whom leave-room) (send screen tell-room (send whom get-location) (list (send whom get-name) "moves from" (send (send whom get-location) get-name) "to" (send to get-name)))
	                                   (send whom change-location to) (send whom enter-room)))
      (super-new (name direction))
      (install) ; initialization
     ))

(define (find-exit-in-direction exits dir)
  ; Given a list of exits, find one in the desired direction.
  (cond ((null? exits) #f)
        ((eq? dir (send (car exits) get-direction))
         (car exits))
        (else (find-exit-in-direction (cdr exits) dir))))

(define (random-exit place)
  (pick-random (send place get-exits)))

;;--------------------
;; person
;;
;; There are several kinds of person:  
;;   There are autonomous persons, including vampires, and there
;;   is the avatar of the user.  The foundation is here.
;;
;; A person can move around (is a mobile-thing),
;; and can hold things (is a container). A person responds to
;; a plethora of messages, including 'SAY to say something.
;;
(define person%
   (class mobile-thing%
      (init name)
      (init-field birthplace (container-part (new container%)))
      (field (health 3) (strength 1))
      (inherit get-name set-name! get-location change-location leave-room destroy creation-site)
      (define/public (get-things) (send container-part get-things))
      (define/public (have-thing? thing) (send container-part have-thing? thing))
      (define/public (add-thing! thing) (send container-part add-thing! thing))      
      (define/public (del-thing! thing) (send container-part del-thing! thing))
      (define/public (get-health) health)
      (define/public (get-strength) strength)
      (define/public (get-thing-by-name thing-name) (send container-part get-thing-by-name thing-name))
      (define/public (say stuff) (send screen tell-room (get-location) (append (list "At" (send (get-location) get-name) (get-name) "says --") stuff)))
      (define/public (have-fit) (send this say '("Yaaaah! I am upset!")))
      (define/public (people-around)        ; other people in room...
                     (remove this (find-all (get-location) person%)))
        
      (define/public (stuff-around)         ; stuff (non people) in room...
                     (filter (lambda (x) (not (is-a? x person%))) (send (get-location) get-things)))
        
      (define/public (peek-around)          ; other people's stuff...
		             (foldr append '() (map (lambda (p) (send p get-things)) (send this people-around))))
     
      (define/public (take thing)
        (when (is-a? thing spell%) (send faculties signal this 'learn-spell))
	(cond ((send this have-thing? thing)  ; already have it
               (send this say (list "I am already carrying" (send thing get-name))) #f)
              ((or (is-a? thing person%)
                   (not (is-a? thing mobile-thing%)))
               (send this say (list "I try but cannot take" (send thing get-name))) #f)
              (else (let ((owner (send thing get-location)))
                      (begin (send this say (list "I take" (send thing get-name) "from" (send owner get-name)))
                             (if (is-a? owner person%) (send owner lose thing this) (send thing change-location this))
                             thing)))))
        
      (define/public (lose thing lose-to) (begin (send this say (list "I lose" (send thing get-name)))
                                                 (when (is-a? thing spell%) (send faculties signal this 'forgot-spell))
	                                             (send this have-fit)
	                                             (send thing change-location lose-to)))
        
      (define/public (drop thing) (begin (send this say (list "I drop" (send thing get-name) "at" (send (get-location) get-name)))
	                                     (send thing change-location (get-location))))
      
      (define/public (go-exit exit)	(send exit use this))
        
      (define/public (go direction) ; symbol -> boolean
	                     (let ((exit (send (get-location) exit-towards direction)))
	                           (if (and exit (is-a? exit exit%)) (send this go-exit exit) (begin (send screen tell-room (get-location) (list "No exit in" direction "direction"))
		                      #f))))
      (define/public (suffer hits perp)
        (send this say (list "Ouch!" hits "hits is more than I want!"))
        (when (and (is-a? this troll%) (<= health hits)) (send faculties signal perp 'kill-troll))
        (begin (set! health (- health hits)) (when (<= health 0) (send this die perp)) health))
        
      (define/public (die perp)         ; depends on global variable heaven -- special place for dead person
	                     (begin (for-each (lambda (item) (send this lose item (get-location))) (send this get-things))
	                            (send screen tell-world '("An earth-shattering, soul-piercing scream is heard..."))
                                    (send faculties signal this 'die)
				    (send this change-location heaven)
	                            (send this destroy)
				    (send this enter-room)))
     
      (define/public (cast-spell-on spell-name target-name)
        (let ((spell (get-spell-by-name (send this get-things) spell-name)) (target (get-person-by-name (send this people-around) target-name)))
         (if (not (and (is-a? spell spell%) (is-a? target person%)))
             (send screen tell-world  '("The spell was mispronounced."))
             (begin (send this say (list (send spell get-incant)))
                    (if (and (is-a? target dementor%) (not (equal? 'patronus spell-name))) ; на дементора действует только patronus
                        (send screen tell-world  '("The spell has no effect on the dementor."))  
                        (send spell use this target))))))

             
      (define/override (enter-room) (let ((others (send this people-around))) (begin (when (not (null? others)) (send this say (cons "Hi" (names-of others)))) #t)))
      (super-new (name name) (origin birthplace))
     ))


;;--------------------
;; autonomous-person
;;
;; activity determines maximum movement
;; miserly determines chance of picking stuff up
(define autonomous-person%
   (class person%
      (init name birthplace)
      (init-field activity miserly)
      (inherit get-name cast-spell-on set-name! get-location change-location enter-room leave-room destroy creation-site get-things have-thing? get-thing-by-name add-thing! del-thing! get-health get-strength say have-fit people-around stuff-around peek-around take lose drop go-exit go suffer)
      (define/override (install) (begin (super install) (send global-clock add-callback (new callback% (name 'move-and-take-stuff) (obj this) (message (lambda () (send this move-and-take-stuff)))))))
      (define/public (move-and-take-stuff)
	      (begin ;; first move
	          (let loop ((moves (random-number activity)))
	               (if (= moves 0) 'done-moving
	                   (begin (move-somewhere) (loop (sub1 moves)))))
	             ;; then take stuff
	          (when (= (random miserly) 0) (take-something))
	          'done-for-this-tick))
      (define/override (die perp) (begin
              (send global-clock remove-callback this 'move-and-take-stuff)
              (send this say '("SHREEEEK!  I, uh, suddenly feel very faint..."))
              (super die perp)))
      (define/public (move-somewhere) (let ((exit (random-exit (get-location)))) (when (not (null? exit)) (send this go-exit exit))))
      (define/public (take-something) (let* ((stuff-in-room (send this stuff-around))
	                                     (other-peoples-stuff (send this peek-around))
	                                     (pick-from (append stuff-in-room other-peoples-stuff)))
	                                     (if (not (null? pick-from)) (send this take (pick-random pick-from)) #f)))	  
      (super-new (name name) (birthplace birthplace))
     ))	  

;;
;; hall-monitor
;;
(define hall-monitor% 
   (class autonomous-person%
     (init name birthplace activity miserly)
     (init-field speed (irritability 10))
     (inherit get-name cast-spell-on set-name! get-location change-location enter-room leave-room destroy creation-site get-things have-thing? get-thing-by-name add-thing! del-thing! get-health get-strength say have-fit people-around stuff-around peek-around take lose drop go-exit go suffer)
     (define/override (install) (begin (super install) (send global-clock add-callback (new callback% (name 'irritate-students) (obj this) (message (lambda () (send this irritate-students)))))))
     (define/public (irritate-students)
         (if (= (random irritability) 0)
	     (let ((people (send this people-around)))
	      (if (not (null? people))
		  (begin
		    (send this say '("What are you doing still up?" "Everyone back to their rooms!"))
		    (for-each (lambda (person) (begin
				(send person emit (list (send person get-name) "goes home to" (send (send person creation-site) get-name)))
				(send person change-location (send person creation-site))))
			      people)
		    'grumped)
		    (send this say '("Grrr... When I catch those students..."))))
	     (when (send this people-around)
		   (send this say '("I'll let you off this once...")))))
     (define/override (die perp) (begin
              (send global-clock remove-callback this 'irritate-students)
              (super die perp)))		   
     (super-new (name name) (birthplace birthplace) (activity activity) (miserly miserly))
   ))
   
;;
;; troll
;;
(define troll% 
   (class autonomous-person%
     (init name birthplace activity miserly)
     (init-field speed (hunger 3))
     (inherit get-name set-name! get-location change-location enter-room leave-room destroy creation-site get-things have-thing? get-thing-by-name add-thing! del-thing! get-health get-strength say have-fit people-around stuff-around peek-around take lose drop go-exit go suffer)
     (define/override (install) (begin (super install) (send global-clock add-callback (new callback% (name 'eat-people) (obj this) (message (lambda () (send this eat-people)))))))
     (define/public (get-hunger) hunger)
     (define/public (eat-people)
        (if (= (random hunger) 0)
	     (let ((people (send this people-around)))
	      (if (not (null? people))
		  (let ((victim (pick-random people)))
		    (send this emit
			 (list (send this get-name) "takes a bite out of"
			       (send victim get-name)))
		    (send victim suffer (random-number 3) this)
                    (when (> (send victim get-health) 0)         ; жертва может ответить тролю
                      (let ((spell (get-attacking-spell victim)))
                        (when (is-a? spell spell%) (send spell use victim this))))
		    'tasty)
		  (send this emit
		       (list (send this get-name) "'s belly rumbles"))))
	    'not-hungry-now))
     (define/override (die perp) (begin
              (send global-clock remove-callback this 'eat-people)
              (super die perp)))		   
     (super-new (name name) (birthplace birthplace) (activity activity) (miserly miserly))
    ))

(define (get-attacking-spell person) ; возвращает атакующее заклинание персонажа, если оно есть
  (let loop ((things (send person get-things)))
    (if (null? things) #f
        (let ((thing (car things)))
          (if (and (is-a? thing spell%)
                   (or (equal? 'patronus (send thing get-name))
                       (equal? 'cruciatus (send thing get-name))))
              thing
              (loop (cdr things)))))))
;;
;; dementor
;;
(define dementor% 
   (class troll%
     (init name birthplace activity miserly)
     (inherit get-name set-name! get-location change-location enter-room leave-room destroy creation-site get-things have-thing? get-thing-by-name add-thing! del-thing! get-health get-strength say have-fit people-around stuff-around peek-around take lose drop go-exit go suffer die install get-hunger)	   
     (define/override (eat-people)
        (if (= (random (send this get-hunger)) 0)
	     (let ((people (send this people-around)))
	      (if (not (null? people))
		  (let ((victim (pick-random people)))
		    (send this emit
			 (list (send this get-name) "takes a bite out of"
			       (send victim get-name)))
		    (send victim suffer (random-number 3) this)
		    'tasty)
		  (send this emit
		       (list (send this get-name) "'s belly rumbles"))))
	    'not-hungry-now))
     (super-new (name name) (birthplace birthplace) (activity activity) (miserly miserly))
    ))   
;;
;; professor
;;
(define professor% 
   (class autonomous-person%
     (init name birthplace activity miserly)
     (init-field speed)
     (inherit get-name cast-spell-on set-name! get-location change-location enter-room leave-room destroy creation-site get-things have-thing? get-thing-by-name add-thing! del-thing! get-health get-strength say have-fit people-around stuff-around peek-around take lose drop go-exit go suffer)
     (define/public (speech)
       (send this say (list (send this get-name) ": I'm professor. I can teach you spells like:" (get-list-spell-names (send this get-things)))))
     (define/public (teach-spell student spell-name)
       (let ((spell (get-spell-by-name (send this get-things) spell-name)))
         (if (is-a? spell spell%)
             (begin (send student add-thing! (clone-spell spell student))
                    (send faculties signal student 'learn-spell)) ; дать сигнал таблице
             (send this say  '("I do not know such a spell.")))))
     (define/override (die perp) (begin
              (send global-clock remove-callback this 'irritate-students)
              (super die perp)))		   
     (super-new (name name) (birthplace birthplace) (activity activity) (miserly miserly) (container-part (new container% (things (get-list-random-spells)))))
   ))

(define (get-list-random-spells) ; выдает список рандомных заклинаний из chamber-of-stata
  (let* ((all-spells (send chamber-of-stata get-things)) (lst-rand-num (get-list-with-random-num (length all-spells))))
    (let loop ((spells '()) (lst lst-rand-num))
      (if (null? lst) spells
          (loop (cons (list-ref all-spells (car lst)) spells) (cdr lst))))))
    
(define (get-list-spell-names things) ; выдает список назаний заклинаний (нужно для печати)
   (let loop ((lst things) (result '()))
     (if (null? lst) result
         (if (is-a? (car lst) spell%)
             (loop (cdr lst) (cons (send (car lst) get-name) result))
             (loop (cdr lst) result)))))

(define (get-spell-by-name things name) ; выдает объект типа spell по имени из предоставленного списка вещей
  (let loop ((lst things))
    (when (not (null? lst)) 
    (if (and (is-a? (car lst) spell%) (equal? name (send (car lst) get-name))) (car lst)
        (loop (cdr lst))))))
;;
;; chosen-on
;;
(define chosen-one% 
   (class autonomous-person%
     (init name birthplace activity miserly)
     (init-field speed)
     (inherit get-name cast-spell-on set-name! get-location change-location enter-room leave-room destroy creation-site get-things have-thing? get-thing-by-name add-thing! del-thing! get-health get-strength say have-fit people-around stuff-around peek-around take lose drop go-exit go)
     (define/override (suffer hits perp)
       (let ((health (get-health)))
         (if (> health hits) ; если наносится не смертельный урон, то аналогично остальным персонажам, инаже нападающий умирает
             (super suffer hits perp)
             (send perp suffer (send perp get-health) this))))
     
     (super-new (name name) (birthplace birthplace) (activity activity) (miserly miserly))
   ))
;;
;; spell
;;
(define spell%
   (class mobile-thing%
      (init name origin)
      (init-field incant action)
      (inherit get-name set-name! get-location change-location enter-room leave-room destroy creation-site)
      (define/public (get-incant) incant)
      (define/public (get-action) action)
      (define/public (use caster target) (action caster target))
      (super-new (name name) (origin origin))
     ))
 
(define (clone-spell spell newloc)
  (new spell% (name (send spell get-name)) (origin newloc) (incant (send spell get-incant)) (action (send spell get-action))))
;;
;; broom
;;
(define broom%
   (class mobile-thing%
      (init name origin)
      (inherit get-name set-name! get-location change-location enter-room leave-room destroy creation-site)
      (define/public (move person room-name)
        (if (and (is-a? person person%) (not (equal? room-name 'heaven)))
            (let ((room (get-room-by-name room-name)))
              (if (is-a? room place%)
                (send person change-location room)
                'error))
            'error))
      (super-new (name name) (origin origin))
     ))

(define (get-room-by-name room-name)
  (let loop ((lst all-rooms))
    (if (null? lst) #f
        (if (equal? room-name (send (car lst) get-name))
            (car lst)
            (loop (cdr lst))))))
;;--------------------
;; avatar
;;
;; The avatar of the user is also a person.
(define avatar%
   (class person%
      (init name birthplace)
      (inherit get-name cast-spell-on set-name! get-location change-location enter-room leave-room destroy creation-site get-things have-thing? get-thing-by-name add-thing! del-thing! get-health get-strength say have-fit people-around stuff-around peek-around take lose drop go-exit suffer)
      (define/public (look-around) ; report on world around you
	   (let* ((place (get-location))
	       (exits (send place get-exits))
	       (other-people (send this people-around))
	       (my-stuff (send this get-things))
	       (stuff (send this stuff-around)))
	  (send screen tell-world (list "You are in" (send place get-name)))
	  (send screen tell-world (if (null? my-stuff) '("You are not holding anything.") (append '("You are holding:") (names-of my-stuff))))
	  (send screen tell-world (if (null? stuff) '("There is no stuff in the room.") (append '("You see stuff in the room:") (names-of stuff))))
	  (send screen tell-world (if (null? other-people) '("There are no other people around you.") (append '("You see other people:") (names-of other-people))))
	  (send screen tell-world (if (not (null? exits)) (append '("The exits are in directions:") (names-of exits))
		                                                    ;; heaven is only place with no exits
		                                                    '("There are no exits... you are dead and gone to heaven!")))
	  'OK))
     
     (define/public (talk-to person-name) ; можно услышать речь персонажа назвав его имя, если он рядом (пока только для профессоров)
       (let ((person (get-person-by-name (send me people-around) person-name)))
         (if (is-a? person professor%) (send person speech)
             (send person say  '("I'm not professor.")))))
     
     (define/public (learn-spell spell-name professor-name) ; можно попросить профессора обучить заклинанию
       (let ((person (get-person-by-name (send this people-around) professor-name)))
         (if (is-a? person professor%) (send person teach-spell this spell-name)
             (send person say  '("I'm not professor.")))))
     
     (define/public (move-on-broom broom-name room-name)
       (let ((broom (get-thing-by-name broom-name)))
             (if (is-a? broom broom%)
                 (send broom move this room-name)
                 'error)))
     
	  (define/override (go direction) (let ((success? (super go direction))) (begin (when success? (send global-clock tick)) success?)))
	  (define/override (die perp) (begin (send this say (list "I am slain!")) (super die perp)))
      (super-new (name name) (birthplace birthplace))
     ))  

(define (get-person-by-name persons name) ; выдает объект типа person по имени из предоставленного списка персонажей
  (let loop ((lst persons))
    (when (not (null? lst)) 
    (if (and (is-a? (car lst) person%) (equal? name (send (car lst) get-name))) (car lst)
        (loop (cdr lst))))))
;;
;; information table
;;
(define info-table%
   (class object%
     (init-field (nerds '()) (killers '()))
     (field (info-nerds '()) (info-killers '()))
     (define/public (study-at-nerds? student) (memq student nerds))
     (define/public (study-at-killers? student) (memq student killers))
     (define/public (study-at-faculty? student) (or (study-at-nerds? student) (study-at-killers? student)))
     (define/public (add-nerd person) (when (and (is-a? person person%) (not (study-at-faculty? person))) (set! nerds (cons person nerds))))
     (define/public (add-killer person) (when (and (is-a? person person%) (not (study-at-faculty? person))) (set! killers (cons person killers))))
     (define/public (expel-from-nerds  person)(set! nerds (remove person nerds)))
     (define/public (expel-from-killers  person)(set! killers (remove person killers)))

     (define/public (get-list-names faculty)
       (let loop ((lst faculty) (result '()))
         (if (null? lst) result
             (loop (cdr lst) (cons (send (car lst) get-name) result)))))

     (define/public (information-about-students)
       (list (append '(Факультет ботанов: ) (get-list-names nerds))
             (append '(Факультет убийц: ) (get-list-names killers))))

     (define/public (signal student action) ; метод вызывается из мест, где произошло действие, кторое должно быть записано в таблицу
       (when (study-at-nerds? student)
         (cond ((equal? action 'learn-spell) (set! info-nerds (cons (list (append '(Ученик ) (cons (send student get-name) '( выучил заклинание: 5 очков))) 5) info-nerds)))
               ((equal? action 'forgot-spell) (set! info-nerds (cons (list (append '(Ученик ) (cons (send student get-name) '( забыл заклинание: -5 очков))) -5) info-nerds)))))
      (when (study-at-killers? student)
        (cond ((equal? action 'kill-troll) (set! info-killers (cons (list (append '(Ученик ) (cons (send student get-name) '( убил троля: 20 очков))) 20) info-killers)))
               ((equal? action 'die) (set! info-killers (cons (list (append '(Ученик ) (cons (send student get-name) '( умер: -10 очков))) -10) info-killers))))))

     (define/public (display-info)
      (let loop ((lst (reverse info-nerds)) (sum 0))
        (if (null? lst) (display-message (append '(Сумма очков факультета ботанов: ) (list sum)))
            (begin (display-message (car (car lst)))
                   (loop (cdr lst) (+ sum (cadr (car lst)))))))
      (let loop ((lst (reverse info-killers)) (sum 0))
        (if (null? lst) (display-message (append '(Сумма очков факультета убийц: ) (list sum)))
            (begin (display-message (car (car lst)))
                   (loop (cdr lst) (+ sum (cadr (car lst)))))))
     )

     (define/public (distribution-hat students) ; распределяет студентов
       (for-each (lambda (s)
                   (when (not (study-at-faculty? s))
                     (let ((n (random 2)))
                       (if (= n 0) (send faculties add-nerd s)
                           (send faculties add-killer s)))))
                 students))
     
     (super-new)
     ))

;;;========================================================================
;;; You can extend this part to extend your world.
;;;========================================================================

;;------------------------------------------------------------
;; Utils to connect places by way of exits

(define (can-go-both-ways from-place dir-to reverse-dir to-place)
  (new exit% (from from-place) (direction dir-to) (to to-place))
  (new exit% (from to-place) (direction reverse-dir) (to from-place)))

;;------------------------------------------------------------
;; Create our world...

(define (create-world)
  ; Create some places
  (let* ((10-250 (new place% (name '10-250)))
        (lobby-10 (new place% (name 'lobby-10)))
        (grendels-den (new place% (name 'grendels-den)))
        (barker-library (new place% (name 'barker-library)))
        (lobby-7 (new place% (name 'lobby-7)))
        (eecs-hq (new place% (name 'eecs-hq)))
        (eecs-ug-office (new place% (name 'eecs-ug-office)))
        (edgerton-hall (new place% (name 'edgerton-hall)))
        (34-301 (new place% (name '34-301)))
        (stata-center (new place% (name 'stata-center)))
        (6001-lab (new place% (name '6001-lab)))
        (building-13 (new place% (name 'building-13)))
        (great-court (new place% (name 'great-court)))
        (student-center (new place% (name 'student-center)))
        (bexley (new place% (name 'bexley)))
        (baker (new place% (name 'baker)))
        (legal-seafood (new place% (name 'legal-seafood)))
        (graduation-stage (new place% (name 'graduation-stage))))
    (begin
    ; Connect up places
    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways grendels-den 'up 'down lobby-10)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east student-center)
    (can-go-both-ways student-center 'south 'north bexley)
    (can-go-both-ways bexley 'west 'east baker)
    (can-go-both-ways lobby-10 'north 'south building-13)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways building-13 'north 'south edgerton-hall)
    (can-go-both-ways edgerton-hall 'up 'down 34-301)
    (can-go-both-ways 34-301 'up 'down eecs-hq)
    (can-go-both-ways 34-301 'east 'west stata-center)
    (can-go-both-ways stata-center 'north 'south stata-center)
    (can-go-both-ways stata-center 'up 'down stata-center)
    (can-go-both-ways eecs-hq 'west 'east eecs-ug-office)
    (can-go-both-ways edgerton-hall 'north 'south legal-seafood)
    (can-go-both-ways eecs-hq 'up 'down 6001-lab)
    (can-go-both-ways legal-seafood 'east 'west great-court)
    (can-go-both-ways great-court 'up 'down graduation-stage)
    
    ; Create some things
    (new thing% (name 'blackboard) (origin 10-250))
    (new thing% (name 'lovely-trees) (origin great-court))
    (new thing% (name 'flag-pole) (origin great-court))
    (new mobile-thing% (name 'tons-of-code) (origin baker))
    (new mobile-thing% (name 'problem-set) (origin 10-250))
    (new mobile-thing% (name 'recitation-problem) (origin 10-250))
    (new mobile-thing% (name 'sicp) (origin stata-center))
    (new mobile-thing% (name 'engineering-book) (origin barker-library))
    (new mobile-thing% (name 'diploma) (origin graduation-stage))
    (new broom% (name 'broom) (origin lobby-10))
    (new broom% (name 'broom) (origin lobby-10))
    
    (list 10-250 lobby-10 grendels-den barker-library lobby-7
          eecs-hq eecs-ug-office edgerton-hall 34-301 6001-lab
          building-13 great-court stata-center
          student-center bexley baker legal-seafood
          graduation-stage))))

(define heaven (new place% (name 'heaven))) ; special place for dead persons

; all spells exist in the chamber-of-stata.  When placing a spell
; in the outside world, the original spell from the chamber-of stata
; is cloned (using clone-spell).
; There are no entrances, exits, or people in the chamber, preventing
;  the spells there from being stolen.

	  
(define (instantiate-spells);пишет не правильное место действия
  (let ((chamber (new place% (name 'chamber-of-stata))))
    (new spell% (name 'boil-spell) (origin chamber) (incant "habooic katarnum") (action (lambda (caster target)
       (send target emit (list (send target get-name) "grows boils on their nose")))))

    (new spell% (name 'slug-spell) (origin chamber) (incant "dagnabbit ekaterin") (action (lambda (caster target)
       (send target emit (list "A slug comes out of" (send target get-name) "'s mouth.")) (new mobile-thing% (name 'slug) (origin (send target get-location))))))

    (new spell% (name 'cruciatus) (origin chamber) (incant "сrucio") (action (lambda (caster target) ; наносит урон равный силе
       (send target emit (list (send target get-name) "feels unbearable pain"))
       (when (is-a? target person%)
         (send target suffer (send caster get-strength) caster)))))
    
    (new spell% (name 'patronus) (origin chamber) (incant "expecto patronum") (action (lambda (caster target) ; наносит урон равный удвоенной(loo силе
       (send target emit (list (send target get-name) "attacked by a patronus"))
       (when (is-a? target person%)
         (send target suffer (* 2 (send caster get-strength)) caster)))))
    
    chamber))

(define (populate-spells rooms)
  (for-each (lambda (room)
	      (clone-spell (pick-random (send chamber-of-stata get-things)) room))
	    rooms))

(define (populate-players rooms) ; раскидывает разные виды персонажей по рандомным комнатам
  (let* ((students (map (lambda (name) (let ((r (pick-random rooms)))
			  (new autonomous-person% (name name) (birthplace r) (activity (random-number 3)) (miserly (random-number 3)))))
			'(ben-bitdiddle alyssa-hacker course-6-frosh lambda-man)))

	 (profs (map (lambda (name) (let ((r (pick-random rooms)))
			  (new professor% (name name) (birthplace r) (activity (random-number 3)) (miserly (random-number 3)) (speed (random-number 3)))))
		     '(Susan-Hockfield Eric-Grimson)))
	 (monitors (map (lambda (name) (let ((r (pick-random rooms)))
			  (new hall-monitor% (name name) (birthplace r) (activity (random-number 3)) (miserly (random-number 3)) (speed (random-number 3)))))
			'(dr-evil mr-bigglesworth)))
	 (trolls (map (lambda (name) (let ((r (pick-random rooms)))
			  (new troll% (name name) (birthplace r) (activity (random-number 3)) (miserly (random-number 3)) (speed (random-number 3)))))
		      '(grendel registrar)))
         (dementors (map (lambda (name) (let ((r (pick-random rooms)))
			  (new dementor% (name name) (birthplace r) (activity (random-number 3)) (miserly (random-number 3)) (speed (random-number 3)))))
		      '(Dementor1 Dementor2)))
         )
    (send faculties distribution-hat students)

    (append students profs        
	    monitors trolls
            dementors)))
  


(define me 'will-be-set-by-setup)
(define all-rooms 'will-be-set-by-setup)
(define chamber-of-stata 'will-be-set-by-setup)
(define faculties  (new info-table%))

(define (setup name)
  (send global-clock reset)
  (send global-clock install)
  (let ((rooms (create-world)))
    (set! chamber-of-stata (instantiate-spells))
    (populate-spells rooms) ;раскидали заклинания по комнатам
    (populate-players rooms) ; раскидали персонажей
    ; добавленеие мальчика, который выжил
    (let ((r (pick-random rooms)))
      (new chosen-one% (name 'hairy-cdr) (birthplace r) (activity (random-number 3)) (miserly (random-number 3)) (speed (random-number 3))))
    (set! me (let ((r (pick-random rooms))) (new avatar% (name name) (birthplace r))))
    (send screen set-me! me)
    (set! all-rooms (cons heaven rooms))
    'ready))

;; Some useful example expressions...

 (setup 'Varya-Kazantseva)
; (run-clock 1)
; (send screen deity-mode #f)
; (send screen deity-mode #t)
; (send me look-around)
; (send me take (thing-named 'engineering-book))
; (send me go 'up)
; (send me go 'down)
; (send me go 'north)
; (send me go 'south)
; (send me go 'west)
; (send me go 'east)
;; Упрощенные команды для консоли
(define look (lambda() (send me look-around))) ; оглядется вокруг

(define up (lambda() (send me go 'up))) ; пойти в соответствующем направлении
(define down (lambda() (send me go 'down)))
(define north (lambda() (send me go 'north)))
(define south (lambda() (send me go 'south)))
(define west (lambda() (send me go 'west)))
(define east (lambda() (send me go 'east)))

(define take (lambda(x) (send me take (thing-named x)))) ; взять вещь по имени

(define cast (lambda(spell target)(send me cast-spell-on spell target))) ; применить заклинание к кому-то

(define talk-to (lambda(x) (send me talk-to x))) ; погоаорить с профессором

(define learn (lambda(spell prof) (send me learn-spell spell prof))) ; выучить заклинание у профессора

(define fly (lambda(broom room)(send me move-on-broom broom room))) ; переместиться на метле в указанное место

(define info (lambda() (send faculties display-info))) ; вывод таблицы достижений студентов по факультетам

(send faculties add-killer me)
(display-list-message (send faculties information-about-students))
(send faculties display-info)







