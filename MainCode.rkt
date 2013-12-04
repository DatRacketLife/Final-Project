#lang racket

(require rsound
         rsound/piano-tones)

(define beats-per-minute 140)
(define (s x)(* 44100 x))
(define (b x)(inexact->exact (* x (s (/ 60 beats-per-minute)))))
(define (m x)(inexact->exact (round (* x (b 4)))))
(define-struct chord (first third fifth))
;; a note is (make-note note-num frames frames)
(define-struct note (pitch time duration) #:transparent)
(define-struct t (number))


(define ps (make-pstream))
(define (both a b) b)
(define (super-both a b c) c)
(pstream-current-frame ps)

(require math/bigfloat)

#;(define D 
    (list
     (make-note (+ t 0) (m 1) 88200)
     (make-note (+ t 4) (m 1) 88200)
     (make-note (+ t 7) (m 1) 88200)
     
     (make-note (+ t 2) (m 2) 88200)
     (make-note (+ t 6) (m 2) 88200)
     (make-note (+ t 9) (m 2) 88200)
     
     (make-note (+ t 5) (m 3) 88200)
     (make-note (+ t 9) (m 3) 88200)
     (make-note (+ t 12) (m 3) 88200)
     
     (make-note (+ t 7) (m 4) 88200)
     (make-note (+ t 11) (m 4) 88200)
     (make-note (+ t 14) (m 4) 88200)))

;; string -> list-of-three-notes
(define-struct prog (first third fifth))

#;(define one (make-prog (make-note (+ t 0) 88200 88200)
                         (make-note (+ t 4) 88200 88200)
                         (make-note (+ t 7) 88200 88200)))


;; play the notes in a list
;; list-of-notes -> pstream
(define (play-notes lon)
  (cond [(empty? lon) ps]
        [else
         (both (play-note (first lon)) 
               (play-notes (rest lon)))]))
;; play a single note
;; note -> pstream
(define (play-note n)
  (cond
    [(equal? (note-pitch n) 0) 
     (pstream-queue ps 
                    (silence (note-duration n)) (note-time n))] 
    [else (pstream-queue
           ps
           (clip (piano-tone (note-pitch n))
                 0 (note-duration n))
           (note-time n))]))

;; list-of-lists -> pstream
(define (play-list loch)
  (cond [(empty? loch) empty]
        [else
         (both (play-notes (first loch))
               (play-list (rest loch)))]))



; Chord Poop

(define (minor-chordmaker x t measure)
  (cond [(string-ci=? x "I")
         (list 
          (make-note (+ t 0) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 3) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 7) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "II")
         (list 
          (make-note (+ t 2) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 5) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 8) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "III")
         (list 
          (make-note (+ t 3) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 7) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 10) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "IV")
         (list 
          (make-note (+ t 5) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 8) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 12) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "V")
         (list 
          (make-note (+ t 7) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 10) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 14) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "VI")
         (list 
          (make-note (+ t 8) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 12) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 15) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "VII")
         (list 
          (make-note (+ t 10) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 14) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 17) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x " ")
         (list 
          (make-note (+ t 12) (+ (pstream-current-frame ps) (m measure)) 0)
          (make-note (+ t 15) (+ (pstream-current-frame ps) (m measure)) 0)
          (make-note (+ t 19) (+ (pstream-current-frame ps) (m measure)) 0))]
        [else x]))

;; create chords from key
;; string -> list
(define (chordmaker x t measure)
  (cond [(string-ci=? x "I")
         (list 
          (make-note (+ t 0) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 4) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 7) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "II")
         (list 
          (make-note (+ t 2) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 6) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 9) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "III")
         (list 
          (make-note (+ t 4) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 7) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 11) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "IV")
         (list 
          (make-note (+ t 5) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 9) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 12) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "V")
         (list 
          (make-note (+ t 7) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 11) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 14) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "VI")
         (list 
          (make-note (+ t 9) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 12) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 16) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x "VII")
         (list 
          (make-note (+ t 11) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 14) (+ (pstream-current-frame ps) (m measure)) 88200)
          (make-note (+ t 18) (+ (pstream-current-frame ps) (m measure)) 88200))]
        [(string-ci=? x " ")
         (list 
          (make-note (+ t 11) (+ (pstream-current-frame ps) (m measure)) 0)
          (make-note (+ t 14) (+ (pstream-current-frame ps) (m measure)) 0)
          (make-note (+ t 18) (+ (pstream-current-frame ps) (m measure)) 0))]
        [else x]))

;; make a list-of-chords from chordmaker
;; list-of-strings -> list-of-chords
(define (progmaker los t indx)
  (cond [(empty? los) empty]
        [else
         (cons (chordmaker (first los) t indx)
               (progmaker (rest los) t (+ 1 indx)))]))

;plays minors 
(define (minor-progmaker los t indx)
  (cond [(empty? los) empty]
        [else
         (cons (minor-chordmaker (first los) t indx)
               (minor-progmaker (rest los) t (+ 1 indx)))]))


#;(check-equal? (progmaker empty 1)
                empty)
#;(check-equal? (progmaker (list "i" "ii") 1)
                (list
                 (list 
                  (make-note (+ t 0) (m 1)) 88200)
                 (make-note (+ t 4) (m 1) 88200)
                 (make-note (+ t 7) (m 1) 88200))
                (list
                 (make-note (+ t 2) (m 2) 88200)
                 (make-note (+ t 6) (m 2) 88200)
                 (make-note (+ t 9) (m 2) 88200)))

;; a list-of-lists is either
;; - empty, or
;; - (cons (cons list-of-lists empty) empty)

#;(define let-it-be
    (list 
     (list
      (make-note (+ t 0) (m 1) 88200)
      (make-note (+ t 4) (m 1) 88200)
      (make-note (+ t 7) (m 1) 88200)
      (make-note (+ t -8) (+ (m 1) (b 3)) 44100)
      (make-note (+ t -7) (+ (m 1) (b 3.5)) 44100))
     (list 
      (make-note (+ t -5) (m 2) 88200)
      (make-note (+ t -1) (m 2) 88200)
      (make-note (+ t 2) (m 2) 88200))
     (list 
      (make-note (+ t -3) (m 3) 88200)
      (make-note (+ t 0) (m 3) 88200)
      (make-note (+ t 4) (m 3) 88200)
      (make-note (+ t -3) (+ (b 2) (m 3)) 88200)
      (make-note (+ t 0) (+ (b 2) (m 3)) 88200)
      (make-note (+ t 4) (+ (b 2) (m 3)) 88200)
      (make-note (+ t -5) (+ (b 3) (m 3)) 88200)
      (make-note (+ t -1) (+ (b 3) (m 3)) 88200)
      (make-note (+ t 2) (+ (b 3) (m 3)) 88200))
     (list 
      (make-note (+ t -7) (m 4) 88200)
      (make-note (+ t -3) (m 4) 88200)
      (make-note (+ t 0) (m 4) 88200)
      (make-note (+ t -12) (m 5) 88200)
      (make-note (+ t -8) (m 5) 88200)
      (make-note (+ t -5) (m 5) 88200)
      (make-note (+ t 0) (m 5) 88200)
      (make-note (+ t -8) (+ (m 5) (b 3)) (b 1))
      (make-note (+ t -7) (+ (m 5) (b 3.5)) (b .5))
      (make-note (+ t -5) (m 6) 88200)
      (make-note (+ t -1) (m 6) 88200)
      (make-note (+ t 2) (m 6) 88200)
      (make-note (+ t 5) (m 7) 88200)
      (make-note (+ t 9) (m 7) 88200)
      (make-note (+ t 12) (m 7) 88200)
      (make-note (+ t 4) (+ (b 2) (m 7)) (b .5))
      (make-note (+ t 7) (+ (b 2) (m 7)) (b .5))
      (make-note (+ t 2) (+ (b 3) (m 7)) (b 1))
      (make-note (+ t 5) (+ (b 3) (m 7)) (b 1))
      (make-note (+ t 0) (m 8) 88200)
      (make-note (+ t 4) (m 8) 88200)
      (make-note (+ t 7) (m 8) 88200))))

; Beat Poop

(define (psq a b) (pstream-queue ps a b))
;beat is a structure

(define-struct beat (sound length))

;rock-loop is a list of beats
(define rock-loop
  (list
   (make-beat o-hi-hat (b 1))
   (make-beat o-hi-hat (b 3))
   (make-beat o-hi-hat (b 5))
   (make-beat o-hi-hat (b 7))
   (make-beat o-hi-hat (+ (m 1) (b 2)))
   (make-beat o-hi-hat (+ (m 1) (b 4)))
   (make-beat o-hi-hat (+ (m 1) (b 6)))
   (make-beat o-hi-hat (+ (m 1) (b 8)))
   (make-beat o-hi-hat (+ (m 2) (b 2)))
   (make-beat o-hi-hat (+ (m 2) (b 4)))
   (make-beat o-hi-hat (+ (m 2) (b 6)))
   (make-beat o-hi-hat (+ (m 2) (b 8)))
   (make-beat o-hi-hat (+ (m 3) (b 2)))
   (make-beat o-hi-hat (+ (m 3) (b 4)))
   (make-beat o-hi-hat (+ (m 3) (b 6)))
   (make-beat o-hi-hat (+ (m 3) (b 8)))
   (make-beat o-hi-hat (+ (m 4) (b 2)))
   (make-beat o-hi-hat (+ (m 4) (b 4)))
   (make-beat o-hi-hat (+ (m 4) (b 6)))
   (make-beat o-hi-hat (+ (m 4) (b 8)))
   (make-beat o-hi-hat (+ (m 5) (b 2)))
   (make-beat o-hi-hat (+ (m 5) (b 4)))
   (make-beat o-hi-hat (+ (m 5) (b 6)))
   (make-beat o-hi-hat (+ (m 5) (b 8)))
   (make-beat o-hi-hat (+ (m 6) (b 2)))
   (make-beat o-hi-hat (+ (m 6) (b 4)))
   (make-beat o-hi-hat (+ (m 6) (b 6)))
   (make-beat o-hi-hat (+ (m 6) (b 8)))
   
   (make-beat snare (b 2))
   (make-beat snare (+ (m 1) (b 2)))
   (make-beat snare (+ (m 2) (b 2)))
   (make-beat snare (+ (m 3) (b 2)))
   (make-beat snare (+ (m 4) (b 2)))
   (make-beat snare (+ (m 5) (b 2)))
   (make-beat snare (+ (m 6) (b 2)))
   (make-beat snare (+ (m 7) (b 2)))
   
   (make-beat crash-cymbal (b 8))
   (make-beat crash-cymbal (b 16))
   (make-beat crash-cymbal (b 24))
   (make-beat crash-cymbal (b 32))
   
   
   (make-beat kick (b 1))
   (make-beat kick (b 2.5))
   (make-beat kick (b 4))
   (make-beat kick (b 6.5))
   (make-beat kick (b 8))
   (make-beat kick (b 9))
   (make-beat kick (b 10.5))
   (make-beat kick (b 12))
   (make-beat kick (b 14.5))
   (make-beat kick (b 16))
   (make-beat kick (+ (m 4)(b 1)))
   (make-beat kick (+ (m 4)(b 2.5)))
   (make-beat kick (+ (m 4) (b 4)))
   (make-beat kick (+ (m 4)(b 6.5)))
   (make-beat kick (+ (m 4)(b 8)))
   (make-beat kick (+ (m 4)(b 9)))
   (make-beat kick (+ (m 4)(b 10.5)))
   (make-beat kick (+ (m 4)(b 12)))
   (make-beat kick (+ (m 4)(b 14.5)))
   (make-beat kick (+ (m 4)(b 16)))))

(define (play-beat p)
  (pstream-queue
   ps
   (rs-scale .25 (beat-sound p))
   (round (+ (pstream-current-frame ps) (beat-length p)))))

(define (play-beats lop)
  (cond [(empty? lop) ps]
        [else
         (both (play-beat (first lop)) 
               (play-beats (rest lop)))]))



; GUI Poop


(require 2htdp/universe
         2htdp/image
         rackunit)

;; a text-box-content is a string or false

;; a text-box is (make-text-box text-box-content pixels pixels)
(define-struct text-box (content x y))

;; a world is (make-world (listof text-box) number)
;; side condition: number can't be >= number of boxes
(define-struct world (tbs has-focus butt) #:transparent)



;A butts is a structure boolean x6
(define-struct butts (rockb punkb funkb majb minb octb) #:transparent)
(define starting-butts (make-butts false false false true false false ))


;; text box positions

(define SBX 1000)
(define SBY 700)
(define RIGHT-PAD (/ SBX 12))
(define BOX-SPACE (* (- SBX (* 2 RIGHT-PAD)) 0.28))

(define ROW1-HEIGHT 275)
(define ROW2-HEIGHT 375)
(define ROW3-HEIGHT 475)
(define COLLUM1 (+ RIGHT-PAD (/ SBX 15)))
(define COLLUM2 (+ COLLUM1 BOX-SPACE))
(define COLLUM3 (+ COLLUM2 BOX-SPACE))
(define COLLUM4 (+ COLLUM3 BOX-SPACE))

(define BOX-0-X COLLUM2)
(define BOX-0-Y 150)
(define BOX-1-X COLLUM1)
(define BOX-1-Y ROW1-HEIGHT)
(define BOX-2-X COLLUM2)
(define BOX-2-Y ROW1-HEIGHT)
(define BOX-3-X COLLUM3)
(define BOX-3-Y ROW1-HEIGHT)
(define BOX-4-X COLLUM4)
(define BOX-4-Y ROW1-HEIGHT)

(define BOX-5-X COLLUM1)
(define BOX-5-Y ROW2-HEIGHT)
(define BOX-6-X COLLUM2)
(define BOX-6-Y ROW2-HEIGHT)
(define BOX-7-X COLLUM3)
(define BOX-7-Y ROW2-HEIGHT)
(define BOX-8-X COLLUM4)
(define BOX-8-Y ROW2-HEIGHT)

(define BOX-9-X COLLUM1)
(define BOX-9-Y ROW3-HEIGHT)
(define BOX-10-X COLLUM2)
(define BOX-10-Y ROW3-HEIGHT)
(define BOX-11-X COLLUM3)
(define BOX-11-Y ROW3-HEIGHT)
(define BOX-12-X COLLUM4)
(define BOX-12-Y ROW3-HEIGHT)

(define RECT1-X 197)
(define RECT2-X 484)
(define RECT3-X 793)
(define RECT4-X 250)
(define RECT5-X 503)
(define RECT6-X 761)

(define ROW1 560)
(define ROW2 635)

(define x1a (- RECT1-X 77))
(define x1b (+ RECT1-X 77))
(define x2a (- RECT2-X 77))
(define x2b (+ RECT2-X 77))
(define x3a (- RECT3-X 77))
(define x3b (+ RECT3-X 77))
(define x4a (- RECT4-X 77))
(define x4b (+ RECT4-X 77))
(define x5a (- RECT5-X 77))
(define x5b (+ RECT5-X 77))
(define x6a (- RECT6-X 77))
(define x6b (+ RECT6-X 77))

(define y1 538)
(define y2 588)
(define y3 630)
(define y4 670)


(define RECTANGLEa
  (rectangle 100 40 "outline" "black"))
(define ROCK
  (bitmap/file "./images/rock.png"))
(define JAZZ
  (bitmap/file "./images/jazz.png"))
(define FUNK
  (bitmap/file "./images/funk.png"))

(define MAJOR
  (bitmap/file "./images/major.png"))
(define MINOR
  (bitmap/file "./images/minor.png"))
(define OCTAVE
  (bitmap/file "./images/octave.png"))


(define SCREEN-BACKGROUND 
  (place-image 
   ROCK
   RECT1-X ROW1
   (place-image
    JAZZ
    RECT2-X ROW1
    (place-image
     FUNK
     RECT3-X (+ 3 ROW1)
     (place-image
      MAJOR
      RECT4-X ROW2
      (place-image
       MINOR
       RECT5-X (+ 2 ROW2)
       (place-image
        OCTAVE
        RECT6-X ROW2
        (bitmap/file "./images/background5.png"))))))))

(define TEXT-SIZE 40)
(define TEXT-BOX-BACKGROUND 
  (bitmap/file "./images/textbox2.png"))
(define FOCUS-BAR (bitmap/file "./images/focus.png"))

;; draw the text box
;; content boolean -> image
(define (draw-text-box-content w)
  (cond [w
         (overlay
          (text/font w 40 "dim gray" "Trebuchet MS" 'swiss 'normal 'bold #f)
          TEXT-BOX-BACKGROUND)]
        [else TEXT-BOX-BACKGROUND]))

;; draw a world
;; world -> image
(define (draw-world world)
  (cond [(= (world-has-focus world) 0)
         (place-image
          FOCUS-BAR
          BOX-0-X (+ 25 BOX-0-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 1)
         (place-image
          FOCUS-BAR
          BOX-1-X (+ 25 BOX-1-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 2)
         (place-image
          FOCUS-BAR
          BOX-2-X (+ 25 BOX-2-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 3)
         (place-image         
          FOCUS-BAR           
          BOX-3-X (+ 25 BOX-3-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 4)
         (place-image         
          FOCUS-BAR           
          BOX-4-X (+ 25 BOX-4-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 5)
         (place-image         
          FOCUS-BAR           
          BOX-5-X (+ 25 BOX-5-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 6)
         (place-image
          FOCUS-BAR
          BOX-6-X (+ 25 BOX-6-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 7)
         (place-image
          FOCUS-BAR
          BOX-7-X (+ 25 BOX-7-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 8)
         (place-image
          FOCUS-BAR
          BOX-8-X (+ 25 BOX-8-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 9)
         (place-image
          FOCUS-BAR
          BOX-9-X (+ 25 BOX-9-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 10)
         (place-image
          FOCUS-BAR
          BOX-10-X (+ 25 BOX-10-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 11)
         (place-image
          FOCUS-BAR
          BOX-11-X (+ 25 BOX-11-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [(= (world-has-focus world) 12)
         (place-image
          FOCUS-BAR
          BOX-12-X (+ 25 BOX-12-Y)
          (draw-all-text-boxes
           (world-tbs world)
           SCREEN-BACKGROUND)
          )]
        [else (draw-all-text-boxes
               (world-tbs world)
               SCREEN-BACKGROUND)]))

;; list-of-text-boxes image -> image
;; draw all of the text boxes on the given image
(define (draw-all-text-boxes lotb back)
  (cond [(empty? lotb) back]
        [else (draw-text-box
               (first lotb)
               (draw-all-text-boxes (rest lotb) back))]))

;; text-box image -> image
;; draw a single text box on an image
(define (draw-text-box tb back)
  (place-image
   (draw-text-box-content (text-box-content tb))
   (text-box-x tb) (text-box-y tb)
   back))


#;(check-equal?
   (draw-text-box (make-text-box " " 150 80)
                  SCREEN-BACKGROUND)
   (place-image
    (rectangle 50 50 "solid" "white")
    150 80
    SCREEN-BACKGROUND))


#;(check-equal? 
   (draw-all-text-boxes empty SCREEN-BACKGROUND)
   SCREEN-BACKGROUND)

#;(check-equal?
   (draw-all-text-boxes (list (make-text-box " " 50 50)
                              (make-text-box " " 150 80))
                        SCREEN-BACKGROUND)
   (place-image
    (rectangle 50 50 "solid" "white")
    150 80
    (place-image
     (rectangle 50 50 "solid" "white")
     50 50
     SCREEN-BACKGROUND)))

#;(check-equal? 
   (draw-all-text-boxes (list (make-text-box "E" 50 80)
                              (make-text-box "Q" 150 80))
                        SCREEN-BACKGROUND)
   (place-image
    (draw-text-box-content "Q")
    150 80
    (place-image
     (overlay
      (text "E" 40 "black")
      TEXT-BOX-BACKGROUND)
     50 80
     SCREEN-BACKGROUND)))


;; take the key, put the character in the world if necessary
;; w key -> world 
(define (text-box-input-key w k)
  (cond [(key=? k "\t") (cond [(<= (world-has-focus w) (- (length (list-tb w)) 2))
                               (make-world (world-tbs w)
                                           (+ 1 (world-has-focus w))
                                           (world-butt w))]
                              [(= (- (length (list-tb w)) 1) (world-has-focus w)) (make-world (world-tbs w)
                                                                                              0
                                                                                              (world-butt w))])
                        ]
        [(key=? k "right") (cond [(<= (world-has-focus w) (- (length (list-tb w)) 2))
                                  (make-world (world-tbs w)
                                              (+ 1 (world-has-focus w))
                                              (world-butt w))]
                                 [(= (- (length (list-tb w)) 1) (world-has-focus w)) (make-world (world-tbs w)
                                                                                                 0
                                                                                                 (world-butt w))])
                           ]
        [(key=? k "left") (cond [(>= (world-has-focus w) 1)
                                 (make-world (world-tbs w)
                                             (- (world-has-focus w) 1)
                                             (world-butt w))]
                                [(= 0 (world-has-focus w)) (make-world (world-tbs w) 
                                                                       (- (length (list-tb w)) 1)
                                                                       (world-butt w))]
                                )]
        [(key=? k "k") (make-world (world-tbs w)
                                   0
                                   (world-butt w))]
        [(key=? k "\b") (make-world 
                          (list (make-text-box " " BOX-0-X BOX-0-Y)
                                (make-text-box " " BOX-1-X BOX-1-Y)
                                (make-text-box " " BOX-2-X BOX-2-Y)
                                (make-text-box " " BOX-3-X BOX-3-Y)
                                (make-text-box " " BOX-4-X BOX-4-Y)
                                (make-text-box " " BOX-5-X BOX-5-Y)
                                (make-text-box " " BOX-6-X BOX-6-Y)
                                (make-text-box " " BOX-7-X BOX-7-Y)
                                (make-text-box " " BOX-8-X BOX-8-Y)
                                (make-text-box " " BOX-9-X BOX-9-Y)
                                (make-text-box " " BOX-10-X BOX-10-Y)
                                (make-text-box " " BOX-11-X BOX-11-Y)
                                (make-text-box " " BOX-12-X BOX-12-Y)
                                )
                          (world-has-focus w)
                          (world-butt w))]
        
        [(key=? k "\r") 
         (cond [(empty? w) w]
               [else               
                (cond
                  [(empty? (list->tones (list-tb w))) w]
                  [(equal? (butts-majb (world-butt w)) true)(both (play-list (progmaker (rest (list->tones (list-tb w))) (first (list->tones (list-tb w))) 0)) w)]
                  [(equal? (butts-minb (world-butt w)) true)(both (play-list (minor-progmaker (rest (list->tones (list-tb w))) (first (list->tones (list-tb w))) 0)) w)])])]
        
        
        [else (make-world
               (update-appropriate-text-box (world-tbs w) k (world-has-focus w))
               (world-has-focus w)
               (world-butt w))]))


; make list from tbs
(define (list-tb tb)
  (cond
    [(empty? tb) empty]
    [else (list
           (text-box-content (first (world-tbs tb)))
           (text-box-content (second (world-tbs tb)))
           (text-box-content (third (world-tbs tb)))
           (text-box-content (fourth (world-tbs tb)))
           (text-box-content (fifth (world-tbs tb)))
           (text-box-content (sixth (world-tbs tb)))
           (text-box-content (seventh (world-tbs tb)))
           (text-box-content (eighth (world-tbs tb)))
           (text-box-content (ninth (world-tbs tb)))
           (text-box-content (tenth (world-tbs tb)))
           (text-box-content (list-ref (world-tbs tb) 10))
           (text-box-content (list-ref (world-tbs tb) 11))
           (text-box-content (list-ref (world-tbs tb) 12))
           )]))

;;list one to four

(define one-four (list 1 2 3 4 5 6))

;;takes list of numbers, returns list of notes
(define (list->notes lon loc)
  (cond
    [(empty? lon) empty]
    [else
     (cons (make-note (first lon) (+ (pstream-current-frame ps) (m (first loc))) 44100)
           (list->notes (rest lon) (rest loc)))]))

; takes list -> list of note pitches
(define (list->tones lol)
  (cond [(empty? lol) empty]
        [
         (string? (first lol)) 
         (cond
           [(string-ci=? (first lol) "a") (cons  57 (list->tones (rest lol)))]
           [(string-ci=? (first lol) "b") (cons  59 (list->tones (rest lol)))]
           [(string-ci=? (first lol) "c") (cons  48 (list->tones (rest lol)))]
           [(string-ci=? (first lol) "d") (cons  50 (list->tones (rest lol)))]
           [(string-ci=? (first lol) "e") (cons  52 (list->tones (rest lol)))]
           [(string-ci=? (first lol) "f") (cons  53 (list->tones (rest lol)))]
           [(string-ci=? (first lol) "g") (cons  55 (list->tones (rest lol)))]
           [(string-ci=? (first lol) " ") empty]
           [else
            (cons (first lol) (list->tones (rest lol)))])]))

;; list-of-text-boxes key number -> list-of-text-boxes
;; update the text box corresponding to the idx
(define (update-appropriate-text-box lotb k idx)
  (cond [(= 0 idx) (cond [(empty? lotb) (error
                                         'update-appropriate-text-box
                                         "ran out of text boxes!")]
                         [else (cons (update-text-box (first lotb) k)
                                     (rest lotb))])]
        [else (cons (first lotb)
                    (update-appropriate-text-box
                     (rest lotb)
                     k
                     (- idx 1)))]))

;; text-box key -> text-box
;; update the text box with the given key
(define (update-text-box tb k)
  (local
    [(define new-content
       (cond [(key=? k "a") "A"]
             [(key=? k "A") "A"]
             [(key=? k "b") "B"]
             [(key=? k "B") "B"]
             [(key=? k "c") "C"]
             [(key=? k "C") "C"]
             [(key=? k "d") "D"]
             [(key=? k "D") "D"]
             [(key=? k "e") "E"]
             [(key=? k "E") "E"]
             [(key=? k "f") "F"]
             [(key=? k "F") "F"]
             [(key=? k "g") "G"]
             [(key=? k "G") "G"]
             [(key=? k "1") "I"]
             [(key=? k "2") "II"]
             [(key=? k "3") "III"]
             [(key=? k "4") "IV"]
             [(key=? k "5") "V"]
             [(key=? k "6") "VI"]
             [(key=? k "7") "VII"]
             [(key=? k " ") " "]
             [(key=? k "\b") " "]
             
             
             [else (text-box-content tb)]))]
    (make-text-box new-content
                   (text-box-x tb)
                   (text-box-y tb))))



;; calling update-appropriate-text-box with empty list is an error!

#| (check-equal? (update-appropriate-text-box (list (make-text-box "A" 10 12)
                                                 (make-text-box "B" 20 132)
                                                 (make-text-box "C" 30 112342))
                                           "A"
                                           1)
              (list (make-text-box "A" 10 12)
                    (make-text-box "A" 20 132)
                    (make-text-box "C" 30 112342)))

(check-equal? (update-text-box (make-text-box "A" 30 79) "B")
              (make-text-box "B" 30 79))

(check-equal? (update-text-box (make-text-box " " 50 50) "a") 
              (make-text-box "A" 50 50))
(check-equal? (update-text-box (make-text-box " " 50 50) "A")
              (make-text-box "A" 50 50))
(check-equal? (update-text-box (make-text-box " " 50 50) " ")
              (make-text-box " " 50 50))
|#

(define (butt-handler w x y evt)
  (cond 
    [(and (>= y y1)
          (and (<= y y2)
               (and (>= x x1a) 
                    (and (<= x x1b) (string=? evt "button-down")))))
     (make-world (world-tbs w) (world-has-focus w) (make-butts true false false (butts-majb (world-butt w)) (butts-minb (world-butt w))  (butts-octb (world-butt w))))]
    [(and (>= y y1)
          (and (<= y y2)
               (and (>= x x2a) 
                    (and (<= x x2b) (string=? evt "button-down")))))
     (make-world (world-tbs w) (world-has-focus w)(make-butts false true false (butts-majb (world-butt w)) (butts-minb (world-butt w))  (butts-octb (world-butt w))))]
    [(and (>= y y1)
          (and (<= y y2)
               (and (>= x x3a) 
                    (and (<= x x3b) (string=? evt "button-down")))))
     (make-world (world-tbs w) (world-has-focus w) (make-butts false false true (butts-majb (world-butt w)) (butts-minb (world-butt w))  (butts-octb (world-butt w))))] 
    [(and (>= y y3)
          (and (<= y y4)
               (and (>= x x4a) 
                    (and (<= x x4b) (string=? evt "button-down")))))
     (make-world (world-tbs w) (world-has-focus w) (make-butts (butts-rockb (world-butt w)) (butts-punkb (world-butt w))(butts-funkb (world-butt w)) true false false))] 
    [(and (>= y y3)
          (and (<= y y4)
               (and (>= x x5a) 
                    (and (<= x x5b) (string=? evt "button-down")))))
     (make-world (world-tbs w) (world-has-focus w) (make-butts (butts-rockb (world-butt w)) (butts-punkb (world-butt w))(butts-funkb (world-butt w)) false true false))] 
    [(and (>= y y3)
          (and (<= y y4)
               (and (>= x x6a) 
                    (and (<= x x6b) (string=? evt "button-down")))))
     (make-world (world-tbs w) (world-has-focus w) (make-butts (butts-rockb (world-butt w)) (butts-punkb (world-butt w))(butts-funkb (world-butt w)) false false true))]
    [else w]))


(big-bang (make-world 
           (list (make-text-box " " BOX-0-X BOX-0-Y)
                 (make-text-box " " BOX-1-X BOX-1-Y)
                 (make-text-box " " BOX-2-X BOX-2-Y)
                 (make-text-box " " BOX-3-X BOX-3-Y)
                 (make-text-box " " BOX-4-X BOX-4-Y)
                 (make-text-box " " BOX-5-X BOX-5-Y)
                 (make-text-box " " BOX-6-X BOX-6-Y)
                 (make-text-box " " BOX-7-X BOX-7-Y)
                 (make-text-box " " BOX-8-X BOX-8-Y)
                 (make-text-box " " BOX-9-X BOX-9-Y)
                 (make-text-box " " BOX-10-X BOX-10-Y)
                 (make-text-box " " BOX-11-X BOX-11-Y)
                 (make-text-box " " BOX-12-X BOX-12-Y)
                 )
           0
           starting-butts)
          
          [to-draw draw-world]
          [on-key text-box-input-key]
          [on-mouse butt-handler]
          [state true])

;; TO BE IMPLEMENTED


;(play-beats rock-loop)
;(play-list let-it-be)
;(play-list (progmaker (list "i" "ii" "iii" "iv" "v" "vi" "vii" "i") 1))
