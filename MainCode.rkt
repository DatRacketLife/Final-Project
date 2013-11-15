#lang racket

(require rsound
         rsound/piano-tones)

(define beats-per-minute 150)
(define (s x)(* 44100 x))
(define (b x)(inexact->exact (* x (s (/ 60 beats-per-minute)))))
(define (m x)(* x (b 4)))
(define-struct chord (first third fifth))
;; a note is (make-note note-num frames frames)
(define-struct note (pitch time duration))
(define t 60)

(define ps (make-pstream))
(define (both a b) b)
(pstream-current-frame ps)

(require math/bigfloat)

(define D 
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

(define one (make-prog (make-note (+ t 0) 88200 88200)
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

;; plays a list of lists
;; list-of-lists -> pstream
(define (play-list loch)
  (cond [(empty? loch) empty]
        [else
         (play-notes (both (play-list (rest loch))
                           (first loch)))]))



; Chord Poop

;; create chords from key
;; string -> list
(define (chordmaker x measure)
  (cond [(string-ci=? x "I")
         (list 
          (make-note (+ t 0) (m measure) 88200)
          (make-note (+ t 4) (m measure) 88200)
          (make-note (+ t 7) (m measure) 88200))]
        [(string-ci=? x "II")
         (list 
          (make-note (+ t 2) (m measure) 88200)
          (make-note (+ t 6) (m measure) 88200)
          (make-note (+ t 1) (m measure) 88200))]
        [(string-ci=? x "III")
         (list 
          (make-note (+ t 4) (m measure) 88200)
          (make-note (+ t 7) (m measure) 88200)
          (make-note (+ t 11) (m measure) 88200))]
        [(string-ci=? x "IV")
         (list 
          (make-note (+ t 5) (m measure) 88200)
          (make-note (+ t 9) (m measure) 88200)
          (make-note (+ t 12) (m measure) 88200))]
        [(string-ci=? x "V")
         (list 
          (make-note (+ t 7) (m measure) 88200)
          (make-note (+ t 11) (m measure) 88200)
          (make-note (+ t 14) (m measure) 88200))]
        [(string-ci=? x "VI")
         (list 
          (make-note (+ t 9) (m measure) 88200)
          (make-note (+ t 12) (m measure) 88200)
          (make-note (+ t 16) (m measure) 88200))]
        [(string-ci=? x "VII")
         (list 
          (make-note (+ t 11) (m measure) 88200)
          (make-note (+ t 14) (m measure) 88200)
          (make-note (+ t 18) (m measure) 88200))]
        [else x]))

;; make a list form chordmaker
;; list-of-strings ->


;; a list-of-lists is either
;; - empty, or
;; - (cons (cons list-of-lists empty) empty)

(define let-it-be
  (list 
   (list
    (make-note (+ t 0) (m 1) 88200)
    (make-note (+ t 4) (m 1) 88200)
    (make-note (+ t 7) (m 1) 88200))
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
    (make-note (+ t 7) (m 8) 88200))
   #;(list
      (make-note (+ t -8) (+ (m 1) (b 3)) 44100)
      (make-note (+ t -7) (+ (m 1) (b 3.5)) 44100))))


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
   (make-beat crash-cymbal (b 8))
   (make-beat crash-cymbal (b 16))
   (make-beat c-hi-hat-1 (b 9))
   (make-beat o-hi-hat (b 10))
   (make-beat c-hi-hat-1 (b 11))
   (make-beat o-hi-hat (b 12))
   (make-beat c-hi-hat-1 (b 13))
   (make-beat o-hi-hat (b 14))
   (make-beat c-hi-hat-1 (b 15))
   (make-beat o-hi-hat (b 16))
   (make-beat kick (b 1))
   (make-beat kick (b 2.5))
   (make-beat snare (b 3))
   (make-beat kick (b 4))
   (make-beat kick (b 6.5))
   (make-beat snare (b 7))
   (make-beat kick (b 8))
   (make-beat snare (b 8.5))
   (make-beat kick (b 9))
   (make-beat kick (b 10.5))
   (make-beat snare (b 11))
   (make-beat kick (b 12))
   (make-beat kick (b 14.5))
   (make-beat snare (b 15))
   (make-beat kick (b 16))
   (make-beat snare (b 16.5))))

(define (play-beat p)
  (pstream-queue
   ps
   (rs-scale .25 (beat-sound p))
   (round (beat-length p))))

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
(define-struct text-box (content x y) #:transparent)

;; a world is (make-world (listof text-box) number)
;; side condition: number can't be >= number of boxes
(define-struct world (tbs has-focus) #:transparent)

(define SCREEN-BACKGROUND 
  (rectangle 300 300 "solid" "light gray"))
(define TEXT-SIZE 40)
(define TEXT-BOX-BACKGROUND 
  (rectangle 50 50 "solid" "white"))

;; draw the text box
;; content boolean -> image
(define (draw-text-box-content w)
  (cond [w
         (overlay
          (text w TEXT-SIZE "black")
          TEXT-BOX-BACKGROUND)]
        [else TEXT-BOX-BACKGROUND]))

;; draw a world
;; world -> image
(define (draw-world world)
  (draw-all-text-boxes
   (world-tbs world)
   SCREEN-BACKGROUND))

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


(check-equal?
 (draw-text-box (make-text-box #f 150 80)
                SCREEN-BACKGROUND)
 (place-image
  (rectangle 50 50 "solid" "white")
  150 80
  SCREEN-BACKGROUND))


(check-equal? 
 (draw-all-text-boxes empty SCREEN-BACKGROUND)
 SCREEN-BACKGROUND)

(check-equal?
 (draw-all-text-boxes (list (make-text-box #f 50 50)
                            (make-text-box #f 150 80))
                      SCREEN-BACKGROUND)
 (place-image
  (rectangle 50 50 "solid" "white")
  150 80
  (place-image
   (rectangle 50 50 "solid" "white")
   50 50
   SCREEN-BACKGROUND)))

(check-equal? 
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
  (cond [(key=? k "\t") (cond [(<= (world-has-focus w) 2)
                               (make-world (world-tbs w)
                                           (+ 1 (world-has-focus w)))]
                              [(= 3 (world-has-focus w)) (make-world (world-tbs w) 0)])
                        ]
        [(key=? k "right") (cond [(<= (world-has-focus w) 2)
                                  (make-world (world-tbs w)
                                              (+ 1 (world-has-focus w)))]
                                 [(= 3 (world-has-focus w)) (make-world (world-tbs w) 0)])
                           ]
        [(key=? k "left") (cond [(= 3 (world-has-focus w)) (make-world (world-tbs w) 2)]
                                [(= 2 (world-has-focus w)) (make-world (world-tbs w) 1)]
                                [(= 1 (world-has-focus w)) (make-world (world-tbs w) 0)]
                                [(= 0 (world-has-focus w)) (make-world (world-tbs w) 3)])
                          ]
        [(key=? k "\r") 
         (cond [(empty? w) "empty"]
               [else
                (both (play-notes (list->notes (list->tones (list-tb w)) one-four)) w)])]
        
        
        [else (make-world
               (update-appropriate-text-box (world-tbs w) k (world-has-focus w))
               (world-has-focus w))]))

;;list one to four

(define one-four (list 1 2 3 4))

;;takes list of numbers, returs list of notes
(define (list->notes lon loc)
  (cond
    [(empty? lon) empty]
    [else
     (cons (make-note (first lon) (+ (pstream-current-frame ps) (m (first loc))) 44100)
           (list->notes (rest lon) (rest loc)))]))
;;takes list of make-tones-> played sound

(define (play-tone p)
  (pstream-queue
   ps
   p
   (rs-frames p)
   ))

(define (play-tones lot)
  (cond [(empty? lot) ps]
        [else
         (both (play-tone (first lot)) 
               (play-tones (rest lot)))]))

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
             [(key=? k " ") #f]
             [(key=? k "\b") #f]
             
             
             [else (text-box-content tb)]))]
    (make-text-box new-content
                   (text-box-x tb)
                   (text-box-y tb))))

; takes list -> list of note pitches
(define (list->tones lol)
  (cond [(empty? lol) empty]
        [else
         (if (string? (first lol)) 
             (cond
               [(string-ci=? (first lol) "a") (cons 69 (list->tones (rest lol)))]
               [(string-ci=? (first lol) "b") (cons 71 (list->tones (rest lol)))]
               [(string-ci=? (first lol) "c") (cons 60 (list->tones (rest lol)))]
               [(string-ci=? (first lol) "d") (cons 62 (list->tones (rest lol)))]
               [(string-ci=? (first lol) "e") (cons 64 (list->tones (rest lol)))]
               [(string-ci=? (first lol) "f") (cons 65 (list->tones (rest lol)))]
               [(string-ci=? (first lol) "g") (cons 67 (list->tones (rest lol)))])
             (cons 0 (list->tones (rest lol))))]))

; make list from tbs
(define (list-tb tb)
  (cond
    [(empty? tb) empty]
    [else (list
           (text-box-content (first (world-tbs tb)))
           (text-box-content (second (world-tbs tb)))
           (text-box-content (third (world-tbs tb)))
           (text-box-content (fourth (world-tbs tb))))]))


;; calling update-appropriate-text-box with empty list is an error!

(check-equal? (update-appropriate-text-box (list (make-text-box "A" 10 12)
                                                 (make-text-box "B" 20 132)
                                                 (make-text-box "C" 30 112342))
                                           "A"
                                           1)
              (list (make-text-box "A" 10 12)
                    (make-text-box "A" 20 132)
                    (make-text-box "C" 30 112342)))

(check-equal? (update-text-box (make-text-box "A" 30 79) "B")
              (make-text-box "B" 30 79))

(check-equal? (update-text-box (make-text-box #f 50 50) "a") 
              (make-text-box "A" 50 50))
(check-equal? (update-text-box (make-text-box #f 50 50) "A")
              (make-text-box "A" 50 50))
(check-equal? (update-text-box (make-text-box #f 50 50) " ")
              (make-text-box #f 50 50))
(define boxes
  (make-world 
   (list (make-text-box #f 45 150)
         (make-text-box #f 115 150)
         (make-text-box #f 185 150)
         (make-text-box #f 255 150))
   0))

(big-bang (make-world 
           (list (make-text-box #f 45 150)
                 (make-text-box #f 115 150)
                 (make-text-box #f 185 150)
                 (make-text-box #f 255 150))
           0)
          
          [to-draw draw-world]
          [on-key text-box-input-key]
          [state true])

;; TO BE IMPLEMENTED:

;[(= (world-has-focus w) 0)
(place-image
 (overlay/align "middle" "bottom"
                (rectangle 50 2 "solid" "red")
                (rectangle 50 50 "solid" "white"))
 45 150
 SCREEN-BACKGROUND
 )


;(play-beats rock-loop)
;(play-list let-it-be)
