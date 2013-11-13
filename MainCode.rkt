#;; Definitions Poop

(require rsound)
(require rsound/piano-tones)
(define beats-per-minute 160)
(define (s x)(* 44100 x))
(define (b x)(round (* x (s (/ 60 beats-per-minute)))))
(define (m x)(* x (b 4)))
(define-struct chord (first third fifth))
;; a note is (make-note note-num frames frames)
(define-struct note (pitch time duration))
(define t 62)

(define ps (make-pstream))
(define (both a b) b)
(define (other-both a b) a)


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
  (pstream-queue
   ps
   (clip (piano-tone (note-pitch n))
         0 (note-duration n))
   (note-time n)))


#;; Chord Poop

;; create chords from key
;; string -> list
(define (chordmaker root x)
  (cond [(string-ci=? x "I")
         (list 
          (make-note (+ root 0) (m 1) 88200)
          (make-note (+ root 4) (m 1) 88200)
          (make-note (+ root 7) (m 1) 88200))]
        [(string-ci=? x "II")
         (list 
          (make-note (+ root 2) (m 1) 88200)
          (make-note (+ root 6) (m 1) 88200)
          (make-note (+ root 9) (m 1) 88200))]
        [(string-ci=? x "III")
         (list 
          (make-note (+ root 4) (m 1) 88200)
          (make-note (+ root 7) (m 1) 88200)
          (make-note (+ root 11) (m 1) 88200))]
        [(string-ci=? x "IV")
         (list 
          (make-note (+ root 5) (m 1) 88200)
          (make-note (+ root 9) (m 1) 88200)
          (make-note (+ root 12) (m 1) 88200))]
        [(string-ci=? x "V")
         (list 
          (make-note (+ root 7) (m 1) 88200)
          (make-note (+ root 11) (m 1) 88200)
          (make-note (+ root 14) (m 1) 88200))]
        [(string-ci=? x "VI")
         (list 
          (make-note (+ root 9) (m 1) 88200)
          (make-note (+ root 12) (m 1) 88200)
          (make-note (+ root 16) (m 1) 88200))]
        [(string-ci=? x "VII")
         (list 
          (make-note (+ root 11) (m 1) 88200)
          (make-note (+ root 14) (m 1) 88200)
          (make-note (+ root 18) (m 1) 88200))]
        [else root]))

;; a list-of-lists is either
;; - empty, or
;; - (cons (cons list-of-lists empty) empty)

(define lolists
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
    (make-note (+ t 4) (m 3) 88200))
   (list 
    (make-note (+ t 7) (m 4) 88200)
    (make-note (+ t 11) (m 4) 88200)
    (make-note (+ t 14) (m 4) 88200)))
  )

;; plays a list of lists
;; list-of-lists -> pstream
(define (play-list loch)
  (cond [(empty? loch) empty]
        [else
         (play-notes (other-both (first loch)
                                 (play-list (rest loch))))]))


(play-list lolists)



#;; Beat Poop

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
   (rs-scale .5 (beat-sound p))
   (round (beat-length p))))

(define (play-beats lop)
  (cond [(empty? lop) ps]
        [else
         (both (play-beat (first lop)) 
               (play-beats (rest lop)))]))

   
(play-beats rock-loop)
