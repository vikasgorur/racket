#lang racket

(require rsound)

(define *morse-readable*
  #hash(["A" . (dot dash)]
        ["B" . (dash dot dot dot)]
        ["C" . (dash dot dash dot)]
        ["D" . (dash dot dot)]
        ["E" . (dot)]
        ["F" . (dot dot dash dot)]
        ["G" . (dash dash dot)]
        ["H" . (dot dot dot dot)]
        ["I" . (dot dot)]
        ["J" . (dot dash dash dash)]
        ["K" . (dash dot dash)]
        ["L" . (dot dash dot dot)]
        ["M" . (dash dash)]
        ["N" . (dash dot)]
        ["O" . (dash dash dash)]
        ["P" . (dot dash dash dot)]
        ["Q" . (dash dash dot dash)]
        ["R" . (dot dash dot)]
        ["S" . (dot dot dot)]
        ["T" . (dash)]
        ["U" . (dot dot dash)]
        ["V" . (dot dot dot dash)]
        ["W" . (dot dash dash)]
        ["X" . (dash dot dot dash)]
        ["Y" . (dash dot dash dash)]
        ["Z" . (dash dash dot dot)]))

(define (build-morse-table)
  (let ([table (make-hash)])
    (hash-for-each *morse-readable*
                   (lambda (k v)
                     (hash-set! table
                                (bytes-ref (string->bytes/utf-8 k) 0)
                                v)))
    table))

(define *morse-table* (build-morse-table))

;; Returns a list of morse letters for a string with no spaces
;; A morse letter is a list containing symbols (dot dash) for that letter
;;
;; (word->morse "SO") => '((dot dot dot) (dash dash dash))
(define (word->morse str)
  (map (lambda (b)
         (hash-ref *morse-table* b))
       (bytes->list (string->bytes/utf-8 (string-upcase str)))))

;; Configuration
;;
;; See https://en.wikipedia.org/wiki/Morse_code#Representation.2C_timing.2C_and_speeds for timing multiples

;; Sampling rate - frames per second
(define *fps* 44100)

;; Desired words per minute
(define *wpm* 13)

;; Dot constant - see https://arachnoid.com/morse_code/ 
(define *dc* 1.2)

;; Length of one dot in frames
(define *dot-length* (round (* (/ *dc* *wpm*) *fps*)))

;; Length of dash in frames
(define *dash-length* (* *dot-length* 3))

;; Pause between tones within a letter
(define *tone-pause* *dot-length*)

;; Pause between letters
(define *letter-pause* (* *dot-length* 3))

;; Pause between words
(define *word-pause* (* *dot-length* 7))

;; Sound generation

(define morse-signal
  (network ()
           (note <= square-wave 800)
           (out = note)))

(define dot-sound
  (signal->rsound *dot-length* morse-signal))

(define dash-sound
  (signal->rsound *dash-length* morse-signal))

(define (symbol->sound sym)
  (case sym
    [(dot) dot-sound]
    [(dash) dash-sound]))

;; Call fn on each element of xs and append the results together
;; after adding a silence of `pause` duration after each result
(define (map-with-pause fn pause xs)
  (apply rs-append
         (map (lambda (x)
                (rs-append (fn x)
                           (silence pause)))
              xs)))

;; Returns a sound for a single morse letter
(define (mletter->sound mletter)
  (map-with-pause symbol->sound *tone-pause* mletter))

;; Returns a sound for a single morse word
(define (mword->sound mword)
  (map-with-pause mletter->sound *letter-pause* mword))

;; Returns a sound for a string containing words separated by a space
(define (sentence->sound str)
  (let ([words (string-split str " ")])
    (map-with-pause (lambda (word) (mword->sound (word->morse word)))
                    *word-pause*
                    words)))    

(define (play-sentence str)
  (play (sentence->sound str)))

(play-sentence "sos")