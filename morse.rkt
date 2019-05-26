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
;; All durations are in number of frames, assuming 44100 frames per second.
(define *dot-length* 4000)

(define *dash-length* (* *dot-length* 4))

(define *tone-pause* (round (/ *dot-length* 8)))

(define *letter-pause* (round (* *dot-length* 4)))

(define *word-pause* (* *letter-pause* 4))

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

;; Returns a sound for a single morse letter
(define (mletter->sound mletter)
  (apply rs-append
         (map (lambda (sym)
                (rs-append (symbol->sound sym)
                           (silence *tone-pause*)))
              mletter)))

;; Returns a sound for a single morse word
(define (mword->sound mletters)
  (apply rs-append
         (map (lambda (letter)
                (rs-append (mletter->sound letter)
                           (silence *letter-pause*)))
              mletters)))

;; Returns a sound for a string containing words separated by a space
(define (sentence->sound str)
  (let ([words (string-split str " ")])
    (apply rs-append
           (map (lambda (word)
                  (rs-append (mword->sound (word->morse word))
                             (silence *word-pause*)))
                words))))
    

(define (play-sentence str)
  (play (sentence->sound str)))

(play-sentence "sos")