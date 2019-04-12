#lang racket/gui

(require csv-reading)

;;; Filenames
(define map-filename "MapperWDB.jpg")
(define meteorites-filename "meteorite-landings.csv")

;;; Minimum and maximum latitude and longitude values
(define-values (lat-min lat-max) (values -90.0 90.0))
(define-values (long-min long-max) (values -180.0 180.0))

;;; Some data counts
(define fell-n (make-parameter 0))
(define found-n (make-parameter 0))
(define other-n (make-parameter 0))
(define invalid-n (make-parameter 0))
(define nomatch-n (make-parameter 0))

;;; (lat-long->x-y canvas lat long) -> (values real? real?)
;;;   canvas : (is-a?/c canvas%
;;;   lat : (real-in -90.0 90.0)
;;;   long : (real-in -180.0 1800.0)
;;; Returns the (x, y) coordinates corresponding to the given lat and long.
(define/contract (lat-long->x-y canvas lat long)
  (-> (is-a?/c canvas%) (real-in -90.0 90.0) real? ; (real-in -180.0 180.0)
      (values real? real?))
  (define width (send canvas get-width))
  (define height (send canvas get-height))
  (values (* width (/ (- long long-min) (- long-max long-min)))
          (- height (* height (/ (- lat lat-min) (- lat-max lat-min))) 1)))

;;; (fall->color fall) -> string?
;;;   fall : string?
;;; Returns the color used to render a specified fall value. Also increments
;;; the data count dynamic variables.
(define/contract (fall->color fall)
  (-> string? string?)
  (case fall
    (("Fell")
     (fell-n (+ (fell-n) 1)) ; Increment fell count
     "darkred")
    (("Found")
     (found-n (+ (found-n) 1)) ; Increment found count
     "black")
    (else
     (other-n (+ (other-n) 1)) ; Increment other count
     "brown")))

;;; (main) -> any
(define (main)
  ;;; Initialize data count dynamic variables.
  (parameterize ((fell-n 0)
                 (found-n 0)
                 (other-n 0)
                 (invalid-n 0)
                 (nomatch-n 0))
    ;; Get the device context for the canvas.
    (define canvas-dc (send canvas get-dc))
    ;; Load the world map bitmap.
    (define map (make-object bitmap% 1024 512))
    (send map load-file map-filename)
    (yield) ; Wait for the load to complete
    (send canvas-dc draw-bitmap map 0 0)
    ;  Beginning of Julian Gardner's work
    ;; Draw grid lines
    (send canvas-dc set-pen "white" 1 'solid)
    (for ((long (in-range long-min long-max 10.0)))
      (if (= long 0.0)
          (send canvas-dc set-alpha 1.0)
          (send canvas-dc set-alpha 0.25))
      (send canvas-dc draw-line (* (/ (+ long 180) 10) (/ 1024 36)) 0 (* (/ (+ long 180) 10) (/ 1024 36)) 512)
      (yield))
    (for ((lat (in-range lat-min lat-max 10.0)))
      (if (= lat 0.0)
          (send canvas-dc set-alpha 1.0)
          (send canvas-dc set-alpha 0.25))
      (send canvas-dc draw-line 0 (* (/ (+ lat 90) 10) (/ 512 18)) 1024 (* (/ (+ lat 90) 10) (/ 512 18)))
      (yield))
    ;Break in Julian Gardner's work
    ;; Parse the meteorite landings file and skip the first row.
    (define parsed-meteorite-landings
      (csv->list (file->string meteorites-filename)))
    (define meteroite-landings (cdr parsed-meteorite-landings))
    (printf "There are ~s meteorite landings in file ~s.~n"
            (length meteroite-landings) meteorites-filename)
    ;; Iterate over all the meteorite landings and put them on the map.
    (for ((landing (in-list meteroite-landings)))
      (match landing
        ((list name id nametype recclass mass fall year reclat reclong GeoLocation)
         (define lat (string->number reclat))
         (define long (string->number reclong))
         ;Julian Gardner's work continues here
         (cond ((and (and lat long) (and (not (= lat 0)) (not (= long 0))))
                (if (and (string->number mass) (not (= (string->number mass) 0)) (> (* (log (string->number mass) 10) 2) 2))
                    (send canvas-dc set-pen (fall->color fall) (* (log (string->number mass) 10) 2) 'solid)
                    (send canvas-dc set-pen (fall->color fall) 2 'solid))
                (send canvas-dc set-alpha 0.5)
                ;End of Julian Gardner's work
                (define-values (x y) (lat-long->x-y canvas lat long))
                (send canvas-dc draw-point x y)
                (yield))
               (else
                (invalid-n (+ (invalid-n) 1)))))
        (_
         (nomatch-n (+ (nomatch-n) 1))
         (void))))
    ;; Print the data counts.
    (printf "Fell    = ~a~n" (fell-n))
    (printf "Found   = ~a~n" (found-n))
    (printf "Other   = ~a~n" (other-n))
    (printf "Invalid = ~a~n" (invalid-n))
    (printf "Nomatch = ~a~n" (nomatch-n))))

;;; Graphical Elements

(define frame
  (instantiate frame%
    ("Meteorites")))

(define menu-bar
  (instantiate menu-bar%
    (frame)))

(define file-menu
  (instantiate menu%
    ("&File" menu-bar)))

(define exit-menu-item
  (instantiate menu-item%
    ("E&xit" file-menu)
    (callback
     (lambda (menu-item event)
       (send frame show #f)))))

(define canvas
  (instantiate canvas%
    (frame)
    (style '(border))
    (min-width 1024)
    (min-height 512)))

(send frame show #t)

(main)
