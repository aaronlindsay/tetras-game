;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tetras-game) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; TETRAS game by Aaron Lindsay
; *~~~* why play tetris when you could play something that's almost tetris? *~~~*


; Controls:
;  left: move tetra left 1 unit
; right: move tetra right 1 unit
;  down: move tetra down 1 unit
; space: drop tetra to the top of the pile
;     a: rotate tetra ccw
;     s: rotate tetra cw
;     p: pause the game

(require 2htdp/image)
(require 2htdp/universe)
; ------------------------------------------------------------------------------
;; Data Definitions

;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.

;; A List of BSets (LOB) is one of:
;; - empty
;; - (cons BSet LOB)

;; A List of Images (LOI) is one of:
;; - empty
;; - (cons Image LOI)

;; A List of Posns (LOP) is one of:
;; - empty
;; - (cons Posn LOP)

;; A Block is a (make-block Number Number Color)
;;  where x represents the 'grid' x-coordinate of a block, and y
;;  represents the grid y-coordinate;
;;   e.g. (1, 1) is the coordinate of the first grid-square
;;  and Color is a symbol from the color library
(define-struct block (x y color))

;; A Tetra is a (make-tetra Posn BSet)
;;  Where the center is the point around which the tetra rotates
;;  when it spins, and blocks is a set of blocks.
(define-struct tetra (center blocks))
 
;; A World is a (make-world Tetra BSet Number)
;;  Tetra represents the currently active tetra block that the user can move
;;  The BSet represents the pile of blocks at the bottom of the screen
;;  The Number represents a pause-state, with 1 being paused
;;  and 0 being unpaused
(define-struct world (tetra pile paused?))

; ------------------------------------------------------------------------------
; TEMPLATES

; Template for a Posn
; Posn -> ?
#; (define (posn-temp p)
     (... (posn-x p) ...
          (posn-y p) ...))

; Template for a Block
; Block -> ?
#; (define (block-temp b)
     (... (block-x b) ...
          (block-y b) ...
          (block-color b) ...))

; Template for a BSet
; BSet -> ?
#; (define (bset-temp bset)
     (cond [(empty? bset) ...]
           [(cons? bset) ...
            (block-temp (first bset)) ...
            (bset-temp (rest bset))]))
      
; Template for a Tetra
; Tetra -> ?
#; (define (tetra-temp t)
     (... (posn-x (tetra-center t)) ...
          (posn-y (tetra-center t)) ...
          (bset-temp (tetra-blocks t)) ...))

; Template for a World
; World -> ?
#; (define (world-temp w)
     ((tetra-temp (world-tetra w)) ...
      (bset-temp (world-pile w)) ...))
                
; ------------------------------------------------------------------------------
; WORLD CONSTANTS
(define BSIZE 20)
(define BHALF (/ BSIZE 2))
(define ROWS 20)
(define COLS 10)
(define BLOCK-BORDER (square 20 'outline 'black))
(define MT-CN (empty-scene (* COLS BSIZE) (* ROWS BSIZE)))

; Tetras (7 types) ; O, I, L, J, T, Z, and S, named after their shapes

(define TET-O (make-tetra
               (make-posn 5.5 -1.5)
               (list (make-block 5 -1 'lime)
                     (make-block 5 -2 'lime)
                     (make-block 6 -1 'lime)
                     (make-block 6 -2 'lime))))
(define TET-I (make-tetra
               (make-posn 5 -1)
               (list (make-block 4 -1 'blue)
                     (make-block 5 -1 'blue)
                     (make-block 6 -1 'blue)
                     (make-block 7 -1 'blue))))
(define TET-L (make-tetra
               (make-posn 5 -1)
               (list (make-block 5 -1 'purple)
                     (make-block 5 -2 'purple)
                     (make-block 5 -3 'purple)
                     (make-block 6 -1 'purple))))
(define TET-J (make-tetra
               (make-posn 6 -1)
               (list (make-block 6 -1 'cyan)
                     (make-block 6 -2 'cyan)
                     (make-block 6 -3 'cyan)
                     (make-block 5 -1 'cyan))))
(define TET-T (make-tetra
               (make-posn 5 -2)
               (list (make-block 5 -1 'darkorange)
                     (make-block 5 -2 'darkorange)
                     (make-block 5 -3 'darkorange)
                     (make-block 6 -2 'darkorange))))
(define TET-Z (make-tetra
               (make-posn 5 -2)
               (list (make-block 5 -1 'pink)
                     (make-block 5 -2 'pink)
                     (make-block 6 -2 'pink)
                     (make-block 6 -3 'pink))))
(define TET-S (make-tetra
               (make-posn 6 -2)
               (list (make-block 5 -2 'red)
                     (make-block 5 -3 'red)
                     (make-block 6 -1 'red)
                     (make-block 6 -2 'red))))
; ------------------------------------------------------------------------------
; tetragen : Number -> Tetra
; randomly chooses a new tetra
(define (tetragen rand)
  (cond [(= rand 0) TET-O]
        [(= rand 1) TET-I]
        [(= rand 2) TET-L]
        [(= rand 3) TET-J]
        [(= rand 4) TET-T]
        [(= rand 5) TET-Z]
        [(= rand 6) TET-S]))
; ------------------------------------------------------------------------------
; Constants to be used in tests

; To-draw constants
(define WORLD-A (make-world ; a world with a non-empty pile
                 (make-tetra
                  (make-posn 8 11)
                  (list (make-block 7 10 'lime)
                        (make-block 8 10 'lime)
                        (make-block 7 11 'lime)
                        (make-block 8 11 'lime)))
                 (list (make-block 5 20 'red))
                 0))

(define WORLD-B (make-world ; a world with an empty pile
                 (make-tetra
                  (make-posn 8 11)
                  (list (make-block 7 10 'lime)
                        (make-block 8 10 'lime)
                        (make-block 7 11 'lime)
                        (make-block 8 11 'lime)))
                 empty
                 0))

(define WORLD-C (make-world ; a world with empty tetra bset and an empty pile
                 (make-tetra
                  (make-posn 8 11)
                  empty)
                 empty
                 0))

; Rotation function constants
(define TET-J-1 ; TET-J in "position 1"
  (make-tetra
   (make-posn 6 4)
   (list (make-block 6 4 'cyan)
         (make-block 5 4 'cyan)
         (make-block 6 3 'cyan)
         (make-block 6 2 'cyan))))
(define TET-J-2 ; TET-J in "position 2", a single clockwise rotation from pos. 1
  (make-tetra
   (make-posn 6 4)
   (list (make-block 6 4 'cyan)
         (make-block 6 3 'cyan)
         (make-block 7 4 'cyan)
         (make-block 8 4 'cyan))))
(define TET-J-3 ; TET-J in "position 3"
  (make-tetra
   (make-posn 6 4)
   (list (make-block 6 4 'cyan)
         (make-block 7 4 'cyan)
         (make-block 6 5 'cyan)
         (make-block 6 6 'cyan))))
(define TET-J-4 ; TET-J in "position 4"
  (make-tetra
   (make-posn 6 4)
   (list (make-block 6 4 'cyan)
         (make-block 6 5 'cyan)
         (make-block 5 4 'cyan)
         (make-block 4 4 'cyan))))

; ------------------------------------------------------------------------------
; Initial world state for the beginning of the game
(define WORLD0
  (make-world (tetragen (random 7)) empty 0))

; big-bang : World -> World
; takes an initial world state and outputs a world animation
(define (tetras initial)
  (big-bang initial
            (to-draw draw-world)
            (on-key key-handler)
            (on-tick next-world .5)
            (stop-when game-over? show-score)))

; ================================ TO-DRAW =====================================
; draw-world : World -> Image
; draws all blocks in the world onto MT-CN, writes "PAUSED" if world is paused
(define (draw-world w)
  (place-images
   (append (if (= 1 (world-paused? w))
               (list (text "PAUSED" 30 'red))
               empty)
           (list (draw-score w))
           (list-images (ghost-blocks (bset-projection
                                       (tetra-blocks (world-tetra w))
                                       (world-pile w))))
           (list-images (tetra-blocks (world-tetra w)))
           (list-images (world-pile w)))
   (append (if (= 1 (world-paused? w))
               (list (make-posn (grid-convert 5.5)
                                (grid-convert 10)))
               empty)
           (list (make-posn (grid-convert 9.5) (grid-convert 1.5)))
           (list-posns (bset-projection (tetra-blocks (world-tetra w))
                                     (world-pile w)))
           (list-posns (tetra-blocks (world-tetra w)))
           (list-posns (world-pile w)))
   MT-CN))

(check-expect ; case : world with a tetra and a pile
 (draw-world WORLD-A)
 (place-images
  (list (draw-block (make-block 7 10 'lime))
        (draw-block (make-block 8 10 'lime))
        (draw-block (make-block 7 11 'lime))
        (draw-block (make-block 8 11 'lime))
        ghost-block
        ghost-block
        ghost-block
        ghost-block
        (text "1" 20 'red))
  (list (make-posn (grid-convert 7) (grid-convert 10))
        (make-posn (grid-convert 8) (grid-convert 10))
        (make-posn (grid-convert 7) (grid-convert 11))
        (make-posn (grid-convert 8) (grid-convert 11))
        (make-posn (grid-convert 7) (grid-convert 19))
        (make-posn (grid-convert 8) (grid-convert 19))
        (make-posn (grid-convert 7) (grid-convert 20))
        (make-posn (grid-convert 8) (grid-convert 20))
        (make-posn (grid-convert 9.5) (grid-convert 1.5)))
  (place-image
   (overlay
    (square 20 'outline 'black)
    (square 20 'solid 'red))
   (grid-convert 5) (grid-convert 20)
   MT-CN)))

(check-expect ; case: a world without a pile
 (draw-world WORLD-B)
 (place-images
  (list (draw-block (make-block 7 10 'lime))
        (draw-block (make-block 8 10 'lime))
        (draw-block (make-block 7 11 'lime))
        (draw-block (make-block 8 11 'lime))
        ghost-block
        ghost-block
        ghost-block
        ghost-block
        (text "0" 20 'red))
  (list (make-posn (grid-convert 7) (grid-convert 10))
        (make-posn (grid-convert 8) (grid-convert 10))
        (make-posn (grid-convert 7) (grid-convert 11))
        (make-posn (grid-convert 8) (grid-convert 11))
        (make-posn (grid-convert 7) (grid-convert 19))
        (make-posn (grid-convert 8) (grid-convert 19))
        (make-posn (grid-convert 7) (grid-convert 20))
        (make-posn (grid-convert 8) (grid-convert 20))
        (make-posn (grid-convert 9.5) (grid-convert 1.5)))
  MT-CN))

(check-expect ; case: a world without a tetra or pile
 (draw-world WORLD-C)
 (place-image (text "0" 20 'red)
              (grid-convert 9.5) (grid-convert 1.5)
              MT-CN))

; ********************* helper functions for to-draw handler *******************

; list-images : BSet -> LOI
; produces a list of images from a set of blocks
(check-expect
 (list-images (tetra-blocks TET-O))
 (list (draw-block (first (tetra-blocks TET-O)))
       (draw-block (second (tetra-blocks TET-O)))
       (draw-block (third (tetra-blocks TET-O)))
       (draw-block (fourth (tetra-blocks TET-O)))))

(define (list-images bset)
  (cond [(empty? bset) empty]
        [(cons? bset)
         (cons
          (draw-block (first bset))
          (list-images (rest bset)))]))

; draw-block : Block -> Image
; draw a block of the referenced color, unless the referenced color
; is white, in which case it draws a ghost block
(check-expect
 (draw-block (make-block 1 1 'blue))
 (overlay
  (square 20 'outline 'black)
  (square 20 'solid 'blue)))

(define (draw-block b)
  (cond [(symbol=? (block-color b) 'white)
         ghost-block]
        [else (overlay
               BLOCK-BORDER
               (square 20 'solid (block-color b)))]))

; draw-score : World -> Image
; displays the current score
(define (draw-score w)
  (text (number->string (count-points w)) 20 'red))

(check-expect (draw-score (make-world 0 (make-list 4 0) 0))
              (text "0" 20 'red))

; image of a ghost-block for projections
(define ghost-block
  (square 20 'outline 'turquoise))
; ------------------------------------------------------------------------------
; list-posns : BSet -> LOP
; produces a list of graphics-coordinate posns from a list of blocks whose
;  positions are in grid-coordinates.                               
(define (list-posns bset)
  (cond [(empty? bset) empty]
        [(cons? bset)
         (cons
          (make-posn
           (grid-convert (block-x (first bset)))
           (grid-convert (block-y (first bset))))
          (list-posns (rest bset)))]))

(check-expect 
 (list-posns ; case: given a list of blocks in a tetra
  (list (make-block 7 10 'lime)
        (make-block 8 10 'lime)
        (make-block 7 11 'lime)
        (make-block 8 11 'lime)))
 (list (make-posn (grid-convert 7) (grid-convert 10))
       (make-posn (grid-convert 8) (grid-convert 10))
       (make-posn (grid-convert 7) (grid-convert 11))
       (make-posn (grid-convert 8) (grid-convert 11))))
(check-expect 
 (list-posns ; case: given a list of blocks in a pile
  (list (make-block 5 20 'red)))
 (list (make-posn (grid-convert 5)
                  (grid-convert 20))))
(check-expect 
 (list-posns empty) empty) ; case: list of posns is empty

; ------------------------------------------------------------------------------
; grid-convert : Number -> Number
; convert a number in grid coordinates into a number representing the pixel
; coordinates of the center of the grid square
(check-expect (grid-convert 1) BHALF)
(check-expect (grid-convert 10) (- (image-width MT-CN) BHALF))

(define (grid-convert n)
   (+ (* (- n 1) BSIZE) BHALF))

; ================================ ON-TICK =====================================

; next-world : World -> World  (on-tick main handler)
; if the game is paused, leave the world alone. if tetra has not yet hit pile
; or bottom, apply gravity to it. otherwise, make a world with a new tetra
; and add the old tetra to the world pile
(define (next-world w)
  (cond [(= 1 (world-paused? w)) w]
        [(not (or (tetra-hit-pile? (world-tetra w) (world-pile w))
                  (tetra-hit-bottom? (world-tetra w))))
         (make-world
          (gravity (world-tetra w))
          (world-pile w)
          (world-paused? w))]
        [else (new-world w)]))

(check-expect ; case : tetra has not yet hit the pile or the bottom
 (next-world
  (make-world
   (make-tetra
    (make-posn 5 9)
    (list (make-block 4 9 'blue)
          (make-block 5 9 'blue)
          (make-block 6 9 'blue)
          (make-block 7 9 'blue))) empty 0))
 (make-world
  (make-tetra
   (make-posn 5 10)
   (list (make-block 4 10 'blue)
         (make-block 5 10 'blue)
         (make-block 6 10 'blue)
         (make-block 7 10 'blue))) empty 0))

(check-random ; case : tetra has hit bottom
 (next-world
  (make-world
   (make-tetra
    (make-posn 1 20)
    (list (make-block 1 20 'purple)
          (make-block 1 19 'purple)
          (make-block 1 18 'purple)
          (make-block 2 20 'purple))) empty 0))
 (make-world
  (tetragen (random 7))
  (list (make-block 1 18 'purple)
        (make-block 1 19 'purple)
        (make-block 1 20 'purple)
        (make-block 2 20 'purple)) 0)) 

; ********************* helper functions for on-tick handler *******************

; gravity : Tetra -> Tetra
; Apply block-descend to each block in the tetra blocks
(define (gravity a-tetra)
  (make-tetra
   (make-posn (posn-x (tetra-center a-tetra))
              (add1 (posn-y (tetra-center a-tetra))))
   (map block-descend (tetra-blocks a-tetra))))

(check-expect ; tetra is falling
 (gravity
  (make-tetra
   (make-posn 5 9)
   (list (make-block 4 9 'blue)
         (make-block 5 9 'blue)
         (make-block 6 9 'blue)
         (make-block 7 9 'blue))))
 (make-tetra
  (make-posn 5 10)
  (list (make-block 4 10 'blue)
        (make-block 5 10 'blue)
        (make-block 6 10 'blue)
        (make-block 7 10 'blue))))            

; block-descend : Block -> Block
; make block descend by one grid square
(define (block-descend a-block)
  (make-block
   (block-x a-block)
   (add1 (block-y a-block))
   (block-color a-block)))

(check-expect
 (block-descend (make-block 4 9 'blue))
 (make-block 4 10 'blue))
; ------------------------------------------------------------------------------
; new-world : World -> World
; make a new world, composed of a new tetra and new pile that has grown and
; had rows deleted accordingly
(define (new-world w)
  (make-world (tetragen (random 7))
              (compress-pile
               (compress-rows
                (compress-rows
                 (compress-rows
                  (compress-rows (delete-rows? (pile-rows (grow-pile w)))
                   20)
                  20)
                 20)
                20))
              (world-paused? w)))

(check-random ; new world with new tetra & old tetra added to the world pile
 (new-world
  (make-world
   (make-tetra
    (make-posn 1 20)
    (list (make-block 1 20 'purple)
          (make-block 1 19 'purple)
          (make-block 1 18 'purple)
          (make-block 2 20 'purple))) empty 0))
 (make-world
  (tetragen (random 7))
  (list (make-block 1 18 'purple)
        (make-block 1 19 'purple)
        (make-block 1 20 'purple)
        (make-block 2 20 'purple)) 0))

; grow-pile : World -> BSet
; add current tetra to pile
(define (grow-pile w)
  (append (tetra-blocks (world-tetra w))
          (world-pile w)))

(check-random ; add tetra to pile
 (grow-pile
  (make-world
   (make-tetra
    (make-posn 1 20)
    (list (make-block 1 20 'purple)
          (make-block 1 19 'purple)
          (make-block 1 18 'purple)
          (make-block 2 20 'purple))) empty 0))
 (list (make-block 1 20 'purple)
       (make-block 1 19 'purple)
       (make-block 1 18 'purple)
       (make-block 2 20 'purple)))

; compress-pile : LOB -> BSet
; converts a compressed LOB into a BSet
(define (compress-pile a-lob)
  (cond [(empty? a-lob) empty]
        [(empty? (first a-lob))
         (compress-pile (rest a-lob))]
        [else (cons (first (first a-lob))
                    (compress-pile (append (cons (rest (first a-lob)) empty)
                                           (rest a-lob))))]))

(check-expect (compress-pile (list empty
                                   (list (make-block 1 2 'blue)
                                         (make-block 2 2 'blue)
                                         (make-block 3 2 'blue))
                                   empty
                                   empty
                                   empty
                                   (list (make-block 1 6 'blue)
                                         (make-block 2 6 'blue)
                                         (make-block 3 6 'blue))))
              (list (make-block 1 2 'blue)
                    (make-block 2 2 'blue)
                    (make-block 3 2 'blue)
                    (make-block 1 6 'blue)
                    (make-block 2 6 'blue)
                    (make-block 3 6 'blue)))

; compress-rows : LOB Number -> LOB
; if a row is empty then drop-blocks all blocks above that row
(define (compress-rows a-lob n)
  (cond [(>= 0 n) (list (cons (make-block 5 -1 'black) empty))]
        [(empty? a-lob) empty]
        [(empty? (list-ref a-lob n))
         (cons empty
                (map (lambda (i)
                       (drop-blocks (list-ref a-lob i)))
                     (range 0 n 1)))]
        [else
         (append (compress-rows a-lob (sub1 n))
                 (cons (list-ref a-lob n) empty))]))

(check-expect
 (compress-rows LOB2 19)
 (list empty
       (list (make-block 1 2 'blue)
             (make-block 2 2 'blue)
             (make-block 3 2 'blue))
       empty
       empty
       empty
       (list (make-block 1 6 'blue)
             (make-block 2 6 'blue)
             (make-block 3 6 'blue))
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty))
(check-expect
 (compress-rows LOB3 19)
 (list empty
       (list (make-block 1 2 'blue)
             (make-block 2 2 'blue)
             (make-block 3 2 'blue))
       empty
       empty
       empty
       (list (make-block 1 6 'blue)
             (make-block 2 6 'blue)
             (make-block 3 6 'blue))
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       empty
       (list (make-block 1 20 'blue)
             (make-block 2 20 'blue)
             (make-block 3 20 'blue))))

; drop-rows : LOB -> LOB
; shift LOB rows down
(define (drop-rows a-lob)
  (map (lambda (i)
         (drop-blocks i))
         a-lob))

(check-expect
 (drop-rows LOB1)
 (list empty
       (list (make-block 1 3 'blue)
             (make-block 2 3 'blue)
             (make-block 3 3 'blue))
       (list (make-block 1 5 'blue)
             (make-block 2 5 'blue)
             (make-block 3 5 'blue))
       (list (make-block 1 6 'blue)
             (make-block 2 6 'blue)
             (make-block 3 6 'blue))))

; drop-blocks : BSet -> BSet
; shift pile elements down 1 unit
(define (drop-blocks a-pile)
  (map (lambda (i)
         (make-block (block-x i)
                     (add1 (block-y i))
                     (block-color i)))
         a-pile))

(check-expect
 (drop-blocks
  (list (make-block 10 10 'blue)
        (make-block 12 12 'green)))
 (list (make-block 10 11 'blue)
       (make-block 12 13 'green)))

; delete-rows? : LOB -> LOB
; remove all rows from the pile that are completed and return a new LOB
(define (delete-rows? a-lob)
  (cond [(empty? a-lob)
         empty]
        [(or (= 10 (length (first a-lob)))
             (empty? (first a-lob)))
         (cons empty (delete-rows? (rest a-lob)))]
        [else (cons (first a-lob)
                    (delete-rows? (rest a-lob)))]))

(check-expect (delete-rows? (list
                             (make-list 10 'block)
                             empty))
              (list empty empty))

(define LOB1 (list empty
                   (list (make-block 1 2 'blue)
                         (make-block 2 2 'blue)
                         (make-block 3 2 'blue))
                   (list (make-block 1 4 'blue)
                         (make-block 2 4 'blue)
                         (make-block 3 4 'blue))
                   (list (make-block 1 5 'blue)
                         (make-block 2 5 'blue)
                         (make-block 3 5 'blue))))
(define LOB2 (list (list (make-block 1 1 'blue)
                         (make-block 2 1 'blue)
                         (make-block 3 1 'blue))
                   empty
                   empty
                   empty
                   (list (make-block 1 5 'blue)
                         (make-block 2 5 'blue)
                         (make-block 3 5 'blue))
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty))
(define LOB3 (list (list (make-block 1 1 'blue)
                         (make-block 2 1 'blue)
                         (make-block 3 1 'blue))
                   empty
                   empty
                   empty
                   (list (make-block 1 5 'blue)
                         (make-block 2 5 'blue)
                         (make-block 3 5 'blue))
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   empty
                   (list (make-block 1 20 'blue)
                         (make-block 2 20 'blue)
                         (make-block 3 20 'blue))))

; pile-rows : BSet -> LOB
; takes a BSet and creates a LOB containing each row in the
; original BSet as a new BSet
(define (pile-rows a-pile)
  (map (lambda (j)
         (filter (lambda (i)
                   (= j (block-y i)))
                 a-pile))
       (range 0 21 1)))

(check-expect (pile-rows (list (make-block 1 2 'red)
                               (make-block 2 2 'blue)
                               (make-block 1 3 'green)))
              (list empty
                    empty
                    (list (make-block 1 2 'red)
                          (make-block 2 2 'blue))
                    (list (make-block 1 3 'green))
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty))
; ================================ ON-KEY ======================================

; key-handler : World KeyEvent -> World  (main on-key handler)
; if legal, move tetra according to key pressed
(define (key-handler w a-key)
  (if (tetra-legal? w a-key)
      (manipulate-tetra w a-key)
      w))
; ********************* helper functions for on-key handler ********************

; manipulate-tetra : World KeyEvent -> World
; manipulates position/orientation of tetra depending on key pressed
(define (manipulate-tetra w a-key)
  (make-world
   (cond [(key=? a-key "left")
          (tetra-shift-left (world-tetra w))]
         [(key=? a-key "right")
          (tetra-shift-right (world-tetra w))]
         [(key=? a-key "s")
          (rotate-tetra-cw (world-tetra w))]
         [(key=? a-key "a")
          (rotate-tetra-ccw (world-tetra w))]
         [(and (key=? a-key "down")
               (not (tetra-hit-pile? (world-tetra w)
                                     (world-pile w)))
               (not (tetra-hit-bottom? (world-tetra w))))
          (gravity (world-tetra w))]
         [(key=? a-key " ")
          (make-tetra
           (tetra-center (world-tetra w))
           (bset-projection (tetra-blocks (world-tetra w))
                         (world-pile w)))]
         [else (world-tetra w)])
   (world-pile w)
   (if (key=? a-key "p")
       (world-paused? (pause-switch w))
       (world-paused? w))))
; ------------------------------------------------------------------------------
; pause-switch : World -> World
; takes a world, pauses it if unpaused, unpauses it if paused
(define (pause-switch w)
  (cond [(= 1 (world-paused? w))
         (make-world
          (world-tetra w)
          (world-pile w)
          0)]
        [(= 0 (world-paused? w))
         (make-world
          (world-tetra w)
          (world-pile w)
          1)]))

(check-expect (pause-switch (make-world ; case where world becomes paused
                             (make-tetra
                              (make-posn 0 0)
                              empty)
                             empty
                             0))
              (make-world
               (make-tetra
                (make-posn 0 0)
                empty)
               empty
               1))
(check-expect (pause-switch (make-world ; case where world becomes unpaused
                             (make-tetra
                              (make-posn 0 0)
                              empty)
                             empty
                             1))
              (make-world
               (make-tetra
                (make-posn 0 0)
                empty)
               empty
               0))
; ------------------------------------------------------------------------------
; tetra-shift-left : Tetra -> Tetra
; apply block-shift-left to each block in BSet
(define (tetra-shift-left a-tetra)
  (make-tetra
   (make-posn (sub1 (posn-x (tetra-center a-tetra)))
              (posn-y (tetra-center a-tetra)))
   (map block-shift-left (tetra-blocks a-tetra))))

(check-expect (tetra-shift-left (make-tetra
                                  (make-posn 1 2)
                                  (list (make-block 1 2 'green)
                                        (make-block 1 3 'green))))
              (make-tetra
               (make-posn 0 2)
               (list (make-block 0 2 'green)
                     (make-block 0 3 'green))))
         
; block-shift-left : Block -> Block
; shifts block position one unit left
(define (block-shift-left a-block)
  (make-block
   (sub1 (block-x a-block))
   (block-y a-block)
   (block-color a-block)))

(check-expect (block-shift-left (make-block 1 2 'green))
              (make-block 0 2 'green))
; ------------------------------------------------------------------------------
; tetra-shift-right : Tetra -> Tetra
; apply block-shift-right to each block in BSet
(define (tetra-shift-right a-tetra)
  (make-tetra
   (make-posn (add1 (posn-x (tetra-center a-tetra)))
              (posn-y (tetra-center a-tetra)))
   (map block-shift-right (tetra-blocks a-tetra))))

(check-expect (tetra-shift-right (make-tetra
                                  (make-posn 1 2)
                                  (list (make-block 1 2 'green)
                                        (make-block 1 3 'green))))
              (make-tetra
               (make-posn 2 2)
               (list (make-block 2 2 'green)
                     (make-block 2 3 'green))))

; block-shift-right : Block -> Block
; shifts block position one unit right
(define (block-shift-right a-block)
  (make-block
   (add1 (block-x a-block))
   (block-y a-block)
   (block-color a-block)))

(check-expect (block-shift-right (make-block 1 2 'green))
              (make-block 2 2 'green))
; ------------------------------------------------------------------------------
; rotate-tetra-cw : Tetra -> Tetra
; rotate the tetra 90 degrees clockwise around its center
(check-expect (rotate-tetra-cw TET-J-1) TET-J-2) ; turn 90 cw to position 2
(check-expect (rotate-tetra-cw TET-J-2) TET-J-3) ; turn from pos. 2 to pos. 3
(check-expect (rotate-tetra-cw TET-J-3) TET-J-4) ; turn from pos. 3 to pos. 4
(check-expect (rotate-tetra-cw TET-J-4) TET-J-1) ; turn 90 cw back to 1st pos.

(define (rotate-tetra-cw a-tetra)
  (make-tetra
   (tetra-center a-tetra)
   (rotate-blocks-cw a-tetra)))

; rotate-blocks-cw : Tetra -> BSet
; rotate all blocks in Tetra 90 degrees clockwise relative to the tetra center.
;; a rotated tetra bset should match the (tetra-blocks) of the rotated tetra
(check-expect (rotate-blocks-cw TET-J-1) (tetra-blocks TET-J-2)) 
(check-expect (rotate-blocks-cw TET-J-2) (tetra-blocks TET-J-3))
(check-expect (rotate-blocks-cw TET-J-3) (tetra-blocks TET-J-4))
(check-expect (rotate-blocks-cw TET-J-4) (tetra-blocks TET-J-1))

(define (rotate-blocks-cw a-tetra)
  (cond [(empty? (tetra-blocks a-tetra)) empty]
        [(cons? (tetra-blocks a-tetra))
         (cons (rotate-cw
                (first (tetra-blocks a-tetra))
                a-tetra)
               (rotate-blocks-cw
                (make-tetra
                 (tetra-center a-tetra)
                 (rest (tetra-blocks a-tetra)))))]))

;; rotate-cw : Block Tetra -> Block
;; Rotate a block 90 clockwise relative to a tetra center
(check-expect (rotate-cw (make-block 5 4 'cyan) TET-J-1)
              (make-block 6 3 'cyan)) ; rotate leftmost block around center
(check-expect (rotate-cw (make-block 6 3 'cyan) TET-J-2)
              (make-block 7 4 'cyan)) ; rotates same block to position 3
(check-expect (rotate-cw (make-block 7 4 'cyan) TET-J-3)
              (make-block 6 5 'cyan)) ; rotates same block to position 4
(check-expect (rotate-cw (make-block 6 5 'cyan) TET-J-4)
              (make-block 5 4 'cyan)) ; rotate the block back to position 1

(define (rotate-cw a-block a-tetra)
  (make-block (turn-x-cw a-block a-tetra)
              (turn-y-cw a-block a-tetra)
              (block-color a-block)))
; ------------------------------------------------------------------------------
; rotate-tetra-ccw : Tetra -> Tetra
; rotate the tetra counter-clockwise
(check-expect (rotate-tetra-ccw TET-J-1) TET-J-4) ; turn 90 ccw to position 4
(check-expect (rotate-tetra-ccw TET-J-2) TET-J-1) ; turn from pos. 2 to pos. 1
(check-expect (rotate-tetra-ccw TET-J-3) TET-J-2) ; turn from pos. 3 to pos. 2
(check-expect (rotate-tetra-ccw TET-J-4) TET-J-3) ; turn from pos. 4 to pos. 3

(define (rotate-tetra-ccw a-tetra)
  (make-tetra
   (tetra-center a-tetra)
   (rotate-blocks-ccw a-tetra)))

; rotate-blocks-ccw : Tetra -> BSet
; rotate all blocks in Tetra-blocks counter-clockwise
(check-expect (rotate-blocks-ccw TET-J-1) (tetra-blocks TET-J-4)) 
(check-expect (rotate-blocks-ccw TET-J-2) (tetra-blocks TET-J-1))
(check-expect (rotate-blocks-ccw TET-J-3) (tetra-blocks TET-J-2))
(check-expect (rotate-blocks-ccw TET-J-4) (tetra-blocks TET-J-3))

(define (rotate-blocks-ccw a-tetra)
  (cond [(empty? (tetra-blocks a-tetra)) empty]
        [(cons? (tetra-blocks a-tetra))
         (cons (rotate-ccw
                (first (tetra-blocks a-tetra))
                a-tetra)
               (rotate-blocks-ccw
                (make-tetra
                 (tetra-center a-tetra)
                 (rest (tetra-blocks a-tetra)))))]))

;; rotate-ccw : Block Tetra -> Block
;; Rotate the block 90 counter-clockwise around the center of the tetra.
(check-expect (rotate-ccw (make-block 6 3 'cyan) TET-J-1)
              (make-block 5 4 'cyan)) ; rotate leftmost block ccw around center
(check-expect (rotate-ccw (make-block 7 4 'cyan) TET-J-2)
              (make-block 6 3 'cyan)) ; rotates pos 2 block back to position 1
(check-expect (rotate-ccw (make-block 6 5 'cyan) TET-J-3)
              (make-block 7 4 'cyan)) ; rotates pos 3 block back to position 2
(check-expect (rotate-ccw (make-block 5 4 'cyan) TET-J-4)
              (make-block 6 5 'cyan)) ; rotates the block back to position 3

(define (rotate-ccw a-block a-tetra)
  (make-block (turn-x-ccw a-block a-tetra)
              (turn-y-ccw a-block a-tetra)
              (block-color a-block)))
; ------------------------------------------------------------------------------
; turn-x-cw : Block Tetra -> Block
; returns x position of new block after rotating the block clockwise around the
; center of the tetra
(check-expect (turn-x-cw (make-block 6 2 'yellow) TET-J-1) 8) ; 1st cw rotation
(check-expect (turn-x-cw (make-block 8 4 'yellow) TET-J-2) 6) ; 2nd cw rotation
(check-expect (turn-x-cw (make-block 6 6 'yellow) TET-J-3) 4) ; 3rd cw rotation
(check-expect (turn-x-cw (make-block 4 4 'yellow) TET-J-4) 6) ; 4th cw rotation                     
                         
(define (turn-x-cw a-block a-tetra)
  (+ (posn-x (tetra-center a-tetra))
     (- (posn-y (tetra-center a-tetra))
        (block-y a-block))))

; turn-y-cw : Block Tetra -> Block
; returns y-coordinate of block after rotating clockwise
(check-expect (turn-y-cw (make-block 6 2 'green) TET-J-1) 4) ; 1st cw rotation
(check-expect (turn-y-cw (make-block 8 4 'green) TET-J-2) 6) ; 2nd cw rotation
(check-expect (turn-y-cw (make-block 6 6 'green) TET-J-3) 4) ; 3rd cw rotation
(check-expect (turn-y-cw (make-block 4 4 'green) TET-J-4) 2) ; 4th cw rotation

(define (turn-y-cw a-block a-tetra)
  (+ (posn-y (tetra-center a-tetra))
     (- (block-x a-block)
        (posn-x (tetra-center a-tetra)))))

; turn-x-ccw : Block Tetra -> Block
; returns x-coordinate of block after rotating counter-clockwise
(check-expect (turn-x-ccw (make-block 6 2 'yellow) TET-J-1) 4) ; 1st ccw rotation
(check-expect (turn-x-ccw (make-block 8 4 'yellow) TET-J-2) 6) ; 2nd ccw rotation
(check-expect (turn-x-ccw (make-block 6 6 'yellow) TET-J-3) 8) ; 3rd ccw rotation
(check-expect (turn-x-ccw (make-block 4 4 'yellow) TET-J-4) 6) ; 4th ccw rotation

(define (turn-x-ccw a-block a-tetra)
  (+ (posn-x (tetra-center a-tetra))
     (- (block-y a-block)
        (posn-y (tetra-center a-tetra)))))

; turn-y-ccw : Block Tetra -> Block
; returns y-coordinate of block after rotating clockwise
(check-expect (turn-y-ccw (make-block 6 2 'green) TET-J-1) 4) ; 1st ccw rotation
(check-expect (turn-y-ccw (make-block 8 4 'green) TET-J-2) 2) ; 2nd ccw rotation
(check-expect (turn-y-ccw (make-block 6 6 'green) TET-J-3) 4) ; 3rd ccw rotation
(check-expect (turn-y-ccw (make-block 4 4 'green) TET-J-4) 6) ; 4th ccw rotation

(define (turn-y-ccw a-block a-tetra)
  (+ (posn-y (tetra-center a-tetra))
     (- (posn-x (tetra-center a-tetra))
        (block-x a-block))))
; ------------------------------------------------------------------------------
; tetra-legal? : World KeyEvent -> Boolean
; returns true if all of the blocks in a tetra will be legal after
; the desired movement is made
(define (tetra-legal? w a-key)
  (blocks-legal? (world-tetra w) (world-pile w) a-key))

(check-expect (tetra-legal? (make-world ; case where true
                             (make-tetra
                              (make-posn 2 2)
                              (list (make-block 2 2 'red)
                                    (make-block 2 3 'red)))
                             empty
                             0)
                            "left")
              true)
(check-expect (tetra-legal? (make-world ; case where false
                             (make-tetra
                              (make-posn 2 2)
                              (list (make-block 2 2 'red)
                                    (make-block 2 3 'red)))
                             (list (make-block 1 2 'red))
                             0)
                            "left")
              false)

; blocks-legal? : Tetra BSet KeyEvent-> Boolean
(define (blocks-legal? a-tetra a-pile a-key)
  (cond [(empty? (tetra-blocks a-tetra)) true]
        [(cons? (tetra-blocks a-tetra))
         (and (block-legal?
               (first (tetra-blocks a-tetra)) a-tetra a-pile a-key)
              (blocks-legal?
               (make-tetra (tetra-center a-tetra)
                           (rest (tetra-blocks a-tetra)))
               a-pile a-key))]))

(check-expect (blocks-legal? (make-tetra ; case where true
                              (make-posn 2 2)
                              (list (make-block 2 2 'red)
                                    (make-block 2 3 'red)))
                             empty
                             "left")
              true)
(check-expect (blocks-legal? (make-tetra ; case where false
                              (make-posn 2 2)
                              (list (make-block 2 2 'red)
                                    (make-block 2 3 'red)))
                             (list (make-block 1 2 'red))
                             "left")
              false)

; block-legal? : Block Tetra BSet KeyEvent -> Boolean
; check if, after the desired movement is made, the selected block is legal
(define (block-legal? a-block a-tetra a-pile a-key)
    (and (central-enough? a-block a-key)
         (turn-x-right-enough? a-block a-tetra a-key)
         (turn-x-left-enough? a-block a-tetra a-key)
         (not (will-overlap? a-block a-tetra a-pile a-key))
         (not (will-submerge? a-block a-tetra a-key))))

(check-expect ; case: block makes a legal move
 (block-legal?
  (make-block 1 10 'purple)
  (make-tetra
   (make-posn 1 10)
   (list (make-block 1 10 'purple)))
  empty
  "right") true)
(check-expect ; case: block about to shift right into pile
 (block-legal?
  (make-block 4 5 'red)
  (make-tetra
   (make-posn 4 5)
   (list (make-block 4 5 'red)))
  (list (make-block 5 5 'blue))
  "right") false)
(check-expect ; case: block about to shift left into pile
 (block-legal?
  (make-block 2 10 'green)
  (make-tetra
   (make-posn 2 10)
   (list (make-block 2 10 'green)))
   (list (make-block 1 10 'yellow))
   "left") false)
(check-expect ; case: block about to submerge below floor on cw turn
 (block-legal?
  (make-block 5 20 'yellow)
  (make-tetra
   (make-posn 5 25)
   (list (make-block 5 25 'yellow)))
  empty
  "s") false)
(check-expect ; case: block about to submerge below floor on ccw turn
 (block-legal?
  (make-block 5 20 'yellow)
  (make-tetra
   (make-posn 5 25)
   (list (make-block 5 25 'yellow)))
   empty
   "a") false)
(check-expect ; case: block about to overlap with block on cw turn
 (block-legal?
  (make-block 6 19 'blue)
  (make-tetra
   (make-posn 6 18)
   empty)
  (list (make-block 5 17 'yellow)
        (make-block 5 18 'yellow)
        (make-block 5 19 'yellow)
        (make-block 5 20 'yellow))
   "s") false)
(check-expect ; case: block about to overlap with block on ccw turn
 (block-legal?
  (make-block 6 17 'blue)
  (make-tetra
   (make-posn 6 18)
   empty)
  (list (make-block 5 17 'yellow)
        (make-block 5 18 'yellow)
        (make-block 5 19 'yellow)
        (make-block 5 20 'yellow))
  "a") false)
(check-expect ; case: block about to go off the screen to the right
 (block-legal?
  (make-block 10 10 'blue)
  (make-tetra
   (make-posn 10 10)
   (list (make-block 10 10 'blue)))
   empty
  "right") false)
(check-expect ; case: block about to go off the screen to the left
 (block-legal?
  (make-block 1 10 'purple)
  (make-tetra
   (make-posn 1 10)
   (list (make-block 1 10 'purple)))
   empty
  "left") false)

; ------------------------------------------------------------------------------
; central-enough? : Block KeyEvent -> Boolean
; will the block be right enough and left enough after the desired shift?
(define (central-enough? a-block a-key)
  (cond [(key=? a-key "left")
         (right-enough? a-block)]
        [(key=? a-key "right")
         (left-enough? a-block)]
        [else true]))

(check-expect ; case : block would go off screen if "left" were pressed
 (central-enough? (make-block 1 10 'red) "left") false)
(check-expect ; case : block would go off screen if "right" were pressed
 (central-enough? (make-block 10 10 'red) "right") false)
(check-expect ; case : block would still be on screen if "right" were pressed
 (central-enough? (make-block 5 10 'red) "right") true)
(check-expect ; case : block would still be on screen if "left" were pressed
 (central-enough? (make-block 5 10 'red) "left") true)

; right-enough? : Block -> Boolean
; will the block be at or to the right of the leftmost border after a shift
; to the left?
(define (right-enough? a-block)
  (<= 1 (block-x (block-shift-left a-block))))

(check-expect ; case : block is at leftmost border and would go off screen
 (right-enough? (make-block 1 10 'red)) false)
(check-expect ; case : block will not go off screen after pressing "left"
 (right-enough? (make-block 2 10 'red)) true)

; left-enough? : Block -> Boolean
; will the block be at or to the left of the rightmost border after a shift
; to the right?
(define (left-enough? a-block)
  (>= 10 (block-x (block-shift-right a-block))))

(check-expect ; case : block is at rightmost border and would go off screen
 (left-enough? (make-block 10 10 'red)) false)
(check-expect ; case : block will not go off screen after pressing "right"
 (left-enough? (make-block 9 10 'red)) true)

; turn-x-right-enough? : Block Tetra KeyEvent -> Boolean
; will the block be at or to the right of the leftmost border?
(define (turn-x-right-enough? a-block a-tetra a-key)
  (<= 1 (cond [(key=? a-key "s")
               (turn-x-cw a-block a-tetra)]
              [(key=? a-key "a")
               (turn-x-ccw a-block a-tetra)]
              [else (block-x a-block)])))

(check-expect ; case : a would-be-invalid ccw rotation
 (turn-x-right-enough?
  (make-block 1 18 'red)
  (make-tetra
   (make-posn 1 19)
   (list
    (make-block 1 19 'red)
    (make-block 1 18 'red)
    (make-block 2 20 'red)
    (make-block 2 19 'red)))
  "a")
 false)
(check-expect ; case : a would-be-invalid cw rotation
 (turn-x-left-enough?
  (make-block 9 17 'red)
  (make-tetra
   (make-posn 10 18)
   empty)
  "s")
 false)
(check-expect ; case : a valid cw rotation
 (turn-x-right-enough?
  (make-block 1 19 'red)
  (make-tetra
   (make-posn 1 19)
   (list
    (make-block 1 19 'red)
    (make-block 1 18 'red)
    (make-block 2 20 'red)
    (make-block 2 19 'red)))
  "s")
 true)
(check-expect ; case : valid ccw rotation
 (turn-x-right-enough?
  (make-block 2 20 'red)
  (make-tetra
   (make-posn 2 19)
   (list
    (make-block 1 19 'red)
    (make-block 1 18 'red)
    (make-block 2 20 'red)
    (make-block 2 19 'red)))
  "s")
 true)


; turn-x-left-enough? : Block Tetra KeyEvent -> Boolean
; will the block be at or to the left of the rightmost border?
(define (turn-x-left-enough? a-block a-tetra a-key)
  (>= 10 (cond [(key=? a-key "s")
                (turn-x-cw a-block a-tetra)]
               [(key=? a-key "a")
                (turn-x-ccw a-block a-tetra)]
               [else (block-x a-block)])))

(check-expect ; case : invalid ccw rotation
 (turn-x-left-enough?
  (make-block 10 19 'red)
  (make-tetra
   (make-posn 10 18)
   empty)
  "a")
 false)
(check-expect ; case : invalid cw rotation
 (turn-x-right-enough?
  (make-block 2 20 'red)
  (make-tetra
   (make-posn 1 19)
   (list
    (make-block 1 19 'red)
    (make-block 1 18 'red)
    (make-block 2 20 'red)
    (make-block 2 19 'red)))
  "s")
 false)
(check-expect ; case : valid cw rotation
 (turn-x-left-enough?
  (make-block 10 19 'red)
  (make-tetra
   (make-posn 10 18)
   empty)
  "s")
 true)
(check-expect ; case : valid ccw rotation
 (turn-x-left-enough?
  (make-block 9 17 'red)
  (make-tetra
   (make-posn 10 18)
   empty)
  "a")
 true)

; ------------------------------------------------------------------------------
; tetra-hit-bottom? : Tetra -> Boolean
; is the block currently on the bottom of the scene

(define (tetra-hit-bottom? a-tetra)
  (cond [(empty? (tetra-blocks a-tetra))
         false]
        [else (or (block-hit-bottom?
                   (first (tetra-blocks a-tetra)))
                  (tetra-hit-bottom?
                   (make-tetra (tetra-center a-tetra)
                               (rest (tetra-blocks a-tetra)))))]))

(check-expect
 (tetra-hit-bottom? ; case: tetra is not at the bottom of the scene
  (make-tetra (make-posn 10 10)
              (list (make-block 10 8 'blue)
                    (make-block 10 9 'blue)
                    (make-block 10 10 'blue)
                    (make-block 10 11 'blue)))) false)
(check-expect
 (tetra-hit-bottom? ; case: tetra is at the bottom of the scene
  (make-tetra (make-posn 10 18)
              (list (make-block 10 17 'blue)
                    (make-block 10 18 'blue)
                    (make-block 10 19 'blue)
                    (make-block 10 20 'blue)))) true)

; block-hit-bottom? : Block -> Boolean
; is the block currently on the bottom of the scene
(define (block-hit-bottom? a-block)
  (= (block-y a-block) 20))

(check-expect (block-hit-bottom? (make-block 10 20 'blue)) true) ; at bottom
(check-expect (block-hit-bottom? (make-block 10 10 'blue)) false) ; mid-screen
; ------------------------------------------------------------------------------
; will-submerge? : Block Tetra KeyEvent -> Boolean
; will the block be underground after the desired rotation?
(define (will-submerge? a-block a-tetra a-key)
  (cond [(key=? a-key "s")
         (> (turn-y-cw a-block a-tetra) 20)]
        [(key=? a-key "a")
         (> (turn-y-ccw a-block a-tetra) 20)]
        [else false]))

(check-expect ; case: cw motion that will not submerge  
 (will-submerge?
  (make-block 1 20 'green)
  (make-tetra
   (make-posn 2 20)
   empty)
  "s")
 false)
(check-expect ; case: ccw motion that will not submerge  
 (will-submerge?
  (make-block 4 20 'green)
  (make-tetra
   (make-posn 2 20)
   empty)
  "a")
 false)
(check-expect ; case: ccw motion that will submerge
 (will-submerge?
  (make-block 1 20 'green)
  (make-tetra
   (make-posn 2 20)
   empty)
  "a")
 true)
(check-expect ; case: cw motion that will submerge
 (will-submerge?
  (make-block 4 20 'green)
  (make-tetra
   (make-posn 2 20)
   empty)
  "s")
 true)

; ------------------------------------------------------------------------------
; will-overlap? : Block Tetra Bset KeyEvent-> Boolean
; would a turn cause the block to overlap with a block in the pile?
(define (will-overlap? a-block a-tetra a-pile a-key)
  (cond [(empty? a-pile) false]
        [(key=? a-key "s") (will-s-overlap? a-block a-tetra a-pile)]
        [(key=? a-key "a") (will-a-overlap? a-block a-tetra a-pile)]
        [(key=? a-key "left") (will-left-overlap? a-block a-tetra a-pile)]
        [(key=? a-key "right") (will-right-overlap? a-block a-tetra a-pile)]
        [else false]))

(check-expect (will-overlap? ; case: bset is empty
               (make-block 4 5 'black) 
               (make-tetra
                (make-posn 1 2)
                empty)
               empty
               "s")
              false)
(check-expect (will-overlap? ; case: key other than s/a/left/right is pressed
               (make-block 4 5 'black)
               (make-tetra
                (make-posn 1 2)
                (list (make-block 1 1 'white)))
               (list (make-block 2 2 'red))
               "z")
              false)

; for other cases, see check-expects for helper functions below, which explore
; each possibile case that could be fed into will-overlap?

(define (will-s-overlap? a-block a-tetra a-pile)
  (cond [(empty? a-pile) false]
        [else
         (or (and (= (turn-y-cw a-block a-tetra)
                     (block-y (first a-pile)))
                  (= (turn-x-cw a-block a-tetra)
                     (block-x (first a-pile))))
             (will-s-overlap? a-block a-tetra (rest a-pile)))]))

(check-expect (will-s-overlap? ; case : will not overlap
               (make-block 5 20 'red)
               (make-tetra
                (make-posn 5 18)
                empty)
               (list (make-block 4 18 'red))) false)
(check-expect (will-s-overlap? ; case : will overlap
               (make-block 5 19 'red)
               (make-tetra
                (make-posn 5 18)
                empty)
               (list (make-block 4 18 'red))) true)

(define (will-a-overlap? a-block a-tetra a-pile)
  (cond [(empty? a-pile) false]
        [else
         (or (and (= (turn-y-ccw a-block a-tetra)
                     (block-y (first a-pile)))
                  (= (turn-x-ccw a-block a-tetra)
                     (block-x (first a-pile))))
             (will-a-overlap? a-block a-tetra (rest a-pile)))]))

(check-expect (will-a-overlap? ; case : will not overlap
               (make-block 5 20 'red)
               (make-tetra
                (make-posn 5 18)
                empty)
               (list (make-block 6 18 'red))) false)
(check-expect (will-a-overlap? ; case : will overlap
               (make-block 5 19 'red)
               (make-tetra
                (make-posn 5 18)
                empty)
               (list (make-block 6 18 'red))) true)

(define (will-left-overlap? a-block a-tetra a-pile)
  (cond [(empty? a-pile) false]
        [else
         (or (and (= (sub1 (block-x a-block))
                     (block-x (first a-pile)))
                  (= (block-y a-block)
                     (block-y (first a-pile))))
             (will-left-overlap? a-block a-tetra (rest a-pile)))]))

(check-expect (will-left-overlap? ; case : will not overlap
               (make-block 5 18 'red)
               (make-tetra
                (make-posn 5 18)
                empty)
               (list (make-block 6 18 'red))) false)
(check-expect (will-left-overlap? ; case : will overlap
               (make-block 5 18 'red)
               (make-tetra
                (make-posn 5 18)
                empty)
               (list (make-block 4 18 'red))) true)

(define (will-right-overlap? a-block a-tetra a-pile)
  (cond [(empty? a-pile) false]
        [else
         (or (and (= (add1 (block-x a-block))
                     (block-x (first a-pile)))
                  (= (block-y a-block)
                     (block-y (first a-pile))))
             (will-right-overlap? a-block a-tetra (rest a-pile)))]))

(check-expect (will-right-overlap? ; case : will overlap
               (make-block 5 18 'red)
               (make-tetra
                (make-posn 5 18)
                empty)
               (list (make-block 6 18 'red))) true)

(check-expect (will-right-overlap? ; case : will not overlap
               (make-block 5 18 'red)
               (make-tetra
                (make-posn 5 18)
                empty)
               (list (make-block 4 18 'red))) false)
                      
         
; ------------------------------------------------------------------------------
; tetra-hit-pile? : Tetra BSet -> Boolean
; are any of the blocks in the tetra currently on top of the pile?
(define (tetra-hit-pile? a-tetra a-pile)
  (cond [(or (empty? a-pile) (empty? (tetra-blocks a-tetra))) false]
        [(cons? a-pile)
         (or (block-hit-pile? (first (tetra-blocks a-tetra)) a-pile)
             (tetra-hit-pile?
              (make-tetra (tetra-center a-tetra)
                          (rest (tetra-blocks a-tetra))) a-pile))]))

(check-expect ; case: tetra has hit the pile
 (tetra-hit-pile?
  (make-tetra
   (make-posn 2 19)
   (list (make-block 1 19 'red)
         (make-block 2 19 'red)
         (make-block 3 19 'red)
         (make-block 4 19 'red)))
  (list (make-block 1 20 'red)
        (make-block 2 20 'red)
        (make-block 3 20 'red)
        (make-block 4 20 'red)))
 true)
(check-expect ; case: tetra has not hit the pile
 (tetra-hit-pile?
  (make-tetra
   (make-posn 2 19)
   (list (make-block 1 18 'red)
         (make-block 2 18 'red)
         (make-block 3 18 'red)
         (make-block 4 18 'red)))
  (list (make-block 1 20 'red)
        (make-block 2 20 'red)
        (make-block 3 20 'red)
        (make-block 4 20 'red)))
 false)

; block-hit-pile? : Block BSet -> Boolean
; is the block currently on top of the pile?
(define (block-hit-pile? a-block a-pile)
  (cond [(empty? a-pile) false]
        [else
         (or (and (= (add1 (block-y a-block))
                     (block-y (first a-pile)))
                  (= (block-x a-block)
                     (block-x (first a-pile))))
             (block-hit-pile? a-block (rest a-pile)))]))

(check-expect ; case : block has hit the pile
 (block-hit-pile?
  (make-block 1 19 'red)
  (list (make-block 1 20 'red)
        (make-block 2 20 'red)
        (make-block 3 20 'red)
        (make-block 4 20 'red))) true)
(check-expect ; case : block has not hit the pile
 (block-hit-pile?
  (make-block 1 18 'red)
  (list (make-block 1 20 'red)
        (make-block 2 20 'red)
        (make-block 3 20 'red)
        (make-block 4 20 'red))) false)

; --------------------------------
; bset-projection : BSet BSet -> BSet
; creates a new bset at the highest legal position on top of the pile
; & directly below the bset
(define (bset-projection bset a-pile)
  (map (lambda (i)
         (make-block (block-x i)
                     (+ (block-y i) (blocks-projection-y bset a-pile))
                     (block-color i)))
       bset))

(check-expect
 (bset-projection
  (list (make-block 4 16 'blue)
        (make-block 5 16 'blue)
        (make-block 6 16 'blue)
        (make-block 7 16 'blue))
  (list (make-block 4 19 'darkorange)
        (make-block 5 19 'darkorange)
        (make-block 6 19 'darkorange)
        (make-block 5 20 'darkorange)
        (make-block 2 19 'pink)
        (make-block 3 19 'pink)
        (make-block 3 20 'pink)
        (make-block 4 20 'pink)
        (make-block 9 20 'lime)
        (make-block 9 19 'lime)
        (make-block 10 20 'lime)
        (make-block 10 19 'lime)))
 (list (make-block 4 18 'blue)
       (make-block 5 18 'blue)
       (make-block 6 18 'blue)
       (make-block 7 18 'blue)))
  
(check-expect
 (bset-projection
  (list (make-block 1 1 'blue)
        (make-block 2 1 'blue)
        (make-block 2 2 'blue)
        (make-block 3 2 'blue))
  (list (make-block 1 18 'blue)
        (make-block 2 19 'blue)
        (make-block 3 19 'blue)
        (make-block 4 19 'blue)))
 (list (make-block 1 17 'blue)
       (make-block 2 17 'blue)
       (make-block 2 18 'blue)
       (make-block 3 18 'blue)))
(check-expect
 (bset-projection
  (list (make-block 1 1 'blue)
        (make-block 1 2 'blue)
        (make-block 2 2 'blue)
        (make-block 2 3 'blue))
  empty)
 (list (make-block 1 18 'blue)
       (make-block 1 19 'blue)
       (make-block 2 19 'blue)
       (make-block 2 20 'blue)))

; blocks-projection-y : BSet BSet -> Number
; gives the number of grid units the bset should be projected downward
(define (blocks-projection-y bset a-pile)
  (cond [(empty? bset) 21]
        [(cons? bset)
         (min (block-projection-y (first bset) a-pile)
              (blocks-projection-y (rest bset) a-pile))]))

(check-expect
 (blocks-projection-y
  (list (make-block 1 1 'blue)
        (make-block 2 1 'blue)
        (make-block 2 2 'blue)
        (make-block 3 2 'blue))
  (list (make-block 1 20 'blue)
        (make-block 2 20 'blue)
        (make-block 3 20 'blue)
        (make-block 4 20 'blue)))
 17)
(check-expect
 (blocks-projection-y
  (list (make-block 1 1 'blue)
        (make-block 1 2 'blue)
        (make-block 2 2 'blue)
        (make-block 2 3 'blue))
  empty)
 17)

; block-projection-y : Block BSet -> Number
; returns the vertical distance between:
; the grid coordinate one above the BSet element directly below the input block
; the input block itself
(define (block-projection-y a-block a-pile)
  (cond [(empty? a-pile)
         (- 20 (block-y a-block))]
        [(= (block-x a-block)
            (block-x (first a-pile)))
         (min (- (sub1 (block-y (first a-pile))) (block-y a-block))
              (block-projection-y a-block (rest a-pile)))]
        [else (block-projection-y a-block (rest a-pile))]))

(check-expect ; case : closest pile element below block is at 20
 (block-projection-y
  (make-block 1 1 'blue)
  (list (make-block 1 20 'blue)))
 18)
(check-expect
 (block-projection-y
  (make-block 2 2 'blue)
  (list (make-block 2 20 'blue)))
 17)
(check-expect ; case : closest pile element below block is at 15
 (block-projection-y
  (make-block 1 1 'blue)
  (list (make-block 1 15 'blue)))
 13)
(check-expect ; case : no pile element below block
 (block-projection-y
  (make-block 3 5 'blue)
  (list (make-block 1 15 'blue)))
 15)
(check-expect ; case : empty pile
 (block-projection-y
  (make-block 2 0 'blue)
  empty)
 20)

; ghost-blocks : BSet -> BSet
; takes a BSet and returns a new BSet of white blocks
(define (ghost-blocks bset)
  (map (lambda (i)
         (make-block (block-x i)
                     (block-y i)
                     'white))
       bset))

(check-expect (ghost-blocks (list (make-block 1 2 'blue)
                                  (make-block 2 3 'red)))
              (list (make-block 1 2 'white)
                    (make-block 2 3 'white)))
; ================================ STOP-WHEN ===================================     

; game-over? : World -> Boolean
; game is over when any element of the pile is off-screen
(define (game-over? w)
  (cond [(empty? (world-pile w)) false]
        [else
         (or (block-off-screen? (first (world-pile w)))
             (game-over? (make-world
                          (world-tetra w)
                          (rest (world-pile w))
                          (world-paused? w))))]))

(check-expect ; case : tetra is neither hitting pile nor off screen
 (game-over?
  (make-world
   (make-tetra
    (make-posn 6 4)
    (list (make-block 5 4 'red)
          (make-block 5 3 'red)
          (make-block 6 5 'red)
          (make-block 6 4 'red)))
   empty 0))
 false)
(check-expect ; case : tetra has hit the pile but is not off screen
 (game-over?
  (make-world
   (make-tetra
    (make-posn 6 18)
    (list (make-block 6 18 'cyan)
          (make-block 6 17 'cyan)
          (make-block 6 16 'cyan)
          (make-block 5 18 'cyan)))
   (list (make-block 5 20 'lime)
         (make-block 5 19 'lime)
         (make-block 6 20 'lime)
         (make-block 6 19 'lime)) 0))
 false)
(check-expect ; case : tetra is off-screen but is not hitting the pile
 (game-over?
(make-world
 (make-tetra
  (make-posn 6 -2)
  (list (make-block 5 -2 'red)
        (make-block 5 -3 'red)
        (make-block 6 -1 'red)
        (make-block 6 -2 'red)))
 (list (make-block 4 20 'blue)
       (make-block 5 20 'blue)
       (make-block 6 20 'blue)
       (make-block 7 20 'blue)) 0))
 false)
(check-expect ; case : Game Over (tetra is both hitting pile AND off screen)
 (game-over?
  (make-world
   (make-tetra
    (make-posn 6 -1)
    (list
     (make-block 6 -1 'cyan)
     (make-block 6 -2 'cyan)
     (make-block 6 -3 'cyan)
     (make-block 5 -1 'cyan)))
   (list
    (make-block 5 2 'purple)
    (make-block 5 1 'purple)
    (make-block 5 0 'purple)
    (make-block 6 2 'purple)
    (make-block 4 3 'blue)
    (make-block 5 3 'blue)
    (make-block 6 3 'blue)
    (make-block 7 3 'blue)
    (make-block 5 5 'red)
    (make-block 5 4 'red)
    (make-block 6 6 'red)
    (make-block 6 5 'red)
    (make-block 5 7 'red)
    (make-block 5 6 'red)
    (make-block 6 8 'red)
    (make-block 6 7 'red)
    (make-block 5 9 'red)
    (make-block 5 8 'red)
    (make-block 6 10 'red)
    (make-block 6 9 'red)
    (make-block 5 12 'purple)
    (make-block 5 11 'purple)
    (make-block 5 10 'purple)
    (make-block 6 12 'purple)
    (make-block 5 15 'darkorange)
    (make-block 5 14 'darkorange)
    (make-block 5 13 'darkorange)
    (make-block 6 14 'darkorange)
    (make-block 5 17 'pink)
    (make-block 5 16 'pink)
    (make-block 6 16 'pink)
    (make-block 6 15 'pink)
    (make-block 5 19 'red)
    (make-block 5 18 'red)
    (make-block 6 20 'red)
    (make-block 6 19 'red)) 0))
  true)
; ------------------------------------------------------------------------------

; block-off-screen? : Block -> Boolean
; is the block off-screen?
(define (block-off-screen? a-block)
  (> 1 (block-y a-block)))

(check-expect ; block is off-screen
 (block-off-screen? (make-block 5 0 'blue))
 true)

(check-expect ; block is on screen
 (block-off-screen? (make-block 5 5 'green))
 false)

; ------------------------------------------------------------------------------

; count-points : World -> Number
; count the number of blocks within the scene
(define (count-points w)
  (cond [(empty? (world-pile w)) 0]
        [(and (block? (first (world-pile w)))
              (not (block-off-screen? (first (world-pile w)))))
         (add1 (count-points
                (make-world (world-tetra w)
                            (rest (world-pile w))
                            (world-paused? w))))]
        [else (count-points
               (make-world (world-tetra w)
                           (rest (world-pile w))
                           (world-paused? w)))]))

(check-expect ; case : 0 blocks in the pile
 (count-points
  (make-world
   (make-tetra
    (make-posn 6 4)
    (list (make-block 5 4 'red)
          (make-block 5 3 'red)
          (make-block 6 5 'red)
          (make-block 6 4 'red)))
   empty 0))
 0)

(check-expect ; case : 36 blocks in pile, one of which is off-screen
 (count-points
  (make-world
   (make-tetra
    (make-posn 6 -1)
    (list
     (make-block 6 -1 'cyan)
     (make-block 6 -2 'cyan)
     (make-block 6 -3 'cyan)
     (make-block 5 -1 'cyan)))
   (list
    (make-block 5 2 'purple)
    (make-block 5 1 'purple)
    (make-block 5 0 'purple)
    (make-block 6 2 'purple)
    (make-block 4 3 'blue)
    (make-block 5 3 'blue)
    (make-block 6 3 'blue)
    (make-block 7 3 'blue)
    (make-block 5 5 'red)
    (make-block 5 4 'red)
    (make-block 6 6 'red)
    (make-block 6 5 'red)
    (make-block 5 7 'red)
    (make-block 5 6 'red)
    (make-block 6 8 'red)
    (make-block 6 7 'red)
    (make-block 5 9 'red)
    (make-block 5 8 'red)
    (make-block 6 10 'red)
    (make-block 6 9 'red)
    (make-block 5 12 'purple)
    (make-block 5 11 'purple)
    (make-block 5 10 'purple)
    (make-block 6 12 'purple)
    (make-block 5 15 'darkorange)
    (make-block 5 14 'darkorange)
    (make-block 5 13 'darkorange)
    (make-block 6 14 'darkorange)
    (make-block 5 17 'pink)
    (make-block 5 16 'pink)
    (make-block 6 16 'pink)
    (make-block 6 15 'pink)
    (make-block 5 19 'red)
    (make-block 5 18 'red)
    (make-block 6 20 'red)
    (make-block 6 19 'red)) 0))
 35)

; show-score : World -> Image
; final image in the game, called after the last to-draw; displays the
; message "GAME OVER - YOU SCORED: " X " POINTS", where X is
; the value obtained from count-points
(define (show-score w)
  (place-images
   (list (text "GAME OVER!" 20 'red)
         (text "YOU SCORED:" 16 'black)
         (text (string-append (number->string (count-points w))
                              " POINTS") 24 'black)
         (overlay (rectangle (grid-convert 7.5)
                             (grid-convert 4)
                             'outline
                             'black)
                  (rectangle (grid-convert 7.5)
                             (grid-convert 4)
                             'solid
                             'white)))
   (list (make-posn (grid-convert 5.5) (grid-convert 9))
         (make-posn (grid-convert 5.5) (grid-convert 10))
         (make-posn (grid-convert 5.5) (grid-convert 11))
         (make-posn (grid-convert 5.5) (grid-convert 10)))
   (draw-world w)))

(check-expect (show-score
               (make-world
                (make-tetra
                 (make-posn 1 1)
                 empty)
                empty 0))
              (place-image
               (text "0" 20 'red)
               (grid-convert 9.5) (grid-convert 1.5)
               (place-images
                (list (text "GAME OVER!" 20 'red)
                      (text "YOU SCORED:" 16 'black)
                      (text (string-append (number->string 0)
                                           " POINTS") 24 'black)
                      (overlay (rectangle (grid-convert 7.5)
                                          (grid-convert 4)
                                          'outline
                                          'black)
                               (rectangle (grid-convert 7.5)
                                          (grid-convert 4)
                                          'solid
                                          'white)))
                (list (make-posn (grid-convert 5.5) (grid-convert 9))
                      (make-posn (grid-convert 5.5) (grid-convert 10))
                      (make-posn (grid-convert 5.5) (grid-convert 11))
                      (make-posn (grid-convert 5.5) (grid-convert 10)))
                MT-CN)))

; ******************************************************************************

; Call tetras on the intial world state
(tetras WORLD0)