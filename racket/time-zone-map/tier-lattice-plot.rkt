#lang racket/base

(require (only-in racket/class send)
         racket/dict
         (only-in racket/draw the-color-database)
         racket/list
         racket/match
         racket/math
         racket/set
         pict
         plot
         "common.rkt"
         "voronoi-plot.rkt")

(define COLUMN_NAMES
  '("Adventurer"
    "SpellBlaster" "NailMaster" "HitlessSlayer"
    "TankHealer" "Platformer"
    "SmoothMover" "FriendMurderer"))

(define ROW_NAMES
  '("S"
    "A"
    "B"
    "C"
    "D"
    "F"))

(define PLACEMENTS
  '(;; S
    ("Compass" (("Adventurer") "S"))
    ("Shaman" (("SpellBlaster") "S"))
    ("QuickSlash" (("SpellBlaster" "NailMaster") "S"))
    ("Strength" (("NailMaster") "S"))
    ("Pride" (("NailMaster") "S"))
    ;; A
    ("GrubSong" (("Adventurer" "TankHealer" "Platformer") "A"))
    ("Wielder" (("Adventurer" "SpellBlaster") "A"))
    ("Glory" (("NailMaster") "A"))
    ("Steady" (("NailMaster") "A"))
    ("Twister" (("SpellBlaster") "A"))
    ("Eater" (("SpellBlaster") "A"))
    ("Heart" (("TankHealer") "A"))
    ("SharpShadow" (("SmoothMover") "A"))
    ;; B
    ("LongNail" (("NailMaster") "B"))
    ("FlukeNest" (("SpellBlaster") "B"))
    ("QuickFocus" (("TankHealer") "B"))
    ("Unn" (("TankHealer") "B"))
    ("Carefree" (("HitlessSlayer" "TankHealer") "B"))
    ("Swarm" (("Adventurer") "B"))
    ("Elegy" (("HitlessSlayer" "Platformer") "B"))
    ("DashMaster" (("SmoothMover") "B"))
    ("WeaverSong" (("FriendMurderer") "B"))
    ;; C
    ("Greed" (("Adventurer") "C"))
    ("Catcher" (("SpellBlaster") "C"))
    ("Fury" (("HitlessSlayer") "C"))
    ("BaldurShell" (("HitlessSlayer" "TankHealer") "C"))
    ("Stalwart" (("TankHealer") "C"))
    ("LifebloodCore" (("TankHealer") "C"))
    ("SprintMaster" (("SmoothMover") "C"))
    ("Spore" (("FriendMurderer") "C"))
    ("Defender" (("FriendMurderer") "C"))
    ;; D
    ("KingSoul" (("SpellBlaster") "D"))
    ("DeepFocus" (("TankHealer") "D") (("Platformer") "A"))
    ("HiveBlood" (("TankHealer") "D") (("Platformer") "C"))
    ("LifebloodHeart" (("TankHealer") "D"))
    ("DreamShield" (("FriendMurderer") "D"))
    ("GrimmChild" (("FriendMurderer") "D"))
    ("Thorns" (("FriendMurderer") "D"))
    ;; F
    ("HeavyBlow" (("NailMaster") "F"))
    ("Joni" (("TankHealer") "F"))
    ("GlowingWomb" (("FriendMurderer") "F"))
    ))

(define X_MIN -1)
(define X_MAX (length COLUMN_NAMES))
(define Y_MIN (- (length ROW_NAMES)))
(define Y_MAX 1)
(define ∆X (- X_MAX X_MIN))
(define ∆Y (- Y_MAX Y_MIN))

(define NAME_COLUMNS
  (add1 (apply max (map string-length COLUMN_NAMES))))

(define FG_COLOR (send the-color-database find-color "black"))

(define DIAMONDS
  (for/list ([rn (in-list ROW_NAMES)]
             #:when #t
             [cn1 (in-list COLUMN_NAMES)]
             [cn2 (in-list (rest COLUMN_NAMES))])
    (cons (list rn cn1 cn2)
          (for*/list ([p (in-list PLACEMENTS)]
                      #:do [(define n (first p))]
                      [e (in-list (rest p))]
                      #:when (and (string-ci=? rn (second e))
                                  (member cn1 (first e) string-ci=?)
                                  (member cn2 (first e) string-ci=?)))
            n))))

(define SQUARES
  (for*/list ([rn (in-list ROW_NAMES)]
              [cn (in-list COLUMN_NAMES)])
    (cons (list rn cn)
          (for*/list ([p (in-list PLACEMENTS)]
                      #:do [(define n (first p))]
                      [e (in-list (rest p))]
                      #:when (string-ci=? rn (second e))
                      [ec (in-list (first e))]
                      #:when (string-ci=? cn ec)
                      #:unless (for*/or ([d (in-list DIAMONDS)])
                                 (and (string-ci=? rn (first (first d)))
                                      (member n (rest d) string-ci=?)
                                      (member cn (rest (first d)) string-ci=?))))
            n))))

;; ---------------------------------------------------------

(define (above* imgs)
  (apply vc-append imgs))

(define (text-lines w max-size color ss)
  (above*
   (for/list ([s (in-list ss)])
     (define size
       (min max-size (font-size/whcl w max-size (+ (string-length s) 2) 1)))
     (cc-superimpose (text s (list color) size)
                     (blank 0 max-size)))))

;; ---------------------------------------------------------

(define (tier-lattice-seeds W_PX H_PX)
  (define PX/X (/ W_PX ∆X))
  (define PX/Y (/ H_PX ∆Y))
  (define FONT_SIZE
    (font-size/whcl PX/X PX/Y NAME_COLUMNS 1))
  (define h0
    (hash (list (vector -1/2 1/2)) (blank)))
  (define hr
    (for/fold ([h h0])
              ([cn (in-list COLUMN_NAMES)]
               [i (in-naturals)])
      (hash-set h (list (vector (+ i 1/2) 1/2))
                (text cn (list FG_COLOR) FONT_SIZE))))
  (define hc
    (for/fold ([h hr])
              ([rn (in-list ROW_NAMES)]
               [i (in-naturals)])
      (hash-set h (list (vector -1/2 (- (+ i 1/2))))
                (text rn (list FG_COLOR) FONT_SIZE))))
  (define hi
    (for/fold ([h hc])
              ([rn (in-list ROW_NAMES)]
               [y (in-naturals)]
               #:when #t
               [cn1 (in-list COLUMN_NAMES)]
               [cn2 (in-list (rest COLUMN_NAMES))]
               [x (in-naturals)]
               #:do [(define ns (dict-ref DIAMONDS (list rn cn1 cn2)))]
               #:unless (empty? ns))
      (hash-set h (list (vector (+ x 1) (- y)))
                (text-lines PX/X FONT_SIZE FG_COLOR ns))))
  (for/fold ([h hi])
            ([cn (in-list COLUMN_NAMES)]
             [x (in-naturals)]
             #:when #t
             [rn (in-list ROW_NAMES)]
             [y (in-naturals)])
    (define ns (dict-ref SQUARES (list rn cn)))
    (hash-set h (list (vector (+ x 1/2) (- (+ y 1/2))))
              (text-lines PX/X FONT_SIZE FG_COLOR ns))))

(define (tier-lattice/point-groups W_PX H_PX point-groups)
  (parameterize ([plot-x-ticks no-ticks]
                 [plot-y-ticks no-ticks]
                 [metric manhattan-distance])
    (plot
     #:width W_PX
     #:height H_PX
     #:x-min X_MIN #:x-max X_MAX
     #:y-min Y_MIN #:y-max Y_MAX
     #:x-label #f
     #:y-label #f
     #:aspect-ratio (/ W_PX H_PX)
     (voronoi-diagram2
      #:x-min X_MIN #:x-max X_MAX
      #:y-min Y_MIN #:y-max Y_MAX
      #:samples 1000
      point-groups))))

(define (tier-lattice W_PX H_PX)
  (tier-lattice/point-groups W_PX H_PX (tier-lattice-seeds W_PX H_PX)))

;; ---------------------------------------------------------

(tier-lattice 900 400)
