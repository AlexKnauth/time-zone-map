#lang racket/base

(require (only-in racket/class send)
         (only-in racket/draw the-color-database)
         racket/list
         racket/match
         racket/math
         pict
         plot
         "common.rkt"
         "voronoi-plot.rkt")

(define MIN_UTC_HOUR_OFFSET -12)
(define MAX_UTC_HOUR_OFFSET +14)

(define UTC_HOUR_W_X 1)
(define DST_HOUR_H_Y 2)

(define LEFT_X (* UTC_HOUR_W_X (- MIN_UTC_HOUR_OFFSET 1/2)))
(define RIGHT_X (* UTC_HOUR_W_X (+ MAX_UTC_HOUR_OFFSET 1/2)))
(define TOP_Y (* 2 DST_HOUR_H_Y))
(define BOT_Y (* -2 DST_HOUR_H_Y))

(define W_X (∆ LEFT_X RIGHT_X))
(define H_Y (∆ BOT_Y TOP_Y))

(define ASPECT_RATIO (/ W_X H_Y))

(define N_DST_1_Y (* 3/4 TOP_Y))
(define N_DST_0_Y (* 1/4 TOP_Y))
(define CENTER_Y 0)
(define S_DST_0_Y (* 1/4 BOT_Y))
(define S_DST_1_Y (* 3/4 BOT_Y))

(define FG_COLOR (send the-color-database find-color "black"))

;; Based on:
;; https://www.timeanddate.com/time/zones/
(define NAMES
  (hash -12                '("AoE" "Baker Island")
        -11                '("NUT" "SST")
        -10                '("CKT" "HST" "TAHT")
        -9                 '("AKST" "GAMT")
        '(N -9 -8)         '("AT" "AKST/AKDT" "Alaska")
        '(S -9.5)          '("UTC-9:30" "MART")
        -8                 '("PST" "Pitcairn")
        '(N -8 -7)         '("PT" "PST/PDT" "Pacific")
        -7                 '("MST" "Arizona" "Sonora" "Yukon")
        '(N -7 -6)         '("MT" "MST/MDT" "Mountain")
        -6                 '("CST" "EAST" "GALT" "Saskatchewan")
        '(N -6 -5)         '("CT" "CST/CDT" "Central")
        '(S -6 -5)         '("EAST/EASST" "Easter Island")
        -5                 '("ACT" "COT" "CST" "EST" "PET" "Acre" "Cuba"
                                   "Quintana Roo" "Cayman Islands")
        '(N -5 -4)         '("ET" "EST/EDT" "Eastern")
        -4                 '("AMT" "AST" "BOT" "CLT" "GYT" "PYT" "VET"
                                   "Amazon")
        '(N -4 -3)         '("AT" "AST/ADT" "Atlantic")
        '(S -4 -3)         '("CLT/CLST" "PYT/PYST" "Chile" "Paraguay")
        -3                 '("ART" "BRT" "CLST" "FKST" "GFT" "PMST" "ROTT" "SRT"
                                   "UYT" "WGT")
        '(N -3.5)          '("UTC-3:30" "NST")
        '(N -3.5 -2.5)     '("NST/NDT" "Newfoundland")
        '(N -3 -2)         '("PMST/PMDT" "WGT/WGST" "Pierre&Miquelon"
                                         "West Greenland")
        -2                 '("FNT" "GST" "South Georgia")
        -1                 '("AZOT" "CVT" "EGT")
        '(N -1 0)          '("AZOT/AZOST" "EGT/EGST" "Azores" "East Greenland")
        0                  '("UTC" "GMT" "WET")
        '(N 0 +1)          '("GMT/BST" "GMT/IST" "WET/WEST" "United Kingdom"
                                       "Western Europe" "Ireland")
        +1                 '("CET" "WAT")
        '(N +1 +2)         '("CET/CEST" "Central Europe")
        +2                 '("CAT" "EET" "IST" "SAST")
        '(N +2 +3)         '("EET/EEST" "IST/IDT" "Eastern Europe" "Isreal")
        +3                 '("AST" "EAT" "MSK" "SYOT" "TRT")
        '(N +3.5)          '("UTC+3:30" "IRST")
        '(N +3.5 +4.5)     '("IRST/IRDT" "Iran")
        +4                 '("AMT" "AZT" "GET" "GST" "MUT" "RET" "SAMT" "SCT"
                                   "Armenia")
        '(N +4.5)          '("UTC+4:30" "AFT")
        +5                 '("AQTT" "MAWT" "MVT" "ORAT" "PKT" "TFT" "TJT"
                                    "TMT" "UZT" "YEKT")
        '(N +5.5)          '("UTC+5:30" "IST" "India")
        '(N +5.75)         '("UTC+5:45" "NPT")
        +6                 '("ALMT" "BST" "BTT" "IOT" "KGT" "OMST" "QYZT"
                                    "VOST" "Bangladesh")
        '(N +6.5)          '("UTC+6:30" "MMT")
        '(S +6.5)          '("UTC+6:30" "CCT")
        +7                 '("CXT" "DAVT" "HOVT" "ICT" "KRAT" "NOVT" "WIB")
        +8                 '("AWST" "BNT" "CAST" "CHOT" "CST" "China" "HKT"
                                    "IRKT"  "MYT" "PHT" "SGT" "ULAT" "WITA")
        '(S +8.75)         '("UTC+8:45" "ACWST")
        +9                 '("JST" "KST" "PWT" "TLT" "WIT" "YAKT")
        '(S +9.5)          '("UTC+9:30" "ACST")
        '(S +9.5 +10.5)    '("ACT" "ACST/ACDT" "Australian" "Central")
        +10                '("AEST" "CHUT" "ChST" "DDUT" "PGT" "VLAT")
        '(S +10 +11)       '("AET" "AEST/AEDT" "Australian" "Eastern")
        '(S +10.5)         '("UTC+10:30" "LHST")
        '(S +10.5 +11)     '("LHST/LHDT" "Lord Howe")
        +11                '("BST" "KOST" "MAGT" "NCT" "NFT" "PONT" "SAKT"
                                   "SBT" "SRET" "VUT" "Bougainville")
        '(S +11 +12)       '("NFT/NFDT" "Norfolk")
        +12                '("ANAT" "FJT" "GILT" "MHT" "NRT" "NZST" "PETT"
                                    "TVT" "WAKT" "WFT")
        '(S +12 +13)       '("NZST/NZDT" "New Zealand")
        '(S +12.75)        '("UTC+12:45" "CHAST")
        '(S +12.75 +13.75) '("CHAST/CHADT" "Chatham Islands")
        +13                '("PHOT" "TKT" "TOT" "WST")
        +14                '("LINT")))

(define NUMBER_COLUMNS 5)
(define NAME_COLUMNS 14)
(define NAME_LINES 9)

(define W_PX 1698)
(define H_PX (exact-ceiling (/ W_PX ASPECT_RATIO)))

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

(define (split-halves lst) (split-at lst (exact-ceiling (* 1/2 (length lst)))))

(define (number->+-string n)
  (if (positive? n)
      (string-append "+" (number->string n))
      (number->string n)))

;; ---------------------------------------------------------

(define (find-room-w k names)
  (match k
    [(or (? integer? ho) `(,_ ,ho))
     (or
      (for*/first ([frac (in-list (list 0.25 0.5 0.75 1))]
                   [off (in-list (list (- ho frac)
                                       (+ ho frac)
                                       (inexact->exact (- ho frac))
                                       (inexact->exact (+ ho frac))))]
                   [k2 (in-list (list off `(N ,off) `(S ,off)))]
                   #:when (hash-has-key? names k2))
        frac)
      1)]
    [`(,ns ,standard ,daylight)
     (define dh (inexact->exact (- daylight standard)))
     (or
      (for*/first ([frac (in-list (list 0.25 0.5 0.75 1))]
                   [off (in-list (list (- standard frac)
                                       (+ standard frac)
                                       (inexact->exact (- standard frac))
                                       (inexact->exact (+ standard frac))))]
                   #:when (hash-has-key? names `(,ns ,off ,(+ off dh))))
        frac)
      1)]))

(define (tz-seeds W_PX H_PX)
  (define HOUR_W_PX (/ W_PX W_X))
  (define HOUR_H_PX (/ H_PX H_Y))
  (define NUMBER_FONT_SIZE
    (font-size/whcl HOUR_W_PX (* 2 HOUR_H_PX) NUMBER_COLUMNS 1))
  (define NAME_FONT_SIZE
    (font-size/whcl HOUR_W_PX HOUR_H_PX NAME_COLUMNS NAME_LINES))

  (define (utc-offset-image w ho lst)
    (define-values [v1 v2] (split-halves lst))
    (define v2* (append v2 (make-list (- (length v1) (length v2)) "")))
    (vc-append
     (text-lines w NAME_FONT_SIZE FG_COLOR v1)
     (text (number->+-string ho) (list FG_COLOR) NUMBER_FONT_SIZE)
     (text-lines w NAME_FONT_SIZE FG_COLOR v2*)))

  (define (fractional-image w lst)
    (text-lines w NAME_FONT_SIZE FG_COLOR lst))

  (for/fold ([h (hash)])
            ([(k v) (in-hash NAMES)])
    (define room-w-px (* HOUR_W_PX (find-room-w k NAMES)))
    (match k
      [(? integer? hour-offset)
       (define x (* UTC_HOUR_W_X hour-offset))
       (hash-set h
                 (list (vector-immutable x CENTER_Y)
                       (vector-immutable x N_DST_0_Y)
                       (vector-immutable x S_DST_0_Y))
                 (utc-offset-image room-w-px hour-offset v))]
      [`(N ,hour-offset)
       (define x (* UTC_HOUR_W_X hour-offset))
       (hash-set h
                 (list (vector-immutable x N_DST_0_Y))
                 (fractional-image room-w-px v))]
      [`(S ,hour-offset)
       (define x (* UTC_HOUR_W_X hour-offset))
       (hash-set h
                 (list (vector-immutable x S_DST_0_Y))
                 (fractional-image room-w-px v))]
      [`(N ,standard ,daylight)
       (define dh (- daylight standard))
       (define x (* UTC_HOUR_W_X (avg standard daylight)))
       (define y (+ N_DST_0_Y (* DST_HOUR_H_Y dh)))
       (hash-set h
                 (list (vector-immutable x y))
                 (fractional-image room-w-px v))]
      [`(S ,standard ,daylight)
       (define dh (- daylight standard))
       (define x (* UTC_HOUR_W_X (avg standard daylight)))
       (define y (- S_DST_0_Y (* DST_HOUR_H_Y dh)))
       (hash-set h
                 (list (vector-immutable x y))
                 (fractional-image room-w-px v))])))

(define (tz-map/point-groups W_PX H_PX point-groups)
  (parameterize ([plot-x-ticks no-ticks]
                 [plot-y-ticks no-ticks]
                 [metric manhattan-distance/half-x])
    (plot
     #:width W_PX
     #:height H_PX
     #:x-min LEFT_X #:x-max RIGHT_X
     #:y-min -4 #:y-max 4
     #:x-label #f
     #:y-label #f
     #:aspect-ratio (/ W_PX H_PX)
     (voronoi-diagram2
      #:x-min LEFT_X #:x-max RIGHT_X
      #:y-min -4 #:y-max 4
      #:samples 1000
      point-groups))))

(define (tz-map2 W_PX H_PX)
  (tz-map/point-groups W_PX H_PX (tz-seeds W_PX H_PX)))

;; ---------------------------------------------------------

(module+ main
  (tz-map2 W_PX H_PX))
