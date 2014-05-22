(ns eight.core
  (:use eight.graphics)
  (:require 
    [eight.graphics :refer :all]
    [clojure.pprint :refer :all]
    [clojure.java.io :as io])
  (:import javax.imageio.ImageIO
           java.awt.image.BufferedImage))

(def square  [[1 0 1 0 1 0 1 0]
              [0 1 0 1 0 1 0 1]
              [1 0 1 0 1 0 1 0]
              [0 1 0 1 0 1 0 1]
              [1 0 1 0 1 0 1 0]
              [0 1 0 1 0 1 0 1]
              [1 0 1 0 1 0 1 0]
              [0 1 0 1 0 1 0 1]])

(def square-4x4 [[0.5 -0.5 -0.5 0.5 0.5 -0.5 -0.5 0.5]
                 [-0.5 0.5 0.5 -0.5 -0.5 0.5 0.5 -0.5]
                 [-0.5 0.5 0.5 -0.5 -0.5 0.5 0.5 -0.5]
                 [0.5 -0.5 -0.5 0.5 0.5 -0.5 -0.5 0.5]
                 [0.5 -0.5 -0.5 0.5 0.5 -0.5 -0.5 0.5]
                 [-0.5 0.5 0.5 -0.5 -0.5 0.5 0.5 -0.5]
                 [-0.5 0.5 0.5 -0.5 -0.5 0.5 0.5 -0.5]
                 [0.5 -0.5 -0.5 0.5 0.5 -0.5 -0.5 0.5]])

(def square-sharp-edge [[0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                        [0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                        [0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                        [0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                        [0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                        [0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                        [0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                        [0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]])

(def square-sharp-line [[0.5 0.5 -0.5 -0.5 -0.5 -0.5 0.5 0.5]
                        [0.5 0.5 -0.5 -0.5 -0.5 -0.5 0.5 0.5]
                        [0.5 0.5 -0.5 -0.5 -0.5 -0.5 0.5 0.5]
                        [0.5 0.5 -0.5 -0.5 -0.5 -0.5 0.5 0.5]
                        [0.5 0.5 -0.5 -0.5 -0.5 -0.5 0.5 0.5]
                        [0.5 0.5 -0.5 -0.5 -0.5 -0.5 0.5 0.5]
                        [0.5 0.5 -0.5 -0.5 -0.5 -0.5 0.5 0.5]
                        [0.5 0.5 -0.5 -0.5 -0.5 -0.5 0.5 0.5]])

(def square-sharp-corner [[0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                          [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                          [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                          [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                          [0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                          [0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                          [-0.5 -0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]
                          [-0.5 -0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5]])

(def square-solid [[0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                   [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                   [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                   [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                   [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                   [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                   [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
                   [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]])

(defn dot-prod-row [r1 r2]
  (apply + (map * r1 r2)))

(defn mult-rows [rs1 rs2]
  (apply + (map dot-prod-row rs1 rs2)))

(defn cos-of-freq [f]
  (fn [x]
    (Math/cos (/ (* x f Math/PI) 8.0))))

(defn norm-cos-of-freq [f]
  (fn [x]
    (+ 64 (* 63 ((cos-of-freq f) x)))))

(defn cos2d-of-freqs [fx fy]
  (fn [x y]
    (/ (+ ((cos-of-freq fx) x)
          ((cos-of-freq fy) y))
       2)))

(defn make-cos-row [fx]
    (mapv (cos-of-freq fx)
      (range 0.5 8.0)))

(defn make-2d-cos-square [fx fy]
  (mapv (fn [a]
          (mapv #(* % ((cos-of-freq fx) a))
               (make-cos-row fy)))
        (range 0.5 8.0)))

(defn draw-bases []
  (doseq [fx (range 0 8)
          fy (range 0 8)]
    (draw-square (make-2d-cos-square fx fy) [(* 10 fx) (* 10 fy)])))

(def basis-functions (into [] (for [fy (range 0 8)]
                       (into [] (for [fx (range 0 8)]
                         (make-2d-cos-square fx fy))))))

(defn decompose-square [square]
  (into [] (for [fy (range 0 8)]
    (into [] (for [fx (range 0 8)]
      (* (if (= fx 0)
           (Math/sqrt (/ 1 8))
           (Math/sqrt (/ 2 8)))
         (if (= fy 0)
           (Math/sqrt (/ 1 8))
           (Math/sqrt (/ 2 8)))
         (mult-rows square (get-in basis-functions [fx fy]))))))))

(defn add-rows [r1 r2]
  (mapv + r1 r2))

(defn add-squares [s1 s2]
  (mapv add-rows s1 s2))

(defn norm-row [a r]
  (mapv #(* a %) r))

(defn norm-square [a s]
  (mapv #(norm-row a %) s))

(defn coefs-by-bases [coefs]
  (into [] (for [fy (range 0 8)]
    (into [] (for [fx (range 0 8)]
      (norm-square (* (if (= fx 0)
                        (Math/sqrt (/ 1 8))
                        (Math/sqrt (/ 2 8)))
                      (if (= fy 0)
                        (Math/sqrt (/ 1 8))
                        (Math/sqrt (/ 2 8)))
                      (get-in coefs [fx fy]))
                   (get-in basis-functions [fx fy])))))))

;; quantization

(def good-quant-table
  [ [16  11  10  16  24  40  51  61]
    [12  12  14  19  26  58  60  55]
    [14  13  16  24  40  57  69  56]
    [14  17  22  29  51  87  80  62]
    [18  22  37  56  68  109 103 77]
    [24  35  55  64  81  104 113 92]
    [49  64  78  87  103 121 120 101]
    [72  92  95  98  112 100 103 99] ])

;; take something [-8, 8] -> [-127, 127]
(defn eight-bit [number]
  (if (pos? number)
    (int (+  0.000001 (* 254 (/ number 16))))
    (int (+ -0.000001 (* 254 (/ number 16))))))

;; [-127, 127] -> [-8, 8]
(defn de-eight-bit [number]
  (/ number 15.875))

(defn quant-coefs [coefs]
  (into [] (for [fy (range 0 8)]
    (into [] (for [fx (range 0 8)]
      (eight-bit (/ (get-in coefs [fx fy])
                    (de-eight-bit (get-in good-quant-table [fx fy])))))))))

(defn de-quant-coefs [coefs]
  (into [] (for [fy (range 0 8)]
    (into [] (for [fx (range 0 8)]
      (de-eight-bit (* (de-eight-bit (get-in good-quant-table [fx fy]))
                       (get-in coefs [fy fx]))))))))

;; decomp onto bases, now you have a square of coefs
;; coefs-by-bases turns those coefs into the bases you need to add up
;; add-square-of-squares turns those squares into an image

(defn add-row-of-squares [row-of-sqs]
  (reduce add-squares row-of-sqs))

(defn add-square-of-squares [sq-of-sqs]
  (reduce add-squares (map add-row-of-squares sq-of-sqs)))

(defn decomp-then-recomp [square]
  (->> square
       decompose-square
       quant-coefs
       de-quant-coefs
       coefs-by-bases
       add-square-of-squares))

(defn draw-before-and-after [square]
  (draw-square square [0 0])
  (draw-square (decomp-then-recomp square) [10 10]))

;; thanks http://samrat.me/blog/2014/04/image-diffing-in-clojure/
;; and    http://nakkaya.com/2010/03/23/steganography-with-clojure-hiding-text-in-images/

(defn read-image
    [path]
    (ImageIO/read (io/file path)))

(defn write-image
    [img path file-format]
    (ImageIO/write img file-format (io/file path)))

(defn px->argb
    [px]
    (let [a (bit-and (bit-shift-right px 24) 0xff)
          r (bit-and (bit-shift-right px 16) 0xff)
          g (bit-and (bit-shift-right px 8) 0xff)
          b (bit-and px 0xff)]
      [a r g b]))

(defn argb->px
  [[a r g b]]
  (bit-or (bit-shift-left a 24)
          (bit-or (bit-shift-left r 16)
                  (bit-or (bit-shift-left g 8)
                          b))))

(def snowden-file (read-image "snowden.png"))

(defn get-snowden []
  (into [] (for [x (range 0 64)]
    (into [] (for [y (range 0 64)]
      (- (* (/ 1.0 127.5) (second (px->argb (.getRGB snowden-file x y)))) 1))))))

(def snowden (get-snowden))

(defn subblock-pixels [x y]
  (let [x0 (* 8 x)
        y0 (* 8 y)]
    (into [] (for [xi (range x0 (+ 8 x0))]
      (into [] (for [yi (range y0 (+ 8 y0))]
        (get-in snowden [xi yi])))))))

(defn draw-big []
  (doseq [x (range 0 8)
          y (range 0 8)]
    (draw-square
      (subblock-pixels x y)
      [(* 8 x) (* 8 y)])
    (draw-square
      (norm-square 0.90 (decomp-then-recomp (subblock-pixels x y)))
      [(+ 65 (* 8 x)) (* 8 y)])))
