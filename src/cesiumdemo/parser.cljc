(ns cesiumdemo.parser
  (:require [clojure.string]))

(defn str->int [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn str->double [s]
  #?(:clj  (Double/parseDouble s)
     :cljs (js/parseFloat s)))

(defn parse-day [s]
  (let [c    (first s)
        root (str->int (subs s 1))]
    (if (zero? root)
      root
      (case c
        (\N \n) (- root)
        root))))

(defn revert [n]
  (let [prefix (if (neg? n) "N" "C")
        root   (str (int (Math/abs n)))
        pad     (if (< (count root) 3)
                  (apply str (repeat (- 3 (count root)) "0"))
                  "")]
    (str prefix  pad root)))
