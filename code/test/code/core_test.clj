(ns code.core-test
  (:require [clojure.test :refer :all]
            [code.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [*default-test-count*
                                                     defspec]]))


(defn times [n]
  (* n *default-test-count*))


(def ε 0.0001)


(def gen-number
  (gen/double* {:min -1E20
                :max 1E20
                :infinite? false
                :NaN? false}))


(def gen-point
  (gen/let [x gen-number
            y gen-number]
    (point x y)))


(def gen-positive-number
  (gen/fmap #(Math/abs %) gen-number))


(def gen-circle
  (gen/let [center gen-point
            radius gen-positive-number]
    (-> circle
        (scale radius)
        (translate (:x center) (:y center)))))


(defn gen-circle-near [p]
  (gen/let [radius gen-positive-number
            d-max (gen/return (* (- 1.0 ε) radius))
            d (gen/double* {:min 0.0
                            :max d-max
                            :infinite? false
                            :NaN? false})
            angle (gen/double* {:min 0.0
                                :max (* Math/PI 2)
                                :infinite? false
                                :NaN? false})
            dx (gen/return (* (Math/cos angle) d))
            dy (gen/return (* (Math/sin angle) d))]
    (-> circle
        (scale radius)
        (translate (+ (:x p) dx) (+ (:y p) dy)))))


(defspec a-point-is-inside-itself
  (prop/for-all [x gen/int
                 y gen/int]
                (inside? (point x y)
                         (point x y))))


(defspec any-other-point-is-outside-of-a-point
  (prop/for-all [[a b] (gen/such-that (fn [[a b]] (not= a b))
                                      (gen/vector gen-point 2))]
                (not (inside? a b))))


(defspec any-circle-is-outside-of-a-point
  (times 5)
  (prop/for-all [p gen-point
                 c gen-circle]
                (not (inside? p c))))


(deftest origin-is-inside-of-unit-circle
  (is (inside? circle origin)))


(defspec points-further-than-radius-from-a-circles-origin-are-outside
  (times 2)
  (prop/for-all [p (gen/such-that #(> (distance % origin)
                                      (:radius circle))
                                  gen-point
                                  (times 2))]
                (not (inside? circle p))))


(defspec points-closer-than-radius-from-a-circles-origin-are-inside
  (prop/for-all [p (gen/such-that #(<= (distance % origin)
                                      (:radius circle))
                                  gen-point
                                  (times 1))]
                (inside? circle p)))


(defspec smaller-circle-with-the-same-origin-is-inside-the-larger-circle
  (prop/for-all [s (gen/choose 1.0 1000.0)
                 multiplier (gen/choose 1.2 2.0)]
                (inside? (scale circle (* s multiplier))
                         (scale circle s))))


(deftest translate-point
  (is (= (point 3 5)
         (translate (point 8 2) -5 3)))
  (is (= origin (translate origin 0 0)))
  (is (= origin (translate (point 11 7) -11 -7))))


(defspec translate-circle
  (prop/for-all [dx gen/int
                 dy gen/int]
                (let [c (translate circle dx dy)]
                  (and (= (:radius c) (:radius circle))
                       (= (:center c)
                          (translate (:center circle) dx dy))))))


(def gen-intersecting-shapes
  (gen/let [p gen-point
            shapes (gen/vector-distinct (gen-circle-near p)
                                        {:min-elements 2
                                         :max-elements 10})]
    [p shapes]))


(defspec a-point-is-inside-an-intersection-if-it-is-inside-all-shapes
  (prop/for-all [[p shapes] gen-intersecting-shapes]
                (inside? (apply intersection shapes) p)))


(def gen-shapes-and-a-point
  (gen/let [shapes (gen/vector-distinct gen-circle
                                          {:min-elements 2
                                           :max-elements 10})
            p (gen/fmap :center (gen/elements shapes))]
    [p shapes]))


(defspec a-point-is-inside-a-union-if-it-is-inside-any-shape
  (prop/for-all [[p shapes] gen-shapes-and-a-point]
                (inside? (apply union shapes) p)))
