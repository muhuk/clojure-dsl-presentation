(ns code.core)


(defprotocol Shape
  (inside? [this other])
  (scale [this s])
  (translate [this dx dy]))


(defrecord Point [x y]
  Shape
  (inside? [this other]
    (and (instance? Point other)
         (= this other)))
  (scale [_ s]
    (->Point (* x s)
             (* y s)))
  (translate [_ dx dy]
    (->Point (+ x dx)
             (+ y dy))))


(defn sq [x]
  (* x x))


(defn distance [a b]
  (Math/sqrt (+ (sq (- (:x a) (:x b)))
                (sq (- (:y a) (:y b))))))


(defrecord Circle [center radius]
  Shape
  (inside? [_ other]
    (condp instance? other
      Point (<= (distance center other) radius)
      Circle (<= (distance (:center other) center)
                 (Math/abs (- (:radius other) radius)))))
  (scale [_ s]
    (->Circle (scale center s) (* radius s)))
  (translate [_ dx dy]
    (->Circle (translate center dx dy) radius)))


(defn point [x y]
  (->Point x y))


(def origin (point 0 0))


(def circle
  (->Circle origin 1))


(defrecord Intersection [shapes]
  Shape
  (inside? [_ other]
    (every? #(inside? % other) shapes))
  (scale [_ s]
    (->Intersection (map #(scale % s) shapes)))
  (translate [_ dx dy]
    (->Intersection (map #(translate % dx dy) shapes))))


(defn intersection [& shapes]
  (->Intersection shapes))


(defrecord Union [shapes]
  Shape
  (inside? [_ other]
    (boolean (some #(inside? % other) shapes)))
  (scale [_ s]
    (->Union (map #(scale % s) shapes)))
  (translate [_ dx dy]
    (->Union (map #(translate % dx dy) shapes))))


(defn union [& shapes]
  (->Union shapes))
