(ns closcad.geometry
  (:require
   [clojure.core.matrix :as m]))

(defn reflect-x-y [& shapes]
  (for [mx [0 1]
        my [0 1]]
    [:mirror [0 my 0]
     (into
      [:mirror [mx 0 0]]
      shapes)]))

(defn reflect-x [& shapes]
  (for [mx [0 1]]
    (into [:mirror [mx 0 0]] shapes)))

(defn base-cylinder [props]
  [:cylinder (merge {:center true}
                    props)])

(defn cube [props]
  (let [props (if (map? props)
                props
                {:size props})]
    [:cube (merge {:center true}
                  props)]))

(defn square [props]
  (let [props (if (map? props)
                props
                {:size props})]
    [:square (merge {:center true}
                  props)]))

(defn sequence-hull* [children]
  (map
   (fn [c1 c2]
     [:hull c1 c2])
   children
   (drop 1 children)))

(defn sequence-hull [& children]
  (into [:union ]
        (sequence-hull* (keep identity children))))

;; d1 lower
;; d2 upper

(defn cylinder [props]
  (let [{:keys [h]} props
        disc-height (or (:disc-height props) 0.02M)

        d2 (or (:d2 props) (:d props))
        d1 (or (:d1 props) (:d props))

        r2 (or (:r2 props) (:r props))
        r1 (or (:r1 props) (:r props))

        c2 (or (:chamfer2 props) (:chamfer props) 0)
        c1 (or (:chamfer1 props) (:chamfer props) 0)]
    (if (= 0 c1 c2)
      [base-cylinder (dissoc props :chamfer :chamfer1 :chamfer2)]
      [sequence-hull
       (when (< 0 (abs c2))
         [:translate [0 0 (* 1/2 h)]
          [cylinder (merge (when-let [d d2]
                             {:d (- d (* 2 c2))})
                           (when-let [r r2]
                             {:r (- r c2)})
                           {:h disc-height}) ]])

       [:translate [0 0 (- (* 1/2 h) (abs c2))]
        [cylinder (merge (when-let [d d2]
                           {:d d})
                         (when-let [r r2]
                           {:r r})
                         {:h disc-height})]]


       [:translate [0 0 (+ (* -1/2 h) (abs c1))]
        [cylinder (merge (when-let [d d1]
                           {:d d})
                         (when-let [r r1]
                           {:r r})
                         {:h disc-height}) ]]

       (when (< 0 (abs c1))
         [:translate [0 0 (* -1/2 h)]
          [cylinder (merge (when-let [d d1]
                             {:d (- d (* 2 c1))})
                           (when-let [r r1]
                             {:r (- r c1)})
                           {:h disc-height})]])])))

#_[chamfered-cylinder2 {:d 10
                       :h 20
                       :chamfer1 2
                       :chamfer2 -1
                       }]
