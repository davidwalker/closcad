(ns closcad.geometry
  (:require
   [clojure.core.matrix :as m]))

(def named-alignments {:center 0
                       :base [0 0 1]
                       :top [0 0 -1]
                       :corner 1})

(defn align-object [{:keys [size align]} & children]
  (let [a (get named-alignments (or align :center) align)]
    (into [:translate (m/mul 1/2 a size)]
          children)))

(defn alignable [comp size-fn]
  (fn [& args]
    (let [[props & children] args]
      (if (and (map? props)
               (:align props))
        [align-object {:align (:align props)
                       :size (size-fn props)}
         (into [comp (dissoc props :align)] children)]
        (apply comp args)))))

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

(defn reflect-y [& shapes]
  (for [my [0 1]]
    (into [:mirror [0 my 0]] shapes)))

(defn reflect-z [& shapes]
  (for [mz [0 1]]
    (into [:mirror [0 0 mz]] shapes)))

(defn blend-shapes
  "blend two shapes together with the given amount where 0 is just s1 and 1 is just s2 "
  [{:keys [s1 s2 percentage]}]
  (let [p (/ percentage 100)]
    [:minkowski
     [:scale (- 1 p) s1]
     [:scale p s2]]))

(defn base-cylinder [props]
  [:cylinder (merge {:center true}
                    props)])

(defn cube-size [s]
  (cond
    (vector? s) s
    (number? s) [s s s]))

(defn normalize-cube-props [props]
  (if (map? props)
    (update props :size cube-size)
    {:size (cube-size props)}))

(defn cube* [props]
  [:cube (merge {:center true}
                (normalize-cube-props props))])

(def cube (alignable cube* (fn [props]
                             (-> props
                                 normalize-cube-props
                                 :size))))

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

(defn update-if-present [o k fn & args]
  (if (contains? o k)
    (apply update o k fn args)
    o))

(defn cylinder-size [props]
  (let [d2 (or (:d2 props) (:d props)
               (* 2 (or (:r2 props) (:r props))))
        d1 (or (:d1 props) (:d props)
               (* 2 (or (:r1 props) (:r props))))

        d (max d1 d2)]
    [d d (:h props)]))

;; d1 lower
;; d2 upper

(declare cylinder)

(defn cylinder* [props]
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

(def cylinder (alignable cylinder* cylinder-size))

#_[chamfered-cylinder2 {:d 10
                       :h 20
                       :chamfer1 2
                       :chamfer2 -1
                       }]


(defn pill* [{:keys [d h]}]
  (let [z-offset (* 1/2 (- h d))]
    [:hull
     [reflect-z
      [:translate [0 0 z-offset]
       [:sphere {:d d}]]]]))

(def pill (alignable pill* cylinder-size))



(defn rounded-rect [{:keys [size r align]}]
  (let [[x y z] size]
    [align-object {:size size :align align}
     [:translate [0 0 (* -1/2 z)]
      [:linear_extrude z
       [:offset r
        [:offset (- r)
         [square [x y]]]]]]]))
