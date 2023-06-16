(ns closcad.core
  (:require
   [clojure.string :as str]))


(def modifiers #{\# \% \! \*})

(def default-args {:cube       :size
                   :sphere     :r
                   :square     :size
                   :circle     :r
                   :scale      :v
                   :resize     :newsize
                   :rotate     :a
                   :translate  :v
                   :mirror     :v
                   :multmatrix :m
                   :color      :c
                   :offset     :r
                   :import     :file})


(defprotocol Renderable
  (render [item ctx])
  (render-arg-val [item]))

(defn renderable? [x]
  (satisfies? Renderable x))

(deftype Scad-literal [s]
  Renderable
  (render [item ctx] s)
  (render-arg-val [item] s))

(defn literal [x]
  (Scad-literal. x))

(defn remove-modifier
  "Remove the modifier from a keyword. e.g. :#cube becomes :cube"
  [n]
  (if (modifiers (first (name n)))
    (keyword (subs (name n) 1))
    n))

(declare render-vector-item)

(defn arg-val [v]
  (cond
    (renderable? v)
    (render-arg-val v)

    (and (vector? v)
         (or (keyword? (first v))
             (fn? (first v))))
    (render-vector-item {:indent 0 :arg true} v)

    (sequential? v)
    (str "["  (apply str (interpose ", " (map arg-val v))) "]")

    (ratio? v)
    (str (double v))

    (string? v)
    (str \" v \" )

    :else
    (str v)))

(defn transform-arg [[k v]]
  (str (name k) " = " (arg-val v)))

(defn normalize-scad-vector [[n & args]]
  (cond
    (nil? args)
    [n {}]

    (map? (first args))
    (into [n] args)

    :else
    (if-let [da ((remove-modifier n) default-args)]
      (into [n {da (first args)}] (rest args))
      (into [n {}] args))))

(defn indent [ctx]
  (apply str (repeat (:indent ctx) " ")))

(declare render-item)

(defmulti render-keyword-vector-item
  (fn [ctx item]
    (first item)))

(defmethod render-keyword-vector-item :default
  [ctx item]
  (let [[n args & children] (normalize-scad-vector item)]
    (str (indent ctx) (name n) "(" (apply str (interpose ", " (map transform-arg args))) ")"
         (if children
           (str " {\n"
                (let [ctx (update ctx :indent + 4)]
                  (apply str (interpose \newline (map #(render-item ctx %) children))))
                "\n" (indent ctx) "}")
           (if (:arg ctx) "" ";")))))


(defn render-use-include [ctx item]
  (let [[command lib-names] item
        lib-names (if (string? lib-names)
                    [lib-names]
                    lib-names)]
    (str/join \newline
              (map (fn [lib-name]
                     (str (indent ctx) (name command) " <" lib-name ">;"))
                   lib-names))))

(doseq [k [:use :include]]
  (defmethod render-keyword-vector-item k
    [ctx item]
    (render-use-include ctx item)))

(defn render-fn-vector-item [ctx item]
  (let [[n & args] item]
    (render-item ctx (apply n args))))

(defn render-vector-item [ctx item]
  (let [f (first item)]
    (cond
      (keyword? f)
      (render-keyword-vector-item ctx item)

      (fn? f)
      (render-fn-vector-item ctx item)

      :else
      (render-item ctx (seq item)))))

(defn render-variables [ctx m]
  (apply str
   (interleave (repeat (indent ctx ))
               (map transform-arg m)
               (repeat ";\n"))))

(defn render-item
  ([ctx item]
   (cond
     (renderable? item)
     (render item ctx)

     (vector? item)
     (render-vector-item ctx item)

     (map? item)
     (render-variables ctx item)

     (seq? item)
     (apply str (interpose "\n" (map (partial render-item ctx) item))))))

(defn scad
  ([& items]
   (render-item {:indent 0} items)))
