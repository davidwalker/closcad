(ns closcad.core
  (:require
   [clojure.string :as str]))

(defn arg-val [v]
  (cond
    (vector? v)
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
    (into [n {}] args)))

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
           ";"))))

(defn render-use-include [ctx item]
  (let [[command lib-name] item]
    (str (indent ctx) (name command) " <" lib-name ">;")))

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
     (vector? item)
     (render-vector-item ctx item)

     (map? item)
     (render-variables ctx item)

     (seq? item)
     (apply str (interpose "\n" (map (partial render-item ctx) item))))))

(defn scad
  ([& items]
   (render-item {:indent 0} items)))
