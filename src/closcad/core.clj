(ns closcad.core
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest testing is]]))

#_(def v [1 2 3])

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



#_(normalize [:foo])
#_(normalize [:foo {:a :b}])
#_(normalize-scad-vector [:foo {:a :b} [:bar] ])
#_(normalize-scad-vector [:foo {:a :b} [:bar] [:baz] ])
#_(normalize-scad-vector [:foo [:bar] [:baz]])

#_(def ctx {:indent 4})

(defn indent [ctx]
  (apply str (repeat (:indent ctx) " ")))

(declare render-item)

(defn render-keyword-item [ctx item]
  (let [[n args & children] (normalize-scad-vector item)]
    (str (indent ctx) (name n) "(" (apply str (interpose ", " (map transform-arg args))) ")"
         (if children
           (str " {\n"
                (let [ctx (update ctx :indent + 4)]
                  (apply str (interpose \newline (map #(render-item ctx %) children))))
                "\n" (indent ctx) "}")
           ";"))))

(defn render-fn-item [ctx item]
  (let [[n args & children] (normalize-scad-vector item)]
    (render-item ctx (apply n args children))))


(defn render-variables [ctx m]
  (apply str
   (interleave (repeat (indent ctx ))
               (map transform-arg m)
               (repeat ";\n"))))

(scad {:a 1 :b 1/2 :c "hi"})

(defn render-item
  ([ctx item]
   (cond
     (vector? item)
     (let [f (first item)]
       (cond
         (keyword? f)
         (render-keyword-item ctx item)

         (fn? f)
         (render-fn-item ctx item)

         :else
         (render-item ctx (seq item))
         ))

     (map? item)
     (render-variables ctx item)

     (seq? item)
     (apply str (interpose "\n" (map (partial render-item ctx) item)))
     )))
;; (scad {:a 1 :c "hi"} [:cube] [:cylinder])

(defn scad
  ([& items]
   (render-item {:indent 0} items)))

(deftest scad-test
  (is (= "cube();"  (scad [:cube {}])))
  (is (= "cube(size = [1, 2, 3]);"  (scad [:cube {:size [1 2 3]}])))
  (is (= "cylinder(d = 10, h = 5);"  (scad [:cylinder {:d 10 :h 5}])))
  (is (= "cylinder(d = 0.5);"  (scad [:cylinder {:d (/ 1 2)}])))

  (is (= "cube(size = [1, 2, 0.5]);"  (scad [:cube {:size [1 2 (/ 1 2)]}])))

  (is (= "cube(a = [1, 2, \"foo\"], b = \"bar\");"  (scad [:cube {:a [1 2 "foo"]
                                                    :b "bar"}])))

  (is (= "a = 1;\nb = 0.5;\nc = \"hi\";\n"  (scad {:a 1 :b 1/2 :c "hi"})))

  (is (= "cube();\ncube();"  (scad [:cube {}] [:cube {}])))

  (is (= "$a = 1;\nb = 0.5;\n$fn = 4;\n"  (scad {:$a 1 "b" 1/2 "$fn" 4})))


  (is (=
"translate(v = [4, 5, 6]) {
    cube(size = [1, 2, 3]);
}"
(scad [:translate
       {:v [4 5 6]}
       [:cube {:size [1 2 3]}]])))

  (is (=
       "rotate(v = [90, 0, 180]) {
    translate(v = [4, 5, 6]) {
        cube(size = [1, 2, 3]);
    }
}"
       (scad [:rotate {:v [90 0 180]}
              [:translate {:v [4 5 6]}
               [:cube {:size [1 2 3]}]]])))


  (let [my-component (fn [{:keys [a b c]}]
                       [:cube {:size [a b c]}])]
    (is (= "cube(size = [1, 2, 3]);"
           (scad [my-component {:a 1 :b 2 :c 3}]))))


  ;; todo would be nice to output comment with component names for easier reading
  #_(let [my-component (fn [{:keys [a b c]}]
                       [:cube {:size [a b c]}])]
    (is (= "/** my-component -> **/\ncube(size = [1, 2, 3]);\n/** <- my-component **/"
           (scad [my-component {:a 1 :b 2 :c 3}]))))


  (is (=
       "difference() {
    cube(size = [1, 2, 3]);
    cylinder(size = [4, 5, 6]);
}"
       (scad [:difference
              [:cube {:size [1 2 3]}]
              [:cylinder {:size [4 5 6]}]])))
  )
