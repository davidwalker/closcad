(ns closcad.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [closcad.core :as scad]))

(deftest scad-test
  (is (= "cube();"  (scad/scad [:cube {}])))
  (is (= "cube(size = [1, 2, 3]);"  (scad/scad [:cube {:size [1 2 3]}])))
  (is (= "cylinder(d = 10, h = 5);"  (scad/scad [:cylinder {:d 10 :h 5}])))
  (is (= "cylinder(d = 0.5);"  (scad/scad [:cylinder {:d (/ 1 2)}])))

  (is (= "cube(size = [1, 2, 0.5]);"  (scad/scad [:cube {:size [1 2 (/ 1 2)]}])))

  (is (= "cube(a = [1, 2, \"foo\"], b = \"bar\");"
         (scad/scad [:cube {:a [1 2 "foo"]
                            :b "bar"}])))

  (is (= "a = 1;\nb = 0.5;\nc = \"hi\";\n"  (scad/scad {:a 1 :b 1/2 :c "hi"})))

  (is (= "cube();\ncube();"  (scad/scad [:cube {}] [:cube {}])))

  (is (= "$a = 1;\nb = 0.5;\n$fn = 4;\n"  (scad/scad {:$a 1 "b" 1/2 "$fn" 4})))

  (is (= "cube(size = [2, 3, 4]);"  (scad/scad [:cube (map inc [1 2 3])])))

  (is (=
"translate(v = [4, 5, 6]) {
    cube(size = [1, 2, 3]);
}"
(scad/scad [:translate
            {:v [4 5 6]}
            [:cube {:size [1 2 3]}]])))

  (is (=
"rotate(v = [90, 0, 180]) {
    translate(v = [4, 5, 6]) {
        cube(size = [1, 2, 3]);
    }
}"
(scad/scad [:rotate {:v [90 0 180]}
            [:translate {:v [4 5 6]}
             [:cube {:size [1 2 3]}]]])))


  (testing "component with args"
    (let [my-component (fn [{:keys [a b c]}]
                         [:cube {:size [a b c]}])]
      (is (= "cube(size = [1, 2, 3]);"
             (scad/scad [my-component {:a 1 :b 2 :c 3}])))))

  (testing "component without args"
    (let [my-component (fn []
                         [:cube {:size [1 2 3]}])]
      (is (= "cube(size = [1, 2, 3]);"
             (scad/scad [my-component ])))))


  ;; TODO: it would be nice to output a comment with component names for easier reading
  #_(let [my-component (fn [{:keys [a b c]}]
                         [:cube {:size [a b c]}])]
    (is (= "/** my-component -> **/\ncube(size = [1, 2, 3]);\n/** <- my-component **/"
           (scad/scad [my-component {:a 1 :b 2 :c 3}]))))


  (is (=
"difference() {
    cube(size = [1, 2, 3]);
    cylinder(size = [4, 5, 6]);
}"
       (scad/scad [:difference
                   [:cube {:size [1 2 3]}]
                   [:cylinder {:size [4 5 6]}]])))


  (is (= "use <my-lib.scad>;"
         (scad/scad [:use "my-lib.scad"])))

  (is (= "include <my-lib.scad>;"
         (scad/scad [:include "my-lib.scad"])))

  (is (= "use <my-lib.scad>;\ncube(size = 10);"
         (scad/scad [[:use "my-lib.scad"]
                     [:cube {:size 10}]])))

  (is (= "use <my-lib.scad>;\nuse <other-lib.scad>;"
         (scad/scad [:use ["my-lib.scad"
                           "other-lib.scad"]]))))


(deftest default-args-test

  (is (= "cube(size = [1, 2, 3]);"
         (scad/scad [:cube [1 2 3]])))

  (is (= "sphere(r = 3);"
         (scad/scad [:sphere 3])))


  (is (= "translate(v = [1, 2, 3]) {
    cube(size = [3, 4, 5]);
}"
         (scad/scad [:translate [1 2 3]
                     [:cube [3 4 5]]])))

  (is (= "import(file = \"foobar.stl\");"
         (scad/scad [:import "foobar.stl"]))))


(deftest modifiers-test
  (doseq [modifier #{"#" "%" "!" "*"}]
    (let [kw (keyword (str modifier "cube"))]
      (is (= (str modifier "cube(size = [1, 2, 3]);")
             (scad/scad [kw {:size [1 2 3]}])))

      (testing "default args with modifiers"
        (is (= (str modifier "cube(size = [1, 2, 3]);")
               (scad/scad [kw [1 2 3]])))))))

(deftest literal-test
  (is (= "literals are rendered exactly as is"
         (scad/scad (scad/literal "literals are rendered exactly as is"))))

  (is (= "cube(foo = my-literal);"
         (scad/scad [:cube {:foo (scad/literal "my-literal")}]))))

(deftest function-call-in-arg-position-test
  (is (= "cube(foo = height(bar = 2));"
         (scad/scad [:cube {:foo [:height {:bar 2}]}]))))
