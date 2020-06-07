(ns closcad.utils
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [closcad.core :as scad])
  (:import
   (java.lang ProcessBuilder)
   (javax.imageio ImageIO)))

(defn write
  [f & v]
  (let [res (apply scad/scad v)]
    (spit f res)))

(defn launch
  [f & v]
  (apply write f v)
  (.start (ProcessBuilder. ["openscad" f])))

(defn preview [& v]
  (let [scad-source    (apply scad/scad v)
        temp-scad-file (java.io.File/createTempFile "closcad-preview" ".scad")
        temp-img-file  (java.io.File/createTempFile "closcad-preview" ".png")]
    (try
      (try
        (spit temp-scad-file scad-source)
        (let [res (shell/sh "openscad"
                            "-o" (.getAbsolutePath temp-img-file)
                            "--imgsize=300,300"
                            "--preview=throwntogether"
                            (.getAbsolutePath temp-scad-file))]
          (if (zero? (:exit res))
            (ImageIO/read temp-img-file)
            (:err res)))
        (finally
          (.delete temp-img-file)))
      (finally
        (.delete temp-scad-file)))))

(defn to-stl
  [f & v]
  (let [scad-source (apply scad/scad v)
        temp-scad-file (java.io.File/createTempFile "closcad" ".scad")
        stl-file       (io/as-file f)]
    (try
      (spit temp-scad-file scad-source)
      (let [res (shell/sh "openscad"
                          "-o" (.getAbsolutePath stl-file)
                          (.getAbsolutePath temp-scad-file))]
        (if (zero? (:exit res))
          (.getAbsolutePath stl-file)
          res))
      (finally
        (.delete temp-scad-file)))))
