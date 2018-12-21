# Closcad

Create OpenSCAD models using hiccup inspired Clojure data structures.

## Usage

At the moment you have to use deps.edn git dependencies. so add something like this to your deps.edn file:

```clojure
closcad {:git/url "https://github.com/davidwalker/closcad"
         :sha ""}
```

Then you just call `closcad.core/scad` passing in your model.

```clojure
(require '[closcad.core :as scad])

(def my-model [:cube {:size [1 3 4]}])

(spit "my-model.scad" (scad/scad my-model))
```


At the moment all arguments must be named so translate would be 

```clojure
[:translate {:v [4 5 6]} 
 [:cube {:size [1 2 3]}]]
```


For more examples have a look at the tests.

## Status

This project is very much experimental. Very little thought has gone into the design. Use at your own risk. If you have suggestions for improvements feel free to open an issue.
