(ns datac.core-test
  (:require [clojure.test :refer :all]
            [datac.core :refer :all]))

(defn pv [f]
  (b! f 
   {:alt (fn [this f] (pv (comp (§> f) this)))
    :$   (fn [f this] (pv (comp ($> f) this)))
    :&1  (fn [this x] (pv (comp (<& x) this)))
    :&2  (fn [x this] (pv (comp (&> x) this)))}))

((pv inc) 1) ;=> 2
((§> inc (pv inc)) 2) ;=> 4
(($> inc (pv vector)) 2 3 4) ;=> [3 4 5]
((&> [inc dec inc] (pv vector)) 2 3 4) ;=> [3 2 5] 
((&> (pv vector) (range)) inc dec pos? neg?) ;=> (1 0 true false 4 ...)