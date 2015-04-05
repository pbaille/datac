(ns datac.core-test
  (:require [clojure.test :refer :all]
            [datac.core :refer :all]))

(testing "invocation"
  (is (= (§> inc 1) 2))
  (is (= (§> nil :anything) :anything))
  (is (= (§> [inc dec] 0) [1 -1])) 
  (is (= (§> {:a inc :b dec} 0) {:b -1, :a 1})) 
  (is (= (§> (list inc dec) 0) (list 1 -1) ))
  (is (= (§> #{inc dec} 0) #{1 -1}))
  (is (= (§> 1 :bar) 1)))

(testing "distribution"
  (is (= ($> :foo 2) :foo)) 
  (is (= ($> inc 2) 3))
  (is (= ($> nil 2) 2)) 

  (is (= ($> inc [1 2 3]) [2 3 4])) 
  (is (= ($> inc {:a 1 :b 2}) {:a 2, :b 3}))
  (is (= ($> inc (list 1 2 3)) (list 2 3 4))) 
  (is (= ($> inc #{1 2 3}) #{4 3 2}))
  (is (= ($> {:a inc :b dec} (list 0 1 2))
         (list {:b -1, :a 1} {:b 0, :a 2} {:b 1, :a 3})))
  (is (= ($> [inc dec] {:a 1 :b 2}) 
         {:a [2 0], :b [3 1]})))

(testing "zipping"
  (is (= (&> :bob 2) :bob))
  (is (= (&> inc 2) 3))
  (is (= (&> nil :zux) :zux))
  (is (= (&> 1 [0 0 0]) [1 0 0]))
  (is (= (&> [inc dec] 1) 2))
  (is (= (&> [inc dec inc dec] [0 0 0 0]) [1 -1 1 -1]))
  (is (= (&> {:a inc :b dec} {:a 1 :b 1}) {:a 2, :b 0}))
  (is (= (&> {2 inc} [0 0 0 0]) [0 0 1 0]))
  (is (= (&> [[inc dec][dec inc]] [1 2]) [[2 0] [1 3]]))
  (is (= (&> {4 dec} #{1 2 3 4}) #{1 3 2})))

(testing "adding"
  (is (= (+> 1 1)  2))
  (is (= (+> "hello " "world!") "hello world!"))
  (is (= (+> [1 2] [3 4]) [1 2 3 4]))
  (is (= (+> {:a 1 :b 2} {:b 3 :c 4}) {:c 4, :a 1, :b 3}))
  (is (= (+> (range 10) []) [0 1 2 3 4 5 6 7 8 9]))
  (is (= (+> "foo" 1) ["foo" 1]))
  (is (= (+> [1 2] {:a 1}) {:a 1, 1 2, 0 1}))
  (is (= (+> #{:a :b} [1]) [:b :a 1]))
  (is (= (+> [1] #{:a :b}) #{1 :a :b}))
  (is (= (+> 0 [1 2 3]) [0 1 2 3]))
  (is (= ((+> inc dec) 10) ((juxt inc dec) 10))))

(testing "zero"
  (is (= (o> 42) 0 ))
  (is (= (o> [1 2 3]) []))
  (is (= (o> #{1 2 3}) #{}))
  (is (= (o> {:a 1}) {}))
  (is (= (o> (range)) ()))
  (is (= (o> "hi!") ""))
  (is (= (o> inc) identity)))


(testing "wrap"
  (is (= (<> nil 1) nil))
  (is (= ((<> inc 1)) ((constantly 1))))
  (is (= (<> [] 1) [1]))
  (is (= (<> [1 2 3] (list 1 2)) [1 2]))
  (is (= (<> {} [:a :b :c]) {2 :c, 1 :b, 0 :a}))
  (is (= (<> #{} [0 0 1]) #{0 1})))













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