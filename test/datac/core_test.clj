(ns datac.core-test
  (:require [clojure.test :refer :all]
            [datac.core :refer :all]))

(§> inc 1)
(§> nil :anything)

(§> [inc dec] 0) ;=> [1 -1]
(§> {:a inc :b dec} 0) ;=> {:b -1, :a 1}
(§> (list inc dec) 0) ;=> (1 -1) 
(§> #{inc dec} 0) ;=> #{1 -1}

(§> 1 :bar) ;=> 1

($> :foo 2) ;=> :foo
($> inc 2) ;=> 3
($> nil 2) ;=> 2

($> inc [1 2 3]) ;=> [2 3 4]
($> inc {:a 1 :b 2}) ;=> {:a 2, :b 3}
($> inc (range)) ;=> (1 2 3 4 5 ...)
($> inc #{1 2 3}) ;=> #{4 3 2}

($> {:a inc :b dec} (range))
;=> ({:b -1, :a 1} {:b 0, :a 2} {:b 1, :a 3}...)
($> [inc dec] {:a 1 :b 2})
;=> {:a [2 0], :b [3 1]}

(&> :bob 2) ;=> :bob
(&> inc 2) ;=> 3
(&> nil :zux) ;=> :zux

(&> 1 [0 0 0]) ;=> [1 0 0]
(&> [inc dec] 1) ;=> 2

(&> [inc dec inc dec] [0 0 0 0])
;=> [1 -1 1 -1]
(&> {:a inc :b dec} {:a 1 :b 1})
;=>  {:a 2, :b 0}
(&> {2 inc} [0 0 0 0])
;=> [0 0 1 0]
(&> [[inc dec][dec inc]] [1 2])
;=> [[2 0] [1 3]]
(&> {4 dec} #{1 2 3 4})
;=> #{1 3 2}

(+> 1 1) ;=> 2
(+> "hello " "world!") ;=> "hello world!"
(+> [1 2] [3 4]) ;=> [1 2 3 4]
(+> {:a 1 :b 2} {:b 3 :c 4}) ;=> {:c 4, :a 1, :b 3}
(+> (range 10) []) ;=> [0 1 2 3 4 5 6 7 9]
(+> "foo" 1) ;=> ["foo" 1]
(+> [1 2] {:a 1}) ;=> {:a 1, 1 2, 0 1}
(+> #{:a :b} [1]) ;=> [:a :b 1]
(+> [1] #{:a :b}) ;=> #{1 :a :b}
(+> 0 [1 2 3]) ;=> [0 1 2 3]
(+> inc dec) ;<=> (juxt inc dec)

(o> 42) ;=> 0 
(o> [1 2 3]) ;=> []
(o> #{1 2 3}) ;=> #{}
(o> {:a 1}) ;=> {}
(o> (range)) ;=> ()
(o> "hi!") ;=> ""
(o> inc) ;=> #<core$identity clojure.core$identity@306f6c8>

(<> nil 1) ;=> nil
(<> inc 1) ;=> (constantly 1)
(<> [] 1) ;=> [1]
(<> [1 2 3] (list 1 2))
;=> [1 2]
(<> {} [:a :b :c])
;=> {2 :c, 1 :b, 0 :a}
(<> #{} [0 0 1])
;=> #{0 1}

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