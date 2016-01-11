(ns datac.core
  (:import [clojure.lang Fn MapEntry IPersistentVector IPersistentList Iterate LongRange LazySeq IPersistentMap IPersistentSet Keyword Symbol Repeat Cycle Cons])
  (:require [utils.futils :as fu]
            [utils.types :refer [t t?]]
            [utils.misc :refer [impl-for defmets]]
            [clojure.test :refer :all]))

(set! *print-length* 50)

;; helpers -----

(defn mentry [k v] (MapEntry. k v))

(defn eseq> [x & mtags]
  (let [tagmap (apply hash-map (mapcat #(vector % true) mtags))]
    (with-meta x (merge tagmap {:type ::eseq}))))

(defn- lazy? [eseq]
  (:lazy (meta eseq)))

(defn eseq->hm [x]
  (reduce #(assoc %1 (key %2) (val %2)) {} x))

(defn eseq->vec [x]
  (reduce
    (fn [r [k v]]
      (if (integer? k)
        (let [c (count r)]
          (if (> c k)
            (assoc r k v)
            (conj (apply conj r (take (- k c) (repeat nil))) v)))
        r))
    []
    x))

(declare §)

(defn zip-eseqs [x y]
  ; is there a way to avoid this crap?
  (let [lx (lazy? x)
        ly (lazy? y)]
    (cond
      (and lx ly) (eseq> (map #(§ (val %1) %2) x y) :lazy)
      ly (let [hx (into {} x)] (eseq> (map #(§ (find hx (key %)) %) y) :lazy))
      lx (let [hy (into {} y)] (eseq> (map #(§ % (find hy (key %))) x) :lazy))
      :else (eseq> (seq (merge-with § (into {} x) (into {} y)))))))

(defn juxt* [& args]
  (fn [& xs] (mapv #(apply § % xs) args)))

;; main impl -----

(defmulti behaviors type)

(defn default-behaviors [x]
  {:fn    (constantly x)
   :zero  x
   :val   x
   :alt   (fn [f] (f x))
   :eseq  (eseq> (list (mentry 0 x)))
   :build (fn [y] (get (into {} y) 0))})

(defn instance-bs [x]
  (when-let [gbs (:get-bs (meta x))] (gbs x)))

(defn get-bs [x]
  (merge (default-behaviors x) (behaviors x) (instance-bs x)))

(defn conj-op [x op]
  (let [opx (:op (meta x))
        op (if opx (conj opx op) [op])]
    (vary-meta x assoc :op op)))

(defn clear-op [x]
  (vary-meta x dissoc :op))

(declare c)

(defn §
  ([x] (conj-op x §))
  ([x y]
   (let [[bx by] (map get-bs [x y])
         op (:op (meta x))]
     (if op
       ((apply c op) (clear-op x) y)
       ((:alt by) (:fn bx))))))

(def f (fu/flip §))

(defn $
  ([x] (conj-op x $))
  ([x y]
   (let [[bx by] (map get-bs [x y])]
     ((:build by) (map (partial § x) (:eseq by))))))

(defn &
  ([x] (conj-op x &))
  ([x y]
   (let [[bx by] (map get-bs [x y])]
     ((:build by) (zip-eseqs (:eseq bx) (:eseq by))))))

(fu/def-curried ◊ [x y]
                (let [[bx by] (map get-bs [x y])]
                  ((:build bx) (eseq> (concat (:eseq bx) (:eseq by))))))

(defn o [x] (:zero (get-bs x)))

;; behaviors set get share

(defn b! [x bfn]
  (vary-meta x assoc :get-bs bfn))

(defn >b
  ([x] (get-bs x))
  ([x y]
   (b! y
       #(merge (default-behaviors %)
               ((impl-for behaviors x) %)
               (if-let [gbs (:get-bs (meta x))] (gbs %))))))

;; extend native types ---

(derive Repeat ::lazy)
(derive Cycle ::lazy)
(derive LazySeq ::lazy)
(derive Cons ::lazy)
(derive Iterate ::lazy)
(derive LongRange ::lazy)

(defmets behaviors

           nil [_]
           {:fn    identity
            :alt   (constantly nil)
            :eseq  (eseq> ())
            :build (constantly nil)}

           ::eseq [x]
           {:eseq  x
            :build identity
            :zero  (eseq> ())}

           Fn [x]
           {:zero identity
            :fn   x}

           Number [x] {:zero 0}
           String [x] {:zero ""}
           Keyword [x] {:zero (keyword "")}
           Symbol [x] {:zero (symbol "")}

           MapEntry [x]
           {:fn    (fn [y] (mentry (key x) (§ (val x) y)))
            :val   (val x)
            :zero  (mentry (key x) nil)
            :alt   (fn [f] (mentry (key x) (§ f (val x))))
            :eseq  (eseq> (list x))
            :build (fn [y] (find (into {} y) (key x)))}

           IPersistentVector [x]
           {:zero  []
            :fn    (fn [y] ((apply juxt* x) y))
            :eseq  (eseq> (map-indexed mentry x))
            :build eseq->vec}

           IPersistentMap [x]
           {:zero  {}
            :fn    (fn [y] (into {} (map #(§ % y) x)))
            :eseq  (eseq> (map identity x))
            :build eseq->hm}

           IPersistentSet [x]
           {:zero  #{}
            :fn    (fn [y] (set ((apply juxt* x) y)))
            :eseq  (eseq> (map-indexed mentry x))
            :build #(set (map second %))}

           IPersistentList [x]
           {:zero  ()
            :fn    (fn [y] (apply list ((apply juxt* x) y)))
            :eseq  (eseq> (map-indexed mentry x))
            :build #(apply list (map second %))}

           ::lazy [x]
           {:zero  (lazy-seq)
            :fn    (fn [y] (map #(§ % y) x))
            :eseq  (eseq> (map-indexed mentry x) :lazy)
            :build #(map second %)}

           :default [x] {})

;; tests ------------------

(testing "§"
  (is (= (§ nil 1) 1))
  (is (= (§ :pouet nil) nil))
  (is (= (§ inc (mentry :a 1)) (mentry :a 2)))
  (is (= (§ inc 1) 2))
  (is (= (§ (mentry :a inc) 2) (mentry :a 3)))
  (is (= (§ (mentry :a 1) 2) (mentry :a 1)))
  (is (= (§ (mentry :a (mentry :b inc)) 2) (mentry :a (mentry :b 3))))
  (is (= (§ 42 2) 42))
  (is (= (§ 42 (mentry :a 1)) (mentry :a 42)))
  (is (= (§ [inc dec] 1) [2 0]))
  (is (= (§ [inc dec] (mentry :a 1)) (mentry :a [2 0])))
  (is (= (§ {:a inc :b dec} 2) {:a 3, :b 1}))
  (is (= (§ #{inc dec} 0) #{1 -1}))
  (is (= (§ (list inc dec) 0) (list 1 -1)))
  (is (= (§ (list [inc dec] [dec inc] {:a [[inc dec] dec] :b dec}) 1)
         (list [2 0] [0 2] {:a [[2 0] 0], :b 0}))))

(testing "$"
  (is (= ($ nil 1) 1))
  (is (= ($ :pouet nil) nil))
  (is (= ($ 2 1) 2))
  (is (= ($ 2 (mentry :a 1)) (mentry :a 2)))
  (is (= ($ 2 inc) 2))
  (is (= ($ inc 1) 2))
  (is (= ($ inc (mentry :a 1)) (mentry :a 2)))
  (is (= ($ (mentry :a inc) (mentry :a 1)) (mentry :a (mentry :a 2))))
  (is (= ($ (mentry :a inc) 2) (mentry :a 3)))
  (is (= ($ (mentry :a 1) inc) (mentry :a 1)))
  (is (= ($ inc [0 1]) [1 2]))
  (is (= ($ [inc dec] [0 1]) [[1 -1] [2 0]]))
  (is (= ($ (mentry :a inc) [1 2])) [(mentry :a 2) (mentry :a 3)]))

(testing "&"
  (is (= (& :bob 2) :bob))
  (is (= (& inc 2) 3))
  (is (= (& 1 inc) 1))
  (is (= (& nil :zux) :zux))
  (is (= (& 1 [0 0 0]) [1 0 0]))
  (is (= (& [inc dec] 1) 2))
  (is (= (& partial inc) inc))
  (is (= (& (mentry 2 inc) [0 0 0 0]) [0 0 1 0]))
  (is (= (& [inc dec inc dec] [0 0 0 0]) [1 -1 1 -1]))
  (is (= (& {:a inc :b dec} {:a 1 :b 1}) {:a 2, :b 0}))
  (is (= (& {2 inc} [0 0 0 0]) [0 0 1 0]))
  (is (= (& [[inc dec] [dec inc]] [1 2]) [[2 0] [1 3]]))
  (is (= (take 10 (& (repeat inc) (range))) (take 10 (next (range)))))
  (is (= (& {0 dec} #{1 2 3 4}) #{0 4 3 2})))

(testing "◊"
  (is (= (◊ 1 {0 2 :a 3}) 2))
  (is (= (◊ nil 1) nil))
  (is (= (◊ [] 1) [1]))
  (is (= (◊ [1 2 3] (list 3 4)) [3 4 3]))
  (is (= (◊ {} [:a :b :c]) {2 :c, 1 :b, 0 :a}))
  (is (= (◊ [] {:a 1 2 0}) [nil nil 0]))
  (is (= (◊ #{} [0 0 1]) #{0 1})))

(testing "b!"
  (let [x (b! [inc dec]
              (fn [x]
                {:fn  identity
                 :alt (fn [f] (conj x f))}))]
    [(§ dec x) (§ x 1) ($ 0 x)]))

;; composition -----------

(defn c [op & ops]
  (fn [x y]
    (if (seq ops)
      (cond
        (= op &) (& ($ #(partial (apply c ops) %) x) y)
        (= op f) (§ (partial (apply c ops) y) x)
        :else (op (partial (apply c ops) x) y))
      (op x y))))

(def f$ (c f $))
(def f& (c f &))
(def $$ (c $ $))
(def && (c & &))
(def &$ (c & $))
(def $& (c $ &))

(testing "c"
  (is (= (f$ [1 2 3] inc)))
  (is (= ($$ inc [[0 1] [2 3]])) [[1 2] [3 4]])
  (is (= (&$ {:a inc :b dec} {:a [1 2] :b [2 3]}) {:b [1 2] :a [2 3]}))
  (is (= (&& {:a [dec inc] :b [inc dec]} {:a [1 2] :b [2 3]}) {:a [0 3] :b [3 2]}))
  (is (= ((c & & $) {:a [dec inc]} {:a [[1 2] [1 2]]}) {:a [[0 1] [2 3]]}))
  (is (= ((c & $ $) {:a inc} {:a [[1 2] [3 4]]}) {:a [[2 3] [4 5]]}))
  (is (= (take 10 (&$ (repeat inc) (repeat (range 10)))) (take 10 (repeat (next (range 11)))))))

;; extras ----------------

(defn !
  "make a data always return itself when applied to anything"
  [x]
  (b! x (fn [x] {:fn (constantly x)})))

(testing "!"
  (is (= (§ (! [inc dec]) 1) [inc dec])))

(defn s>
  "stack"
  [& xs]
  (b! (t 'Stack xs)
      (fn [x]
        {:fn  #(reduce f % x)
         :alt #(apply s> (conj (vec x) %))})))

(testing "s>"
  (is (= (§ (s> inc dec inc) 0)) 1)
  (is (= (§ (§ (partial * 5) (s> inc dec inc)) 0) 5)))

(defn r>
  "reductions"
  [& xs]
  (b! (t 'Reductions xs)
      (fn [x]
        {:fn  #(reductions f % x)
         :alt #(s> x %)})))

(testing "r>"
  (is (= (§ (r> inc inc inc) 0) (list 0 1 2 3)))
  (is (= (§ (§ reverse (r> inc inc inc)) 0) (list 3 2 1 0))))

(defn cr>
  "cyclic reductions"
  [& xs]
  (b! (t 'CyclicReductions xs)
      (fn [x]
        {:fn  #(reductions f % (cycle x))
         :alt #(s> x %)})))

(testing "cr>"
  (is (= (take 10 (§ (cr> inc dec) 0)) (take 10 (cycle [0 1]))))
  (is (= (take 10 (§ (§ #(map inc %) (cr> inc dec)) 0)) (take 10 (cycle [1 2])))))

(comment

  "incubator experiments"

  ;; args

  (defn args* [& kvs]
    (let [ks (take-nth 2 kvs)
          find-key (into (apply hash-map (interleave ks ks)) (map-indexed vector ks))
          v (apply sorted-map kvs)]
      (b! v
          (fn [x]
            {:build (fn [y] (reduce #(assoc %1 (find-key (key %2)) (val %2)) y (filter (comp number? key) y)))}))))

  (testing "args*"
    #_(& [1 2] (args* :a 0 :b 0)))

  ;; function type

  (def ? '?)
  (defn ?? [x] (when (= ? x) true))
  (defn v [x] (:val (get-bs x)))

  (defn dfn* [v fun]
    (b! v
        (fn [x]
          {:build (fn [y] (dfn* (◊ v y) fun))
           :val   (if (seq (keep (comp ?? val) x)) x (fun x))
           :fn    (fn [y])})))

  (defmacro dfn [argv & body]
    (let [kws (mapv keyword argv)
          v (into {} (map #(mentry % ?) kws))]
      `(dfn* ~v (fn [x#] (apply (fn fun ~argv ~@body)
                                (mapv (fn [y#] (y# x#)) ~kws))))))

  (testing "dfn"
    (is (= 4 (v (& {:a inc :b 2} (& {:a 1} (dfn [a b] (+ a b)))))))
    ())

  (filter (comp number? key) (eseq> {0 1 :a 1})))