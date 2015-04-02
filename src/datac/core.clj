(ns datac.core
  (:import [clojure.lang Counted Fn MapEntry IPersistentVector IPersistentList LazySeq IPersistentMap IPersistentSet Seqable Sequential])
  (:use utils.futils)
  (:require [alex-and-georges.debug-repl :as drepl]))

(defmacro dr [] `(drepl/debug-repl))

(set! *print-length* 5)

;; utils -------------

  (defn with-t [t x] (vary-meta x assoc :type t))
  
  (defn wtof [x y] (with-t (type x) y))
  (defn wmof [x y] (with-meta y (meta x)))

  (defmacro defmets [n & body]
    `(do ~@(map (fn [spec] `(defmethod ~n ~@spec)) 
                (partition 3 body))))
 
  (defn me [k v] (MapEntry. k v))
  (defn me? [x] (instance? MapEntry x))
  
;; eseq --------------
  
  (defn eseq> [x] (with-t ::eseq x))
  
  (defn eseq! 
    ([x] (get-in (meta x) [:behaviors :eseq]))
    ([x f] (vary-meta x assoc-in [:behaviors :eseq] f)))
  
  (def-curried >eseq [x y] (eseq! y (eseq! x)))
  
  (defmulti eseq-m type)
  
  (defmets eseq-m
    nil [x] () 
    ::eseq [x] x
    MapEntry [x] (list x)
    IPersistentMap [x] (if-let [s (seq x)] s ())
    IPersistentSet [x] (map me x x)
    Sequential [x] (map-indexed me x)
    :default [x] (list (me 0 x)))
  
  (defn eseq [x] 
    (eseq> 
      (if-let [e (:eseq (meta x))] 
        (e x) 
        (eseq-m x))))
  
  (defn eseq->hm [x]
    (reduce #(assoc %1 (key %2) (val %2)) {} x))
  
;; build -------------
  
  (defn <>! 
    ([x] (get-in (meta x) [:behaviors :<>]))
    ([x f] (vary-meta x assoc-in [:behaviors :<>] f)))
  
  (def-curried ><> [x y] (<>! y (<>! x)))

  (defmulti <>m (comp type arg1))

  (defmets <>m
    nil [x y] nil
    ::eseq [x y] (wmof x (eseq y))
    Fn [x y] (constantly y)
    MapEntry [x y] (first (eseq y))
    IPersistentVector [x y] (wmof x (mapv second (eseq y)))
    IPersistentSet [x y] (wmof x (set (keep second (eseq y))))
    IPersistentMap [x y] (wmof x (eseq->hm (eseq y)))
    IPersistentList [x y] (wmof x (seq (map second (eseq y))))
    LazySeq [x y] (wmof x (map second (eseq y)))
    Sequential [x y] (wmof x (map second (eseq y)))
    :default [x y] y)

  (def-curried <> [x y]
    (if-let [b (:<> (meta x))] 
      (b x y) 
      (<>m x y)))
 
;; alter -------------
  
  (defn alt! 
    ([x] (get-in (meta x) [:behaviors :alt]))
    ([x f] (vary-meta x assoc-in [:behaviors :alt] f)))
  
  (def-curried >alt [x y] (alt! y (alt! x)))
  
  (defmulti altm (comp type arg1))

  (defn alt [x f]
    (if-let [a (alt! x)]
      (a x f)
      (altm x f)))
    
  (defmethod altm :default [x f] (f x))
  
;; invok -------------

  (declare &> $> *> §>)

  (defn §! 
    ([x] (get-in (meta x) [:behaviors :§]))
    ([x f] (vary-meta x assoc-in [:behaviors :§] f)))
  
  (def-curried >§ [x y] (§! y (§! x)))

  (defmulti §m (comp type arg1))

  (defmets §m 
    nil [x y] y
    Fn [x y] (x y)
    MapEntry [x y] (§> (val x) y)
    Seqable [x y] ($> #(*> % y) x)
    :default [x _] x)

  (def-curried §> [x y] 
    (if-let [i (§! x)]
      (alt y (partial i x))
      (alt y (partial §m x))))

;; dist --------------
  
  (defn $! 
    ([x] (get-in (meta x) [:behaviors :$]))
    ([x appkw] (vary-meta x assoc-in [:behaviors :$] appkw)))
  
  (def-curried >$ [x y] ($! y ($! x)))

  (defmulti $m (comp type arg2))

  (defmets $m 
    nil [f x] nil
    MapEntry [f x] (me (key x) (*> f (val x)))
    Seqable [f x] (<> x ($> f (eseq x)))
    ::eseq [f x] (wmof x (eseq> (map ($> f) x)))
    :default [f x] (*> f x))

  (prefer-method $m ::eseq Seqable)

  (def-curried $> [f x]
    (if-let [m ($! x)]
      (m f x)
      ($m f x)))

;; zip ---------------

  (defn mape [f x]
    (eseq> (map f (eseq x))))
  
  (defn &1! 
    ([x] (get-in (meta x) [:behaviors :&1]))
    ([x appkw] (vary-meta x assoc-in [:behaviors :&1] appkw)))
  
  (def-curried >&1 [x y] (&1! y (&1! x)))
  
  (defn &2! 
    ([x] (get-in (meta x) [:behaviors :&2]))
    ([x appkw] (vary-meta x assoc-in [:behaviors :&2] appkw)))
  
  (def-curried >&2 [x y] (&2! y (&2! x)))
  
  (defmulti &m (comp type arg1))

  (defmets &m 
    IPersistentMap [x y] (<> y (mape #($> (get x (key %) nil) %) y))
    Counted [x y] (&m (<> {} x) y)
    LazySeq [x y] (<> y (mape #($> (nth x  (key %) nil) %) y))
    :default [x y] (&m {0 x} y))
  
  (def-curried &> [x y]
    (if-let [z (or (&2! y) (&1! x))] 
      (z x y)
      (if (coll? y)
        (&m x y)
        (val (first (&m x y))))))
  
;; plus --------------

  (defn +! 
    ([x] (get-in (meta x) [:behaviors :+]))
    ([x appkw] (vary-meta x assoc-in [:behaviors :+] appkw)))
  
  (def-curried >+ [x y] (+! y (+! x)))
  
  (defmulti +m (comp type arg2))
  
  (defmets +m 
    Fn [x y] (if (fn? x) (juxt x y) [x y])
    java.lang.Number [x y] (if (number? x) (+ x y) [x y])
    String [x y] (if (string? x) (str x y) [x y])
    Seqable [x y] (<> y (eseq> (concat (eseq x) (eseq y))))
    :default [x y] [x y])
  
  (def-curried +> [x y]
    (if-let [p (+! x)] 
      (p x y)
      (+m x y)))

;; zero --------------
  
  (defn o! 
    ([x] (get-in (meta x) [:behaviors :o]))
    ([x appkw] (vary-meta x assoc-in [:behaviors :o] appkw)))
  
  (def-curried >o [x y] (o! y (o! x)))
  
  (defmulti om type)
  
  (defmets om
    Fn [x] identity
    java.lang.Number [x] 0
    String [x] ""
    Seqable [x] (<> x [])
    :default [x] x)
  
  (defn o> [x]
    (if-let [zf (o! x)]
      (zf x)
      (om x)))
  
;; wild --------------
  
  (defn *! 
    ([x] (get-in (meta x) [:behaviors :*]))
    ([x appkw] (vary-meta x assoc-in [:behaviors :*] appkw)))
  
  (def-curried >* [x y] (*! y (*! x)))

  (defmulti *m (comp type arg1))
  
  (defmethod *m :default [x y] (§> x y))
  
  (def-curried *> [x y] 
    (if-let [*f (*! x)]
      (*f x y)
      (*m x y)))

;; behaviors sharing --

  (defn b! 
    ([x] (:behaviors (meta x)))
    ([x bm] (vary-meta x assoc :behaviors bm)))

  (def-curried >b [x y]
    (vary-meta x assoc :behaviors (merge (b! x) (b! y))))
  
  (def-curried >b! [x y]
    (vary-meta x assoc :behaviors (b! y)))

;; flipped ------------

  (def <§ (flip §>))
  (def <* (flip *>))
  (def <$ (flip $>))
  (def <& (flip &>))
  (def <+ (flip +>))

;; wtf ----------------

  (def-curried $$> [a b] ($> ($> $> a) b))
  (def-curried $&> [a b] ($> ($> &> a) b))
  (def-curried *$> [a b] (*> ($> $> a) b))
  (def-curried *&> [a b] (*> ($> &> a) b))
  (def-curried &$> [a b] (&> ($> $> a) b))
  (def-curried &&> [a b] (&> ($> &> a) b))

;; extras -------------
  
  ; (defn s> 
  ;   "stack"
  ;   [& xs]
  ;   (with-meta xs 
  ;    {:invok #(reduce <* %2 %1)
  ;     :alt #(apply s> %2 %1)}))
  
  ; (defn r> 
  ;   "reductions"
  ;   [& xs]
  ;   (with-meta xs 
  ;    {:invok #(reductions <* %2 %1)
  ;     :alt #(s> (partial invok %1) %2)}))
  
  ; (defn cr> 
  ;   "cyclic reductions"
  ;   [& xs]
  ;   (with-meta xs 
  ;    {:invok #(reductions <* %2 (cycle %1))
  ;     :alt #(s> (partial invok %1) %2)}))
  
  ; (defn zm 
  ;   "zipped hash-map"
  ;   [& pairs]
  ;   (& (apply hash-map pairs)))
  
  ; (defn nm 
  ;   "nested hash-map
  ;   (nm [:a :b] 1 :c 2) => {:c 2, :a {:b 1}}"
  ;   [& pairs]
  ;    (reduce 
  ;     (fn [acc [cs v]] (assoc-in acc cs v))
  ;     {}
  ;     (map (fn [[a b]] [(if (vector? a) a [a]) b]) 
  ;          (partition 2 pairs))))
  
  ; (defn- in-helper [op & pairs]
  ;   (apply s>
  ;     (map 
  ;       (fn it [[[x & xs] v]] 
  ;         (zm x (if (seq xs) (it [xs (op v)]) (op v))))
  ;       (map (fn [[a b]] [(if (vector? a) a [a]) b]) 
  ;            (partition 2 pairs)))))
  
  ; (defn *>> [x & pairs] (*> (apply in-helper *> pairs) x))
  ; (defn f>> [x & pairs] (*> (apply in-helper f> pairs) x))
  ; (defn $>> [x & pairs] (*> (apply in-helper $ pairs) x))
  ; (defn &>> [x & pairs] (*> (apply in-helper & pairs) x))
  
  ; (defn squad 
  ;   "group of elements that defer invocation and alteration to their leader
  ;   (*> (squad [inc dec dec]) 1) => 2
  ;   (*> inc (squad [0 1 2])) => 1
  ;   (*> inc (squad last [12 2 3 8])) => 9"
  ;   ([x] 
  ;    (cond 
  ;     (sequential? x) ((squad first) x)
  ;     (fn? x)
  ;     (fn [xs] 
  ;      (with-meta xs
  ;       {:alt #(*> %2 (x xs))
  ;        :invok #(*> (x xs) %2)}))))
  ;   ([lead-finder xs] ((squad lead-finder) xs)))
  
  ; (comment 
  ;   (f>> {:a {:b 1} :c 2} [:a :b] inc :c dec)
  ;   (*>> {:a {:b 1} :c 2} [:a :b] inc :c dec)
  ;   ($>> {:a {:b [1 1]} :c [6 2]} [:a :b] inc :c dec)
  ;   (&>> {:a {:b [1 1]} :c [6 2]} [:a :b] [inc dec] :c [dec inc])
    
  ;   (*> (squad [inc dec dec]) 1)
  ;   (*> inc (squad [0 1 2]))
  ;   (*> inc (squad last [12 2 3 8]))
  ;   )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
