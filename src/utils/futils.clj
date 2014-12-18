(ns utils.futils)
 
(defmacro cfn 
  "curried anonymous function"
  [name args & body]
  (let [[name args body] 
        (if (symbol? name) 
          [name args body] 
          [(gensym) name (cons args body)])
        args-vecs (map #(vec (take % args)) (range 1 (count args)))
        ar0 `([] ~name)
        arities (map #(list % `(partial ~name ~@%)) args-vecs)]
    `(fn ~name 
       ~ar0 
       ~@arities 
       (~args ~@body))))

(defmacro def-curried [name args & body]
  `(def ~name (cfn ~name ~args ~@body)))

(defmacro c-expr [n]
  (let [args-syms (repeatedly (inc n) gensym)]
    `(cfn ~(vec args-syms) ~args-syms)))

(def f1 (c-expr 1))
(def f2 (c-expr 2))
(def f3 (c-expr 3))

(defn arg [x] #(% x))
(defn args [& xs] #(apply % xs))

(defn arg1 [x & _] x)
(defn arg2 [_ x & _] x)
(defn arg3 [_ _ x & _] x)

(def-curried zip2 [f a b] (map f a b))
(def-curried flip [f a b] (f b a))

(def id identity)
(def l list)
(defn typ= [a b] (= (type a)(type b)))
(defn hm [mentries] (apply hash-map (mapcat id mentries)))  

(defn >> [& xs] (apply comp (reverse xs)))