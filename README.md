# Datac

A clojure library for data combinations


## Overview

```
(require datac.core)
```

### invocation (sort of)

the ```§>``` operator represent invocation, it takes any combination of two arguments, and sort of invoke the first arg on the second arg

- for functions nothing fancy

```clj
(§> inc 1) ;=> 2
```

- for nil it's like identity function

```clj
(§> nil :anything) ;=> :anything
``` 

- for core collections types:

```clj
(§> [inc dec] 0) ;=> [1 -1]

(§> {:a inc :b dec} 0) ;=> {:b -1, :a 1}

(§> (list inc dec) 0) ;=> (1 -1) 

(§> #{inc dec} 0) ;=> #{1 -1}
```

- for anything else, just ignore second argument 

```clj
(§> 1 :bar) ;=> 1
``` 

### distribution 

the ```$>``` operator is like clojure.core/map but preserve context, and use ```*>``` (that we will explain later) instead of normal invocation.

- on non sequential types it behaves as §>

```clj
($> :foo 2) ;=> :foo

($> inc 2) ;=> 3

($> nil 2) ;=> 2
```

- on core collections:

```clj
($> inc [1 2 3]) ;=> [2 3 4]

($> inc {:a 1 :b 2}) ;=> {:a 2, :b 3}

($> inc (range)) ;=> (1 2 3 4 5 ...)

($> inc #{1 2 3}) ;=> #{4 3 2}
```
- taking advantage of new core collection's invocation behavior

```clj
($> {:a inc :b dec} (range))
;=> ({:b -1, :a 1} {:b 0, :a 2} {:b 1, :a 3}...)

($> [inc dec] {:a 1 :b 2})
;=> {:a [2 0], :b [3 1]}
```

### zipping 

the ```&>``` operator is for zipping together two pieces of data

- with non sequential data it behave like ```§>```

```clj
(&> :bob 2) ;=> :bob

(&> inc 2) ;=> 3

(&> nil :zux) ;=> :zux
```

- when only one of the arg is sequential it do what it can

```clj
(&> 1 [0 0 0]) ;=> [1 0 0]
(&> [inc dec] 1) ;=> 2
```

- when the two args are collections 

```clj
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
```

### adding 

the ```+>``` operator is for adding together two pieces of data

```clj
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
  ```

### zero 

the ```o>``` return the identity value of any data 

```clj
(o> 42) ;=> 0 
(o> [1 2 3]) ;=> []
(o> #{1 2 3}) ;=> #{}
(o> {:a 1}) ;=> {}
(o> (range)) ;=> ()
(o> "hi!") ;=> ""
(o> inc) ;=> #<core$identity clojure.core$identity@306f6c8>
```

### wrapping

the ```<>``` put the value of his second arg into the context of his first arg,
you may not often use it directly, but the implementation of distribution and zipping rely on it, and when we'll see how to extend our own types to datac operators it will be useful.

- on nil it returns nil

```clj
(<> nil 1) ;=> nil
```
- on functions it wrap second arg in a function that always returns it

```clj
(<> inc 1) ;=> (constantly 1)

```
- on core collections  

```clj  
(<> [] 1) ;=> [1]

(<> [1 2 3] (list 1 2))
;=> [1 2]

(<> {} [:a :b :c])
;=> {2 :c, 1 :b, 0 :a}

(<> #{} [0 0 1])
;=> #{0 1}

```

- on anything else it will return second args unchanged

```clj
(<> 1 inc) ;=> inc
(<> "aze" [1 2]) ;=> [1 2]
```

## more...

all those operators have their flipped equivalents

```clj
(= ($> inc [1 2 3]) 
   (<$ [1 2 3] inc))
```

and are curried 

```clj
(= (($> inc) [1 2 3])
   ($> inc [1 2 3]))
```

### the wild operator

by default the ```*>``` operator behave like ```§>```

but you can assign a wild behavior to any piece of data via the ```*!``` function

```clj
(let [zm (*! {:a inc :b dec} &>)]
  (*> zm {:a 1 :b 1})) 
;=> {:a 2, :b 0}
```

the point of this operator may not be clear but I find that it yields to powerful flexibility when starting to define functions relying on it.

## usage beyond defaults

all of this is implemented with multimethods and metadata, and can be extended to any data instance or type

### extension table 

op | multi | get/set | share | keyword
:-: | :-: | :-: | :-: | :-:
§> | §m | §! | >§ | :§
$> | $m | $! | >$ | :$
&> | &m | &! | >& | :&
+> | +m | +! | >+ | :+
o> | om | o! | >o | :o
<> | <>m | <>! | ><> |:<>
alt | altm |alt! | >alt | :alt

### instance level extension

at the instance level, behaviors are added to metadata under the :behaviors key. 

you can directly conj a behavior into metadata like this:

```
; here we specify §> behavior for my-data 
(vary-meta my-data assoc-in [:behaviors :§] (fn [x y] ...))
```

or with the corresponding setter:

```
; same here with 
(§! my-data (fn [x y] ...))
```  

it is possible to insert several behaviors at the time like this:

```
(b! my-data 
  {:§ (fn [x y] ...)
   :$ (fn [x y] ...)})
```

or pass a behavior from a data to another 

```
; here we pass invocation behavior of x to y
(>§ x y)
```

or merge all behaviors of x into y's

```
(>b x y)
```

or replace y's behaviors by x's 

```
(>b! x y)
```

maybe sometimes you just want to get a specific behavior from x 

```
(§! x) 
```

or all behaviors 

```
(b! x)
```

#### invocation 

you can redefine the way that a piece of data is invoked via the ```§!``` function
it takes 2 arguments:

- the piece of data for which you want to redefine invocation 
- the invocation function
  
```clj
(let [dumb-vec (§! [] 
               (fn [this that] 
                 (str "i'm " this " , when given " that " I return a dumb str")))]
  (§> dumb-vec 1))

;=> "i'm [] , when given 1 I return a dumb str"
```
  
when called with only 1 arg, ```§!``` return the current implementation of ```§>```
  
#### distribution

same here with the ```$!``` function that takes the instance to extend and the new implementation.
the implementation must be a function of 2 arguments 
  - the function to distribute over your data 
  - your data
  
```clj
(let [hid-cyc ($! [1 2] (fn [f this] (map f (cycle this))))]
  ($> inc hid-cyc))

;=> (2 3 2 3 2 ...)
```
  
#### zipping 

zipping is a bit more complicated, because you may want to be able to dispatch on either the first or second argument, that's why their is 2 different way of extending to ```&>```
  
```clj
;; when you want to extend your instance as the first argument of &> use &1!

(let [cyclic-zip (&1! [inc dec] #(&> (cycle %1) %2))]
  (&> cyclic-zip (range)))
;=> (1 0 3 2 5 ...)

;; else $2!

(let [x (&2! [1 2 3] (fn [x y] :up-to-you))]
  (&> [inc dec] x))
;=> :up-to-you 

;; if both 1st and 2nd arguments have custom implementations, the 2nd's one will be used

```
  
#### alteration 
  
the ```alt!``` function let you define the way that an arbitrary function is applied on your data, here a dumb exemple that just make the data unalterable

```clj
(let [unalterable (alt! [1 2] (fn [this f] this))]
  (§> set unalterable))

;=> [1 2]
```
  
#### wrapping

```clj
(let [fixed-keys (<>! {:a 1 :b 2} 
                      (fn [x y] (select-keys y (keys x))))]
  (<> fixed-keys {:a 3 :b 4 :c 5}))

;=> {:a 3, :b 4} 
```

#### several at once 

```clj
(b! my-data
  {:$ (fn [x y] ...)
   :§ (fn [x y] ...)})
```
  
### type based extension
  
if you want to extend a whole type to any datac operator, you can do it via multimethods
note that it dispatch on type and not on class, so you can avoid to define new types, and just rely on a type tag in metadata, like the example below

```clj
(defn foo 
  "assign the type tag 'foo to x"
  [x] (vary-meta x assoc :type 'foo))

;now we can impl $m for 'foo instances
(defmethod $m 'foo [f x]
  (println "pouet"))

($> inc (foo [1 2 3])) ;=> prints "pouet"
```
  
- all main operations can be extended like this, see op/multi table below
  
op | multi | get/set | share | keyword
:-: | :-: | :-: | :-: | :-:
§> | §m | §! | >§ | :§
$> | $m | $! | >$ | :$
&> | &m | &! | >& | :&
+> | +m | +! | >+ | :+
o> | om | o! | >o | :o
<> | <>m | <>! | ><> |:<>
alt | altm |alt! | >alt | :alt

### behavior sharing 

It is possible to share behaviors between data

- you can either pass a specific implementation from one data to another 

```clj
(>$ x y) ;=> set the $> implementation of x to be the same as y
```

the same is possible with ```>&```, ```>§```, ```>+```, ```>o``` and ```>*```

- you can merge all the behaviors of x into y's behaviors 

```clj
(>b x y)
```

- or replace y behaviors by x behaviors 

```clj
(>b! x y)
```


### eseq abstraction

the eseq abstraction is the common denominator in datac, all data can be expressed via it.
eseq stands for "map-entries sequence", let's see it in action: 

- on collections 

```clj
(eseq [1 2 3]) ;=> ([0 1] [1 2] [2 3])
(eseq (list 1 2 3)) ;=> ([0 1] [1 2] [2 3])
(eseq (range)) ;=> ([0 0] [1 1] [2 2] ...)
(eseq #{:aze :ert}) ;=> ([:aze :aze] [:ert :ert])
(eseq {:a 1 :b 2}) ;=> ([:a 1][:b 2])
```

- on nil 

```clj
(eseq nil) ;=> ()
```

- on non sequentials

```clj
(eseq 1) ;=> ([0 1])
(eseq (MapEntry. :a 0)) ;=> ([:a 0])
```

All main operators implementations lays on this abstraction, so by extending your type to it, you can benefit from almost all of datac functionalities.

like operators and properties, you can extand any instance or type to it: 

- instance extension 

```clj
;another dumb example, a vector that eseq itself reversed
(let [rvec (eseq! [1 2 3] #(eseq (reverse %)))]
  (eseq rvec))
  
;=> ([0 3][1 2][2 1])
```

- type extension 

```clj
(defmethod eseqm 'foo [x] 
  (map vector (map - (range)) x))

(eseq (foo [1 2 3 4])) 
;=> ([0 1] [-1 2] [-2 3] [-3 4])
```

## Motivation

I like clojure core data types a lot and want to use them as often as possible, being capable to just insert a behavior in it when needed really seems to make sense to me at the moment.

Avoiding to use defrecord and deftype when not really needed

A way to represent computations as raw data instead of functions
 
```clj
(def stats {:max (partial apply max) 
            :min (partial apply min) })

(§> stats [0 1 2 3 4]) 
;=> {:min 0, :max 4}

(§> (assoc stats :sum (partial reduce +))
    [0 9 6 78 2])
;=> {:min 0, :max 78, :sum 95}
    
```

## more concrete example 

Sometimes we want treat functions as values, but sometimes we want to treat them as their result value, here is two way to do it 

```clj
(defn pv [f]
  (b! f 
   {:alt (fn [this f] (pv (comp (§> f) this)))
    :$   (fn [f this] (pv (comp ($> f) this)))
    :&2  (fn [this x] (pv (comp (&> x) this)))}))
```
or: 

```clj
(defn pv [f]
  (-> f 
   (alt! (fn [this f] (pv (comp (§> f) this))))
   ($!   (fn [f this] (pv (comp ($> f) this))))
   (&2!  (fn [this x] (pv (comp (&> x) this))))))
   
```

and in both case it should work:

```clj
((pv inc) 1)
((§> inc (pv inc)) 2)
(($> inc (pv vector)) 2 3 4)
((&> [inc dec inc] (pv vector)) 2 3 4)

```





