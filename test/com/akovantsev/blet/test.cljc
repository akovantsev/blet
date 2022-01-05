(ns com.akovantsev.blet.test
  (:require
   [clojure.walk :as walk]
   [#?(:cljs cljs.pprint :clj clojure.pprint) :refer [pprint]]
   [com.akovantsev.blet.core :as core]))


(def GENSYM-RE #"(vec|seq|map|first)__(\d+)")

(defn assert= [msg x y]
  (assert (= x y)
    (str msg "\n"
      (with-out-str (pprint x))
      "\nnot=\n\n"
      (with-out-str (pprint y)))))


;; I failed to with-redefs `gensym` inside the `destructure`, so this
;; resets generated symbols' counts, to make example-based tests work:
(defn reset-gensym [form]
  (let [!n  (atom {})
        r!  (fn replace [form]
              (if-let [[_ pref N] (re-matches GENSYM-RE (name form))]
                (let [n (or (get @!n N)
                          (let [len (count @!n)]
                            (swap! !n assoc N len)
                            len))]
                  (symbol (str pref "__" n)))
                form))
        f  (fn replacesym [form]
             (if (and (symbol? form) (nil? (namespace form))) (r! form) form))]
    (walk/postwalk f form)))



(assert= "reset-gensym"
  ;; copypaste because of macroexpand-1 implementation in cljs
  (reset-gensym (macroexpand-1 '(let [{:keys [:x ::y] [a b & tail] :z} {}])))
  (reset-gensym (macroexpand-1 '(let [{:keys [:x ::y] [a b & tail] :z} {}]))))


(assert= "destructure support example 1"
  (reset-gensym
    (macroexpand-1
      '(core/blet [[x y & tail] (range 10)]
         (cond
           1 x
           2 y
           3 tail))))
  '(if 1
     (let* [vec__0   (range 10)
            seq__1   (clojure.core/seq vec__0)
            first__2 (clojure.core/first seq__1)
            x        first__2]
       x)
     (if
      2 (let* [vec__0   (range 10)
               seq__1   (clojure.core/seq vec__0)
               seq__1   (clojure.core/next seq__1)
               first__2 (clojure.core/first seq__1)
               y        first__2]
          y)
        (if 3
          (let* [vec__0 (range 10)
                 seq__1 (clojure.core/seq vec__0)
                 seq__1 (clojure.core/next seq__1)
                 seq__1 (clojure.core/next seq__1)
                 tail seq__1]
            tail)
          nil))))


(assert= "example"
  (reset-gensym
    (macroexpand-1
      '(core/blet [x 1
                   y 2
                   z 3
                   {:keys [::x :z] :strs [yolo] :as foo :syms [sup]} {}
                   t :foo
                   a (println x y)
                   b (println x z)
                   c (println y z)]
         (cond
           1 a
           2 b
           3 c))))
  '(if 1
     (let* [y      2
            map__0 {}
            map__0 #?(:clj (if (clojure.core/seq? map__0)
                             (clojure.lang.PersistentHashMap/create (clojure.core/seq map__0))
                             map__0)
                      :cljs (if (cljs.core/implements? cljs.core/ISeq map__0)
                              (clojure.core/apply cljs.core/hash-map map__0)
                              map__0))
            x      (#?(:clj clojure.core/get :cljs cljs.core/get) map__0 :com.akovantsev.blet.test/x)
            a      (println x y)]
       a)
     (if 2
       (let* [map__0 {}
              map__0 #?(:clj (if (clojure.core/seq? map__0)
                               (clojure.lang.PersistentHashMap/create (clojure.core/seq map__0))
                               map__0)
                        :cljs (if (cljs.core/implements? cljs.core/ISeq map__0)
                                (clojure.core/apply cljs.core/hash-map map__0)
                                map__0))
              x (#?(:clj clojure.core/get :cljs cljs.core/get) map__0 :com.akovantsev.blet.test/x)
              z (#?(:clj clojure.core/get :cljs cljs.core/get) map__0 :z)
              b (println x z)]
         b)
       (if 3
         (let* [y      2
                map__0 {}
                map__0 #?(:clj (if (clojure.core/seq? map__0)
                                 (clojure.lang.PersistentHashMap/create (clojure.core/seq map__0))
                                 map__0)
                          :cljs (if (cljs.core/implements? cljs.core/ISeq map__0)
                                  (clojure.core/apply cljs.core/hash-map map__0)
                                  map__0))
                z      (#?(:clj clojure.core/get :cljs cljs.core/get) map__0 :z)
                c      (println y z)]
           c)
         nil))))


(assert= "test code elimination with locals shadowing 1"
  (macroexpand-1
    '(core/blet [x 1 y (fn [x] x)] (cond 1 (y 1))))
  '(if 1 (let* [y (fn [x] x)] (y 1)) nil))


(assert= "locals shadowing 2"
  (macroexpand-1
    '(core/blet [x 1 y (let [x 2] x)] (cond 1 (y 1))))
  '(if 1 (let* [y (let [x 2] x)] (y 1)) nil))


(assert= "locals shadowing 3"
  (macroexpand-1
    '(core/blet [x 1 y (binding [x 2] x)] (cond 1 (y 1))))
  '(if 1 (let* [y (binding [x 2] x)] (y 1)) nil))


(assert= "locals shadowing 4"
  (macroexpand-1
    '(core/blet [x 1 y (for [x [1 2]] x)] (cond 1 y)))
  '(if 1 (let* [y (for [x [1 2]] x)] y) nil))


(assert= "locals shadowing 5"
  (macroexpand-1
    '(core/blet [x 1 y (fn x [a] a)] (cond 1 (y 1))))
  '(if 1 (let* [y (fn x [a] a)] (y 1)) nil))


(assert= "dont eval let bindings before cond dispatch"
  2
  (core/blet [A (atom 0)
              a (do (swap! A + 1) @A)
              b (do (swap! A + 2) @A)
              c (do (swap! A + 3) @A)]
    (cond
      false a
      true  b
      false c)))


(assert= "dont eval let bindings before cond dispatch"
  (+ 0 10 20 2)
  (core/blet [A (atom 0)
              a (do (swap! A + 1) @A)
              b (do (swap! A + 2) @A)
              c (do (swap! A + 3) @A)]
    (cond
      ;; plz don't ever do it tho, omg.
      (do (swap! A + 10) false) a
      (do (swap! A + 20) true)  b
      (do (swap! A + 50) false) c)))


(println "Tests are done with asserts, so if this is printed out – all is good.")