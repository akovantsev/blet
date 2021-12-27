(ns com.akovantsev.blet.test
  (:require
   [clojure.walk :as walk]
   [clojure.spec.alpha :as s]
   [clojure.core.specs.alpha :as cs]
   [com.akovantsev.blet.core :as core]
   [com.akovantsev.blet.impl :as impl]))


(defn assert= [msg x y]
  (assert (= x y)
    (str msg "\n"
      (with-out-str (clojure.pprint/pprint x))
      "\nnot=\n"
      (with-out-str (clojure.pprint/pprint y)))))


;; I failed to with-redefs `gensym` inside the `destructure`, so this
;; resets generated symbols' counts, so example-based tests would work:
(defn reset-gensym [form]
  (let [!n (atom {})
        r!  (fn replace [form]
              (if-let [[_ pref N] (re-matches #"(vec|seq|map|first)__(\d+)" (name form))]
                (let [n (or (get @!n N)
                          (let [len (count @!n)]
                            (swap! !n assoc N len)
                            len))]
                  (symbol (str pref "__" n)))
                form))
        f  (fn replacesym [form]
             (if (simple-symbol? form) (r! form) form))]
    (walk/postwalk f form)))


(let [form '(let [{:keys [:x ::y] [a b & tail] :z} {}])]
  (assert= "reset-gensym"
    (reset-gensym (macroexpand-1 form))
    (reset-gensym (macroexpand-1 form))))


(assert= "destructure support example 1"
  (reset-gensym
    (macroexpand-1
      '(core/blet [[x y & tail] (range 10)]
         (cond
           1 x
           2 y
           3 tail))))
  '(if 1
     (let [vec__0   (range 10)
           seq__1   (clojure.core/seq vec__0)
           first__2 (clojure.core/first seq__1)
           x        first__2]
       x)
     (if
      2 (let [vec__0   (range 10)
              seq__1   (clojure.core/seq vec__0)
              seq__1   (clojure.core/next seq__1)
              first__2 (clojure.core/first seq__1)
              y        first__2]
          y)
        (if 3
          (let [vec__0 (range 10)
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
     (let [y      2
           map__0 {}
           map__0 (if (clojure.core/seq? map__0)
                    (clojure.lang.PersistentHashMap/create (clojure.core/seq map__0))
                    map__0)
           x      (clojure.core/get map__0 :com.akovantsev.blet.test/x)
           a      (println x y)]
       a)
     (if 2
       (let [map__0 {}
             map__0 (if (clojure.core/seq? map__0)
                      (clojure.lang.PersistentHashMap/create (clojure.core/seq map__0))
                      map__0)
             x (clojure.core/get map__0 :com.akovantsev.blet.test/x)
             z (clojure.core/get map__0 :z)
             b (println x z)]
         b)
       (if 3
         (let [y      2
               map__0 {}
               map__0 (if (clojure.core/seq? map__0)
                        (clojure.lang.PersistentHashMap/create (clojure.core/seq map__0))
                        map__0)
               z      (clojure.core/get map__0 :z)
               c      (println y z)]
           c)
         nil))))


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



(def test-bindings
  '[{:keys [::x :z] :strs [yolo] :as foo :syms [sup]} 1
    zzz 10
    [{:as                                     as1
      top1                                    10,
      top2                                    10
      [v1 {:as v2 :keys [v2k1 :v2k2 ::v2k3]}] 2
      ::keys                                  [qkeys1 qkeys2]
      :strs                                   [str1 str2]
      ::syms                                  [qsyms1 qsyms2]
      :syms                                   [foo/sym1 sym2],
      :keys                                   [:deps.blet/keys1 :keys2 keys3]
      :or                                     {or1 1
                                               or2 2}}
     [v21 v22 & v2tail]
     v12
     & v1tail]    {}])