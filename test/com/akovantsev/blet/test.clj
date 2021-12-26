(ns com.akovantsev.blet.test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.core.specs.alpha :as cs]
   [com.akovantsev.blet.core :as core]
   [com.akovantsev.blet.impl :as impl]))


(defn assert= [msg x y] (assert (= x y) (str msg "\n" (impl/spy x) "\nnot=\n" (impl/spy y))))


(assert= "require clojure 1.10. 1.9 has different core.specs dispatch keywords."
  (s/conform ::cs/binding-form '[a])
  '[:seq-destructure {:forms [[:local-symbol a]]}])


(let [form '[{:as x :or {y a z b}} s [d f] & tail]
      res (impl/get-locals form)]
  (assert= "locals extraction"
    (set (::impl/left-names res))
    (set '[x y z s s d f tail]))
  (assert= "deps extraction"
    (set (::impl/left-forms res))
    (set '[a b])))


(assert= "example"
  (macroexpand-1
    '(core/blet [x 1
                 y 2
                 z 3
                 t :foo
                 a (println x y)
                 b (println x z)
                 c (println y z)]
       (cond
         1 a
         2 b
         3 c)))
  '(if 1
     (let [x 1 y 2 a (println x y)] a)
     (if 2
       (let [x 1 z 3 b (println x z)] b)
       (if 3
         (let [y 2 z 3 c (println y z)] c)
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