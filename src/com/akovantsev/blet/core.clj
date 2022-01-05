(ns com.akovantsev.blet.core
  (:require [com.akovantsev.blet.impl :as impl]))


(def ^:dynamic *default-print-len* 32) ;;fixme: move it inside the macro, and make available in cljs too


(defn -get-destructure-fn [env]
  ;; thanks! https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html#gotcha-4-clj-macros
  (if (:ns env)
    (resolve 'cljs.core/destructure)
    (resolve 'clojure.core/destructure)))


(defmacro blet
  "Put all the bindings you want into single `let`,
   and then write compact readable `cond`.
   ```
   (blet [x 1
          y 2
          z 3
          t :unused
          a (println x y)
          b (println x z)
          c (println y z)]
     (cond
       1 a
       2 b
       3 c)))
   ```
   Blet will rearrange things for you, and declare
   things just in time. Even side effects (don't push it tho).
   ```
   (if 1
     (let [x 1 y 2 a (println x y)] a)
     (if 2
       (let [x 1 z 3 b (println x z)] b)
       (if 3
         (let [y 2 z 3 c (println y z)] c)
         nil)))
  ```
  "
  [bindings cond-form]
  (let [form# (binding [impl/*destructure* (-get-destructure-fn &env)]
                (impl/blet bindings cond-form))]
    `~form#))


(defmacro blet!
  [bindings cond-form]
  (let [form# (binding [impl/*destructure* (-get-destructure-fn &env)]
                (impl/blet bindings cond-form {::impl/print? true}))]
    `(binding [*print-length* (or *print-length* ~*default-print-len*)]
       ~form#)))
