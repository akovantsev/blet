(ns com.akovantsev.blet.core
  (:require
    [com.akovantsev.blet.impl :as impl]
    [com.akovantsev.blet.env :as env])
  #?(:cljs (:require-macros [com.akovantsev.blet.core :refer [blet blet!]])))


(defn -get-destructure-fn [env]
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
  (let [form# (binding [env/*js?* (env/js? &env)]
                (impl/blet bindings cond-form))]
    `~form#))


(defmacro blet!
  [bindings cond-form]
  (let [form# (binding [env/*js?* (env/js? &env)]
                (impl/blet bindings cond-form {::impl/print? true}))]
    `~form#))
