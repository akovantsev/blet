(ns com.akovantsev.blet.core
  (:require
   [clojure.string :as str]
   [com.akovantsev.blet.impl2 :as impl]
   #?(:clj  [clojure.walk :as walk]
      :cljs [com.akovantsev.blet.fake :as walk])
   #?(:clj  [cyrik.cljs-macroexpand.expand :as m]
      :cljs [com.akovantsev.blet.fake :as m]))
  #?(:cljs (:require-macros [com.akovantsev.blet.core :refer [blet blet!]])))

#?(:clj
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
      [bindings body & bodies]
      (let [form#  (list 'let bindings (cons ::impl/BODY (cons body bodies)))
            form2# (if-not (:ns &env)
                     (walk/macroexpand-all form#)
                     (binding [m/*environment* &env]
                       (m/macroexpand-all form#)))]
        (impl/blet &form form2# false nil))))

#?(:clj
    (defmacro blet!
      [bindings body & bodies]
      (let [form#  (list 'let bindings (cons ::impl/BODY (cons body bodies)))
            form2# (if-not (:ns &env)
                     (walk/macroexpand-all form#)
                     (binding [m/*environment* &env]
                       (m/macroexpand-all form#)))
            ;; https://stackoverflow.com/a/10958098
            line#  (format "%s %s"
                     (str/join "/" (take-last 3 (str/split *file* #"/")))
                     (:line (meta &form)))]
        (impl/blet &form form2# true line#))))