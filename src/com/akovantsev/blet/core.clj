(ns com.akovantsev.blet.core
  (:require
   [clojure.walk :as walk]
   [clojure.string :as str]
   [com.akovantsev.blet.impl2 :as impl]
   [cyrik.cljs-macroexpand.expand :as m]))


(def NOPRINT identity)


(defn macroexpand-all
  "Like `cyrik.cljs-macroexpand.expand/macroexpand-all`, but does not expand quoted forms"
  ([form & [env]]
   (binding [m/*environment* (or env m/*environment*)]
     (let [md       (meta form)
           quoted?  (and (seq? form) (-> form first (= 'quote)))
           form*    (if-not (seq? form)
                      form
                      (try
                        (m/normalized-macroexpand form)
                        (catch ClassNotFoundException e form)))
           expanded (if-not quoted?
                      (walk/walk macroexpand-all identity form*)
                      form*)]
       (if md
         (m/merge-meta expanded (macroexpand-all md))
         expanded)))))


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
  (let [form#  (list 'let bindings (cons :com.akovantsev.blet/BODY (cons body bodies)))
        form2# (macroexpand-all form# (or &env {}))]
    (impl/blet &form form2# false nil)))


(defmacro blet!
  [bindings body & bodies]
  (let [form#  (list 'let bindings (cons :com.akovantsev.blet/BODY (cons body bodies)))
        form2# (macroexpand-all form# (or &env {}))
        ;; https://stackoverflow.com/a/10958098
        line#  (format "%s %s"
                 (str/join "/" (take-last 3 (str/split *file* #"/")))
                 (:line (meta &form)))]
    (impl/blet &form form2# true line#)))