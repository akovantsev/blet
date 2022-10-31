(ns com.akovantsev.blet.utils
  #?(:clj (:import [clojure.lang PersistentQueue]))
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [#?(:clj clojure.pprint :cljs cljs.pprint) :refer [pprint]]))


(defn spy [x] (pprint x) x)

(defn assert=
  [msg x y]
  (assert (= x y)
    (str msg "\n"
      (with-out-str (pprint x))
      "\nnot=\n\n"
      (with-out-str (pprint y)))))


(defn queue [init]
  #?(:cljs (into #queue[] init)
     :clj  (into PersistentQueue/EMPTY init)))

;; cant use postwalk anywhere because need to detect and dont touch quoted forms:
(defn quoted? [form] (and (seq? form) (= (first form) 'quote)))
(defn prewalk [form f]
  (if (quoted? form)
    form
    (walk/walk #(prewalk % f) identity (f form))))


(defn fixed-point [xs]
  (reduce #(if (= %1 %2) (reduced %1) %2) xs))

(defn until-fixed-point [x f]
  (->> x (iterate f) (fixed-point)))

(defn get-syms [form]
  (->> form (tree-seq coll? seq) (filter symbol?) set))

(defn any-pred? [x & preds]
  (reduce #(if (%2 x) (reduced true) false) false preds))


(defn split-by [pred coll]
  (loop [before []
         todo coll]
    (if (empty? todo)
      [before nil nil]
      (let [[x & tail] todo]
        (if (pred x)
          [before x (vec tail)]
          (recur (conj before x) tail))))))

(assert (= (split-by #{1} [0 0 1 0 0 1 0]) [[0 0] 1 [0 0 1 0]]))


(defn reset-gensym [form]
  (let [!n  (atom {})
        r!  (fn replace [sym]
              (let [[prefix suffix] (-> sym name (str/split #"__+" 2))]
                (if-not suffix
                  sym
                  (let [n (or (get @!n sym)
                            (let [len (count @!n)]
                              (swap! !n assoc sym len)
                              len))]
                    (symbol (str prefix "__" n))))))]
    (prewalk form
      (fn replacesym [form]
        (if (and (symbol? form) (nil? (namespace form))) (r! form) form)))))
