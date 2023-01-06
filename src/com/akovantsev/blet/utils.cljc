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

(def sconj (fnil conj #{}))

(defn loop*?   [form] (and (seq? form) (= (first form) 'loop*)))
(defn let*?    [form] (and (seq? form) (= (first form) 'let*)))
(defn case*?   [form] (and (seq? form) (= (first form) 'case*)))
(defn if?      [form] (and (seq? form) (= (first form) 'if)))
(defn finally? [form] (and (seq? form) (= (first form) 'finally)))
(defn catch?   [form] (and (seq? form) (= (first form) 'catch)))

(defn bodies->body [bodies]
  (if (-> bodies count (< 2))
    (first bodies)
    (cons 'do bodies)))


;(macroexpand '(case x 1 a 2 b c))
;js   (case* G__24 [[1] [2]] [a b] c))
;clj  (case* G__41 0 0 c {1 [1 a], 2 [2 b]} :compact :int))
;$ planck ;ClojureScript 1.10.914
;cljs.user=> (macroexpand '(case x a 2 b 4 c))
;   (let* [G__23 x] (cljs.core/cond (cljs.core/= 'a G__23) 2 (cljs.core/= 'b G__23) 4 :else c))
;cljs.user=> (macroexpand '(case x 1 a 2 b c))
;   (let* [G__24 x] (case* G__24 [[1] [2]] [a b] c))
#?(:clj (defn map-case-vals [fv branches]
          ;; {1 [1 a], 2 [2 b]} -> {1 [1 (fv a)], 2 [2 (fv b)]}
          (let [f2 (fn [[x y]] [x (fv y)])
                f+ (fn [pairs] (->> pairs (partition 2) (mapcat f2) (vec)))]
            (reduce-kv (fn rf [m k v] (assoc m k (f+ v))) (sorted-map) branches))))


(defn update-case [case-form sym-fn branch-fn default-fn]
  #?(:cljs (let [[case_ sym preds branches default] case-form]
             (list case_ (sym-fn sym) preds (mapv branch-fn branches) (default-fn default)))

     :clj  (let [[case_ sym x1 x2 default branches & tail] case-form]
             (apply list case_ (sym-fn sym) x1 x2 (default-fn default) (map-case-vals branch-fn branches) tail))))

(defn update-case-branches [form branch-f] (update-case form identity branch-f branch-f))


(defn update-try [form tail-fn]
  (let [[try_ & bodies-catches-finaly] form]
    (apply list try_ (map tail-fn bodies-catches-finaly))))

(defn update-catch [form bodies-f]
  (let [[catch_ ex_ sym & bodies] form]
    (apply list catch_ ex_ sym (bodies-f bodies))))

(defn update-finally [form bodies-f]
  (let [[finally_ & bodies] form]
    (apply list finally_ (bodies-f bodies))))



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

(defn get-sym-freqs [form]
  (->> form (tree-seq coll? seq) (filter symbol?) frequencies))

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



;; cant use pre/post walk, because:
;; 1) can't touch anything inside of (quote ...)
;; 2) need to skip levels in some forms:
;; (f (let [x expr] body))
;; then (let [x (f expr)] (f body)),
;; not  (let (f [x expr]) (f body))
(defn identity2 [x _] x)
(defn make-stateful-walker
  ([state-init seq-mm] (make-stateful-walker state-init seq-mm identity2))
  ([state-init seq-mm sym-f]
   (fn stateful-walker
     ([form] (stateful-walker form state-init))
     ([form state]
      (let [seq-f (-> seq-mm methods :default)]
        (cond
          (seq?  form)   (seq-mm form state)
          (vector? form) (vec (seq-f form state))
          (set? form)    (set (seq-f form state))
          (map? form)    (into (empty form) (apply hash-map (seq-f (into [] cat form) state)))
          (symbol? form) (sym-f form state)
          :else          form))))))


(defn make-stateless-walker
  ([seq-mm]
   (make-stateless-walker seq-mm identity))
  ([seq-mm sym-f]
   (fn stateless-walker [form]
     (let [seq-f (-> seq-mm methods :default)]
       (cond
         (seq?  form)   (seq-mm form)
         (vector? form) (vec (seq-f form))
         (set? form)    (set (seq-f form))
         (map? form)    (into (empty form) (apply hash-map (seq-f (into [] cat form))))
         (symbol? form) (sym-f form)
         :else          form)))))

;; the difference between walker and lifter:
;; walker's seq-f never changes type (vec/map/set) of input form
;; lifter might wrap input form, so coersion has to be applied by seq-f
(defn make-lifter [seq-mm seq-f]
  (fn lifter [form]
    (cond
      (seq? form)    (seq-mm form)
      (vector? form) (seq-f form vec)
      (set? form)    (seq-f form set)
      (map? form)    (seq-f (into [] cat form) #(into (empty form) (apply hash-map %)))
      :else          form)))