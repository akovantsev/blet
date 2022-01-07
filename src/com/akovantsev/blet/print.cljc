(ns com.akovantsev.blet.print
  (:require [clojure.string :as str]))


(def space (list 'quote (symbol " ")))

(defn pad [sym maxlen]
  (list 'quote
    (if-not maxlen
      sym
      (symbol
        (str/join
          (take maxlen
            (concat (when sym (name sym)) (repeat " "))))))))


(defmulti tag identity)
(defmethod tag ::binding-form   [_] (symbol ":=="))
(defmethod tag ::binding-evaled [_] '===)
(defmethod tag ::pred-form      [_] '???)
(defmethod tag ::branch-form    [_] '>>>)


(defmulti printfn (fn printfn-dispatch [tag blet-id maxlen label val] tag))
(defmethod printfn :default [k blet-id maxlen label val]
  (list 'clojure.core/prn blet-id space (list 'quote (tag k)) space (pad label maxlen) space val))

(defmethod printfn ::start [k blet-id _ _ _]
  (list 'clojure.core/prn (list 'quote (symbol (str/join (repeat 50 "."))))))
