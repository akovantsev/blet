(ns com.akovantsev.blet.print
  (:require
   [clojure.string :as str]
   [com.akovantsev.blet.utils :as u]))


(def ^:dynamic *default-print-len* 32)


(defn pad [sym maxlen]
  (str/join
    (take maxlen
      (concat (when sym (name sym)) (repeat " ")))))

(defn print-start [file-and-line]
  (list 'clojure.core/prn (list 'quote (symbol (str/join (repeat 50 ".")))) (list 'symbol file-and-line)))


(defn printfn [tag cfg label val]
  (let [{::keys [blet-id maxlen]} cfg]
  ;; binding is here around every print statement, and not around entire blet!,
  ;; because binding inside e.g. loops causes "Cannot recur across try", see tests.
    (list 'binding ['clojure.core/*print-length* (list `or
                                                   'clojure.core/*print-length*
                                                   'com.akovantsev.blet.print/*default-print-len*)]
      (list 'clojure.core/prn blet-id (list 'quote (symbol (str "  " tag "  " (pad label maxlen) "  "))) val))))

(declare insert-prints)
(defn insert-prints-seq-default [form cfg] (map #(insert-prints % cfg) form))
(defn insert-prints-seq-let     [form cfg]
  (let [[let_ pairs & bodies] form
        body   (u/bodies->body bodies)
        __     (gensym "__")
        wrap   (fn [[sym expr]]
                 [__ (printfn (symbol "let ") cfg sym (list 'quote expr))
                  sym (insert-prints expr cfg)
                  __ (printfn (symbol "=   ") cfg sym sym)])
        pairs+ (->> pairs (partition 2) (mapcat wrap) (vec))]
    (list let_ pairs+
      (printfn (symbol "body") cfg nil (list 'quote body))
      (insert-prints body cfg))))


(defn insert-prints-seq-if [form cfg]
  (let [[if_ pred then else] form
        pred-sym (gensym "pred__")]
    (list 'do
      (printfn (symbol "if  ") cfg nil (list 'quote pred))
      (list 'let* [pred-sym (insert-prints pred cfg)]
        (printfn (symbol "=   ") cfg nil pred-sym)
        (list 'if pred-sym
          (printfn (symbol "then") cfg nil (list 'quote then))
          (printfn (symbol "else") cfg nil (list 'quote else)))
        (list if_ pred-sym
          (insert-prints then cfg)
          (insert-prints else cfg))))))


(defn insert-prints-seq-case [form cfg]
  (let [[case_ sym & _] form
        wrap-branch  (fn [prefix branch]
                       (list 'do
                         (printfn (symbol prefix) cfg nil (list 'quote branch))
                         (insert-prints branch cfg)))
        wrap-then   #(wrap-branch "then" %)
        wrap-else   #(wrap-branch "else" %)]
    (list 'do
      (printfn (symbol "case") cfg sym sym)
      (u/update-case form identity wrap-then wrap-else))))


(defn insert-prints-seq-do [form cfg]
  (if (-> form meta ::printed?)
    form
    (insert-prints-seq-default form cfg)))


(defmulti  insert-prints-seq      (fn [form cfg] (first form)))
(defmethod insert-prints-seq :default [form cfg] (insert-prints-seq-default form cfg))
(defmethod insert-prints-seq   'case* [form cfg] (insert-prints-seq-case form cfg))
(defmethod insert-prints-seq    'let* [form cfg] (insert-prints-seq-let form cfg))
(defmethod insert-prints-seq      'if [form cfg] (insert-prints-seq-if form cfg))
(defmethod insert-prints-seq      'do [form cfg] (insert-prints-seq-do form cfg))


(def insert-prints (u/make-stateful-walker nil insert-prints-seq))

(defn insert-prints-init [form orig file-line]
  (let [blet-id (list 'quote (gensym "blet__"))
        maxlen  (->> form
                  (tree-seq coll? seq)
                  (filter u/let*?)
                  (mapcat second)
                  (partition 2)
                  (map #(-> % first str count))
                  (reduce max 0))
        cfg     {::blet-id blet-id ::maxlen maxlen}]
    (list (vary-meta 'do assoc ::printed? true)
      (print-start file-line)
      (printfn (symbol "src ") cfg nil (list 'quote orig))
      (printfn (symbol "    ") cfg nil (list 'quote form))
      ;; cant print blet output ~ (let [bletsym expr] (print bletsym) bletsym)
      ;; due to "Can only recur from tail position" when blet! is inside loop:
      (insert-prints form cfg))))