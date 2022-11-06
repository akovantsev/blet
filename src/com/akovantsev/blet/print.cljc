(ns com.akovantsev.blet.print
  (:require
   [clojure.string :as str]
   [com.akovantsev.blet.utils :as u]))


(def ^:dynamic *default-print-len* 32)

;; Forms are no longer inlined in print statements because
;; it was too easy to hit jvm's method's 64kb code_length limit:
;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.9.1
;; > Syntax error (IndexOutOfBoundsException) compiling fn*
;; > Method code too large!
;; So now, prints are done as function call which
;; looks up form in a meta of empty map passed to print fn.
;; Empty map is the smallest(?) literal supporting meta, which
;; does not require resolution, like e.g. symbols:
;; > CompilerException java.lang.RuntimeException: Unable to resolve symbol: blet__8254
;; Like keeping forms in atom - using meta avoids inlining form as "plain text" into method
;; and does not trigger code_length limit.
;; Unlike keeping forms in atom - forms actually survive clj -> cljs transition.
;; (atom, ofc, ends up being available in clljs, but empty :D)
;; Fingers crossed! :D

(defn pad [sym maxlen]
  (str/join
    (take maxlen
      (concat (when sym (name sym)) (repeat " ")))))

(defn print-start [file-and-line]
  (prn (symbol (str/join (repeat 50 "."))) (symbol file-and-line)))


(defn pv [blet-name tag maxlen label value]
  (let [label+ (str "  " tag "  " (pad label maxlen) "  ")]
    ;; binding is here around every print statement, and not around entire blet!,
    ;; because binding inside e.g. loops causes "Cannot recur across try", see tests.
    (binding [*print-length* (or *print-length* *default-print-len*)]
      (prn (symbol blet-name) (symbol label+) value))))

(defn pf [blet-id tag maxlen label meta-holder]
  (pv blet-id tag maxlen label (-> meta-holder meta ::form)))

(defn make-print-form [blet-name tag maxlen label form]
  (list `pf blet-name tag maxlen (some-> label name) (with-meta {} {::form (list 'quote form)})))

(defn make-print-value [blet-name tag maxlen label value]
  (list `pv blet-name tag maxlen (some-> label name) value))


(declare insert-prints)

(defn insert-prints-seq-default [form cfg]
  (if (-> form meta ::printed?)
    form
    (map #(insert-prints % cfg) form)))


(defn insert-prints-seq-let     [form [blet-name maxlen :as cfg]]
  (let [[let_ pairs & bodies] form
        body      (u/bodies->body bodies)
        __        (gensym "__")
        wrap      (fn [[sym expr]]
                    [__   (make-print-form blet-name "let " maxlen sym expr)
                     sym (insert-prints expr cfg)
                     __  (make-print-value blet-name "=   " maxlen sym sym)])
        pairs+    (->> pairs (partition 2) (mapcat wrap) (vec))]
    (list let_ pairs+
      (make-print-form blet-name "body" maxlen nil body)
      (insert-prints body cfg))))


(defn insert-prints-seq-if [form [blet-name maxlen :as cfg]]
  (let [[if_ pred then else] form
        pred-sym (gensym "pred__")]
    (list 'do
      (make-print-form blet-name "if  " maxlen nil pred)
      (list 'let* [pred-sym (insert-prints pred cfg)]
        (make-print-value blet-name "=   " maxlen nil pred-sym)
        (list 'if pred-sym
          (make-print-form blet-name "then" maxlen nil then)
          (make-print-form blet-name "else" maxlen nil else))
        (list if_ pred-sym
          (insert-prints then cfg)
          (insert-prints else cfg))))))


(defn insert-prints-seq-case [form [blet-name maxlen :as cfg]]
  (let [[case_ sym & _] form
        wrap-branch  (fn [prefix branch]
                       (list 'do
                         (make-print-form blet-name prefix maxlen nil branch)
                         (insert-prints branch cfg)))
        wrap-then   #(wrap-branch "then" %)
        wrap-else   #(wrap-branch "else" %)]
    (list 'do
      (make-print-value blet-name "case" maxlen sym sym)
      (u/update-case form identity wrap-then wrap-else))))


(defmulti  insert-prints-seq      (fn [form cfg] (first form)))
(defmethod insert-prints-seq :default [form cfg] (insert-prints-seq-default form cfg))
(defmethod insert-prints-seq   'case* [form cfg] (insert-prints-seq-case form cfg))
(defmethod insert-prints-seq    'let* [form cfg] (insert-prints-seq-let form cfg))
(defmethod insert-prints-seq      'if [form cfg] (insert-prints-seq-if form cfg))


(def insert-prints (u/make-stateful-walker nil insert-prints-seq))

(defn insert-prints-init [form orig file-line]
  (let [blet-name (name (gensym "blet__"))
        maxlen    (->> form
                    (tree-seq coll? seq)
                    (filter u/let*?)
                    (mapcat second)
                    (partition 2)
                    (map #(-> % first str count))
                    (reduce max 0))
        cfg     [blet-name maxlen]]
    (let [form+ (list 'do
                  (list `print-start file-line)
                  (make-print-form blet-name "src " maxlen nil orig)
                  (make-print-form blet-name "    " maxlen nil form)
                  ;(list `print-form "    " blet-name maxlen (with-meta {} {::form (list 'quote form)}))
                  ;; cant print blet output ~ (let [bletsym expr] (print bletsym) bletsym)
                  ;; due to "Can only recur from tail position" when blet! is inside loop:
                  (insert-prints form cfg))]
      (vary-meta form+ assoc ::printed? true))))
