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
;; looks up form in atom under blet-id > form-name
;; Fingers crossed! :D
(def !forms (atom {}))

(defn reg-form! [blet-name form-name form]
  (swap! !forms assoc-in [blet-name form-name] form))


(defn pad [sym maxlen]
  (str/join
    (take maxlen
      (concat (when sym (name sym)) (repeat " ")))))

(defn print-start [file-and-line]
  (prn (symbol (str/join (repeat 50 "."))) (symbol file-and-line)))


(defn print-value [tag blet-name maxlen label value]
  (let [label+ (str "  " tag "  " (pad label maxlen) "  ")]
    ;; binding is here around every print statement, and not around entire blet!,
    ;; because binding inside e.g. loops causes "Cannot recur across try", see tests.
    (binding [*print-length* (or *print-length* *default-print-len*)]
      (prn (symbol blet-name) (symbol label+) value))))

(defn print-labeled-form [tag blet-id maxlen sym]
  (print-value tag blet-id maxlen sym (get-in @!forms [blet-id sym])))

(defn print-form [tag blet-id maxlen sym]
  (print-value tag blet-id maxlen nil (get-in @!forms [blet-id sym])))


(declare insert-prints)
(defn insert-prints-seq-default [form cfg] (map #(insert-prints % cfg) form))
(defn insert-prints-seq-let     [form [blet-name maxlen :as cfg]]
  (let [[let_ pairs & bodies] form
        body      (u/bodies->body bodies)
        __        (gensym "__")
        body-name (name (gensym "body__"))
        wrap      (fn [[sym expr]]
                    (let [sym-name (name sym)]
                      (reg-form! blet-name sym-name expr)
                      [__  (list `print-labeled-form "let " blet-name maxlen sym-name)
                       sym (insert-prints expr cfg)
                       __  (list `print-value "=   " blet-name maxlen sym-name sym)]))
        pairs+    (->> pairs (partition 2) (mapcat wrap) (vec))]
    (reg-form! blet-name body-name body)
    (list let_ pairs+
      (list `print-form "body" blet-name maxlen body-name)
      (insert-prints body cfg))))


(defn insert-prints-seq-if [form [blet-name maxlen :as cfg]]
  (let [[if_ pred then else] form
        pred-sym (gensym "pred__")
        pred-name (name (gensym "else__"))
        then-name (name (gensym "else__"))
        else-name (name (gensym "else__"))]
    (reg-form! blet-name pred-name pred)
    (reg-form! blet-name then-name then)
    (reg-form! blet-name else-name else)
    (list 'do
      (list `print-form "if  " blet-name maxlen pred-name)
      (list 'let* [pred-sym (insert-prints pred cfg)]
        (list `print-value "=   " blet-name maxlen nil pred-sym)
        (list 'if pred-sym
          (list `print-form "then" blet-name maxlen then-name)
          (list `print-form "else" blet-name maxlen else-name))
        (list if_ pred-sym
          (insert-prints then cfg)
          (insert-prints else cfg))))))


(defn insert-prints-seq-case [form [blet-name maxlen :as cfg]]
  (let [[case_ sym & _] form
        wrap-branch  (fn [prefix branch]
                       (let [branch-name (name (gensym "branch__"))]
                         (reg-form! blet-name branch-name branch)
                         (list 'do
                           (list `print-form prefix blet-name maxlen branch-name)
                           (insert-prints branch cfg))))
        wrap-then   #(wrap-branch "then" %)
        wrap-else   #(wrap-branch "else" %)
        sym-name    (name sym)]
    (list 'do
      (list `print-value "case" blet-name maxlen sym-name sym)
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
  (let [blet-name (name (gensym "blet__"))
        orig-name "orig"
        form-name "form"
        maxlen    (->> form
                    (tree-seq coll? seq)
                    (filter u/let*?)
                    (mapcat second)
                    (partition 2)
                    (map #(-> % first str count))
                    (reduce max 0))
        cfg     [blet-name maxlen]]
    (reg-form! blet-name orig-name orig)
    (reg-form! blet-name form-name form)
    (list (vary-meta 'do assoc ::printed? true)
      (list `print-start file-line)
      (list `print-form "src " blet-name maxlen orig-name)
      (list `print-form "    " blet-name maxlen form-name)
      ;; cant print blet output ~ (let [bletsym expr] (print bletsym) bletsym)
      ;; due to "Can only recur from tail position" when blet! is inside loop:
      (insert-prints form cfg))))