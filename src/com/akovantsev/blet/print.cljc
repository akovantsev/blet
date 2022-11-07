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


(declare insert-prints)

(defn insert-prints-seq-default [form cfg] (map #(insert-prints % cfg) form))

(defn insert-prints-seq-let     [form [mpf mpv :as cfg]]
  (let [[let_ pairs & bodies] form
        wrap (fn [body [sym expr]]
               (if (-> sym meta ::no-print)
                 (list (apply list let_ [sym expr] body))
                 (list
                   (mpf "let " sym expr)
                   (apply list let_
                     [sym (insert-prints expr cfg)]
                     (mpv "=   " sym sym)
                     body))))
        res  (reduce wrap
               (apply list
                 (mpf "body" nil (u/bodies->body bodies))
                 (insert-prints-seq-default bodies cfg))
               (->> pairs (partition 2) (reverse)))]
    (if (u/let*? res)
      res
      (cons 'do res))))



(defn insert-prints-seq-if [form [mpf mpv :as cfg]]
  (let [[if_ pred then else] form
        pred-sym (gensym "pred__")]
    (list 'do
      (mpf "if  " nil pred)
      (list 'let* [pred-sym (insert-prints pred cfg)]
        (mpv "=   " nil pred-sym)
        (list 'if pred-sym
          (mpf "then" nil then)
          (mpf "else" nil else))
        (list if_ pred-sym
          (insert-prints then cfg)
          (insert-prints else cfg))))))


(defn insert-prints-seq-case [form [mpf mpv :as cfg]]
  (let [[case_ sym & _] form
        wrap-branch  (fn [prefix branch]
                       (list 'do
                         (mpf prefix nil branch)
                         (insert-prints branch cfg)))
        wrap-then   #(wrap-branch "then" %)
        wrap-else   #(wrap-branch "else" %)]
    (list 'do
      (mpv "case" sym sym)
      (u/update-case form identity wrap-then wrap-else))))


(defmulti  insert-prints-seq      (fn [form cfg] (first form)))
(defmethod insert-prints-seq :default [form cfg] (insert-prints-seq-default form cfg))
(defmethod insert-prints-seq   'case* [form cfg] (insert-prints-seq-case form cfg))
(defmethod insert-prints-seq    'let* [form cfg] (insert-prints-seq-let form cfg))
(defmethod insert-prints-seq      'if [form cfg] (insert-prints-seq-if form cfg))

(def insert-prints (u/make-stateful-walker ["mpf" "mpv"] insert-prints-seq))


(defn pv [blet-name maxlen value meta-holder]
  (let [{::keys [tag label]} (meta meta-holder)
        label+ (str "  " tag "  " (pad label maxlen) "  ")]
    ;; binding is here around every print statement, and not around entire blet!,
    ;; because binding inside e.g. loops causes "Cannot recur across try", see tests.
    (binding [*print-length* (or *print-length* *default-print-len*)]
      (prn (symbol blet-name) (symbol label+) value))))

(defn pf [blet-id maxlen meta-holder]
  (pv blet-id maxlen (-> meta-holder meta ::form) meta-holder))

(defn make-print-form [psym tag label form]
  (list psym (with-meta {} {::tag tag ::label (some-> label name)  ::form (list 'quote form)})))

;; cant store and retreive value from meta now,
;; because if sym is not in source (but is in meta)
;; in case of nested blet!s - outer blet does not rename sym inside meta,
;; and you will get:
;; > Unable to resolve symbol: b__28238 in this context
;; fixme:
;; proper way to fix it - preserve pre-prints form of inner blet!, and in outer blet! use that one
;; instead of with-prints one.
;; this way old and new print inserts will not conflict.

(defn with-prints [form] form)

(defn make-print-value [psym tag label value]
  (list psym value (with-meta {} {::tag tag ::label (some-> label name)})))

(defn insert-prints-init [form orig file-line]
  (let [blet-name (name (gensym "blet__"))
        maxlen    (->> form
                    (tree-seq coll? seq)
                    (filter u/let*?)
                    (mapcat second)
                    (partition 2)
                    (map #(-> % first str count))
                    (reduce max 0))
        pf-sym  (vary-meta (gensym "pf__") assoc ::no-print true)
        pv-sym  (vary-meta (gensym "pv__") assoc ::no-print true)
        mpf     (partial make-print-form pf-sym)
        mpv     (partial make-print-value pv-sym)]

    ;; somehow using customidentity fn here instead of 'do
    ;; fails blet! inside loop test about recur from tail position.
    (list (vary-meta 'do assoc ::orig-form (list 'quote form))
      (list 'let* [pf-sym (list `partial `pf blet-name maxlen)
                   pv-sym (list `partial `pv blet-name maxlen)]
        (list `print-start file-line)
        (mpf "src " nil orig)
        (mpf "    " nil form)
        ;; cant print blet output ~ (let [bletsym expr] (print bletsym) bletsym)
        ;; due to "Can only recur from tail position" when blet! is inside loop:
        (insert-prints form [mpf mpv])))))
