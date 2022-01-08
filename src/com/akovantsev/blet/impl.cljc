(ns com.akovantsev.blet.impl
  (:require
   [clojure.set :as set]
   [com.akovantsev.blet.env :as env]
   [com.akovantsev.blet.print :as bp]))


;; todo: always include `_` bindings, for things like print/log


(def ^:dynamic *destructure*) ;; destructure is different for clj and cljs

(defn -get-destructure-fn []
  (if env/*js?*
    (resolve 'cljs.core/destructure)
    (resolve 'clojure.core/destructure)))

;; review: binding does not work for ^:dynamic here, for some reason:
(defn gen-blet-id [] (list 'quote (gensym "blet__")))


;;; form deps resolution ;;;

(declare get-form-deps)

(defmulti get-list-deps (fn [locals [f & args :as form]] f))

(defmethod get-list-deps :default [locals form] (get-form-deps locals (vec form)))
(defmethod get-list-deps `quote [locals form] #{})


(defn get-let-deps [locals [_ bindings & body]]
  (let [[locals deps] (->> bindings
                        (*destructure*)
                        (partition 2)
                        (reduce
                          (fn rf [[locals deps] [local form]]
                            [(disj locals local) (into deps (get-form-deps locals form))])
                          [locals #{}]))]
    (into deps (get-form-deps locals (vec body)))))


(defn get-def-deps [locals form]
  (let [f (fn
            ([_ sym] #{})
            ([_ sym init] (get-form-deps locals init))
            ([_ sym doc init] (get-form-deps locals init)))]
    (apply f form)))


(defn get-defn-deps [locals [_sym & tail]]
  (let [[fname tail] (if (-> tail first symbol?) [(first tail) (rest tail)] [nil tail])
        tail         (if (-> tail first string?) (rest tail) tail)
        [meta1 tail] (if (-> tail first map?)    [(first tail) (rest tail)] [nil tail])
        one?         (-> tail first vector?)
        [meta2 tail] (if (or one? (not (-> tail last map?)))
                       [nil tail]
                       [(last tail) (butlast tail)])
        arities      (if one? [tail] tail)
        locals       (disj locals fname)
        arg-syms     (fn as [args] (map first (partition 2 (*destructure* [args nil]))))
        parse-arity  (fn pa [[args & body]]
                       (get-form-deps (reduce disj locals (arg-syms args)) (vec body)))]
    (apply set/union
      (get-form-deps locals meta1)
      (get-form-deps locals meta2)
      (map parse-arity arities))))


;;todo as->
(defmethod get-list-deps 'let [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'let* [locals form] (get-let-deps locals form))
;(defmethod get-list-deps 'letfn [locals form] (get-let-deps locals (macroexpand-1 form)))  ; => letfn*
;(defmethod get-list-deps 'letfn* [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'for [locals form] (get-let-deps locals form)) ;;fixme :let :when keywords support
(defmethod get-list-deps 'loop [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'dotimes [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'binding [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'if-let [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'if-some [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'when-let [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'when-some [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'when-first [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'with-open [locals form] (get-let-deps locals form))
(defmethod get-list-deps 'with-redefs [locals form] (get-let-deps locals form))

(defmethod get-list-deps 'def [locals form] (get-def-deps locals form))
(defmethod get-list-deps 'defonce [locals form] (get-def-deps locals form))

(defmethod get-list-deps 'fn* [locals form] (get-defn-deps locals form)) ;; #(+ 1 %)
(defmethod get-list-deps 'fn [locals form] (get-defn-deps locals form))
(defmethod get-list-deps 'defn [locals form] (get-defn-deps locals form))
(defmethod get-list-deps 'defn- [locals form] (get-defn-deps locals form))

(defmethod get-list-deps `defmethod [locals [_defm mname dispval & fn-tail]]
  (set/union
    (get-form-deps locals dispval)
    (get-defn-deps (disj locals mname) [`fn fn-tail])))


(defmethod get-list-deps `catch [locals [_catch _ex as & body]] (get-form-deps (disj locals as) (vec body)))


(defn get-form-deps [locals form]
  (assert (set? locals))
  (cond
    (empty? locals) #{}
    (symbol? form)  (set/intersection locals #{form})
    (vector? form)  (reduce into #{} (map #(get-form-deps locals %) form))
    (map? form)     (get-form-deps locals (into [] cat form))
    (set? form)     (get-form-deps locals (vec form))
    (seq? form)     (get-list-deps locals form)
    :else           #{}))



;;; blet impl ;;;

(defn blet [bindings cond-form & [{:as opts :keys [::print?]}]]
 (binding [*destructure* (-get-destructure-fn)]
  ;; same bindings asserts as in clojure.core/let:
  (assert (-> bindings vector?))
  (assert (-> bindings count even?))
  (assert (-> cond-form first (= 'cond)))
  (assert (-> cond-form count odd?))
  (let [branches  (vec (rest cond-form))
        form-deps (fn [form name->id]
                    (let [locals  (-> name->id keys set)]
                      (->> form
                        (get-form-deps locals)
                        (map name->id)
                        (set))))
        blet-id   (gen-blet-id)
        pairs     (->> bindings
                    (*destructure*)
                    (partition 2))

        maxlen    (when print?
                    (->> pairs
                      (map #(-> % first name count))
                      (reduce max)))

        stats     (->> pairs
                    (map-indexed vector)
                    (reduce
                      (fn [m [idx [left right]]]
                        (let [{:keys [::name->binding-id
                                      ::bindings
                                      ::binding-id->dep-ids]} m
                              direct-deps (form-deps right name->binding-id)
                              all-deps    (->> direct-deps
                                            (mapcat binding-id->dep-ids)
                                            (into direct-deps))]
                          (-> m
                            (update-in [::bindings] conj [left right])
                            (cond->
                              print?
                              (->
                                (assoc-in [::print-before idx]
                                  ['_ (bp/printfn ::bp/binding-form blet-id maxlen left (list 'quote right))])
                                (assoc-in [::print-after idx]
                                  ['_ (bp/printfn ::bp/binding-evaled blet-id maxlen left left)])))
                            (assoc-in [::name->binding-id left] idx)
                            (assoc-in [::binding-id->dep-ids idx] all-deps))))
                      {::bindings            []
                       ::print-before        {}
                       ::print-after         {}
                       ::name->binding-id    {}
                       ::binding-id->dep-ids {}
                       ::declared            {}
                       ::to-declare          {}}))

        stats     (->> branches
                    (map-indexed vector)
                    (reduce
                      (fn [m [idx branch]]
                        (let [{:keys [::declared
                                      ::name->binding-id
                                      ::binding-id->dep-ids]} m
                              prev-pred-id (if (even? idx) (- idx 2) (- idx 1))
                              declared     (declared prev-pred-id #{})
                              direct-deps  (form-deps branch name->binding-id)
                              all-deps     (->> direct-deps
                                             (mapcat binding-id->dep-ids)
                                             (into direct-deps))
                              to-declare   (reduce disj all-deps declared)]
                          (-> m
                            (assoc-in [::all-deps idx] all-deps)
                            (assoc-in [::declared idx] (into declared (when (even? idx) to-declare)))
                            (assoc-in [::to-declare idx] to-declare))))
                      stats))

        {:keys [::bindings
                ::print-before
                ::print-after
                ::to-declare]} stats

        wrap-let  (fn [branch-id form]
                    (let [dep-ids (->> branch-id (to-declare) (sort <))
                          wrap-pr (fn [V id]
                                    (-> V
                                      (into (print-before id))
                                      (into (bindings id))
                                      (into (print-after id))))]
                      (if (empty? dep-ids)
                        form
                        (list 'let* (reduce wrap-pr [] dep-ids) form))))]
    ;(prn printings)
    ;(clojure.pprint/pprint [stats branches]))
    (loop [idx  (-> branches count dec)
           form nil]
      (let [branch (get branches idx)
            pred?  (even? idx)
            res?   (odd? idx)
            branch (if-not print?
                     branch
                     (list 'do
                       (bp/printfn (if pred? ::bp/pred-form ::bp/branch-form) blet-id maxlen nil (list 'quote branch))
                       branch))]
        (cond
          (= -1 idx)  (if print?
                        (list 'do (bp/printfn ::bp/start blet-id nil nil nil) form)
                        form)
          res?        (recur (dec idx) (list (wrap-let idx branch) form))
          pred?       (recur (dec idx) (wrap-let idx (concat ['if branch] form)))))))))
