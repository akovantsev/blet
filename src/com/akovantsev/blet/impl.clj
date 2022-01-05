(ns com.akovantsev.blet.impl
  (:require [clojure.string :as str]))


(def ^:dynamic *destructure*)

;; todo: detect nested bindings (let, fn, binding, ...) shadowing the
;; symbols defined in blet, to avoid unused (potentially sideeffectful) declarations
;; todo: always include `_` bindings, for things like print/log


(defn blet [bindings cond-form & [{:as opts :keys [::print?]}]]
  ;; same bindings asserts as in clojure.core/let:
  (assert (-> bindings vector?))
  (assert (-> bindings count even?))
  (assert (-> cond-form first (= 'cond)))
  (assert (-> cond-form count odd?))
  (let [branches  (vec (rest cond-form))
        branch?   #(and (or (vector? %) (seq? %) (map? %) (list? %)) (-> % first (not= 'quote)))
        form-deps (fn [form name->id]
                    (->> form
                      (tree-seq branch? seq)
                      (map name->id)
                      (remove nil?)
                      (into #{})))
        space     (list 'quote (symbol " "))
        pairs     (->> bindings
                    (*destructure*)
                    (partition 2))

        maxlen    (when print?
                    (->> pairs
                      (map #(-> % first name count))
                      (reduce max)))
        padname   (fn padname [sym]
                    (list 'quote (symbol
                                   (str/join
                                     (take maxlen
                                       (concat (name sym) (repeat " ")))))))

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
                                  ['_ (list 'clojure.core/prn (list 'quote '<<<) space (padname left) space (list 'quote right))])
                                (assoc-in [::print-after idx]
                                  ['_ (list 'clojure.core/prn (list 'quote '===) space (padname left) space left)])))
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
                    (let [dep-ids (->> branch-id (to-declare) (sort <))]
                      (if (empty? dep-ids)
                        form
                        (concat
                          ['let*
                           (reduce
                             (fn [V id]
                               (-> V
                                 (into (print-before id))
                                 (into (bindings id))
                                 (into (print-after id))))
                             [] dep-ids)]
                          [form]))))]
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
                       (list 'clojure.core/prn (list 'quote (if pred? '??? '>>>)) space (list 'quote branch))
                       branch))]
        (cond
          (= -1 idx)  form
          res?        (recur (dec idx) (list (wrap-let idx branch) form))
          pred?       (recur (dec idx) (wrap-let idx (concat ['if branch] form))))))))
