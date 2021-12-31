(ns com.akovantsev.blet.impl)

;; todo: detect nested bindings (let, fn, binding, ...) shadowing the
;; symbols defined in blet, to avoid unused (potentially sideeffectful) declarations
;; todo: always include `_` bindings, for things like print/log
(def GENSYM-RE #"(vec|seq|map|first)__(\d+)")

(defn gensym? [x] (boolean (re-matches GENSYM-RE (name x))))


(defn blet [bindings COND & [{:as opts :keys [::print-user-defined? ::print-all?]}]]
  ;; same bindibgs asserts as in clojure.core/let:
  (assert (-> bindings vector?))
  (assert (-> bindings count even?))
  (assert (-> COND first (= 'cond)))
  (assert (-> COND count odd?))
  (let [branches  (vec (rest COND))
        branch?   #(and (or (vector? %) (seq? %) (map? %) (list? %)) (-> % first (not= 'quote)))
        form-deps (fn [form name->id]
                    (->> form
                      (tree-seq branch? seq)
                      (map name->id)
                      (remove nil?)
                      (into #{})))
        stats     (->> bindings
                    (destructure)
                    (partition 2)
                    (map-indexed vector)
                    (reduce
                      (fn [m [idx [left right]]]
                        (let [{:keys [::name->binding-id
                                      ::bindings
                                      ::binding-id->dep-ids]} m
                              print?      (or print-all?
                                              (and print-user-defined?
                                                   (not (gensym? left))))
                              direct-deps (form-deps right name->binding-id)
                              all-deps    (->> direct-deps
                                            (mapcat binding-id->dep-ids)
                                            (into direct-deps))]
                          (-> m
                            (update-in [::bindings] conj [left right])
                            (cond->
                              print? (assoc-in [::printings (count bindings)]
                                       ['_ (list clojure.core/print (str (name left) ": "))
                                        '_ (list clojure.core/prn   left)]))
                            (assoc-in [::name->binding-id left] idx)
                            ;; todo left here too:
                            (assoc-in [::binding-id->dep-ids idx] all-deps))))
                      {::bindings            []
                       ::printings           {}
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
                ::printings
                ::to-declare]} stats
        wrap-let  (fn [branch-id form]
                    (let [dep-ids (->> branch-id (to-declare) (sort <))]
                      (if (empty? dep-ids)
                        form
                        (concat
                          ['let*
                           (reduce
                             (fn [V id]
                               (let [b (bindings id)
                                     p (printings id)]
                                 (-> V
                                   (into b)
                                   (cond->
                                     p (into p)))))
                             [] dep-ids)]
                           ;(vec (mapcat bindings dep-ids))]
                          [form]))))]
    ;(prn printings)
    ;(clojure.pprint/pprint [stats branches]))
    (loop [idx  (-> branches count dec)
           form nil]
      (let [branch (get branches idx)]
        (cond
          (= -1 idx)  form
          (odd? idx)  (recur (dec idx) (list (wrap-let idx branch) form))
          (even? idx) (recur (dec idx) (wrap-let idx (concat ['if branch] form))))))))
