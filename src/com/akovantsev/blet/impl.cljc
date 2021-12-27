(ns com.akovantsev.blet.impl)

;; todo: detect nested bindings (let, fn, binding, ...) shadowing the
;; symbols defined in blet, to avoid unused (potentially sideeffectful) declarations
;; todo: always include `_` bindings, for things like print/log


(defn blet [bindings COND]
  ;; same bindibgs asserts as in clojure.core/let:
  (assert (-> bindings vector?))
  (assert (-> bindings count even?))
  (assert (-> COND first (= 'cond)))
  (assert (-> COND count odd?))
  (let [branches  (vec (rest COND))
        branch?   #(and (seqable? %) (-> % first (not= 'quote)))
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
                                      ::binding-id->dep-ids]} m
                              direct-deps (form-deps right name->binding-id)
                              all-deps    (->> direct-deps
                                            (mapcat binding-id->dep-ids)
                                            (into direct-deps))]
                          (-> m
                            (update ::bindings conj [left right])
                            (update ::name->binding-id assoc left idx)
                            ;; todo left here too:
                            (update ::binding-id->dep-ids assoc idx all-deps))))
                      {::bindings            []
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
                            (update ::all-deps assoc idx all-deps)
                            (update ::declared assoc idx (into declared (when (even? idx) to-declare)))
                            (update ::to-declare assoc idx to-declare))))
                      stats))

        {:keys [::bindings
                ::to-declare]} stats
        wrap-let  (fn [branch-id form]
                    (let [dep-ids (->> branch-id (to-declare) (sort <))]
                      (if (empty? dep-ids)
                        form
                        (concat
                          ['let
                           (vec (mapcat bindings dep-ids))]
                          [form]))))]
    ;(clojure.pprint/pprint [stats branches]))
    (loop [idx  (-> branches count dec)
           form nil]
      (let [branch (get branches idx)]
        (cond
          (= -1 idx)  form
          (odd? idx)  (recur (dec idx) (list (wrap-let idx branch) form))
          (even? idx) (recur (dec idx) (wrap-let idx (concat ['if branch] form))))))))
