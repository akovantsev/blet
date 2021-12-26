(ns com.akovantsev.blet.impl
  (:require
   [clojure.spec.alpha :as s]
   [clojure.core.specs.alpha :as cs]))

;; todo: support one last extra form as default
;; todo: detect nested bindings (let, fn, binding, ...) shadowing the
;; symbols defined in blet, to avoid unused (potentially sideeffectful) declarations
;; todo: always include `_` bindings, for things like print/log



(defn spy [x] (clojure.pprint/pprint x) x)
(defmacro labeled [syms] `(zipmap '~syms ~syms))

(defn dequalify [x] (-> x name symbol))
(defn mergecat [ms] (reduce (partial merge-with concat) ms))

(declare get-locals)
(defn map-form-locals [m]
  (let [{:keys [::left-forms
                ::left-names]} (->> m
                                 (keys)
                                 (sequence
                                   (comp
                                     (remove keyword?)
                                     (map get-locals)))
                                 (mergecat))]
    {::left-forms (concat
                    left-forms
                    (->> m :or vals))
     ::left-names (concat
                    left-names
                    (->> m :as vector)
                    (->> m :or keys)
                    (->> m :strs)
                    (->> m :syms (map dequalify))
                    (->> m :keys (map dequalify))
                    (->> m (mapcat
                             (fn [[k v]]
                               (when (s/valid? ::cs/ns-keys [k v])
                                 (map dequalify v))))))}))


(defn get-locals-conformed [[tag form]]
  (case tag
    ;;clj 1.9 vs 1.10 spec dispatch keys -___- :
    (:sym :local-symbol)    {::left-names [form]}
    (:map :map-destructure) (map-form-locals form)
    (:seq :seq-destructure) (mergecat
                              (map get-locals-conformed
                                (remove nil?
                                  (concat
                                    ;;; 1.9 fixme:
                                    ;(-> form :elems set)
                                    ;[(-> form :as)
                                    ; (-> form :rest :form)]
                                    ;; 1.10
                                    (-> form :forms set)
                                    [(some->> form :as-form :as-sym (conj [:local-symbol]))
                                     (-> form :rest-forms :form)]))))))

(defn get-locals [left]
  (get-locals-conformed (s/conform ::cs/binding-form left)))


(defn blet [bindings COND]
  (s/assert ::cs/bindings bindings)
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
                    (partition 2)
                    (map-indexed vector)
                    (reduce
                      (fn [m [idx [left right]]]
                        (let [{:keys [::name->binding-id
                                      ::binding-id->dep-ids]} m
                              {:keys [::left-forms ::left-names]} (get-locals left)
                              forms       [left-forms right]
                              direct-deps (form-deps forms name->binding-id)
                              all-deps    (->> direct-deps
                                            (mapcat binding-id->dep-ids)
                                            (into direct-deps))]
                          (-> m
                            (update ::bindings conj [left right])
                            (update ::name->binding-id merge (zipmap left-names (repeat idx)))
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
    ;(spy (labeled [stats branches]))
    (loop [idx  (-> branches count dec)
           form nil]
      (let [branch (get branches idx)]
        (cond
          (= -1 idx)  form
          (odd? idx)  (recur (dec idx) (list (wrap-let idx branch) form))
          (even? idx) (recur (dec idx) (wrap-let idx (concat ['if branch] form))))))))
