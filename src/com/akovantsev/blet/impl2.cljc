(ns com.akovantsev.blet.impl2
  (:require
   [com.akovantsev.blet.utils :as u]
   [com.akovantsev.blet.print :as p]))


(defn uses? [form sym] (-> form u/get-syms (contains? sym)))


(defn sym-root-name [sym]
  (-> sym name (u/until-fixed-point #(-> #"(.+)__\d+(?:[_0-9]|auto)*" (re-matches %) second (or %)))))


;; always rename, because clojure/script reuses or|and gensym for different or/ands in the same form:
(defn rename-sym [k state]
  (let [{::keys [orig]} state
        ko (get orig k k)
        pr (sym-root-name ko)
        kn (gensym (str pr "__"))] ;; __ so that gensym-reset in tests would work on these too.
    [(-> kn (with-meta (meta ko)))
     (-> state
       (update ::bound u/sconj k)
       (update ::rename assoc k kn)
       (update ::orig assoc kn ko))]))


(declare rename)
(defn rename-seq-default [form state] (map #(rename % state) form))

(defn -not-supported [form]
  (throw (ex-info (str "\n`" (first form) "` is not supported yet, sorry.\n") {'form form})))

(defn rename-seq-let [form state]
  (let [[f_ [k v :as pair] & bodies] form
        _ (assert (-> pair count #{2}) "call rename only after lets-split")
        [kn state+] (rename-sym k state)]
   (apply list
     f_ [kn (rename v state)]
     (rename bodies state+))))

(defn rename-seq-loop [form state]
  (let [[f_ pairs & bodies] form
        [state+ pairs+] (->> pairs
                          (partition 2)
                          (reduce
                            (fn [[s ps] [k v]]
                              (let [[k+ s+] (rename-sym k s)
                                    v+      (rename v s)]
                                [s+ (conj ps k+ v+)]))
                            [state []]))]
    (apply list
      f_ pairs+
      (rename bodies state+))))


(defn rename-seq-case [form state]
  (let [f #(rename % state)]
    (u/update-case form f f f)))


(defn throw-on-binding [form state]
  ;; (binding [x 1] x) expands to:
  ;;in cljs:
  ; ...
  ; (do
  ;  (set! x x-temp-val__1)
  ;  (try x
  ;   (finally (set! x x-orig-val__1))))
  ;
  ;; in clj:
  ;  (do
  ;    (clojure.core/push-thread-bindings (clojure.core/hash-map #'x 1))
  ;    (try x
  ;     (finally (clojure.core/pop-thread-bindings)))
  ; fixme: figureout way to not rename bindings and not throw
  ;; binding is macroexpanded into flat form, not into nested
  ;; so cant not-rename a (binding[sym v]) and not-rename everything inside.
  ;; gonna throw  for now:
  (let [[f_ sym & tail] form]
    (if-not (-> state ::bound (contains? sym))
      (apply list f_ sym (rename-seq-default tail state))
      (throw (ex-info
               (str "Probably shadowed ^:dynamic symbol `" sym "`, and trying to bind it later.")
               {::form form ::state state})))))

;;fixme: destructure fn args

(defn rename-arity [[args & bodies :as arity] state]
  (loop [ks    []
         state state
         todo1 args]
    (if (empty? todo1)
      (cons ks (rename bodies state))
      (let [[k1 & todo2] todo1
            [k2 state+] (rename-sym k1 state)]
        (recur (conj ks k2) state+ todo2)))))

(defn rename-seq-fn-named [[fn*_ n x :as form] state]
  ;; not -always-reg so that (let* [f (fn f ...)]) would keep `f`s in sync:
  (let [[n2 state+] (rename-sym n state)]
    (concat [fn*_ n2]
      (map #(rename-arity % state+)
        (if (vector? x)
          [(-> form rest rest)]    ;; (fn* f [a] b))
          (-> form rest rest)))))) ;; (fn* f ([a] b))

(defn rename-seq-fn-anon [[fn*_ x :as form] state]
  (let [fname  (gensym "fn__")
        state+ (update state ::bound u/sconj fname)]
    (apply list fn*_ fname
      (map #(rename-arity % state+)
        (if (vector? x)
          [(-> form rest)]    ;; (fn* [a] b)
          (-> form rest))))))  ;; (fn* ([a] b))

(defn rename-seq-fn [[fn*_ x :as form] state]
  (if (symbol? x)
    (rename-seq-fn-named form state)
    (rename-seq-fn-anon form state)))

(defn rename-seq-catch [form state]
  (let [[catch_ ex sym & bodies] form
        [sym+ state+] (rename-sym sym state)
        bodies+       (rename bodies state+)]
    (apply list catch_ ex sym+ bodies+)))



(defmulti  rename-seq      (fn [form state] (first form)))
(defmethod rename-seq :default [form state] (rename-seq-default form state))
(defmethod rename-seq   'quote [form state] form)
(defmethod rename-seq  'letfn* [form state] (-not-supported form))
(defmethod rename-seq   'case* [form state] (rename-seq-case form state))
(defmethod rename-seq   'loop* [form state] (rename-seq-loop form state))
(defmethod rename-seq    'let* [form state] (rename-seq-let form state))
(defmethod rename-seq     'fn* [form state] (rename-seq-fn form state))
(defmethod rename-seq     'var [form state] (throw-on-binding form state))
(defmethod rename-seq    'set! [form state] (throw-on-binding form state))
(defmethod rename-seq   'catch [form state] (rename-seq-catch form state))

(defn lookup-symbol [sym state]
  (if-let [[sym sym2] (-> state ::rename (find sym))]
    sym2
    sym))

(def rename (u/make-stateful-walker {::bound #{} ::rename {} ::orig {}} rename-seq lookup-symbol))






(declare   split-lets)
(defn      split-lets-seq-let      [form]
  (let [[let_ pairs & bodies] form]
    (case (count pairs)
      0 (split-lets (u/bodies->body bodies))
      2 (apply list let_ (split-lets pairs) (split-lets bodies))
      ;else
      (let [pair   (vec (take 2 pairs))
            pairs- (vec (drop 2 pairs))]
        (list let_ (split-lets pair)
          (split-lets (apply list let_ pairs- bodies)))))))

(defn      split-lets-seq-default  [form] (map split-lets form))
(defmulti  split-lets-seq      (fn [form] (first form)))
(defmethod split-lets-seq :default [form] (split-lets-seq-default form))
(defmethod split-lets-seq   'quote [form] form)
(defmethod split-lets-seq    'let* [form] (split-lets-seq-let form))

(def split-lets (u/make-stateless-walker split-lets-seq))





(declare lift-ifs)

(defn lift-ifs-seq-default [form coll-coerce]
  (let [[before IF after] (u/split-by u/if? form)]
    (if (nil? IF)
      (coll-coerce (map lift-ifs form)) ;; because {} is passed as [...]
      (let [[if_ test then else] IF]
        (list if_ test
          (coll-coerce (map lift-ifs (concat before [then] after)))
          (coll-coerce (map lift-ifs (concat before [else] after))))))))

(defn lift-ifs-seq-if [form]
  (let [[if_ pred then else] form]
    (list if_ (lift-ifs pred) (lift-ifs then) (lift-ifs else))))

;; cant lift any ifs from loop, eg:
;(lift-if
;  (walk/macroexpand-all
;    (loop []
;      (if (even? (rand-nth [1 2]))
;        (do (prn :nope) (recur))
;        :done))))


(defn lift-ifs-seq-let [form]
  (let [[let_ pair & bodies]  form
        [sym expr]  pair
        body  (u/bodies->body bodies)]
    (assert (-> pair count (= 2)) pair)
    (cond
      (u/if? expr)
      (let [[if_ pred then else] expr]
        (list if_ pred
          (lift-ifs (list let_ [sym then] body))
          (lift-ifs (list let_ [sym else] body))))

      ;; this can push down let into one of if's branches for more laziness
      (u/if? body)
      (let [[if_ pred then else] body
            pred? (uses? pred sym)
            then? (uses? then sym)
            else? (uses? else sym)]
        (cond
          (or pred? (and then? else?))
          (list let_ [sym (lift-ifs expr)] (lift-ifs body))

          then?  (list if_ pred      (list let_ [sym (lift-ifs expr)] (lift-ifs then))  else)
          else?  (list if_ pred then (list let_ [sym (lift-ifs expr)] (lift-ifs else)))
          :else  (list let_ [sym (lift-ifs expr)] (lift-ifs body))))

      :else
      (list let_ [sym (lift-ifs expr)] (lift-ifs body)))))

;; review:
;; can force binding of all if predicates to let: (if a b c) -> (let [p a] (if p b c))
;; forbid lifting any lets from trycatch, but lift ifs out of trycatch,
;; so it would lift anything bound before the try
(defn lift-ifs-seq-try     [form] (u/update-try form lift-ifs))
(defn lift-ifs-seq-catch   [form] (u/update-catch form lift-ifs))
(defn lift-ifs-seq-finally [form] (u/update-finally form lift-ifs))
(defn lift-ifs-seq-case    [form] (u/update-case-branches form lift-ifs))


(defmulti  lift-ifs-seq      (fn [form] (first form)))
(defmethod lift-ifs-seq :default [form] (lift-ifs-seq-default form seq))
(defmethod lift-ifs-seq   'quote [form] form)
(defmethod lift-ifs-seq     'fn* [form] form)
(defmethod lift-ifs-seq   'loop* [form] form)
(defmethod lift-ifs-seq      'if [form] (lift-ifs-seq-if form))
(defmethod lift-ifs-seq   'case* [form] (lift-ifs-seq-case form))
(defmethod lift-ifs-seq    'let* [form] (lift-ifs-seq-let form))
(defmethod lift-ifs-seq     'try [form] (lift-ifs-seq-try form))
(defmethod lift-ifs-seq   'catch [form] (lift-ifs-seq-catch form))
(defmethod lift-ifs-seq 'finally [form] (lift-ifs-seq-finally form))

(def lift-ifs (u/make-lifter lift-ifs-seq lift-ifs-seq-default))







(declare lift-cases)
(defn lift-case-seq-default [form coll-coerce]
  ;; see lift-if-seq-default
  (let [[before CASE after] (u/split-by u/case*? form)]
    (if (nil? CASE)
      (coll-coerce (map lift-cases form)) ;; because {} is passed as [...]
      (u/update-case-branches CASE #(coll-coerce (map lift-cases (concat before [%] after)))))))


(defn lift-case-seq-if [form]
  (let [[if_ pred then else] form]
    (if (u/case*? pred)
      (u/update-case-branches pred #(list if_ % then else))
      (list if_ (lift-cases pred) (lift-cases then) (lift-cases else)))))


(defn lift-case-seq-let [form]
  (let [[let_ [sym expr] & bodies] form]
    (if (u/case*? expr)
      (u/update-case-branches expr #(apply list let_ [sym %] bodies))
      (apply list let_ [sym (lift-cases expr)] (map lift-cases bodies)))))


(defn lift-case-seq-try     [form] (u/update-try form lift-cases))
(defn lift-case-seq-catch   [form] (u/update-catch form lift-cases))
(defn lift-case-seq-finally [form] (u/update-finally form lift-cases))
(defn lift-case-seq-case    [form] (u/update-case-branches form lift-cases))


(defmulti  lift-cases-seq      (fn [form] (first form)))
(defmethod lift-cases-seq :default [form] (lift-case-seq-default form seq))
(defmethod lift-cases-seq   'quote [form] form)
(defmethod lift-cases-seq     'fn* [form] form)
(defmethod lift-cases-seq   'loop* [form] form)
(defmethod lift-cases-seq      'if [form] (lift-case-seq-if form))
(defmethod lift-cases-seq   'case* [form] (lift-case-seq-case form))
(defmethod lift-cases-seq    'let* [form] (lift-case-seq-let form))
(defmethod lift-cases-seq     'try [form] (lift-case-seq-try form))
(defmethod lift-cases-seq   'catch [form] (lift-case-seq-catch form))
(defmethod lift-cases-seq 'finally [form] (lift-case-seq-finally form))

(def lift-cases (u/make-lifter lift-cases-seq lift-case-seq-default))







(declare lift-lets)
(defn lift-lets-seq-default [form coll-coerce]
  (let [[before LET after] (u/split-by u/let*? form)]
    (if (nil? LET)
      (coll-coerce (map lift-lets form))
      (let [[let_ pair & bodies] LET
            body (u/bodies->body bodies)]
        (list let_ pair
          (coll-coerce (concat before [body] after)))))))


(defn lift-lets-seq-if [form]
  (let [[if_ pred then else] form]
    ;; pred is visible for both then&else, so lifting only from pred:
    (if-not (u/let*? pred)
      (list if_ (lift-lets pred) (lift-lets then) (lift-lets else))
      (let [[let_ pair & bodies] pred
            body (u/bodies->body bodies)]
        (list let_ pair
          (list if_ body (lift-lets then) (lift-lets else)))))))


(defn lift-lets-seq-let [form]
  (let [[let1 [sym1 expr1] & bodies1] form
        body1 (u/bodies->body bodies1)]
    ;; recursively lifts from exprs first, then - from body.
    ;; never wrap return in recur - rely on fixed-point wrapper later.
    ;; add clause to cond only when need to lift higher than root form.
    (if-not (u/let*? expr1)
      (list let1 [sym1 (lift-lets expr1)]
        (lift-lets body1))
      (let [[let2 [sym2 expr2] & bodies2] expr1
            body2 (u/bodies->body bodies2)]
        (list let2 [sym2 (lift-lets expr2)]
          (list let1 [sym1 (lift-lets body2)]
            body1))))))


;;dont lift anything from trycatch:
(defn lift-lets-seq-try     [form] (u/update-try form lift-lets))
(defn lift-lets-seq-catch   [form] (u/update-catch form lift-lets))
(defn lift-lets-seq-finally [form] (u/update-finally form lift-lets))
(defn lift-lets-seq-case    [form] (u/update-case-branches form lift-lets))


(defmulti  lift-lets-seq      (fn [form] (first form)))
(defmethod lift-lets-seq :default [form] (lift-lets-seq-default form seq))
(defmethod lift-lets-seq   'quote [form] form)
(defmethod lift-lets-seq     'fn* [form] form)
(defmethod lift-lets-seq   'loop* [form] form)
(defmethod lift-lets-seq      'if [form] (lift-lets-seq-if form))
(defmethod lift-lets-seq    'let* [form] (lift-lets-seq-let form))
(defmethod lift-lets-seq   'case* [form] (lift-lets-seq-case form))
(defmethod lift-lets-seq     'try [form] (lift-lets-seq-try form))
(defmethod lift-lets-seq   'catch [form] (lift-lets-seq-catch form))
(defmethod lift-lets-seq 'finally [form] (lift-lets-seq-finally form))


(def lift-lets (u/make-lifter lift-lets-seq lift-lets-seq-default))






(declare dedupe-lets)
(defn dedupe-lets-seq-default [form bound] (map #(dedupe-lets % bound) form))
(defn dedupe-lets-seq-let     [form bound]
  (let [[let_ [sym expr :as pair] & bodies] form]
    (if (or (bound sym)
          ;; removes unused lets added eg. when def has or, but in diffrerent branch.
          (-> bodies (uses? sym) not))
      (-> bodies u/bodies->body (dedupe-lets bound))
      (map #(dedupe-lets % (conj bound sym)) form))))


(defmulti  dedupe-lets-seq      (fn [form bound] (first form)))
(defmethod dedupe-lets-seq   'quote [form bound] form)
(defmethod dedupe-lets-seq :default [form bound] (dedupe-lets-seq-default form bound))
(defmethod dedupe-lets-seq    'let* [form bound] (dedupe-lets-seq-let form bound))

(def dedupe-lets (u/make-stateful-walker #{} dedupe-lets-seq))






(declare   dedupe-ifs)
(defn      dedupe-ifs-default      [form] (map dedupe-ifs form))
(defn      dedupe-ifs-seq-if       [form]
  (let [[if_ pred then else] form]
    (if (= then else)
      then
      (list if_ (dedupe-ifs pred) (dedupe-ifs then) (dedupe-ifs else)))))

(defmulti  dedupe-if-seq      (fn [form] (first form)))
(defmethod dedupe-if-seq   'quote [form] form)
(defmethod dedupe-if-seq :default [form] (dedupe-ifs-default form))
(defmethod dedupe-if-seq      'if [form] (dedupe-ifs-seq-if form))

(def dedupe-ifs (u/make-stateless-walker dedupe-if-seq))






(declare   merge-lets)
(defn merge-lets-seq-let [form]
  (let [[let_ pairs & bodies] form
        body (u/bodies->body bodies)]
    ;; at this point (is called after all lifts are done)
    ;; there should be no lets in pairs, only in bodies:
    (if-not (u/let*? body)
      (list let_ pairs (merge-lets body))
      (let [[let2_ pair2 & bodies2] body]
        (apply list let_ (into pairs pair2)
          (merge-lets bodies2))))))

(defn      merge-lets-seq-default  [form] (map merge-lets form))
(defmulti  merge-lets-seq      (fn [form] (first form)))
(defmethod merge-lets-seq   'quote [form] form)
(defmethod merge-lets-seq :default [form] (merge-lets-seq-default form))
(defmethod merge-lets-seq    'let* [form] (merge-lets-seq-let form))

(def  merge-lets  (u/make-stateless-walker merge-lets-seq))





(declare wrap)
(defn wrap-seq-default [form defs] (map #(wrap % defs) form))
(defn wrap-sym         [sym  defs]
  (if (contains? defs sym)
    (list 'let* [sym (wrap (defs sym) defs)] sym)
    sym))

(defn wrap-opaque      [form defs]
  (let [syms (->> form u/get-syms (filter #(contains? defs %)))]
    (reduce
      (fn [res x] (list 'let* [x (wrap (defs x) defs)] res))
      form
      (sort syms))))


(defmulti  wrap-seq      (fn [form defs] (first form)))
(defmethod wrap-seq :default [form defs] (wrap-seq-default form defs))
(defmethod wrap-seq   'quote [form defs] form)
(defmethod wrap-seq     'fn* [form defs] (wrap-opaque form defs))
(defmethod wrap-seq   'loop* [form defs] (wrap-opaque form defs))
(defmethod wrap-seq     'try [form defs] (wrap-opaque form defs)) ;;review: mb wrap-opaq for body and finally, but recur for catch inside?

(def wrap (u/make-stateful-walker nil wrap-seq wrap-sym))





(declare   optimize-ifs)
(defn optimize-ifs-seq-if [form]
  (let [[if_ pred then else] form]
    (cond
      (u/any-pred? pred nil? false?)
      (optimize-ifs else)
      (u/any-pred? pred map? set? vector? string? number? true? uuid? keyword?)
      (optimize-ifs then)
      :else
      (list if_ (optimize-ifs pred) (optimize-ifs then) (optimize-ifs else)))))

(defn      optimize-ifs-seq-default  [form] (map optimize-ifs form))
(defmulti  optimize-ifs-seq      (fn [form] (first form)))
(defmethod optimize-ifs-seq :default [form] (optimize-ifs-seq-default form))
(defmethod optimize-ifs-seq   'quote [form] form)
(defmethod optimize-ifs-seq      'if [form] (optimize-ifs-seq-if form))

(def optimize-ifs (u/make-stateless-walker optimize-ifs-seq))





(declare simplify-loops)
(defn simplify-loops-seq-loop [form]
  ;; must run before split-lets, and syms-renaming, to keep this impl simple:
  ;; (loop [x (f y)] ...) -> (let [x (f y)] (loop [x x] ...))
  (let [[loop_ pairs & bodies] form
        pairs* (partition 2 pairs)
        syms   (map first pairs*)
        exprs  (map second pairs*)
        body   (u/bodies->body bodies)]
    (assert (every? symbol? syms))
    (if (every? symbol? exprs)
      (list loop_ pairs (simplify-loops body))
      (list 'let* pairs
        (list loop_ (vec (interleave syms syms))
          (simplify-loops body))))))

(defn      simplify-loops-seq-default  [form] (map simplify-loops form))
(defmulti  simplify-loops-seq      (fn [form] (first form)))
(defmethod simplify-loops-seq   'quote [form] form)
(defmethod simplify-loops-seq :default [form] (simplify-loops-seq-default form))
(defmethod simplify-loops-seq   'loop* [form] (simplify-loops-seq-loop form))

(def simplify-loops (u/make-stateless-walker simplify-loops-seq))







(declare   collapse-aliases)
(defn      collapse-aliases-seq-let      [form]
  (let [[let_ [sym expr] & bodies] form
        body (u/bodies->body bodies)]
    (if (symbol? expr)
      (collapse-aliases (rename body {::rename {sym expr}}))
      (list let_ [sym (collapse-aliases expr)]
        (collapse-aliases body)))))

(defn      collapse-aliases-seq-default  [form] (map collapse-aliases form))
(defmulti  collapse-aliases-seq      (fn [form] (first form)))
(defmethod collapse-aliases-seq :default [form] (collapse-aliases-seq-default form))
(defmethod collapse-aliases-seq    'let* [form] (collapse-aliases-seq-let form))
(defmethod collapse-aliases-seq   'quote [form] form)

(def collapse-aliases (u/make-stateless-walker collapse-aliases-seq))







(defn parse [norm-form]
  (loop [form norm-form
         defs {}]
    (case (first form)
      :com.akovantsev.blet/BODY
      [defs (u/bodies->body (rest form))]

      let*
      (let [[let_ [sym expr] body] form]
        (recur body (assoc defs sym expr))))))




(defn lift-branches [form]
  (-> form
    (u/until-fixed-point lift-ifs)
    (u/until-fixed-point lift-cases)
    (u/until-fixed-point lift-lets)))




(defn blet [&form_ let-binds-body print? file-line]
  (let [normal (-> let-binds-body
                 (u/until-fixed-point optimize-ifs)
                 (u/until-fixed-point simplify-loops)
                 (u/until-fixed-point split-lets)
                 (rename))
        [defs body] (parse normal)]
    (-> body
      (wrap defs)
      (u/until-fixed-point lift-branches)
      (u/until-fixed-point dedupe-lets)
      (u/until-fixed-point dedupe-ifs)
      (u/until-fixed-point collapse-aliases)
      (u/until-fixed-point merge-lets)
      (cond->
        print? (p/insert-prints-init &form_ file-line)))))