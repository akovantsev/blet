(ns com.akovantsev.blet.impl2
  (:require
   [com.akovantsev.blet.utils :as u]
   [com.akovantsev.blet.print :as p]))


(defn bodies->body [bodies]
  (if (-> bodies count (< 2))
    (first bodies)
    (cons 'do bodies)))

(defn uses? [form sym] (-> form u/get-syms (contains? sym)))

(defn loop*?   [form] (and (seq? form) (= (first form) 'loop*)))
(defn let*?    [form] (and (seq? form) (= (first form) 'let*)))
(defn case*?   [form] (and (seq? form) (= (first form) 'case*)))
(defn if?      [form] (and (seq? form) (= (first form) 'if)))
(defn finally? [form] (and (seq? form) (= (first form) 'finally)))
(defn catch?   [form] (and (seq? form) (= (first form) 'catch)))


(defn sym-root-name [sym]
  (-> sym name (u/until-fixed-point #(-> #"(.+)__\d+(?:[_0-9]|auto)*" (re-matches %) second (or %)))))

;; always rename, because clojure/script reuses or|and gensym for different or/ands in the same form:
(defn rename-sym [k state]
  (let [{::keys [orig]} state
        ko (get orig k k)
        pr (sym-root-name ko)
        kn (gensym (str pr "__"))] ;; __ so that gensym-reset in tests would work on these too.
    [kn (-> state
          (update ::bound conj k)
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

;(macroexpand '(case x 1 a 2 b c))
;js   (case* G__24 [[1] [2]] [a b] c))
;clj  (case* G__41 0 0 c {1 [1 a], 2 [2 b]} :compact :int))
;$ planck ;ClojureScript 1.10.914
;cljs.user=> (macroexpand '(case x a 2 b 4 c))
;   (let* [G__23 x] (cljs.core/cond (cljs.core/= 'a G__23) 2 (cljs.core/= 'b G__23) 4 :else c))
;cljs.user=> (macroexpand '(case x 1 a 2 b c))
;   (let* [G__24 x] (case* G__24 [[1] [2]] [a b] c))
#?(:clj (defn map-case-vals [fv branches]
          ;; {1 [1 a], 2 [2 b]} -> {1 [1 (fv a)], 2 [2 (fv b)]}
          (let [f2 (fn [[x y]] [x (fv y)])
                f+ (fn [pairs] (->> pairs (partition 2) (mapcat f2) (vec)))]
            (reduce-kv (fn rf [m k v] (assoc m k (f+ v))) {} branches))))


(defn update-case [case-form sym-fn branch-fn default-fn]
  #?(:cljs (let [[case_ sym preds branches default] case-form]
             (list case_ (sym-fn sym) preds (mapv branch-fn branches) (default-fn default)))

     :clj  (let [[case_ sym x1 x2 default branches & tail] case-form]
             (apply list case_ (sym-fn sym) x1 x2 (default-fn default) (map-case-vals branch-fn branches) tail))))

(defn update-case-branches [form branch-f] (update-case form identity branch-f branch-f))


(defn update-try [form tail-fn]
  (let [[try_ & bodies-catches-finaly] form]
    (apply list try_ (map tail-fn bodies-catches-finaly))))

(defn update-catch [form bodies-f]
  (let [[catch_ ex_ sym & bodies] form]
    (apply list catch_ ex_ sym (bodies-f bodies))))

(defn update-finally [form bodies-f]
  (let [[finally_ & bodies] form]
    (apply list finally_ (bodies-f bodies))))


(defn rename-seq-case [form state]
  (let [f #(rename % state)]
    (update-case form f f f)))


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
        state+ (update state ::bound conj fname)]
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

(defn rename
  ([form] (rename form {::bound #{} ::rename {} ::orig {}}))
  ([form state]
   (let [-rename #(rename % state)]
     (cond
       (seq?  form)   (rename-seq form state)
       (map? form)    (apply hash-map (mapcat -rename form))   ;; (empty (first {1 1})) => nil, and it reverses map
       (vector? form) (mapv -rename form)
       (set? form)    (set (mapv -rename form))
       (symbol? form) (if-let [[form form2] (-> state ::rename (find form))] form2 form)
       :else          form))))



(defn split-let [form]
  (if-not (let*? form)
    form
    (let [[f pairs & bodies] form]
      (case (count pairs)
        0 (case (count bodies)
            0 nil
            1 (first bodies)
            ;else
            (cons 'do bodies))
        2 form
        ;else
        (list f (vec (take 2 pairs))
          (apply list f (vec (drop 2 pairs)) bodies))))))


(defn split-lets [form] (u/prewalk form split-let))



(declare lift-if)

(defn lift-if-seq-default [form coll-coerce]
  (let [[before IF after] (u/split-by if? form)]
    (if (nil? IF)
      (coll-coerce (map lift-if form)) ;; because {} is passed as [...]
      (let [[if_ test then else] IF]
        (list if_ test
          (coll-coerce (map lift-if (concat before [then] after)))
          (coll-coerce (map lift-if (concat before [else] after))))))))

(defn lift-if-seq-if [form]
  (let [[if_ pred then else] form]
    (list if_ (lift-if pred) (lift-if then) (lift-if else))))

;; cant lift any ifs from loop, eg:
;(lift-if
;  (walk/macroexpand-all
;    (loop []
;      (if (even? (rand-nth [1 2]))
;        (do (prn :nope) (recur))
;        :done))))


(defn lift-if-seq-let [form]
  (let [[let_ pair & bodies]  form
        [sym expr]  pair
        body  (bodies->body bodies)]
    (assert (-> pair count (= 2)) pair)
    (cond
      (if? expr) (let [[if_ pred then else] expr]
                   (list if_ pred
                     (lift-if (list let_ [sym then] body))
                     (lift-if (list let_ [sym else] body))))

      ;; this can push down let into one of if's branches for more laziness
      (if? body) (let [[if_ pred then else] body
                       pred? (uses? pred sym)
                       then? (uses? then sym)
                       else? (uses? else sym)]
                   (cond
                     (or pred? (and then? else?))
                     (list let_ [sym (lift-if expr)] (lift-if body))

                     then?  (list if_ pred      (list let_ [sym (lift-if expr)] (lift-if then))  else)
                     else?  (list if_ pred then (list let_ [sym (lift-if expr)] (lift-if else)))
                     :else  (list let_ [sym (lift-if expr)] (lift-if body))))

      :else     (list let_ [sym (lift-if expr)] (lift-if body)))))

;; review:
;; can force binding of all if predicates to let: (if a b c) -> (let [p a] (if p b c))
;; forbid lifting any lets from trycatch, but lift ifs out of trycatch,
;; so it would lift anything bound before the try
(defn lift-if-seq-try     [form] (update-try form lift-if))
(defn lift-if-seq-catch   [form] (update-catch form lift-if))
(defn lift-if-seq-finally [form] (update-finally form lift-if))
(defn lift-if-seq-case    [form] (update-case-branches form lift-if))


(defmulti  lift-if-seq      (fn [form] (first form)))
(defmethod lift-if-seq :default [form] (lift-if-seq-default form seq))
(defmethod lift-if-seq   'quote [form] form)
(defmethod lift-if-seq     'fn* [form] form)
(defmethod lift-if-seq   'loop* [form] form)
(defmethod lift-if-seq      'if [form] (lift-if-seq-if form))
(defmethod lift-if-seq   'case* [form] (lift-if-seq-case form))
(defmethod lift-if-seq    'let* [form] (lift-if-seq-let form))
(defmethod lift-if-seq     'try [form] (lift-if-seq-try form))
(defmethod lift-if-seq   'catch [form] (lift-if-seq-catch form))
(defmethod lift-if-seq 'finally [form] (lift-if-seq-finally form))

(defn lift-if [form]
  (cond
    (seq? form)    (lift-if-seq form)
    (vector? form) (lift-if-seq-default form vec)
    (set? form)    (lift-if-seq-default form set)
    (map? form)    (lift-if-seq-default (into [] cat form) (partial apply hash-map))
    :else          form))


(defn lift-ifs [form] (u/until-fixed-point form lift-if))





(declare lift-case)
(defn lift-case-seq-default [form coll-coerce]
  ;; see lift-if-seq-default
  (let [[before CASE after] (u/split-by case*? form)]
    (if (nil? CASE)
      (coll-coerce (map lift-if form)) ;; because {} is passed as [...]
      (update-case-branches CASE #(coll-coerce (map lift-case (concat before [%] after)))))))

(defn lift-case-seq-if [form]
  (let [[if_ pred then else] form]
    (if (case*? pred)
      (update-case-branches pred #(list if_ % then else))
      (list if_ (lift-case pred) (lift-case then) (lift-case else)))))


(defn lift-case-seq-let [form]
  (let [[let_ [sym expr] & bodies] form]
    (if (case*? expr)
      (update-case-branches expr #(apply list let_ [sym %] bodies))
      (apply list let_ [sym (lift-case expr)] (map lift-case bodies)))))


(defn lift-case-seq-try     [form] (update-try form lift-case))
(defn lift-case-seq-catch   [form] (update-catch form lift-case))
(defn lift-case-seq-finally [form] (update-finally form lift-case))
(defn lift-case-seq-case    [form] (update-case-branches form lift-case))


(defmulti  lift-case-seq      (fn [form] (first form)))
(defmethod lift-case-seq :default [form] (lift-case-seq-default form seq))
(defmethod lift-case-seq   'quote [form] form)
(defmethod lift-case-seq     'fn* [form] form)
(defmethod lift-case-seq   'loop* [form] form)
(defmethod lift-case-seq      'if [form] (lift-case-seq-if form))
(defmethod lift-case-seq   'case* [form] (lift-case-seq-case form))
(defmethod lift-case-seq    'let* [form] (lift-case-seq-let form))
(defmethod lift-case-seq     'try [form] (lift-case-seq-try form))
(defmethod lift-case-seq   'catch [form] (lift-case-seq-catch form))
(defmethod lift-case-seq 'finally [form] (lift-case-seq-finally form))



(defn lift-case [form]
  (cond
    (seq? form)    (lift-case-seq form)
    (vector? form) (lift-case-seq-default form vec)
    (set? form)    (lift-case-seq-default form set)
    (map? form)    (lift-case-seq-default (into [] cat form) (partial apply hash-map))
    :else          form))


(defn lift-cases [form] (u/until-fixed-point form lift-case))




(declare lift-let)
(defn lift-let-seq-default [form coll-coerce]
  (let [[before LET after] (u/split-by let*? form)]
    (if (nil? LET)
      (coll-coerce (map lift-let form))
      (let [[let_ pair & bodies] LET
            body (bodies->body bodies)]
        (list let_ pair
          (coll-coerce (concat before [body] after)))))))

(defn lift-let-seq-if [form]
  (let [[if_ pred then else] form]
    ;; pred is visible for both then&else, so lifting only from pred:
    (if-not (let*? pred)
      (list if_ (lift-let pred) (lift-let then) (lift-let else))
      (let [[let_ pair & bodies] pred
            body (bodies->body bodies)]
        (list let_ pair
          (list if_ body (lift-let then) (lift-let else)))))))

(defn lift-let-seq-let [form]
  (let [[let1 [sym1 expr1] & bodies1] form
        body1 (bodies->body bodies1)]
    ;; recursively lifts from exprs first, then - from body.
    ;; never wrap return in recur - rely on fixed-point wrapper later.
    ;; add clause to cond only when need to lift higher than root form.
    (if-not (let*? expr1)
      (list let1 [sym1 (lift-let expr1)]
        (lift-let body1))
      (let [[let2 [sym2 expr2] & bodies2] expr1
            body2 (bodies->body bodies2)]
        (list let2 [sym2 (lift-let expr2)]
          (list let1 [sym1 (lift-let body2)]
            body1))))))


;;dont lift anything from trycatch:
(defn lift-let-seq-try     [form] (update-try form lift-let))
(defn lift-let-seq-catch   [form] (update-catch form lift-let))
(defn lift-let-seq-finally [form] (update-finally form lift-let))
(defn lift-let-seq-case    [form] (update-case-branches form lift-let))


(defmulti  lift-let-seq      (fn [form] (first form)))
(defmethod lift-let-seq :default [form] (lift-let-seq-default form seq))
(defmethod lift-let-seq   'quote [form] form)
(defmethod lift-let-seq     'fn* [form] form)
(defmethod lift-let-seq   'loop* [form] form)
(defmethod lift-let-seq      'if [form] (lift-let-seq-if form))
(defmethod lift-let-seq    'let* [form] (lift-let-seq-let form))
(defmethod lift-let-seq   'case* [form] (lift-let-seq-case form))
(defmethod lift-let-seq     'try [form] (lift-let-seq-try form))
(defmethod lift-let-seq   'catch [form] (lift-let-seq-catch form))
(defmethod lift-let-seq 'finally [form] (lift-let-seq-finally form))

(defn lift-let [form]
  (cond
    (seq? form)    (lift-let-seq form)
    (vector? form) (lift-let-seq-default form vec)
    (set? form)    (lift-let-seq-default form set)
    (map? form)    (lift-let-seq-default (into [] cat form) (partial apply hash-map))
    :else          form))

(defn lift-lets [form] (u/until-fixed-point form lift-let))





(declare dedupe-lets)
(defn dedupe-lets-seq-default [form bound] (map #(dedupe-lets % bound) form))
(defn dedupe-lets-seq-let     [form bound]
  (let [[let_ [sym expr :as pair] & bodies] form]
    (if (or (bound sym)
          ;; removes unused lets added eg. when def has or, but in diffrerent branch.
          (-> bodies (uses? sym) not))
      (-> bodies bodies->body (dedupe-lets bound))
      (map #(dedupe-lets % (conj bound sym)) form))))


(defmulti  dedupe-lets-seq      (fn [form bound] (first form)))
(defmethod dedupe-lets-seq :default [form bound] (dedupe-lets-seq-default form bound))
(defmethod dedupe-lets-seq    'let* [form bound] (dedupe-lets-seq-let form bound))

(defn dedupe-lets
  ([form] (dedupe-lets form #{}))
  ([form bound]
   (cond
     (seq?  form)   (dedupe-lets-seq form bound)
     (vector? form) (vec (dedupe-lets-seq-default form bound))
     (set? form)    (set (dedupe-lets-seq-default form bound))
     (map? form)    (apply hash-map (dedupe-lets-seq-default (into [] cat form) bound))
     :else          form)))


(defn dedupe-ifs [form]
  (u/until-fixed-point form
    #(u/prewalk %
       (fn [x]
         (if-not (if? x)
           x
           (let [[if_ pred then else] x]
             (if (= then else)
               then
               x)))))))

(defn merge-lets [form]
  (u/until-fixed-point form
    #(u/prewalk %
       (fn [x]
         (if-not (let*? x)
           x
           (let [[let_ pairs & bodies] x
                 body (bodies->body bodies)]
             (if-not (let*? body)
               x
               (let [[let2_ pair2 & bodies2] body]
                 (apply list let_ (into pairs pair2) bodies2)))))))))



(declare wrap)
(defn wrap-seq-default [form defs] (map #(wrap % defs) form))
(defn wrap-sym         [sym  defs] (list 'let* [sym (wrap (defs sym) defs)] sym))
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


(defn wrap [form defs]
  (let [f #(wrap % defs)]
    (cond
      (seq?  form)   (wrap-seq form defs)
      (map? form)    (apply hash-map (mapcat f form))
      (vector? form) (mapv f form)
      (set? form)    (set (mapv f form))
      (symbol? form) (if (contains? defs form)  (wrap-sym form defs)  form)
      :else          form)))


(defn optimize-if [form]
  (if-not (if? form)
    form
    (let [[if_ pred then else] form]
      (cond
        (u/any-pred? pred nil? false?)   else
        (u/any-pred? pred map? set? vector? string? number? true? uuid? keyword?)   then
        :else   form))))


(defn optimize-ifs [form] (u/until-fixed-point form #(u/prewalk % optimize-if)))



(declare simplify-loop)
(defn simplify-loop-seq-default [form] (map simplify-loop form))
(defn simplify-loop-seq [form]
  ;; must run before split-lets, and syms-renaming, to keep this impl simple:
  (if-not (loop*? form)
    (simplify-loop-seq-default form)
    (let [[loop_ pairs & bodies] form
          pairs* (partition 2 pairs)
          syms   (map first pairs*)
          exprs  (map second pairs*)
          body   (bodies->body bodies)]
      (assert (every? symbol? syms))
      (if (every? symbol? exprs)
        (list loop_ pairs (simplify-loop body))
        (list 'let* pairs
          (list loop_ (vec (interleave syms syms))
            body))))))

(defn simplify-loop [form]
  (cond
    (seq? form)    (simplify-loop-seq form)
    (vector? form) (vec (simplify-loop-seq-default form))
    (set? form)    (set (simplify-loop-seq-default form))
    (map? form)    (apply hash-map (simplify-loop-seq-default (into [] cat form)))
    :else          form))

(defn simplify-loops [form] (u/until-fixed-point form simplify-loop))



(defn collapse-aliases [form]
  (u/until-fixed-point form
    #(u/prewalk %
       (fn f [form]
         (if-not (let*? form)
           form
           (let [[let_ [sym expr] & bodies] form]
             (if (symbol? expr)
               (-> bodies bodies->body (rename {::rename {sym expr}}))
               form)))))))



(declare insert-prints)
(defn insert-prints-seq-default [form blet-id maxlen __] (map #(insert-prints % blet-id maxlen __) form))
(defn insert-prints-seq-let     [form blet-id maxlen __]
  (let [[let_ pairs & bodies] form
        body   (bodies->body bodies)
        wrap   (fn [[sym expr]]
                 [__ (p/printfn (symbol "let ") blet-id maxlen sym (list 'quote expr))
                  sym (insert-prints expr blet-id maxlen __)
                  __ (p/printfn (symbol "=   ") blet-id maxlen sym sym)])
        pairs+ (->> pairs (partition 2) (mapcat wrap) (vec))]
    (list let_ pairs+
      (p/printfn (symbol "body") blet-id maxlen nil (list 'quote body))
      (insert-prints body blet-id maxlen __))))


(defn insert-prints-seq-if [form blet-id maxlen __]
  (let [[if_ pred then else] form
        pred-sym (gensym "pred__")]
    (list 'let* [__       (p/printfn (symbol "if  ") blet-id maxlen nil (list 'quote pred))
                 pred-sym (insert-prints pred blet-id maxlen __)
                 __       (p/printfn (symbol "=   ") blet-id maxlen nil pred-sym)
                 __       (list 'if pred-sym
                            (p/printfn (symbol "then") blet-id maxlen nil (list 'quote then))
                            (p/printfn (symbol "else") blet-id maxlen nil (list 'quote else)))]
      (list if_ pred-sym
        (insert-prints then blet-id maxlen __)
        (insert-prints else blet-id maxlen __)))))


(defn insert-prints-seq-case [form blet-id maxlen __]
  (let [[case_ sym & _] form
        wrap-branch  (fn [prefix branch]
                       (list 'do
                         (p/printfn (symbol prefix) blet-id maxlen nil (list 'quote branch))
                         (insert-prints branch blet-id maxlen __)))
        wrap-then   #(wrap-branch "then" %)
        wrap-else   #(wrap-branch "else" %)]
    (list 'let* [__ (p/printfn (symbol "case") blet-id maxlen sym sym)]
      (update-case form identity wrap-then wrap-else))))

(defmulti  insert-prints-seq      (fn [form blet-id maxlen __] (first form)))
(defmethod insert-prints-seq :default [form blet-id maxlen __] (insert-prints-seq-default form blet-id maxlen __))
(defmethod insert-prints-seq   'case* [form blet-id maxlen __] (insert-prints-seq-case form blet-id maxlen __))
(defmethod insert-prints-seq    'let* [form blet-id maxlen __] (insert-prints-seq-let form blet-id maxlen __))
(defmethod insert-prints-seq      'if [form blet-id maxlen __] (insert-prints-seq-if form blet-id maxlen __))


(defn insert-prints
  ([form orig file-line]
   (let [blet-id (list 'quote (gensym "blet__"))
         __      (gensym "__")
         maxlen  (->> form
                   (tree-seq coll? seq)
                   (filter let*?)
                   (mapcat second)
                   (partition 2)
                   (map #(-> % first str count))
                   (reduce max 0))]
     (list 'do
       (p/print-start file-line)
       (p/printfn (symbol "src ") blet-id maxlen nil (list 'quote orig))
       (p/printfn (symbol "form") blet-id maxlen nil (list 'quote form))
       (insert-prints form blet-id maxlen __))))
  ([form blet-id maxlen __]
   (cond
     (seq? form)    (insert-prints-seq form blet-id maxlen __)
     (vector? form) (vec (insert-prints-seq-default form blet-id maxlen __))
     (set? form)    (set (insert-prints-seq-default form blet-id maxlen __))
     (map? form)    (apply hash-map (insert-prints-seq-default (into [] cat form) blet-id maxlen __))
     :else          form)))


(defn parse [norm-form]
  (loop [form norm-form
         defs {}]
    (case (first form)
      ::BODY  [defs (bodies->body (rest form))]
      let*    (let [[let_ [sym expr] body] form]
                (recur body (assoc defs sym expr))))))



(defn blet [&form_ let-binds-body print? file-line]
  (let [[defs form] (-> let-binds-body (optimize-ifs) (simplify-loops) (split-lets) (rename) (parse))]
    (-> form
      (wrap defs)
      (u/until-fixed-point #(-> % (lift-ifs) (lift-cases) (lift-lets)))
      (dedupe-lets)
      (dedupe-ifs)
      (collapse-aliases)
      (merge-lets)
      (cond->
        print? (insert-prints &form_ file-line)))))
