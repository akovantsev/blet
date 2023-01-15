(ns com.akovantsev.blet.test2
  (:require
   [com.akovantsev.blet.print :as p]
   [com.akovantsev.blet.utils :as u :refer [assert= reset-gensym]]
   [com.akovantsev.blet.impl2 :as impl]
   [clojure.string :as str]
   [com.akovantsev.blet.core :refer [blet blet! blet? macroexpand-all NOPRINT]]))

;;fixme handle rename in quote

(assert= "reset-gensym"
  ;; copypaste because of macroexpand-1 implementation in cljs
  (reset-gensym (macroexpand-1 '(let [{:keys [:x ::y] [a b & tail] :z} {}])))
  (reset-gensym (macroexpand-1 '(let [{:keys [:x ::y] [a b & tail] :z} {}]))))


(assert= "split-lets" (impl/split-lets '(let* [a 1 b 2 c 3] a b)) '(let* [a 1] (let* [b 2] (let* [c 3] a b))))
(assert= "split-lets" (impl/split-lets '(let* [a 1] a)) '(let* [a 1] a))
(assert= "split-lets" (impl/split-lets '(let* [] a b)) '(do a b))
(assert= "split-lets" (impl/split-lets '(let* [] a)) 'a)
(assert= "split-lets" (impl/split-lets '(let* [a (let* [b 1 d 2] b)] a)) '(let* [a (let* [b 1] (let* [d 2] b))] a))
(assert= "split-lets" (impl/split-lets '{a (let* [x 1 y 2] [x y])}) '{a (let* [x 1] (let* [y 2] [x y]))})
(assert= "split-lets" (impl/split-lets '(let* [a 1 b 1] (+ 1 (let* [c 2 d 2] c)))) '(let* [a 1] (let* [b 1] (+ 1 (let* [c 2] (let* [d 2] c))))))
(assert= "split-lets"
  (impl/split-lets
    '(let* [a 1 b (let* [d 1 e (let* [x 1 y 2] (let* [s 2 f 3] s))] d e)
            c 2] a b))
  '(let* [a 1]
     (let* [b (let* [d 1] (let* [e (let* [x 1] (let* [y 2] (let* [s 2] (let* [f 3] s))))] d e))]
       (let* [c 2] a b))))


(def lift-ifs #(u/until-fixed-point % impl/lift-ifs))

(assert= "lift-if 1" (lift-ifs '(a b (if test then else) c)) '(if test (a b then c) (a b else c)))
(assert= "lift-if 2" (lift-ifs '#{a b (if test then else) c}) '(if test #{a c then b} #{a else c b}))
(assert= "lift-if 3" (lift-ifs '{a b (if test then else) c}) '(if test {a b, then c} {a b, else c}))
(assert= "lift-if 4" (lift-ifs '{a b c (if test then else)}) '(if test {a b, c then} {a b, c else}))

(assert= "lift-if 5"
  (lift-ifs '(let* [d 3] (let* [a (+ 1 (if b b 2))] a)))
  '(let* [d 3] (if b
                 (let* [a (+ 1 b)] a)
                 (let* [a (+ 1 2)] a))))

(assert= "lift-if 6"
  (lift-ifs '(let* [b 3] (let* [a (+ 1 (if b b 2))] a)))
  '(let* [b 3]
     (if b
       (let* [a (+ 1 b)] a)
       (let* [a (+ 1 2)] a))))

(assert= "lift-if 7: if as predicate:"
  (reset-gensym
    (macroexpand
      '(blet [a (if b 1 2)]
         [(cond a 3)])))
  '[3])

(= 3 (let [b 5]
       (blet [a (if b 1 2)]
         (cond a 3))))

(assert (= (impl/lift-lets '(+ 1 (let* [b 1] b c) 2)))  '(let* [b 1] (+ 1 (do b c) 2)))
(assert (= (impl/lift-lets '(if (let* [a 1] (+ a 2)) b c)) '(let* [a 1] (if (+ a 2) b c))))
(assert (= (impl/lift-lets '(let* [a (let* [x 1] x y)] a b)) '(let* [x 1] (let* [a (do x y)] (do a b)))))
(assert= "lift let"
  '(let* [a 1] (let* [x 2] (do a x)))
  (impl/lift-lets '(let* [a 1] (let* [x 2] a x))))

(impl/dedupe-lets '(let* [x 1] [(let* [x 1] 2 [3 (let* [y 2] (let* [x 1] y))])]))

(def dedupe-ifs #(u/until-fixed-point % impl/dedupe-ifs))

(assert= "dedupe-ifs 1"
  (dedupe-ifs
    '(let* [or__0 1]
       (if or__0
         (let* [q or__0 w q x w] (if or__0 [x w] [x w]))
         (let* [q 2     w q x w] (if or__0 [x w] [x w])))))
  '(let* [or__0 1]
     (if or__0
       (let* [q or__0 w q x w] [x w])
       (let* [q 2     w q x w] [x w]))))

(assert= "dedupe-ifs quoted"
  (dedupe-ifs
    '(let* [or__0 1]
       '(if or__0
          (let* [q or__0 w q x w] (if or__0 [x w] [x w]))
          (let* [q 2     w q x w] (if or__0 [x w] [x w])))))
  '(let* [or__0 1]
     '(if or__0
        (let* [q or__0 w q x w] (if or__0 [x w] [x w]))
        (let* [q 2     w q x w] (if or__0 [x w] [x w])))))

(-> '(let* [a 1] (let* [b 2] (let* [c 3] [(let* [d 4] x)])))
  (u/until-fixed-point impl/merge-lets))



(assert= "lift-ifs"
  (lift-ifs '(let* [x (if p t e)] x))
  '(if p (let* [x t] x)
         (let* [x e] x)))

(assert= "lift-ifs"
  (lift-ifs '(let* [e2 (if p1 t1 e1)] (if p2 t2 e2)))
  '(if p1 (if p2 t2 (let* [e2 t1] e2))
          (if p2 t2 (let* [e2 e1] e2))))

(assert= "lift-ifs"
  (lift-ifs '(let* [t2 (if p1 t1 e1)] (if p2 t2 e2)))
  '(if p1 (if p2 (let* [t2 t1] t2) e2)
          (if p2 (let* [t2 e1] t2) e2)))

(assert= "lift-ifs"
  (lift-ifs '(let* [x (if p t e)] y))
  '(if p (let* [x t] y) (let* [x e] y)))

(assert= "lift-ifs"
  (lift-ifs '(let* [p 1] (let* [x (if p t e)] y)))
  '(let* [p 1] (if p (let* [x t] y) (let* [x e] y))))

(assert= "lift-ifs"
  (lift-ifs '(let* [y 1] (let* [x (if p t e)] y)))
  '(let* [y 1] (if p (let* [x t] y) (let* [x e] y))))

(assert= "lift-ifs"
  (lift-ifs '(let* [z 1] (let* [x (if p t e)] y)))
  '(let* [z 1] (if p (let* [x t] y) (let* [x e] y))))


(assert= "simplify-loops"
  (reset-gensym
    (macroexpand-1
      '(blet [] (loop* [x [1 2 3] x (foo x 2)] [x x]))))
  '(let* [x__0 [1 2 3]
          x__1 (foo x__0 2)]
    (loop* [x__2 x__1
            x__3 x__2]
      [x__3 x__3])))




(assert= "macroexpand works"
  (reset-gensym
    (macroexpand-1
      '(blet [x 1] (when (foo 1) x))))
  '(if (foo 1) (let* [x__0 1] (do x__0)) nil))

(assert= "shallow if optimization"
  (reset-gensym
    (macroexpand-1
      '(blet [] (if false 1 2))))
  '2)


(assert= "deep if optimization 1"
  (reset-gensym
    (macroexpand-1
      '(blet [] [(if false 1 [(cond 2 3 false 4 nil 5 6 7)])])))
  '[[3]])

(assert= "deep if optimization 2"
  (reset-gensym
    (macroexpand-1
      '(blet [] [(if false 1 [(cond (2) 3 false 4 nil 5 6 7)])])))
  '(if (2) [[3]] [[7]]))

(assert= "destructure support example 1"
  (reset-gensym
    (macroexpand-1
      '(blet [[x y & tail] (range 10)
              [a b & tail2] tail]
         [a b tail2 x y tail])))
  '(let* [vec__0    (range 10)
          seq__1    (clojure.core/seq vec__0)
          seq__2    (clojure.core/next seq__1)
          seq__3    (clojure.core/next seq__2)
          seq__4    (clojure.core/seq seq__3)
          first__5  (clojure.core/first seq__4)
          seq__6    (clojure.core/next seq__4)
          first__7  (clojure.core/first seq__6)
          seq__8    (clojure.core/next seq__6)
          first__9  (clojure.core/first seq__1)
          first__10 (clojure.core/first seq__2)]
     [first__5 first__7 seq__8 first__9 first__10 seq__3]))


(assert= "tests fix of https://github.com/akovantsev/blet/issues/1"
  '(let* [vec__0   [1 2]
          seq__1   (clojure.core/seq vec__0)
          seq__2   (clojure.core/next seq__1)
          first__3 (clojure.core/first seq__2)]
     (if (not first__3)
       :_
       (let* [first__4 (clojure.core/first seq__1)] first__4)))

  (reset-gensym
    (macroexpand-1
      '(blet [[a b & _] [1 2]]  ;; with &
         (cond
           (not b)   :_          ;; use b before using a
           true      a)))))


(assert= "destructure support example 1"
  '(if (1)
    (let* [vec__0 (range 10)
           seq__1 (clojure.core/seq vec__0)
           first__2 (clojure.core/first seq__1)]
      first__2)
    (if (2)
     (let* [vec__0 (range 10)
            seq__1 (clojure.core/seq vec__0)
            seq__3 (clojure.core/next seq__1)
            first__4 (clojure.core/first seq__3)]
       first__4)
     (if (3)
       (let* [vec__0 (range 10)
              seq__1 (clojure.core/seq vec__0)
              seq__3 (clojure.core/next seq__1)
              seq__5 (clojure.core/next seq__3)]
         seq__5)
      nil)))
  (reset-gensym
    (macroexpand-1
      '(blet [[x y & tail] (range 10)]
         (cond
           (1) x
           (2) y
           (3) tail)))))


(assert= "test code elimination with locals shadowing 1"
  (reset-gensym
    (macroexpand-1
      '(blet [x 1 y (fn [x] x)] (cond 1 (y 1)))))
  '(let* [y__0 (fn* fn__1 ([x__2] x__2))] (y__0 1)))



(assert= "locals shadowing 2"
  '(if (1) (let* [x__0 2] (x__0 1)) nil)
  (reset-gensym
    (macroexpand-1
      '(blet [x 1 y (let [x 2] x)] (cond (1) (y 1))))))


(macroexpand-1
  '(blet! []
     (loop []
       (if (even? (rand-nth [1 2 3]))
         (do "must terminate soon" (recur))
         "done"))))

(assert= "dont lift lets out of loop"
  (reset-gensym
    (macroexpand-1
      '(blet [] (loop  [] (let  [x    (rand-nth [1 2 3])] (if (even? x)    (recur) done))))))
  '(loop* [] (let* [x__0 (rand-nth [1 2 3])] (if (even? x__0) (recur) done))))



(assert= "locals shadowing 4"
  (blet [x 1 y (for [z [2 3]] (+ z x))] (cond (odd? 1) y))
  '(3 4))

(reset-gensym
  (macroexpand-1
    '(blet [x 1
            z (loop [z [true false]] (if (or z false) x 2))
            y z]
       y)))

(assert= "locals shadowing 5"
  (reset-gensym
    (macroexpand-1
      '(blet [x 1 y (fn x [a] a)] (cond foo (y 1)))))
  '(if foo (let* [y__0 (fn* x__1 ([a__2] a__2))] (y__0 1)) nil))



(assert= "dont eval let bindings before cond dispatch 1"
  2
  (blet [A (atom 0)
         a (do (swap! A + 1) @A)
         b (do (swap! A + 2) @A)
         c (do (swap! A + 3) @A)]
    (cond
      ((constantly false)) a
      ((constantly true))  b
      ((constantly false)) c)))


(assert= "dont eval let bindings before cond dispatch 2"
  (+ 0 10 20 2)
  (blet [A (atom 0)
         a (do (swap! A + 1) @A)
         b (do (swap! A + 2) @A)
         c (do (swap! A + 3) @A)]
    (cond
      ;; plz don't ever do it tho, omg.
      (do (swap! A + 10) false) a
      (do (swap! A + 20) true)  b
      (do (swap! A + 50) false) c)))



(println "Testing blet! inside loop due to: 'Cannot recur across try.'")
(loop []
  ;; if print-len binding does not work - gonna try to print infinite range:
  (blet! [a (range)]
    (cond (empty? a) (recur))))

(println "\nAnd now with shorter *default-print-len*:")
(binding [p/*default-print-len* 5]
  (loop []
    (blet! [a (range)]
      (cond (empty? a) (recur)))))

(println "\nAnd now with *print-length*:")
(binding [*print-length*        10
          p/*default-print-len* 5]
  (loop []
    (blet! [a (range)]
      (cond (empty? a) (recur)))))

(println "\nAnd now with *print-length*:")
(binding [*print-length*        10
          p/*default-print-len* 5]
  (blet! [a (range)]
    (loop []
      (cond (empty? a) (recur)))))


(assert= "declared outside of fn will stay outside of fn"
  '(let* [a__0 1 f__1 (fn* f__2 ([] a__0))] f__1)
  (reset-gensym
    (macroexpand
      '(blet [a 1
              f (fn f [] a)]
         f))))


(assert= "declared inside of fn will stay inside of fn"
  '(let* [f__0 (fn* f__1 ([] (let* [a__2 2] a__2)))] f__0)
  (reset-gensym
    (macroexpand
      '(blet [a 1
              f (fn f [] (let [a 2] a))]
         f))))


(assert= "anon fns get named"
  '(let* [f__0 (fn* f__1 ([f__2] f__2))] f__0)
  (reset-gensym
    (macroexpand
      '(blet [f (fn f [f] f)] f))))

#?(:clj (defn ME [form] (reset-gensym (macroexpand form))))

(assert= "fn is opaque 1: syms used inside fn, but declared outside fn - get declared outside fn"
  '(let* [b__0 (bbbb)]
     (if b__0
       (let* [a__1 (aaaa) c__2 (fn* fn__3 ([] (if a__1 a__1 b__0)))] c__2)
       (let* [a__4 (aaaa) c__5 (fn* fn__6 ([] a__4))] c__5)))
  (reset-gensym
    (macroexpand
      '(blet [a (aaaa)
              b (bbbb)
              d (or b a)
              c (fn [] (or a b d))]
         c))))

(assert= "fn is opaque 2"
  '(let* [a__0 (aaaa)]
    (if a__0
     (let* [c__1 (fn* fn__2 ([] a__0))] c__1)
     (let* [b__3 (bbbb)]
      (if b__3
       (let* [c__4 (fn* fn__5 ([] b__3))] c__4)
       (let* [c__6 (fn* fn__7 ([] a__0))] c__6)))))
  (reset-gensym
    (macroexpand
      '(blet [a (aaaa)
              b (bbbb)
              d (or b a)
              e (or a b d)
              c (fn [] e)]
         c))))

(assert= "fn is opaque 3"
  (reset-gensym (macroexpand '(blet [a aaaa b bbbb d (or b a) e (or a b d) c (fn [] e)] c)))
  (reset-gensym (macroexpand '(blet [a aaaa b bbbb d (or b a) e (or a b d) c #(-> e)]   c))))


(assert= "rename try-catch-finally 1"
  '(try e (catch Exception e__0 e__0) (finally e))
  (reset-gensym
    (impl/rename
      (macroexpand
        '(try e (catch Exception e e) (finally e))))))

;;review: maybe can lift if in both try and finally?
(assert= "rename try-catch-finally 2"
  '(let* [e__0 1 c__1 3 b__2 2]
     (try e__0 (catch Exception e__3 e__3 b__2) (finally e__0 c__1)))
  (reset-gensym
    (macroexpand
      '(blet [e 1 b 2 c 3]
         (try e (catch Exception e e b) (finally e c))))))


(assert= "lazy chain deps"
  '(if bbbb bbbb aaaa)
  (reset-gensym
    (macroexpand
      '(blet [a aaaa
              b bbbb
              d (or b a)]
         d))))


(assert= "lazy chain deps2"
  '(if bbbb bbbb aaaa)
  (reset-gensym
    (macroexpand
      '(blet [a aaaa
              b bbbb]
         (or b a)))))

(assert= "opaque try"
  '(let* [b__0 2 c__1  3]
     (if b__0
       (f c__1 c__1)
       (let* [a__2  1
              d__3  (try a__2 (catch ex e__4 b__0) (finally c__1))] (f d__3 c__1))))
  (reset-gensym
    (macroexpand
      '(blet [a 1
              b 2
              c 3
              d (if b c (try a (catch ex e b) (finally c)))]
         (f d c)))))

(assert= "opaque quote"
  '(let* [d__0 ('a 'b 'c) c__1 3] (f d__0 c__1 'c))
  (reset-gensym
    (macroexpand
      '(blet [a 1
              b 2
              c 3
              d ('a 'b 'c)]
         (f d c 'c)))))



(assert= "opaque loop 1"
  '(let* [a__0 1]
     (if a__0
       (loop* [d__1 a__0] (if (even? d__1) d__1 (recur (foo d__1))))
       (let* [b__2 2]
         (loop* [d__3 b__2] (if (even? d__3) d__3 (recur (foo d__3)))))))
  (reset-gensym
    (macroexpand
      '(blet [a 1
              b 2]
         (loop [d (or a b)]
           (if (even? d) d (recur (foo d))))))))

(assert= "opaque loop 2"
  '(let* [b__0 2 a__1 1]
     (loop* []
       ;; if is not lifted above let because a and b are already bound outside loop
       (let* [d__2 (if a__1 a__1 b__0)]
         (if (even? d__2)
           d__2
           (recur (foo d__2))))))

  (reset-gensym
    (macroexpand
      '(blet [a 1
              b 2]
         (loop []
           (let [d (or a b)]
             (if (even? d) d (recur (foo d)))))))))

(assert= "nested blets inside opaque loop 1"
  '(let* [b__0 2 a__1 1]
     (loop* []
       (if a__1
         (if (even? a__1) a__1 (recur (foo a__1)))
         (if (even? b__0) b__0 (recur (foo b__0))))))
  (reset-gensym
    (macroexpand
      '(blet [a 1
              b 2]
         (loop []
           (blet [d (or a b)]
             (if (even? d) d (recur (foo d)))))))))

(println "\nTesting nested blet! does not blow up:")
(blet! [a 1]
  [(blet! [b 2]
     [b])])

(assert= "long cases do not blow up (because of sorted-map in case*):" 1 (blet [] (case 1, 1 1 2 2 3 3 4 4 5 5 6 6 7 7)))

(reset-gensym
  (macroexpand
    '(blet [a 2
            b 3
            c (case a 1 :a [2 a] [:b b] :c)]
       (if foo
         [1 b c]
         [2 a c]))))


(println "\nTesting NOPRINT prints expr, but does not print value:")
(blet! [a (NOPRINT (inc 2))
        b (inc 3)]
  [a b])


(assert= "eliminate dead branches 1"
  '(if a 1 3)
  (reset-gensym
    (macroexpand
      '(blet [] (if a 1 (if a 2 3))))))


(assert= "eliminate dead branches 2"
  '(if a 1 (foo 3))
  (reset-gensym
    (macroexpand
      '(blet [] (if a 1 (foo (if a 2 3)))))))

(assert= "eliminate dead branches 3"
  '[a]
  (reset-gensym
    (macroexpand
      '(blet [] [(if a a a)]))))

(let [a (inc 1)] (blet [] (if a a a)))

(assert= "eliminate dead branches 5"
  '[a]
  (reset-gensym
    (macroexpand
      '(blet [] [(if (or a a (and a a)) (or a a a) (and a))]))))


(assert= "eliminate dead branches 4"
  '(if b b c)
  (reset-gensym
    (macroexpand
      '(blet [a (or b c (and b c))]
         (if b a c)))))


(assert= "guard map destructuring defensive ifs from blet modification:"
  ;; this if gonna be different if tests are run with clj 1.11:
  '(let* [map__0 {}
          map__1 #?(:cljs (cljs.core/--destructure-map map__0)
                    :clj (if (clojure.core/seq? map__0)
                           (. clojure.lang.PersistentHashMap create (clojure.core/seq map__0))
                           map__0))
          a__2 #?(:cljs (cljs.core/get map__1 :a)
                  :clj (clojure.core/get map__1 :a))]
     a__2)
  (reset-gensym
    (macroexpand
      '(blet [{:keys [:a :b]} {}]
         a))))


(time
  (defn add-line-keyword [node kw-id kw]
    (blet [{:keys [:text :spans]} node
           lower   (str/lower-case text)
           idxs+   (loop [idx 0 done #{}]
                     (if-let [idx (str/index-of lower kw idx)]
                       (let [end (+ idx (count kw))]
                         (recur end (into done (range idx end))))
                       done))
           blanks  (zipmap (range (count lower)) (repeat #{}))
           by-idx  (->> spans
                     (map (fn [[[a b] v]] (zipmap (range a (inc b)) (repeat v))))
                     (reduce merge blanks))
           by-idx2 (reduce
                     (fn [m idx] (update m idx u/sconj kw-id))
                     by-idx
                     idxs+)
           spans2  (->> by-idx2
                     (sort-by first)
                     (partition-by val)
                     (reduce
                       (fn [!m idxed]
                         (assoc! !m [(-> idxed first key) (-> idxed last key)] (-> idxed first val)))
                       (transient {}))
                     (persistent!))
           line+   (-> node
                     (assoc :spans spans2)
                     (update :kw-ids u/sconj kw-id))]
      (cond
        (empty? text)  node
        (empty? idxs+) node
        :else          line+))))


(time
  (reset-gensym
    (macroexpand
      '(blet [{:keys [::a ::b ::c ::d ::e]} {}]
         (cond
           a        (or a b)
           (or b d) (or b a c)
           e        (or b a (and d a)))))))

(assert= "case descructuring"
  (reset-gensym
    (macroexpand
      '(blet [a (+ 1 2)
              b (inc 7)]
         (case a 2 (inc b) :foo))))
  #?(:clj
     '(let* [a__0 (+ 1 2)]
        (case* a__0 0 0 :foo {2 [2 (let* [b__1 (inc 7)] (inc b__1))]} :compact :int))
     :cljs
     '(let* [a__0 (js* "(~{} + ~{})" 1 2)]
        (case* a__0 [[2]]
          [(let* [b__1 (js* "(~{} + ~{})" 7 1)] (js* "(~{} + ~{})" b__1 1))]
          :foo))))


(println "\n\nTests are done with asserts, so if this is printed out – all is good.")