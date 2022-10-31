```
⚠️ blet v2 is of beta quality. see tests. Docs – TBA.
```

# blet
Even better let+cond clojure macro.

Use it, but don't abuse it.


# What

<table>
<tr>
 <th>macro</th>
 <td>clojure.core/let</td>
 <td>com.akovantsev.blet.core/blet</td>
</tr>
<tr>
<th>form</th>
<td>

```clojure
(let [x 1
      y 2
      z 3
      t :foo
      a (println x y)
      b (println x z)
      c (println y z)]
  (cond
    1 a
    2 b
    3 c))
```

 </td>
 <td>

```clojure
(blet [x 1
       y 2
       z 3
       t :foo
       a (println x y)
       b (println x z)
       c (println y z)]
  (cond
    1 a
    2 b
    3 c))
```

</td>
</tr>
<tr>
<th>output</th>
<td>

```
1 2
1 3
2 3
=> nil
```

</td>
<td>

```
1 2
=> nil
```

</td>
</tr>
<tr>
<th>macroexpand</th>
<td>

```clojure
(let* [x 1
       y 2
       z 3
       t :foo
       a (println x y)
       b (println x z)
       c (println y z)]
  (cond
    1 a
    2 b
    3 c))
```

</td>
<td>

```clojure
(if 1
  (let* [x 1
         y 2
         a (println x y)] a)
  (if 2
    (let* [x 1
           z 3
           b (println x z)] b)
    (if 3
      (let* [y 2
             z 3
             c (println y z)] c)
      nil)))
```

</td> 
</tr>
</table>

# Install
```clojure
;; in deps.edn
{:deps {github-akovantsev/blet
        {:git/url "https://github.com/akovantsev/blet"
         :sha     "5aca99d960406878bae5680e4e672a5084adfc6b"}}} ;; actual sha
```

```clojure
;; in clojure/clojurescript
(:require [com.akovantsev.blet.core :refer [blet]])
```

# Core value proposition, `blet`
 
interleaves `let` bindings and `cond` branches, preserving* order of both, so that:
 - only bindings required 1) to get to 2) and evaluate winning `cond` branch, are declared.
 - you don't have to solve that puzzle yourself each time upon changes, by manually rearranging code,
  extrating duplicate bindings to functions, and what not.

*You only need t(w)o:
1) write `blet` bindings in a valid dependency order (as you would do in `let` anyway), and to
2) choose correct order of `cond` branches (as you'd do anyway).

Both are essential, but already are natural to anybody (yes?).
As `blet` relies on them for bindings interleaving with cond branches.

The only code analisys done is to figure out symbol declarations and dependencies between bindings and branches.

`clojure.core/destructure` is leveraged to do that.


# Rationale
For general intro and rationale I refer you to the excelent: https://github.com/Engelberg/better-cond/blob/86d93ffb143881bf6057b00e8345a0c6c0ba4969/README.md#rationale .

Except, `blet` addresses this (to some extent): https://github.com/Engelberg/better-cond/blob/86d93ffb143881bf6057b00e8345a0c6c0ba4969/README.md#cant-you-just-put-all-the-name-bindings-at-the-top-before-your-cond

My core beef with `better-cond` – it is (was?) not supported by https://cursive-ide.com/ (and, I'd imagine, not only Cursive).
And it hurts the most, where people reach for `b/cond` the most: in the long-ass branchy functions.
It results in a wall of unresolved symbols, and a blind spot to (again, some) IDEs. 

I am all for reading code carefully and for knowing what exactly is committed into a project 
(~5? years of python development with no IDE support, besides the syntax highlighting).
But sometimes you just want to rename things with a single hot-key press,
and not go fulltext search on repo to make sure nobody has died.


# TODO
- more convoluted poster examples
- always inlude `_` bindings, to allow everyone's best friend `(let [_ (println stuff)])`, and unconditional sideeffects in general.
- ~~support extra `cond` branch as default (like in `case`).~~
  For now, I'd like to preserve both let's and cond's original syntax, so you could just change `blet` to `let` for pure forms, and that's it.



# Non goals

Code simplification, like:
 - inlining definitions:
    ```clojure
    (let [x 1] x)
    ;;=>
    x
    ```
- ~~removing nested unused definitions~~
    ```clojure
    (let [[x y] (range), {:keys [a b]} {}]
      [x a])
    ;;=>
    (let [[x  ] (range), {:keys [a  ]} {}]
      [x a])
    ```
    To add prior clojure versions support I replaced custom `clojure.core.specs`-based bindings parsing code with use of
     `clojure.core/destructure`. As a result, I got 1) unused nested bindings elimination, and 2) slightly harder to read macroexpansion (see tests for examples).

- shortcircuiting truthy branches with literals as predicates (hm, this one is interesting!)
    ```clojure
    (if true 
      a
      (...))
    ;;=>
    a
    ```

The only code simplification done is: unused entire bindings don't get included into output.

```clojure
(macroexpand-1 
  '(blet [x 1
          y 2]
     (cond
       true y)))
;;=> 
(if true
 (let [y 2] y)
 nil)
```

# Wait, there is more!
`blet` has a `blet!` variant with printing included, so you don't have to manually insert print statements while debugging.
It limits `clojure.core/*print-length*` too, if it is `nil`:

```clojure
(blet! [[a b c & tail]   (range)
        n                (count tail)
        foo              (conj (range 5) a)]
  (cond
    false   1
    nil     2
    (< b 0) n
    true    (conj foo (blet! [x    (+ 2 a)
                              xxx  (* 10 a)
                              yyyy (+ 3 x)]
                        (cond
                          (pos? -1) x
                          (pos? b)  (- yyyy 10))))))
;; prints out:
..................................................
blet__2223   ???                 false
blet__2223   ???                 nil
blet__2223   :==   vec__2224     (range)
blet__2223   ===   vec__2224     (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 ...)
blet__2223   :==   seq__2225     (clojure.core/seq vec__2224)
blet__2223   ===   seq__2225     (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 ...)
blet__2223   :==   seq__2225     (clojure.core/next seq__2225)
blet__2223   ===   seq__2225     (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 ...)
blet__2223   :==   first__2226   (clojure.core/first seq__2225)
blet__2223   ===   first__2226   1
blet__2223   :==   b             first__2226
blet__2223   ===   b             1
blet__2223   ???                 (< b 0)
blet__2223   ???                 true
blet__2223   :==   first__2226   (clojure.core/first seq__2225)
blet__2223   ===   first__2226   1
blet__2223   :==   a             first__2226
blet__2223   ===   a             1
blet__2223   :==   foo           (conj (range 5) a)
blet__2223   ===   foo           (1 0 1 2 3 4)
blet__2223   >>>                 (conj foo (blet! [x (+ 2 a) xxx (* 10 a) yyyy (+ 3 x)] (cond (pos? -1) x (pos? b) (- yyyy 10))))
..................................................
blet__2228   ???          (pos? -1)
blet__2228   ???          (pos? b)
blet__2228   :==   x      (+ 2 a)
blet__2228   ===   x      3
blet__2228   :==   yyyy   (+ 3 x)
blet__2228   ===   yyyy   6
blet__2228   >>>          (- yyyy 10)
;; and returns:
=> (-4 1 0 1 2 3 4)
```

Where:
- `:==` is a binding form, as it appears in macroexpand
- `===` is a printed value of the result of that binding
- `???` is a predicate form
- `>>>` is a winner branch form


Print is customizable:

```clojure
(:require
  [com.akovantsev.blet.print :as p]
  [com.akovantsev.blet.core :refer [blet!]])


;; padding tags to the same length is on you:
(defmethod p/tag ::p/branch-form [_] (symbol "🤮 "))

;; 1) don't forget to quote things.
;; 2) returned form will be evaled either clj or cljs, so namespaces matter
;;    (e.g. 'clojure.pprint/pprint vs 'cljs.pprint/pprint), sorry.
(defmethod p/printfn ::p/pred-form [k blet-id maxlen label val]
  (list 'clojure.core/prn (list 'quote 'pewpew) k val))

(blet! [a (inc 1)]
  (cond (= 2 2) a))

;; prints out:
..................................................
pewpew :com.akovantsev.blet.print/pred-form (= 2 2)
blet__2227   :==   a   (inc 1)
blet__2227   ===   a   2
blet__2227   🤮        a
;; returns:
=> 2
```
