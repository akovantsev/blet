# blet
Even better let+cond clojure macro.

Use it, but don't abuse it.


# What

```clojure
;; in clojure/clojurescript
(:require [com.akovantsev.blet.core :refer [blet]])

(macroexpand-1
 '(blet [x 1
         y 2
         z 3
         t :foo
         a (println x y)
         b (println x z)
         c (println y z)]
    (cond
      1 a
      2 b
      3 c)))

;;=>
(if 1
  (let [x 1
        y 2
        a (println x y)] a)
  (if 2
    (let [x 1
          z 3
          b (println x z)] b)
    (if 3
      (let [y 2
            z 3
            c (println y z)] c)
      nil)))
```

# Install
```clojure
;; in deps.edn
{:deps {github-akovantsev/blet
        {:git/url "https://github.com/akovantsev/blet"
         :sha     "2a8dd6f376dcb3e8dca37bde8a10ec1aa168e0ee"}}} ;; actual sha
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
(blet! [[_ _ a & tail]   (range)
        foo              (conj (range 5) a)]
  (cond 
    false   1
    nil     2
    (< a 0) (count tail) ;; gonna be huge!
    true    (conj foo a)))

;; prints out: 
???   false
???   nil
<<<   vec__8594     (range)
===   vec__8594     (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 ...)
<<<   seq__8595     (clojure.core/seq vec__8594)
===   seq__8595     (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 ...)
<<<   seq__8595     (clojure.core/next seq__8595)
===   seq__8595     (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 ...)
<<<   seq__8595     (clojure.core/next seq__8595)
===   seq__8595     (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 ...)
<<<   first__8596   (clojure.core/first seq__8595)
===   first__8596   2
<<<   a             first__8596
===   a             2
???   (< a 0)
???   true
<<<   foo           (conj (range 5) a)
===   foo           (2 0 1 2 3 4)
>>>   (conj foo a)
;; and returns:
=> (2 2 0 1 2 3 4)
```

Where:
- `<<<` is a binding form, as it appears in macroexpand
- `===` is a printed value of the result of that binding
- `???` is a predicate form
- `>>>` is a winner branch form