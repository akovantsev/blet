# blet
Even better let+cond clojure macro.

Use it, but don't abuse it.

# Installation
```clojure
;; deps.edn
{:deps {github-akovantsev/blet
        {:git/url "https://github.com/akovantsev/blet"
         :sha     "f71528368268222db56875405a6db75c2a4f532d"}}} ;; actual sha
```


# What

```clojure
;; clojure/clojurescript
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

## Core value proposition, `blet`
 
interleaves `let` bindings and `cond` branches, preserving* order of both, so that only bindings
required 1) to get to 2) and evaluate winning `cond` branch, are declared.

*You only need t(w)o:
1) write `blet` bindings in a valid dependency order (as you would do in `let` anyway), and to
2) choose correct order of `cond` branches (as you'd do anyway).

Both are essential, but already are natural to anybody (yes?).
As `blet` relies on them for bindings interleaving with cond branches.

The only code analisys done is to figure out symbol declarations and dependencies between bindings and branches.

`clojure.core/destructure` is leveraged to do that.


# Weaknesses

- Does not (yet) detects nested bindings, which shadow, but not use symbols defined earlier
```clojure
(macroexpand-1 
  '(blet [x (sideeffect!)
          y (let [x 2] x)]
     (cond
       true y)))

;; =>
(if true 
  (let [x (sideeffect!)
        y (let [x 2] x)] y)
  nil)

;; when really it should be this, since x is never used 
;; (setting aside entire sideeffect-in-let-binding-thing talk for now):
(if true 
  (let [y (let [x 2] x)] y)
  nil)
``` 


# TODO
- more convoluted poster examples
- always inlude `_` bindings, to allow everyone's best friend `(let [_ (println stuff)])`, and unconditional sideeffects in general.
- detect shadowing of declared, but unused locals in nested forms like `let/fn/letfn/binding/with-redefs/etc.`
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
