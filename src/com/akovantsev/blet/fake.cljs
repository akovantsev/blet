(ns com.akovantsev.blet.fake)

;; this ns is to avoid these warnings while running tests:
;WARNING: No such namespace: m, could not locate m.cljs, m.cljc, or JavaScript source providing "m" at line 66 /.../blet/src/com/akovantsev/blet/core.cljc
;WARNING: Use of undeclared Var m/*environment* at line 66 /.../blet/src/com/akovantsev/blet/core.cljc

;WARNING: Use of undeclared Var com.akovantsev.blet.fake/macroexpand-all at line 63 /.../blet/src/com/akovantsev/blet/core.cljc
;WARNING: Use of undeclared Var clojure.walk/macroexpand-all at line 64 /.../blet/src/com/akovantsev/blet/core.cljc

(def ^:dynamic *environment*)
(defn macroexpand-all [&args])