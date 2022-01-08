(ns com.akovantsev.blet.env)

(def ^:dynamic *js?*)

;; thanks! https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html#gotcha-4-clj-macros
(defn js? [macro-env] (boolean (:ns macro-env)))