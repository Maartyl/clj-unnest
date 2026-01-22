
(ns maa)

;; inspired by ->> and better-cond

(defn nest [[op & [arg & xss :as xs]]]
  (cond
    ;; handles empty clauses as well -> nil
    (not xs) op
    #_`(~@op (<| ~@xs))
    (list? op) (concat op [(nest xs)])
    #_`(~op ~arg (<| ~@xss))
    (symbol? op) (list op arg (nest xss))
    ;;only other opt for op should be a symbol -- otherwise undefined
    :else (throw (ex-info "<| op ∉ #{list symbol}" {:op op :remains xs}))))


;;clj-kondo support in .clj-kondo/config.edn
(defmacro <|
  "Instead of nesting with body as last term. Supports most macros (any ops):
   use (<| op expr ...<|)  for (op expr body) -> if single arg before body: works without extra parens
   use (<| (op ...) ...<|) for (op ... body) -> use explicit list instead of just a symbol expr pair.
     (<|
       let [a (foo)]
       (if (odd? a) :early-ret)
       do (println a)
       let [a (quot a 2)]
       (if (odd? a) 2)
       :innermost)."
  [& clauses]
  (nest clauses))

;;dbg only
#_(comment
    (defmacro macrgs [& clauses] clauses)
    macroexpand-1
    clojure.walk/macroexpand-all

    (macroexpand-1
     '(<|
       let [a 5]
       (if (odd? a) :odd)
       do (println :even)
       (foo)
       :innermost))

    ())
