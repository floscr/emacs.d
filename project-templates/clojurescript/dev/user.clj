(ns user)

(defn hashp-tap [form]
  `(let [result-sym# ~form]
     (tap> result-sym#)
     result-sym#))

(defn hashpr [form]
  (let [result-sym (gensym "result")]
    `(let [~result-sym ~form]
       (js/console.log (pr-str '~form) ~result-sym)
       ~result-sym)))
