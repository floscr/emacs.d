(ns app.utils.css.shadow.css.spacing)

;; https://tailwindcss.com/docs/customizing-spacing#default-spacing-scale
(def spacing
  {0 "0"
   0.5 "0.125rem"
   1 "0.25rem"
   1.5 "0.375rem"
   2 "0.5rem"
   2.5 "0.625rem"
   3 "0.75rem"
   3.5 "0.875rem"
   4 "1rem"
   5 "1.25rem"
   6 "1.5rem"
   7 "1.75rem"
   8 "2rem"
   9 "2.25rem"
   10 "2.5rem"
   11 "2.75rem"
   12 "3rem"
   13 "3.25rem"
   14 "3.5rem"
   15 "3.75rem"
   16 "4rem"
   17 "4.25rem"
   18 "4.5rem"
   19 "4.75rem"
   20 "5rem"
   24 "6rem"
   28 "7rem"
   32 "8rem"
   36 "9rem"
   40 "10rem"
   44 "11rem"
   48 "12rem"
   52 "13rem"
   56 "14rem"
   60 "15rem"
   64 "16rem"
   96 "24rem"})

(def ^:const spacing-vars
  (->>
   spacing
   (map (fn [[k v]] [(str "--spacing-" k) v]))
   (into {})))
