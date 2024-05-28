(ns app.styles
  (:require
   [cljss.core :refer-macros [inject-global] :as css]))

(defn inject-init-styles! []
  (inject-global
   {":root"
    {"--spacing-0" "0"
     "--spacing-0-5" "0.125rem"
     "--spacing-1-5" "0.375rem"
     "--spacing-1" "0.25rem"
     "--spacing-2-5" "0.625rem"
     "--spacing-2" "0.5rem"
     "--spacing-3-5" "0.875rem"
     "--spacing-3" "0.75rem"
     "--spacing-4" "1rem"
     "--spacing-5" "1.25rem"
     "--spacing-6" "1.5rem"
     "--spacing-7" "1.75rem"
     "--spacing-8" "2rem"
     "--spacing-9" "2.25rem"
     "--spacing-10" "2.5rem"
     "--spacing-11" "2.75rem"
     "--spacing-12" "3rem"
     "--spacing-13" "3.25rem"
     "--spacing-14" "3.5rem"
     "--spacing-15" "3.75rem"
     "--spacing-16" "4rem"
     "--spacing-17" "4.25rem"
     "--spacing-18" "4.5rem"
     "--spacing-19" "4.75rem"
     "--spacing-20" "5rem"
     "--spacing-24" "6rem"
     "--spacing-28" "7rem"
     "--spacing-32" "8rem"
     "--spacing-36" "9rem"
     "--spacing-40" "10rem"
     "--spacing-44" "11rem"
     "--spacing-48" "12rem"
     "--spacing-52" "13rem"
     "--spacing-56" "14rem"
     "--spacing-60" "15rem"
     "--spacing-96" "24rem"
     "--spacing-64" "16rem"

     "--black" "0 0% 0%"
     "--background" "0 0% 100%"
     "--foreground" "240 10% 3.9%"
     "--card" "0 0% 100%"
     "--card-foreground" "240 10% 3.9%"
     "--popover" "0 0% 100%"
     "--popover-foreground" "240 10% 3.9%"
     "--primary" "240 5.9% 10%"
     "--primary-foreground" "0 0% 98%"
     "--secondary" "240 4.8% 95.9%"
     "--secondary-foreground" "240 5.9% 10%"
     "--muted" "0deg 0% 94.64%"
     "--muted-foreground" "240 3.8% 46.1%"
     "--accent" "240 4.8% 95.9%"
     "--accent-foreground" "240 5.9% 10%"
     "--destructive" "0 72.22% 50.59%"
     "--destructive-foreground" "0 0% 98%"
     "--border" "0.89 0.01 249.49" ;;oklch
     "--input" "240 5.9% 90%"
     "--ring" "240 5% 64.9%"
     "--radius" "0.5rem"
     ;; oklch test
     "--focus" "62.3% 0.2 255"

     ;; Ring
     "--ring-offset-width" "2px"
     "--ring-offset-color" "1 0 0"
     "--ring-inset" "1px"
     "--ring-color" "var(--focus)"}
    ".dark"
    {"--background" "240 10% 3.9%"
     "--foreground" "0 0% 98%"
     "--card" "240 10% 3.9%"
     "--card-foreground" "0 0% 98%"
     "--popover" "240 10% 3.9%"
     "--popover-foreground" "0 0% 98%"
     "--primary" "0 0% 98%"
     "--primary-foreground" "240 5.9% 10%"
     "--secondary" "240 3.7% 15.9%"
     "--secondary-foreground" "0 0% 98%"
     "--muted" "240 3.7% 15.9%"
     "--muted-foreground" "240 5% 64.9%"
     "--accent" "240 3.7% 15.9%"
     "--accent-foreground" "0 0% 98%"
     "--destructive" "0 62.8% 30.6%"
     "--destructive-foreground" "0 85.7% 97.3%"
     "--border" "0.39 0.01 249.49"
     "--input" "240 3.7% 15.9%"
     "--ring-offset-color" "0 0 0"}
    "button" {:all "unset"
              :outline "revert"
              :cursor "pointer"
              :box-sizing "inherit"}

    "input" {:all "unset"
             :box-sizing "inherit"
             :cursor "text"}
    "label" {:display "block"}
    "table" {:text-indent 0
             :border-color "inherit"
             :border-collapse "collapse"}
    "html" {:box-sizing "border-box"}
    "*, *:before, *:after" {:box-sizing "inherit"}
    "html, *, :after, :before" {:border "0 solid oklch(var(--border))"}
    "#root" {}
    "a" {:color "inherit"}
    :body {:margin 0
           :font-family "Geist,ui-sans-serif,system-ui,sans-serif,\"Apple Color Emoji\",\"Segoe UI Emoji\",\"Segoe UI Symbol\",\"Noto Color Emoji\""
           :line-height 1.5
           :color "hsl(var(--primary))"
           :background "hsl(var(--background))"
           "-webkit-text-size-adjust" "100%"
           :text-rendering "optimizeLegibility"}
    "blockquote, dd, dl, figure, h1, h2, h3, h4, h5, h6, hr, p, pre" {:margin 0}}))

(inject-init-styles!)
