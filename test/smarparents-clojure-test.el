(require 'ert)
(require 'smartparens-clojure)
(require 'clojure-mode)

(sp-ert-deftest sp-test-clojure-wrap-with-prefix
  :let ((sp-pairs sp--test-basic-pairs))
  :mode 'clojure-mode
  "Ensure correct wrap-pair over prefixed expressions."
  (sp-test-kbd-macro "|#{...}M" "(" "(|#{...}M)")
  (sp-test-kbd-macro "|^{:a 1}M" "(" "(|^{:a 1}M)")
  (sp-test-kbd-macro "|#(xyzzy 1 2 %)M" "(" "(|#(xyzzy 1 2 %)M)")
  (sp-test-kbd-macro "|#_(comment reader macro)M" "(" "(|#_(comment reader macro)M)")
  (sp-test-kbd-macro "|~unquote-meM" "(" "(|~unquote-meM)")
  (sp-test-kbd-macro "|`~'unquote-meM" "(" "(|`~'unquote-meM)")
  (sp-test-kbd-macro "|~@(splice-me)M" "(" "(|~@(splice-me)M)")
  (sp-test-kbd-macro "|`(quote-me)M" "(" "(|`(quote-me)M)")
  (sp-test-kbd-macro "|@(deref-me)M" "(" "(|@(deref-me)M)")
  (sp-test-kbd-macro "|@deref-meM" "(" "(|@deref-meM)")
  (sp-test-kbd-macro "|#?@(:clj [3 4] :cljs [5 6])M" "(" "(|#?@(:clj [3 4] :cljs [5 6])M)")
  (sp-test-kbd-macro "|#?(:clj     Double/NaN
     :cljs    js/NaN
     :default nil)M" "(" "(|#?(:clj     Double/NaN
     :cljs    js/NaN
     :default nil)M)"))

(sp-ert-deftest sp-test-clojure-wrap-with-fence-prefix
  :let ((sp-pairs sp--test-basic-pairs))
  :mode 'clojure-mode
  "Ensure correct wrap-pair over #-prefixed expressions."
  (sp-test-kbd-macro "|#\"...\"M" "(" "(#\"...\")")
  (sp-test-kbd-macro "|#~xyzzyM" "(" "(#~xyzzy)"))
