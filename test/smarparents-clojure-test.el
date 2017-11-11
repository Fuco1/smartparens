(require 'ert)
(require 'smartparens-clojure)
(require 'clojure-mode)

(ert-deftest sp-test-clojure-slurp-with-prefix ()
  "Ensure we correctly slurp over prefixed expressions."
  (let ((before "(do|
  (foo) #{...}
  (foo) ^{:a 1}
  (foo) #(xyzzy 1 2 %)
  (foo) #_(comment reader macro)
  (foo) ~unquote-me
  (foo) `~'unquote-me
  (foo) ~@(splice-me)
  (foo) `(quote-me)
  (foo) @(deref-me)
  (foo) @deref-me
  (foo) #?@(:clj [3 4] :cljs [5 6])
  (foo) #?(:clj     Double/NaN
           :cljs    js/NaN
           :default nil))")
        (after "(do
  (foo #{...})
  (foo ^{:a 1})
  (foo #(xyzzy 1 2 %))
  (foo #_(comment reader macro))
  (foo ~unquote-me)
  (foo `~'unquote-me)
  (foo ~@(splice-me))
  (foo `(quote-me))
  (foo @(deref-me))
  (foo @deref-me)
  (foo #?@(:clj [3 4] :cljs [5 6]))
  (foo #?(:clj     Double/NaN
          :cljs    js/NaN
          :default nil)))"))
    (sp-test-with-temp-buffer before (clojure-mode)
      (while (re-search-forward "(foo" nil t)
        (sp-forward-slurp-sexp)
        (forward-line))
      (should (equal (buffer-string) after)))))

(ert-deftest sp-test-clojure-wrap-with-prefix ()
  "Ensure correct wrap-pair over prefixed expressions."
  (let ((before "(do
  |#{...}
  ^{:a 1}
  #(xyzzy 1 2 %)
  #_(comment reader macro)
  ~unquote-me
  `~'unquote-me
  ~@(splice-me)
  `(quote-me)
  @(deref-me)
  @deref-me
  #?@(:clj [3 4] :cljs [5 6])
  #?(:clj     Double/NaN
     :cljs    js/NaN
     :default nil))")
        (after "(do
  (#{...})
  (^{:a 1})
  (#(xyzzy 1 2 %))
  (#_(comment reader macro))
  (~unquote-me)
  (`~'unquote-me)
  (~@(splice-me))
  (`(quote-me))
  (@(deref-me))
  (@deref-me)
  (#?@(:clj [3 4] :cljs [5 6]))
  (#?(:clj     Double/NaN
      :cljs    js/NaN
      :default nil)))"))
    (sp-test-with-temp-buffer before (clojure-mode)
      (while (< (current-column) 3)
        (sp-wrap-with-pair "(")
        (forward-line)
        ;; FIXME: sp-wrap-with-pair keeps region active for no clear reason
        (deactivate-mark)
        (back-to-indentation))
      (should (equal (buffer-string) after)))))

(ert-deftest sp-test-clojure-wrap-with-fence-prefix ()
  "Ensure correct wrap-pair over #-prefixed expressions."
  (let ((before "(do
  |#\"...\"
  #~xyzzy)")
        (after "(do
  (#\"...\")
  (#~xyzzy))"))
    (sp-test-with-temp-buffer before (clojure-mode)
      (while (not (eobp))
        (back-to-indentation)
        (sp-wrap-with-pair "(")
        (forward-line)
        ;; FIXME: sp-wrap-with-pair keeps region active
        (deactivate-mark))
      (should (equal (buffer-string) after)))))

;; FIXME: fails
;; (ert-deftest sp-test-clojure-barf-with-fence-prefix ()
;;   "Ensure correct forward-barf over #-prefixed expressions."
;;   (let ((before "(do|
;;   (#{...})
;;   (#(xyzzy 1 2 %))
;;   (#_(comment reader macro))
;;   (#?@(:clj [3 4] :cljs [5 6]))
;;   (#?(:clj     Double/NaN
;;       :cljs    js/NaN
;;       :default nil)))")
;;         (after "(do
;;   ()#{...}
;;   ()#(xyzzy 1 2 %)
;;   ()#_(comment reader macro)
;;   ()#?@(:clj [3 4] :cljs [5 6])
;;   ()#?(:clj     Double/NaN
;;       :cljs    js/NaN
;;       :default nil))"))
;;     (sp-test-with-temp-buffer before (clojure-mode)
;;       (while (re-search-forward "(#" nil t)
;;         (backward-char)
;;         (sp-forward-barf-sexp)
;;         (forward-line))
;;       (should (equal (buffer-string) after)))))
