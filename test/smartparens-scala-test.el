(require 'scala-mode)
(require 'smartparens-scala)

(ert-deftest sp-test-scala-bracket ()
  "Close a normal bracket"
  (sp-test-with-temp-buffer "def foo|"
      (scala-mode)
    (execute-kbd-macro "(bar: String")
    (should (equal (buffer-string) "def foo(bar: String)"))))

(ert-deftest sp-test-scala-bracket-space ()
  "Close a normal bracket with padding on SPACE."
  (sp-test-with-temp-buffer "foo.map|"
      (scala-mode)
    (execute-kbd-macro "( _.toString")
    (should (equal (buffer-string) "foo.map( _.toString )"))))

(ert-deftest sp-test-scala-curly ()
  "Close a curly bracket"
  (sp-test-with-temp-buffer "foo.map|"
      (scala-mode)
    (execute-kbd-macro "{f => f.toString")
    (should (equal (buffer-string) "foo.map{f => f.toString}"))))

(ert-deftest sp-test-scala-curly-space ()
  "Close a curly bracket with padding on SPACE"
  (sp-test-with-temp-buffer "foo.map |"
      ;; it might be nice in a future update to not need the prefix space
      (scala-mode)
    (execute-kbd-macro "{ f => f.toString")
    (should (equal (buffer-string) "foo.map { f => f.toString }"))))

(ert-deftest sp-test-scala-curly-newline ()
  "Close a curly bracket with an indented block on newline"
  (sp-test-with-temp-buffer "foo.map |"
      (scala-mode)
    (execute-kbd-macro (kbd "{ RET f SPC => SPC f.toString"))
    (sp-buffer-equals "foo.map {\n  f => f.toString|\n}")))

(ert-deftest sp-test-scala-curly-wrap ()
  "Wrap a region in an indented block"
  (sp-test-with-temp-buffer "|fooM"
      (scala-mode)
    (execute-kbd-macro "{")
    (should (equal (buffer-string) "{\n  foo\n}"))))
