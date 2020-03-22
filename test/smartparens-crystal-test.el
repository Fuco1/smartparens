(require 'crystal-mode)
(require 'smartparens-crystal)

(defun sp-crystal-eq-ignore-indent (a b)
  (equal (replace-regexp-in-string "^ *" "" a)
         (replace-regexp-in-string "^ *" "" b)))


(ert-deftest sp-test-crystal-delete-pair ()
  (sp-test-with-temp-buffer "class Foo
  def foo_for|
  end
end"
      (crystal-mode)
    (execute-kbd-macro (kbd "<backspace>|"))
    (should (equal (buffer-string) "class Foo
  def foo_fo|
  end
end"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic pairs
(defun sp-crystal-test-slurp-assert (n in _ expected)
  (shut-up
    (with-temp-buffer
      (crystal-mode)
      (smartparens-mode +1)
      (save-excursion
        (insert in))
      (goto-char (search-forward (regexp-quote "X")))
      (delete-char -1)
      (sp-forward-slurp-sexp n)
      (delete-trailing-whitespace)
      (should
       (sp-crystal-eq-ignore-indent (buffer-string) expected)))))

(sp-ert-deftest sp-test-crystal-slurp-forward
  (sp-crystal-test-slurp-assert 1 "
if teXst
end
foo
" :=> "
if test
  foo
end
")

  (sp-crystal-test-slurp-assert 1 "
if teXst
end
if test2
  foo
end
" :=> "
if test
  if test2
    foo
  end
end
")

  (sp-crystal-test-slurp-assert 1 "
if teXst
end
foo.bar
" :=> "
if test
  foo
end.bar
")

  (sp-crystal-test-slurp-assert 2 "
if teXst
end
foo.bar
" :=> "
if test
  foo.bar
end
")

  (sp-crystal-test-slurp-assert 3 "
if teXst
end
foo.
  bar.
  bar
" :=> "
if test
  foo.
    bar.
    bar
end
")

  (sp-crystal-test-slurp-assert 5 "
beginX
end
test(1).test[2].test
" :=> "
begin
  test(1).test[2].test
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
Module::Class
" :=> "
begin
  Module::Class
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
foo_bar
" :=> "
begin
  foo_bar
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
foo?
" :=> "
begin
  foo?
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
foo!
" :=> "
begin
  foo!
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
@foo
" :=> "
begin
  @foo
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
@@foo
" :=> "
begin
  @@foo
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
$foo
" :=> "
begin
  $foo
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
&foo
" :=> "
begin
  &foo
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
&:foo
" :=> "
begin
  &:foo
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
!x
" :=> "
begin
  !x
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
class_name
" :=> "
begin
  class_name
end
")

  (sp-crystal-test-slurp-assert 1 "
beginX
end
:foo
" :=> "
begin
  :foo
end
")

  )

(sp-ert-deftest sp-test-crystal-slurp-backward
  (sp-crystal-test-slurp-assert -1 "
foo.bar
begin X
end
" :=> "
begin
  foo.bar
end
")

  (sp-crystal-test-slurp-assert -1 "
foo.class
begin X
end
" :=> "
begin
  foo.class
end
")

  (sp-crystal-test-slurp-assert -1 "
@foo
begin X
end
" :=> "
begin
  @foo
end
")

  (sp-crystal-test-slurp-assert -1 "
foo?
begin X
end
" :=> "
begin
  foo?
end
")

  (sp-crystal-test-slurp-assert -1 "
foo!
begin X
end
" :=> "
begin
  foo!
end
")

  (sp-crystal-test-slurp-assert -1 "
!foo
begin X
end
" :=> "
begin
  !foo
end
")

  (sp-crystal-test-slurp-assert -1 "
:foo
begin X
end
" :=> "
begin
  :foo
end
")

  (sp-crystal-test-slurp-assert -1 "
@@foo
begin X
end
" :=> "
begin
  @@foo
end
")

  (sp-crystal-test-slurp-assert -1 "
&:foo
begin X
end
" :=> "
begin
  &:foo
end
")

  (sp-crystal-test-slurp-assert -1 "
::Class
beginX
end
" :=> "
begin
  ::Class
end
")

  ;; Indentation is a bit off here.
  (sp-crystal-test-slurp-assert -1 "
foo.
  class.
  bar
begin X
end
" :=> "
begin
  foo.
    class.
        bar
    end
")

  (sp-crystal-test-slurp-assert -1 "
if test
 foo.bar
end
begin X
end
" :=> "
begin
  if test
    foo.bar
  end
end
")

  (sp-crystal-test-slurp-assert -3 "
test(1).test[2].test
beginX
end
" :=> "
begin
  test(1).test[2].test
end
")

  (sp-crystal-test-slurp-assert -1 "
Module::Class
beginX
end
" :=> "
begin
  Module::Class
end
")

  )

(sp-ert-deftest sp-test-crystal-slurp-on-single-line
  (sp-crystal-test-slurp-assert 1 "
test {X} test
" :=> "
test { test }
")

  (sp-crystal-test-slurp-assert 2 "
test {X} test; test
" :=> "
test { test; test }
")

  (sp-crystal-test-slurp-assert -1 "
test test {X}
" :=> "
test { test }
")

  (sp-crystal-test-slurp-assert -2 "
test test; test {X}
" :=> "
test { test; test }
")

)

(sp-ert-deftest sp-test-crystal-slurp-with-inline-blocks
  (sp-crystal-test-slurp-assert 1 "
if teXst
end
foo if true
" :=> "
if test
  foo
end if true
")

  (sp-crystal-test-slurp-assert 3 "
if teXst
end
foo if true
" :=> "
if test
  foo if true
end
")

  (sp-crystal-test-slurp-assert 2 "
if teXst
end
foo = if true
        bar
      end
" :=> "
if test
  foo = if true
          bar
        end
end
")
  )

(defun sp-crystal-test-barf-assert (n in _ expected)
  (shut-up
    (with-temp-buffer
      (crystal-mode)
      (smartparens-mode +1)
      (save-excursion
        (insert in))
      (goto-char (search-forward (regexp-quote "X")))
      (delete-char -1)
      (sp-forward-barf-sexp n)
      (delete-trailing-whitespace)
      (should
       (sp-crystal-eq-ignore-indent (buffer-string) expected)))))

(sp-ert-deftest sp-test-crystal-barf-forward
  (sp-crystal-test-barf-assert 1 "
if teXst
  foo
end
" :=> "
if test
end
foo
")

  (sp-crystal-test-barf-assert 1 "
if teXst
  if test2
    foo
  end
end
" :=> "
if test
end
if test2
  foo
end
")

  (sp-crystal-test-barf-assert 1 "
if teXst
  foo.bar
end
" :=> "
if test
  foo
end.bar
")

  (sp-crystal-test-barf-assert 2 "
if teXst
  foo.bar
end
" :=> "
if test
end
foo.bar
")

  (sp-crystal-test-barf-assert 3 "
if teXst
  foo.
    bar.
    bar
end
" :=> "
if test
end
foo.
  bar.
  bar
")

  (sp-crystal-test-barf-assert 5 "
beginX
  test(1).test[2].test
end
" :=> "
begin
end
test(1).test[2].test
")

  (sp-crystal-test-barf-assert 5 "
beginX
  test ? a : b
end
" :=> "
begin
end
test ? a : b
")

  (sp-crystal-test-barf-assert 1 "
beginX
  ::Class
end
" :=> "
begin
end
::Class
")

  (sp-crystal-test-barf-assert 1 "
beginX
  Module::Class
end
" :=> "
begin
end
Module::Class
")

  (sp-crystal-test-barf-assert 1 "
beginX
  ::Module::Class
end
" :=> "
begin
end
::Module::Class
")
  )

(sp-ert-deftest sp-test-crystal-barf-backward
  (sp-crystal-test-barf-assert -1 "
begin
  fooX
end
" :=> "
foo
begin
end
")

  (sp-crystal-test-barf-assert -1 "
begin
  foo.barX
end
" :=> "
foo.bar
begin
end
")

  (sp-crystal-test-barf-assert -1 "
begin
  if test
    foo.bar
  endX
end
" :=> "
if test
  foo.bar
end
begin
end
")

  (sp-crystal-test-barf-assert -1 "
begin
  test(1).test[2].testX
end
" :=> "
test(1).test[2].test
begin
end
")

  (sp-crystal-test-barf-assert
   (cond
    ((version< emacs-version "24.4") -5)
    ((version< emacs-version "25.0") -4)
    (t -3)) "
begin
  test ? a : bX
end
" :=> "
test ? a : b
begin
end
")

  (sp-crystal-test-barf-assert -1 "
begin
  ::ClassX
end
" :=> "
::Class
begin
end
")

  (sp-crystal-test-barf-assert -1 "
begin
  Module::ClassX
end
" :=> "
Module::Class
begin
end
")

  (sp-crystal-test-barf-assert -1 "
begin
  ::Module::ClassX
end
" :=> "
::Module::Class
begin
end
")
  )

(sp-ert-deftest sp-test-crystal-barf-on-single-line
  (sp-crystal-test-barf-assert 1 "
test { Xtest }
" :=> "
test { } test
")

  (sp-crystal-test-barf-assert 2 "
test { Xtest; test }
" :=> "
test { } test; test
")

  (sp-crystal-test-barf-assert -1 "
test { Xtest }
" :=> "
test test { }
")

  (sp-crystal-test-barf-assert -2 "
test { test; testX }
" :=> "
test test; test { }
")

)
(sp-ert-deftest sp-test-crystal-barf-with-inline-blocks
;;   (sp-crystal-test-barf-assert 2 "
;; if teXst
;;   foo if true
;; end
;; " :=> "
;; if test
;; end
;; foo if true
;; ")

  (sp-crystal-test-barf-assert 2 "
if teXst
  foo = if true
          bar
        end
end
" :=> "
if test
end
foo = if true
        bar
      end
")
  )

(defun sp-crystal-test-splice-assert (n in _ expected)
  (shut-up
    (with-temp-buffer
      (crystal-mode)
      (smartparens-mode +1)
      (save-excursion
        (insert in))
      (goto-char (search-forward (regexp-quote "X")))
      (delete-char -1)
      (sp-splice-sexp n)
      (delete-trailing-whitespace)
      (should
       (sp-crystal-eq-ignore-indent (buffer-string) expected)))))

(sp-ert-deftest sp-test-crystal-splice
  (sp-crystal-test-splice-assert 1 "
if teXst
end
" :=> "
test
")

  (sp-crystal-test-splice-assert 1 "
begin
  bool = a | bX
end
" :=> "
bool = a | b
")

  (sp-crystal-test-splice-assert 1 "
if foo
  if baXr
  end
end
" :=> "
if foo
  bar
end
")

  (sp-crystal-test-splice-assert 1 "
if foo
  begin
    barX
  end
end
" :=> "
if foo
  bar
end
")

  (sp-crystal-test-splice-assert 1 "
def forX
end
" :=> "
for
")

  (sp-crystal-test-splice-assert 1 "
begin
  for_funX
end
" :=> "
for_fun
")

  (sp-crystal-test-splice-assert 1 "
begin
  fun_forX
end
" :=> "
fun_for
")

  (sp-crystal-test-splice-assert 1 "
begin
  @forX
end
" :=> "
@for
")

  (sp-crystal-test-splice-assert 1 "
begin
  $forX
end
" :=> "
$for
")

  (sp-crystal-test-splice-assert 1 "
if foo
  test if baXr
end
" :=> "
foo
test if bar
")

  (sp-crystal-test-splice-assert 1 "
beginX
  end_of_game
end
" :=> "
end_of_game
")

  (sp-crystal-test-splice-assert 1 "
if foo
  [] if baXr
end
" :=> "
foo
[] if bar
")

  (sp-crystal-test-splice-assert 1 "
if foo
  begin
  end if baXr
end
" :=> "
foo
begin
end if bar
")

  ;; TODO: should not leave two spaces after splice
  (sp-crystal-test-splice-assert 1 "
if foo
  foo = if baXr
          v
        end
end
" :=> "
if foo
  foo =  bar
  v
end
")

  (sp-crystal-test-splice-assert 1 "
foo(ifX test; bar; end)
" :=> "
foo( test; bar; )
")

  (sp-crystal-test-splice-assert 1 "
begin
  object.classX
end
" :=> "
object.class
")

  (sp-crystal-test-splice-assert 1 "
begin
  object.
    classX
end
" :=> "
object.
  class
")

  (sp-crystal-test-splice-assert 1 "
begin
  # object.
  classX
  end
end
" :=> "
begin
  # object.
end
")

  (sp-crystal-test-splice-assert 1 "
begin
  foo.send(\"#{object}\").
    classX
end
" :=> "
foo.send(\"#{object}\").
  class
")

  )

(ert-deftest sp-test-crystal-kill-whole-line-t ()
  "If the point it as bol we should kill the resulting empty line as well."
  (let ((kill-whole-line t))
    (sp-test-with-temp-buffer "begin
|  foo
end"
        (crystal-mode)
      (call-interactively 'sp-kill-hybrid-sexp)
      (sp-buffer-equals "begin
|end"))

    (sp-test-with-temp-buffer "begin
|  if test
    \"hello\"
  end
end"
        (crystal-mode)
      (call-interactively 'sp-kill-hybrid-sexp)
      (sp-buffer-equals "begin
|end"))))

(ert-deftest sp-test-crystal-kill-whole-line-nil ()
  "Do not kill the resulting empty line and indent accordingly."
  (sp-test-with-temp-buffer "begin
|  foo
end"
      (crystal-mode)
    (call-interactively 'sp-kill-hybrid-sexp)
    (sp-buffer-equals "begin
  |
end"))
  (sp-test-with-temp-buffer "begin
|  if test
    \"hello\"
  end
end"
      (crystal-mode)
    (call-interactively 'sp-kill-hybrid-sexp)
    (sp-buffer-equals "begin
  |
end")))

;; #638
(ert-deftest sp-test-crystal-parse-code-with-tabs ()
  (sp-test-with-temp-buffer "module Rfmt\n\tmodule Rewriters\n\t\tclass AlignEq < Parser::Rewriter\n\t\t\tdef on_begin(node)\n\t\t\t\teq_nodes = []\n\n\t\t\t\tnode.children.each do |child_node|\n\t\t\t\t\tif assignment?(child_node)\n\t\t\t\t\t\teq_nodes << child_node\n\t\t\t\t\telsif eq_nodes.any?\n\t\t\t\t\t\talign(eq_nodes)\n\t\t\t\t\t\teq_nodes = []\n\t\t\t\t\tend\n\t\t\t\tend\n\n\t\t\t\talign(eq_nodes)\n\n\t\t\t\tsuper\n\t\t\tend\n\n\t\t\tdef align(eq_nodes)\n\t\t\t\taligned_column = eq_nodes.\n\t\t\t\t\t  map { |node| node.loc.operator.column }.\n\t\t\t\t\t  max\n\n\t\t\t\teq_nodes.each do |node|\n\t\t\t\t\tif(column = node.loc.operator.column) < aligned_column\n\t\t\t\t\t\tinsert_before node.loc.operator, ' ' * (aligned_column - column)\n\t\t\t\t\tend\n\t\t\t\tend\n\t\t\tend\n\t\tend\n\tend\nend"
      (crystal-mode)
    (goto-char (point-max))
    (equal (sp-get-thing t) '(:beg 1 :end 642 :op "module" :cl "end" :prefix "" :suffix ""))
    (goto-char (point-min))
    (equal (sp-get-thing) '(:beg 1 :end 642 :op "module" :cl "end" :prefix "" :suffix ""))))
