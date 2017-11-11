(require 'smartparens-ruby)

(defun sp-ruby-eq-ignore-indent (a b)
  (equal (replace-regexp-in-string "^ *" "" a)
         (replace-regexp-in-string "^ *" "" b)))


(ert-deftest sp-test-ruby-delete-pair ()
  (sp-test-with-temp-buffer "class Foo
  def foo_for|
  end
end"
      (ruby-mode)
    (execute-kbd-macro (kbd "<backspace>|"))
    (should (equal (buffer-string) "class Foo
  def foo_fo|
  end
end"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic pairs
(defun sp-ruby-test-slurp-assert (n in _ expected)
  (shut-up
    (with-temp-buffer
      (ruby-mode)
      (smartparens-mode +1)
      (save-excursion
        (insert in))
      (goto-char (search-forward (regexp-quote "X")))
      (delete-char -1)
      (sp-forward-slurp-sexp n)
      (delete-trailing-whitespace)
      (should
       (sp-ruby-eq-ignore-indent (buffer-string) expected)))))

(ert-deftest sp-test-ruby-slurp-forward ()
  (sp-ruby-test-slurp-assert 1 "
if teXst
end
foo
" :=> "
if test
  foo
end
")

  (sp-ruby-test-slurp-assert 1 "
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

  (sp-ruby-test-slurp-assert 1 "
if teXst
end
foo.bar
" :=> "
if test
  foo
end.bar
")

  (sp-ruby-test-slurp-assert 2 "
if teXst
end
foo.bar
" :=> "
if test
  foo.bar
end
")

  (sp-ruby-test-slurp-assert 3 "
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

  (sp-ruby-test-slurp-assert 5 "
beginX
end
test(1).test[2].test
" :=> "
begin
  test(1).test[2].test
end
")

  (sp-ruby-test-slurp-assert
   (cond
    ((version< emacs-version "24.4") 5)
    ((version< emacs-version "25.0") 4)
    (t 3))
   "
beginX
end
test ? a : b
" :=> "
begin
  test ? a : b
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
Module::Class
" :=> "
begin
  Module::Class
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
foo_bar
" :=> "
begin
  foo_bar
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
foo?
" :=> "
begin
  foo?
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
foo!
" :=> "
begin
  foo!
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
@foo
" :=> "
begin
  @foo
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
@@foo
" :=> "
begin
  @@foo
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
$foo
" :=> "
begin
  $foo
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
&foo
" :=> "
begin
  &foo
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
&:foo
" :=> "
begin
  &:foo
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
?x
" :=> "
begin
  ?x
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
!x
" :=> "
begin
  !x
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
class_name
" :=> "
begin
  class_name
end
")

  (sp-ruby-test-slurp-assert 1 "
beginX
end
:foo
" :=> "
begin
  :foo
end
")

  )

(ert-deftest sp-test-ruby-slurp-backward ()
  (sp-ruby-test-slurp-assert -1 "
foo.bar
begin X
end
" :=> "
begin
  foo.bar
end
")

  (sp-ruby-test-slurp-assert -1 "
foo.class
begin X
end
" :=> "
begin
  foo.class
end
")

  (sp-ruby-test-slurp-assert -1 "
@foo
begin X
end
" :=> "
begin
  @foo
end
")

  (sp-ruby-test-slurp-assert -1 "
foo?
begin X
end
" :=> "
begin
  foo?
end
")

  (sp-ruby-test-slurp-assert -1 "
foo!
begin X
end
" :=> "
begin
  foo!
end
")

  (sp-ruby-test-slurp-assert -1 "
!foo
begin X
end
" :=> "
begin
  !foo
end
")

  (sp-ruby-test-slurp-assert -1 "
?f
begin X
end
" :=> "
begin
  ?f
end
")

  (sp-ruby-test-slurp-assert -1 "
:foo
begin X
end
" :=> "
begin
  :foo
end
")

  (sp-ruby-test-slurp-assert -1 "
@@foo
begin X
end
" :=> "
begin
  @@foo
end
")

  (sp-ruby-test-slurp-assert -1 "
&:foo
begin X
end
" :=> "
begin
  &:foo
end
")

  (sp-ruby-test-slurp-assert -1 "
::Class
beginX
end
" :=> "
begin
  ::Class
end
")

  ;; Indentation is a bit off here, but it works great in Enhanced Ruby Mode.
  (sp-ruby-test-slurp-assert -1 "
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

  (sp-ruby-test-slurp-assert -1 "
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

  (sp-ruby-test-slurp-assert -3 "
test(1).test[2].test
beginX
end
" :=> "
begin
  test(1).test[2].test
end
")

  (sp-ruby-test-slurp-assert
   (cond
    ((version< emacs-version "24.4") -5)
    ((version< emacs-version "25.0") -4)
    (t -3)) "
test ? a : b
beginX
end
" :=> "
begin
  test ? a : b
end
")

  (sp-ruby-test-slurp-assert -1 "
Module::Class
beginX
end
" :=> "
begin
  Module::Class
end
")

  )

(ert-deftest sp-test-ruby-slurp-on-single-line ()
  (sp-ruby-test-slurp-assert 1 "
test {X} test
" :=> "
test { test }
")

  (sp-ruby-test-slurp-assert 2 "
test {X} test; test
" :=> "
test { test; test }
")

  (sp-ruby-test-slurp-assert -1 "
test test {X}
" :=> "
test { test }
")

  (sp-ruby-test-slurp-assert -2 "
test test; test {X}
" :=> "
test { test; test }
")

)

(ert-deftest sp-test-ruby-slurp-with-inline-blocks ()
  (sp-ruby-test-slurp-assert 1 "
if teXst
end
foo if true
" :=> "
if test
  foo
end if true
")

  (sp-ruby-test-slurp-assert 3 "
if teXst
end
foo if true
" :=> "
if test
  foo if true
end
")

  (sp-ruby-test-slurp-assert 2 "
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

(defun sp-ruby-test-barf-assert (n in _ expected)
  (shut-up
    (with-temp-buffer
      (ruby-mode)
      (smartparens-mode +1)
      (save-excursion
        (insert in))
      (goto-char (search-forward (regexp-quote "X")))
      (delete-char -1)
      (sp-forward-barf-sexp n)
      (delete-trailing-whitespace)
      (should
       (sp-ruby-eq-ignore-indent (buffer-string) expected)))))

(ert-deftest sp-test-ruby-barf-forward ()
  (sp-ruby-test-barf-assert 1 "
if teXst
  foo
end
" :=> "
if test
end
foo
")

  (sp-ruby-test-barf-assert 1 "
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

  (sp-ruby-test-barf-assert 1 "
if teXst
  foo.bar
end
" :=> "
if test
  foo
end.bar
")

  (sp-ruby-test-barf-assert 2 "
if teXst
  foo.bar
end
" :=> "
if test
end
foo.bar
")

  (sp-ruby-test-barf-assert 3 "
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

  (sp-ruby-test-barf-assert 5 "
beginX
  test(1).test[2].test
end
" :=> "
begin
end
test(1).test[2].test
")

  (sp-ruby-test-barf-assert 5 "
beginX
  test ? a : b
end
" :=> "
begin
end
test ? a : b
")

  (sp-ruby-test-barf-assert 1 "
beginX
  ::Class
end
" :=> "
begin
end
::Class
")

  (sp-ruby-test-barf-assert 1 "
beginX
  Module::Class
end
" :=> "
begin
end
Module::Class
")

  (sp-ruby-test-barf-assert 1 "
beginX
  ::Module::Class
end
" :=> "
begin
end
::Module::Class
")
  )

(ert-deftest sp-test-ruby-barf-backward ()
  (sp-ruby-test-barf-assert -1 "
begin
  fooX
end
" :=> "
foo
begin
end
")

  (sp-ruby-test-barf-assert -1 "
begin
  foo.barX
end
" :=> "
foo.bar
begin
end
")

  (sp-ruby-test-barf-assert -1 "
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

  (sp-ruby-test-barf-assert -1 "
begin
  test(1).test[2].testX
end
" :=> "
test(1).test[2].test
begin
end
")

  (sp-ruby-test-barf-assert
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

  (sp-ruby-test-barf-assert -1 "
begin
  ::ClassX
end
" :=> "
::Class
begin
end
")

  (sp-ruby-test-barf-assert -1 "
begin
  Module::ClassX
end
" :=> "
Module::Class
begin
end
")

  (sp-ruby-test-barf-assert -1 "
begin
  ::Module::ClassX
end
" :=> "
::Module::Class
begin
end
")
  )

(ert-deftest sp-test-ruby-barf-on-single-line ()
  (sp-ruby-test-barf-assert 1 "
test { Xtest }
" :=> "
test { } test
")

  (sp-ruby-test-barf-assert 2 "
test { Xtest; test }
" :=> "
test { } test; test
")

  (sp-ruby-test-barf-assert -1 "
test { Xtest }
" :=> "
test test { }
")

  (sp-ruby-test-barf-assert -2 "
test { test; testX }
" :=> "
test test; test { }
")

)
(ert-deftest sp-test-ruby-barf-with-inline-blocks ()
;;   (sp-ruby-test-barf-assert 2 "
;; if teXst
;;   foo if true
;; end
;; " :=> "
;; if test
;; end
;; foo if true
;; ")

  (sp-ruby-test-barf-assert 2 "
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

(defun sp-ruby-test-splice-assert (n in _ expected)
  (shut-up
    (with-temp-buffer
      (ruby-mode)
      (smartparens-mode +1)
      (save-excursion
        (insert in))
      (goto-char (search-forward (regexp-quote "X")))
      (delete-char -1)
      (sp-splice-sexp n)
      (delete-trailing-whitespace)
      (should
       (sp-ruby-eq-ignore-indent (buffer-string) expected)))))

(ert-deftest sp-test-ruby-splice ()
  (sp-ruby-test-splice-assert 1 "
if teXst
end
" :=> "
test
")

  (sp-ruby-test-splice-assert 1 "
begin
  bool = a | bX
end
" :=> "
bool = a | b
")

  (sp-ruby-test-splice-assert 1 "
if foo
  if baXr
  end
end
" :=> "
if foo
  bar
end
")

  (sp-ruby-test-splice-assert 1 "
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

  (sp-ruby-test-splice-assert 1 "
def forX
end
" :=> "
for
")

  (sp-ruby-test-splice-assert 1 "
begin
  for_funX
end
" :=> "
for_fun
")

  (sp-ruby-test-splice-assert 1 "
begin
  fun_forX
end
" :=> "
fun_for
")

  (sp-ruby-test-splice-assert 1 "
begin
  @forX
end
" :=> "
@for
")

  (sp-ruby-test-splice-assert 1 "
begin
  $forX
end
" :=> "
$for
")

  (sp-ruby-test-splice-assert 1 "
if foo
  test if baXr
end
" :=> "
foo
test if bar
")

  (sp-ruby-test-splice-assert 1 "
beginX
  end_of_game
end
" :=> "
end_of_game
")

  (sp-ruby-test-splice-assert 1 "
if foo
  [] if baXr
end
" :=> "
foo
[] if bar
")

  (sp-ruby-test-splice-assert 1 "
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
  (sp-ruby-test-splice-assert 1 "
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

  (sp-ruby-test-splice-assert 1 "
foo(ifX test; bar; end)
" :=> "
foo( test; bar; )
")

  (sp-ruby-test-splice-assert 1 "
begin
  object.classX
end
" :=> "
object.class
")

  (sp-ruby-test-splice-assert 1 "
begin
  object.
    classX
end
" :=> "
object.
  class
")

  (sp-ruby-test-splice-assert 1 "
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

  (sp-ruby-test-splice-assert 1 "
begin
  foo.send(\"#{object}\").
    classX
end
" :=> "
foo.send(\"#{object}\").
  class
")

  )

(ert-deftest sp-test-ruby-kill-whole-line-t ()
  "If the point it as bol we should kill the resulting empty line as well."
  (let ((kill-whole-line t))
    (sp-test-with-temp-buffer "begin
|  foo
end"
        (ruby-mode)
      (call-interactively 'sp-kill-hybrid-sexp)
      (sp-buffer-equals "begin
|end"))

    (sp-test-with-temp-buffer "begin
|  if test
    \"hello\"
  end
end"
        (ruby-mode)
      (call-interactively 'sp-kill-hybrid-sexp)
      (sp-buffer-equals "begin
|end"))))

(ert-deftest sp-test-ruby-kill-whole-line-nil ()
  "Do not kill the resulting empty line and indent accordingly."
  (sp-test-with-temp-buffer "begin
|  foo
end"
      (ruby-mode)
    (call-interactively 'sp-kill-hybrid-sexp)
    (sp-buffer-equals "begin
  |
end"))
  (sp-test-with-temp-buffer "begin
|  if test
    \"hello\"
  end
end"
      (ruby-mode)
    (call-interactively 'sp-kill-hybrid-sexp)
    (sp-buffer-equals "begin
  |
end")))

;; #638
(ert-deftest sp-test-ruby-parse-code-with-tabs ()
  (sp-test-with-temp-buffer "module Rfmt\n\tmodule Rewriters\n\t\tclass AlignEq < Parser::Rewriter\n\t\t\tdef on_begin(node)\n\t\t\t\teq_nodes = []\n\n\t\t\t\tnode.children.each do |child_node|\n\t\t\t\t\tif assignment?(child_node)\n\t\t\t\t\t\teq_nodes << child_node\n\t\t\t\t\telsif eq_nodes.any?\n\t\t\t\t\t\talign(eq_nodes)\n\t\t\t\t\t\teq_nodes = []\n\t\t\t\t\tend\n\t\t\t\tend\n\n\t\t\t\talign(eq_nodes)\n\n\t\t\t\tsuper\n\t\t\tend\n\n\t\t\tdef align(eq_nodes)\n\t\t\t\taligned_column = eq_nodes.\n\t\t\t\t\t  map { |node| node.loc.operator.column }.\n\t\t\t\t\t  max\n\n\t\t\t\teq_nodes.each do |node|\n\t\t\t\t\tif(column = node.loc.operator.column) < aligned_column\n\t\t\t\t\t\tinsert_before node.loc.operator, ' ' * (aligned_column - column)\n\t\t\t\t\tend\n\t\t\t\tend\n\t\t\tend\n\t\tend\n\tend\nend"
      (ruby-mode)
    (goto-char (point-max))
    (equal (sp-get-thing t) '(:beg 1 :end 642 :op "module" :cl "end" :prefix "" :suffix ""))
    (goto-char (point-min))
    (equal (sp-get-thing) '(:beg 1 :end 642 :op "module" :cl "end" :prefix "" :suffix ""))))
