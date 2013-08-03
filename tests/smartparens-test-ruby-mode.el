(require 'smartparens-test-env)
(require 'smartparens-ruby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic pairs

(defun sp-ruby-test-slurp-assert (n in _ expected)
  (with-temp-buffer
    (ruby-mode)
    (smartparens-mode +1)
    (save-excursion
      (insert in))
    (goto-char (search-forward (regexp-quote "X")))
    (delete-char -1)
    (sp-forward-slurp-sexp n)
    (delete-trailing-whitespace)
    (should (equal (buffer-string) expected))))

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

  (sp-ruby-test-slurp-assert 5 "
beginX
end
test(1).test[2].test
" :=> "
begin
  test(1).test[2].test
end
")

  (sp-ruby-test-slurp-assert 5 "
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
  )

(ert-deftest sp-test-ruby-slurp-backward ()
  (sp-ruby-test-slurp-assert -2 "
foo.bar
begin X
end
" :=> "
begin
  foo.bar
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

  (sp-ruby-test-slurp-assert -5 "
test(1).test[2].test
beginX
end
" :=> "
begin
  test(1).test[2].test
end
")

  (sp-ruby-test-slurp-assert -5 "
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

  (sp-ruby-test-slurp-assert 2 "
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
  (with-temp-buffer
    (ruby-mode)
    (smartparens-mode +1)
    (save-excursion
      (insert in))
    (goto-char (search-forward (regexp-quote "X")))
    (delete-char -1)
    (sp-forward-barf-sexp n)
    (delete-trailing-whitespace)
    (should (equal (buffer-string) expected))))

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
  Module::Class
end
" :=> "
begin
end
Module::Class
")
  )

(ert-deftest sp-test-ruby-barf-backward ()
  (sp-ruby-test-barf-assert -1 "
begin
  foo.barX
end
" :=> "
foo. begin
       bar
     end
")

  (sp-ruby-test-barf-assert -2 "
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

  (sp-ruby-test-barf-assert -5 "
begin
  test(1).test[2].testX
end
" :=> "
test(1).test[2].test
begin
end
")

  (sp-ruby-test-barf-assert -5 "
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
  Module::ClassX
end
" :=> "
Module::Class
begin
end
")

  )

(ert-deftest sp-test-ruby-barf-with-inline-blocks ()
  (sp-ruby-test-barf-assert 2 "
if teXst
  foo if true
end
" :=> "
if test
end
foo if true
")

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
  (with-temp-buffer
    (ruby-mode)
    (smartparens-mode +1)
    (save-excursion
      (insert in))
    (goto-char (search-forward (regexp-quote "X")))
    (delete-char -1)
    (sp-splice-sexp n)
    (delete-trailing-whitespace)
    (should (equal (buffer-string) expected))))

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
if foo
  test if baXr
end
" :=> "
foo
test if bar
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

  )



(provide 'smartparens-test-ruby-mode)
