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
    (should (equal (buffer-string) expected))))

(ert-deftest sp-test-ruby-slurp ()
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
    (should (equal (buffer-string) expected))))

(ert-deftest sp-test-ruby-splice ()
  (sp-ruby-test-splice-assert 1 "
if teXst
end
" :=> "
test
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
