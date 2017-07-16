(require 'smartparens-text)
(require 'message)

(ert-deftest sp-test-text-mode-insert-emoticon-smiley-strict-mode ()
  "#713"
  (sp-test-with-temp-buffer "|"
      (message-mode)
    (smartparens-strict-mode 1)
    (execute-kbd-macro ":)")
    (sp-buffer-equals ":)")))

(ert-deftest sp-test-text-mode-insert-emoticon-smiley-with-nose-strict-mode ()
  "#713"
  (sp-test-with-temp-buffer "|"
      (message-mode)
    (smartparens-strict-mode 1)
    (execute-kbd-macro ":-)")
    (sp-buffer-equals ":-)")))

(ert-deftest sp-test-text-mode-insert-emoticon-frowny-strict-mode ()
  "#713"
  (sp-test-with-temp-buffer "|"
      (message-mode)
    (smartparens-strict-mode 1)
    (execute-kbd-macro ":(")
    (sp-buffer-equals ":(")))

(ert-deftest sp-test-text-mode-insert-emoticon-frowny-with-nose-strict-mode ()
  "#713"
  (sp-test-with-temp-buffer "|"
      (message-mode)
    (smartparens-strict-mode 1)
    (execute-kbd-macro ":-(")
    (sp-buffer-equals ":-(")))

(ert-deftest sp-test-text-mode-skip-emoticon-when-navigating ()
  "#713"
  (sp-test-with-temp-buffer "|(foo :) bar :( baz)"
      (message-mode)
    (sp-forward-sexp)
    (sp-buffer-equals "(foo :) bar :( baz)|")))

(ert-deftest sp-test-text-mode-wrap-emoticons-strict-mode ()
  "#713"
  (sp-test-with-temp-buffer "| foo :) bar :( baz M"
      (message-mode)
    (smartparens-strict-mode 1)
    (execute-kbd-macro "(")
    (sp-buffer-equals "(| foo :) bar :( baz M)")))

(ert-deftest sp-test-text-mode-kill-region-with-emoticons-strict-mode ()
  "#713"
  (sp-test-with-temp-buffer "foo | foo :) bar :( baz M"
      (message-mode)
    (smartparens-strict-mode 1)
    (call-interactively 'sp-kill-region)
    (sp-buffer-equals "foo |")))
