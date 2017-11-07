(require 'elixir-mode)
(require 'smartparens-elixir)

(defun sp-test-elixir-parse (initial sexp &optional back)
  (declare (indent 1))
  (sp-test-with-temp-buffer initial
      (elixir-mode)
    (should (equal (sp-get-thing back) sexp))))

(defun sp-test-insertion-elixir (initial keys result)
  (sp-test-with-temp-buffer initial
      (elixir-mode)
    (execute-kbd-macro keys)
    (sp-buffer-equals result)))

(ert-deftest sp-test-elixir-parse-defmodule-empty-forward ()
  "Parse defmodule correctly"
  (sp-test-elixir-parse "|defmodule HelloWorld do
end"
    '(:beg 1 :end 28 :op "defmodule" :cl "end" :prefix "" :suffix "")))

(ert-deftest sp-test-elixir-parse-defmodule-empty-backward ()
  "Parse defmodule correctly"
  (sp-test-elixir-parse "defmodule HelloWorld do
end|"
    '(:beg 1 :end 28 :op "defmodule" :cl "end" :prefix "" :suffix "") :back))

(ert-deftest sp-test-elixir-parse-defmodule-with-method-forward ()
  "Parse def correctly"
  (sp-test-elixir-parse "|defmodule HelloWorld do
  def hello od
    IO.puts \"Hello world\"
  end
end"
    '(:beg 1 :end 75 :op "defmodule" :cl "end" :prefix "" :suffix "")))

(ert-deftest sp-test-elixir-parse-defmodule-with-method-backward ()
  "Parse def correctly"
  (sp-test-elixir-parse "defmodule HelloWorld do
  def hello od
    IO.puts \"Hello world\"
  end
end|"
    '(:beg 1 :end 75 :op "defmodule" :cl "end" :prefix "" :suffix "") :back))

(ert-deftest sp-test-elixir-parse-defmodule-from-inside-forward ()
  "Parse def correctly"
  (sp-test-elixir-parse "defmodule HelloWorld do
  def hello od
    IO.puts \"Hello world\"
  end|
end"
    '(:beg 1 :end 75 :op "defmodule" :cl "end" :prefix "" :suffix "")))

(ert-deftest sp-test-elixir-parse-def-forward ()
  "Parse def correctly"
  (sp-test-elixir-parse "defmodule HelloWorld do
  |def hello od
    IO.puts \"Hello world\"
  end
end"
    '(:beg 27 :end 71 :op "def" :cl "end" :prefix "" :suffix "")))

(ert-deftest sp-test-elixir-parse-def-backward ()
  "Parse def correctly"
  (sp-test-elixir-parse "defmodule HelloWorld do
  def hello od
    IO.puts \"Hello world\"
  end|
end"
    '(:beg 27 :end 71 :op "def" :cl "end" :prefix "" :suffix "") :back))

(ert-deftest sp-test-elixir-parse-fn-multiline ()
  "Parse fn-end pair"
  (sp-test-elixir-parse "def la do
  task = Task.async |fn ->
    IO.puts \"remote print\"
    :timer.sleep(1000)
  end
  IO.puts \"#{inspect Task.await task}\"
end"
    '(:beg 31 :end 92 :op "fn" :cl "end" :prefix "" :suffix "")))

(ert-deftest sp-test-elixir-parse-if-forward ()
  "Parse if-end pair"
  (sp-test-elixir-parse "def world do
  |if true do
    IO.puts \"hello\"
  end

  unless false do
    IO.puts \"world\"
  end
end"
    '(:beg 16 :end 52 :op "if" :cl "end" :prefix "" :suffix "")))

(ert-deftest sp-test-elixir-parse-unless-forward ()
  "Parse if-end pair"
  (sp-test-elixir-parse "def world do
  if true do
    IO.puts \"hello\"
  end

  |unless false do
    IO.puts \"world\"
  end
end"
    '(:beg 56 :end 97 :op "unless" :cl "end" :prefix "" :suffix "")))

(ert-deftest sp-test-elixir-receive-block-insertion ()
  (sp-test-insertion-elixir "|" "receive" "receive do
  |
end"))

(ert-deftest sp-test-elixir-case-block-insertion ()
  (sp-test-insertion-elixir "|" "case " "case | do
end"))

(ert-deftest sp-test-elixir-if-block-insertion-in-comment ()
  (sp-test-insertion-elixir "# comment |" "if " "# comment if |"))

(ert-deftest sp-test-elixir-forward-slurp ()
  "Ensure that commas are handled properly when slurping forward"
  (sp-test-with-temp-buffer "[1, [|2], :a]"
      (elixir-mode)
    (sp-forward-slurp-sexp)
    (should (equal (buffer-string) "[1, [2, :a]]"))))

(ert-deftest sp-test-elixir-backward-slurp ()
  "Ensure that commas are handled properly when slurping backward"
  (sp-test-with-temp-buffer "[1,[|2],:a]"
      (elixir-mode)
    (sp-backward-slurp-sexp)
    (should (equal (buffer-string) "[[1,2],:a]"))))

(ert-deftest sp-test-elixir-forward-barf ()
  "Ensure that commas are handled properly when barfing forward"
  (sp-test-with-temp-buffer "{1, {2,|:a}}"
      (elixir-mode)
    (sp-forward-barf-sexp)
    (should (equal (buffer-string) "{1, {2},:a}"))))

(ert-deftest sp-test-elixir-backward-barf ()
  "Ensure that commas are handled properly when barfing backward"
  (sp-test-with-temp-buffer "{1,{2,|:a}}"
      (elixir-mode)
    (sp-backward-barf-sexp)
    (should (equal (buffer-string) "{1,2,{:a}}"))))
