(defun sp-bc-buffer-equals (result)
  "Compare buffer to RESULT.

RESULT is a string which should equal the result of
`buffer-string' called in the current buffer.

If RESULT contains |, this represents the position of `point' and
should match.

If RESULT contains M, this represents the position of `mark' and
should match."
  (expect (buffer-string) :to-equal (replace-regexp-in-string "[|M]" "" result))
  (when (string-match-p "|" result)
    (expect (1+ (string-match-p
                 "|" (replace-regexp-in-string "[M]" "" result)))
            :to-be (point)))
  (when (string-match-p "M" result)
    (should (1+ (string-match-p
                 "M" (replace-regexp-in-string "[|]" "" result)))
            :to-be (mark))))

(provide 'smartparens-buttercup-helpers)
