;; ruby-mode-hook
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook 'highlight-symbol-mode)
(add-hook 'ruby-mode-hook (lambda () (setq whitespace-style '(face trailing tabs tab-mark))))

;; set auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; indent setting
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column)) indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\)) (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent (indent-line-to indent) (when (> offset 0) (forward-char offset)))))

;; encodingのマジックコメント除去
(setq ruby-insert-encoding-magic-comment nil)
