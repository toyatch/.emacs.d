(view-mode t)
(defun view-mode-hook0 ()
  "Hooks for Web mode."
  (setq keys '("a" "e"  "o"  "g"  "b" "n" "p" "f"))
  (while keys
    (define-key view-mode-map (kbd (car keys)) (kbd (concatenate 'string "C-" (car keys))))
    (setq keys (cdr keys)))
  (define-key view-mode-map (kbd "h") 'backward-word)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "l") 'forward-word))
  (define-key view-mode-map (kbd "u") 'scroll-down-command)
(add-hook 'view-mode-hook 'view-mode-hook0)

;; keybids
(define-key global-map (kbd "C-x C-q") 'view-mode)
