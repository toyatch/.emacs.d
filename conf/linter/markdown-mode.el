(unless (require 'markdown-mode nil t) (package-refresh-contents) (package-install 'markdown-mode) (require 'markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-hook 'markdown-mode-hook (lambda ()
                                (setq indent-tabs-mode nil)
                                (setq whitespace-style '(face trailing tabs tab-mark))
                                (setq tab-width 2)
                                ))

;; keybinds
(define-key markdown-mode-map (kbd "C-x C-p") 'markdown-outline-previous)
(define-key markdown-mode-map (kbd "C-x C-n") 'markdown-outline-next)
