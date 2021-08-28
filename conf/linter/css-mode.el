(add-to-list 'auto-mode-alist '(".*\\.css\\'" . css-mode))

(add-hook 'css-mode-hook (lambda ()
                            (setq indent-tabs-mode nil)
                            (setq tab-width 2) ;; default 4
                            (setq whitespace-style '(face trailing tabs tab-mark))
                            ))
