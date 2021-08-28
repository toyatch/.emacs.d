(add-to-list 'auto-mode-alist '(".*\\.js\\'" . js-mode))

(add-hook 'js-mode-hook (lambda ()
                            (setq indent-tabs-mode nil)
                            (setq js-indent-level 2) ;; default 4
                            (setq js2-strict-missing-semi-warning nil)
                            (setq whitespace-style '(face trailing tabs tab-mark))
                            ))
