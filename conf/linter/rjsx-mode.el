(add-to-list 'auto-mode-alist '("actions\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("reducers\\/.*\\.js\\'" . rjsx-mode))

(add-hook 'rjsx-mode-hook (lambda ()
                            (setq indent-tabs-mode nil)
                            (setq js-indent-level 2) ;; default 4
                            (setq js2-strict-missing-semi-warning nil)
                            (setq whitespace-style '(face trailing tabs tab-mark))
                            ))
