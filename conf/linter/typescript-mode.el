(unless (require 'tide nil t) (package-refresh-contents) (package-install 'tide) (require 'tide))

(add-to-list 'auto-mode-alist '("\\/.*\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("actions\\/.*\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("containers\\/.*\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("reducers\\/.*\\.ts\\'" . typescript-mode))

(add-hook 'typescript-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil)
                                  (setq typescript-indent-level 2) ;; default 4
                                  (setq js2-strict-missing-semi-warning nil)
                                  (setq whitespace-style '(face trailing tabs tab-mark))
                                  (tide-setup)
                                  (flycheck-mode t)
                                  (setq flycheck-check-syntax-automatically '(save mode-enabled))
                                  (eldoc-mode t)
                                  (company-mode-on)
                                  ))
