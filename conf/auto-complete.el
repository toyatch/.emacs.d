(unless (require 'auto-complete-config nil t) (package-refresh-contents) (package-install 'auto-complete) (require 'auto-complete-config))
(ac-config-default)
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)

; keybinds
(define-key global-map (kbd "C-x a") 'auto-complete-mode)
