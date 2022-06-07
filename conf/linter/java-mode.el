(unless (require 'lsp-java nil t) (package-refresh-contents) (package-install 'lsp-java) (require 'llsp-java))

(add-hook 'java-mode-hook (lambda ()
                            ;(setq indent-tabs-mode nil)
                            (setq tab-width 4)
                            (set-face-foreground 'whitespace-tab "gray33")
                            (set-face-background 'whitespace-tab nil)
                            ))
