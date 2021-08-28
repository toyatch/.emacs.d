(unless (require 'multi-term nil t) (package-refresh-contents) (package-install 'multi-term) (require 'multi-term))
(add-hook 'term-mode-hook (lambda () (setq whitespace-style '(face trailing tabs tab-mark)) (linum-mode 0) ))

;; keybinds
(define-key global-map    (kbd "C-x C-t") 'ansi-term)
(define-key global-map    (kbd "M-t")     'ansi-term)
(define-key term-mode-map (kbd "C-x C-k") 'term-char-mode)
;;(define-key term-mode-map (kbd "C-p")     'term-send-previous-line)
;;(define-key term-mode-map (kbd "C-n")     'term-send-next-line)
(define-key term-mode-map (kbd "C-h")     'term-send-backspace)
(define-key term-mode-map (kbd "C-r")     'term-send-reverse-search-history)
(define-key term-mode-map (kbd "C-y")     'term-paste)
(define-key term-raw-map  (kbd "C-y")     'term-paste)

