(unless (require 'bind-key nil t) (package-refresh-contents) (package-install 'bind-key) (require 'bind-key))

;;-----------------------------------------------------------------------------
;; all windows
;;-----------------------------------------------------------------------------
(bind-key* "C-z" 'undo)
(bind-key* "C-x C-a" 'mark-page)

;; scroll
(bind-key* "C-x C-p" 'scroll-down)
(bind-key* "C-x C-n" 'scroll-up)
(bind-key* "M-p" 'previous-line)
(bind-key* "M-n" 'next-line)

;; controll window
(bind-key* "C-x C-x C-b" 'windmove-left)
(bind-key* "C-x C-x C-f" 'windmove-right)
(bind-key* "C-x C-x C-p" 'windmove-up)
(bind-key* "C-x C-x C-n" 'windmove-down)
(bind-key* "M-q"         'other-window)

;;-----------------------------------------------------------------------------
;; 各map毎
;;-----------------------------------------------------------------------------
(define-key global-map (kbd "C-u") 'backward-kill-word)      ; like bash
(define-key global-map (kbd "C-k") 'kill-line)               ; like bash & default
(define-key global-map (kbd "C-h") 'delete-backward-char)    ; like bash
(define-key global-map (kbd "C-d") 'delete-char)             ; like bash & default
(define-key global-map (kbd "C-r") 'isearch-backward)        ; like bash & default
(define-key global-map (kbd "C-l") 'recenter)                ; like bash

(define-key global-map (kbd "C-w") 'kill-region)             ; original
(define-key global-map (kbd "M-w") 'kill-ring-save)          ; original
(define-key global-map (kbd "C-y") 'yank)                    ; original

(define-key global-map (kbd "C-o") 'mark-sexp)
(define-key global-map (kbd "M-o") 'mark-sexp)
(define-key global-map (kbd "C-q") 'toggle-truncate-lines)
(define-key global-map (kbd "M-g") 'grep-find)

(define-key global-map (kbd "C-x l") 'linum-mode)
(define-key global-map (kbd "C-t")   'untabify)
(define-key global-map (kbd "C-x C-g") 'goto-line)
