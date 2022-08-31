(customize-set-variable
 'package-archives '(("org"   . "https://orgmode.org/elpa/")
                     ("melpa" . "https://melpa.org/packages/")
                     ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(defun find-or-install-package (x)
  (unless (package-installed-p x)
    (package-refresh-contents)
    (package-install x)))

;;-----------------------------------------------------------------------------
;; basic
;;-----------------------------------------------------------------------------
;;(tool-bar-mode 0)                   ;; ツールバー非表示
;;(scroll-bar-mode 0)                 ;; スクロールバー非表示
(load-theme 'wheatgrass)              ;; theme
(setq inhibit-startup-screen t)       ;; スタートアップメッセージ非表示
(menu-bar-mode 0)                     ;; メニューバー非表示
(set-language-environment 'Japanese)  ;; 言語設定
(prefer-coding-system 'utf-8)         ;; 文字コード設定
(setq ring-bell-function 'ignore)     ;; beep抑止
(setq make-backup-files nil)          ;; backupfile抑止

;; whitespace
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode 1)

;; 行番号表示
(global-linum-mode 2)
(setq linum-format "%4d ")

;; 環境変数
(setenv "LANG" "ja_JP.UTF-8")

;;-----------------------------------------------------------------------------
;; basic-keybind
;;-----------------------------------------------------------------------------
(define-key global-map (kbd "C-z")     'undo)
(define-key global-map (kbd "C-x C-z") 'undo)

(define-key global-map (kbd "C-x C-a") 'mark-page)

(define-key global-map (kbd "M-p")     'previous-line)
(define-key global-map (kbd "M-n")     'next-line)

(define-key global-map (kbd "M-q")     'other-window)

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

;;-----------------------------------------------------------------------------
;; helm
;;-----------------------------------------------------------------------------
(find-or-install-package 'helm)
(require 'helm-config)

;; settings
(helm-mode 1)
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)

;; keymap
(define-key helm-map            (kbd "C-h")     'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h")     'delete-backward-char)
(define-key helm-read-file-map  (kbd "C-h")     'delete-backward-char)
(define-key helm-map            (kbd "TAB")     'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB")     'helm-execute-persistent-action)
(define-key helm-read-file-map  (kbd "TAB")     'helm-execute-persistent-action)
(define-key helm-map            (kbd "C-b")     (kbd "C-l"))
(define-key helm-find-files-map (kbd "C-b")     (kbd "C-l"))
(define-key helm-read-file-map  (kbd "C-b")     (kbd "C-l"))
(define-key helm-map            (kbd "C-f")     (kbd "TAB"))
(define-key helm-find-files-map (kbd "C-f")     (kbd "TAB"))
(define-key helm-read-file-map  (kbd "C-f")     (kbd "TAB"))
(define-key helm-map            (kbd "C-z")     (kbd "C-g"))
(define-key helm-find-files-map (kbd "C-z")     (kbd "C-g"))
(define-key helm-read-file-map  (kbd "C-z")     (kbd "C-g"))
(define-key global-map          (kbd "C-x C-f") 'helm-find-files)
(define-key global-map          (kbd "C-x C-b") 'helm-mini)
(define-key global-map          (kbd "M-x")     'helm-M-x)
(define-key global-map          (kbd "M-y")     'helm-show-kill-ring) ; overrige yank-pop

;;-----------------------------------------------------------------------------
;; swiper
;; ----------------------------------------------------------------------------
(find-or-install-package 'swiper)
(require 'swiper)

(define-key global-map          (kbd "C-s")     'swiper-thing-at-point)
(define-key global-map          (kbd "M-s")     'swiper)
(define-key global-map          (kbd "C-M-s")   'swiper-all-thing-at-point)

;; ----------------------------------------------------------------------------
;; undo-tree
;; ----------------------------------------------------------------------------
(find-or-install-package 'undo-tree)
(require 'undo-tree)

(setq undo-tree-mode-lighter "")
(setq undo-tree-auto-save-history nil)
(global-undo-tree-mode 1)

(defun undo-tree-visualize-start ()
  (interactive)
  (setq-default truncate-lines t)
  (undo-tree-visualize)
  (undo-tree-visualize-undo))

(define-key global-map                    (kbd "C-z")     'undo-tree-undo)
(define-key global-map                    (kbd "C-x C-z") 'undo-tree-visualize-start)
(define-key undo-tree-visualizer-mode-map (kbd "C-z")     'undo-tree-visualize-undo)
(define-key undo-tree-visualizer-mode-map (kbd "C-y")     'undo-tree-visualize-redo)
(define-key undo-tree-visualizer-mode-map (kbd "C-g")     'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "C-j")     'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "C-m")     'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "g")       'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "j")       'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "m")       'undo-tree-visualizer-quit)

;; ----------------------------------------------------------------------------
;; auto-complete
;; ----------------------------------------------------------------------------
(find-or-install-package 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)

(define-key global-map (kbd "C-x a") 'auto-complete-mode)

;; ----------------------------------------------------------------------------
;; lsp-mode
;; ----------------------------------------------------------------------------
(find-or-install-package 'lsp-mode)
