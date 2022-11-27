(cd "~/")
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
(load-theme 'wheatgrass)              ;; theme
(setq inhibit-startup-screen t)       ;; スタートアップメッセージ非表示
(menu-bar-mode 0)                     ;; メニューバー非表示
(set-language-environment 'Japanese)  ;; 言語設定
(prefer-coding-system 'utf-8)         ;; 文字コード設定
(setq ring-bell-function 'ignore)     ;; beep抑止

;; backupfile抑止
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

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
(define-key global-map (kbd "C-x p")   'windmove-up)
(define-key global-map (kbd "C-x n")   'windmove-down)
(define-key global-map (kbd "C-x f")   'windmove-right)
(define-key global-map (kbd "C-x b")   'windmove-left)
(define-key global-map (kbd "C-^")     'enlarge-window)

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

;; ----------------------------------------------------------------------------
;; dimmer
;; ----------------------------------------------------------------------------
(find-or-install-package 'dimmer)
(setq dimmer-fraction 0.4)
(setq dimmer-exclusion-regexp "^\\*helm\\|^ \\*Minibuf")
(dimmer-activate)

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

;; ----------------------------------------------------------------------------
;; eshell-mode
;; ----------------------------------------------------------------------------
;; FIXME: CTRL ALTの入れ替えをしたい
;; TODO: eshellのgit diffで色分けをしたい
(define-key global-map (kbd "M-t") 'eshell)

(defun recenter-with-highlight-diff-color ()
  (interactive)
  (recenter)
  (unhighlight-regexp "^\+.*")
  (highlight-regexp   "^\+.*" 'hi-green-b)
  (unhighlight-regexp "^\-.*")
  (highlight-regexp   "^\-.*" 'hi-red-b))

(add-hook 'eshell-mode-hook
  (lambda ()
   (setq whitespace-style '(face trailing tabs tab-mark))
   (linum-mode 0)
   ;;(define-key eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
   ;;(define-key eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
   (define-key eshell-mode-map (kbd "C-l") 'recenter-with-highlight-diff-color)
   (define-key eshell-mode-map (kbd "C-r") 'helm-eshell-history)))

;; ----------------------------------------------------------------------------
;; TypeScripts
;; ----------------------------------------------------------------------------
;; tide & lsp(eslint)
(find-or-install-package 'tide)
(find-or-install-package 'company)
(add-to-list 'auto-mode-alist '("\\/.*\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\/.*\\.tsx\\'" . typescript-mode))

(add-hook 'typescript-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq typescript-indent-level 2) ;; default 4
    (setq js2-strict-missing-semi-warning nil)
    (setq whitespace-style '(face trailing tabs tab-mark))
    (tide-setup)
    (lsp t)
    (flycheck-mode t)
    (company-mode t)
    (auto-complete-mode nil)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (add-node-modules-path)
    ))
(define-key global-map (kbd "C-x C-n") 'next-error)
(define-key global-map (kbd "C-x C-p") 'previous-error)
(define-key global-map (kbd "C-x C-e") 'flycheck-list-errors)
(define-key global-map (kbd "C-x C-j") 'tide-jump-to-definition)
(define-key global-map (kbd "C-x C-h") 'tide-jump-back)

;; preteer
(find-or-install-package 'add-node-modules-path)
(defun apply-prettier ()
  (interactive)
  (shell-command (format "%s --write %s"
    (shell-quote-argument (executable-find "prettier"))
    (shell-quote-argument (expand-file-name buffer-file-name))))
  (revert-buffer t t t))
(add-hook 'typescript-mode-hook
  (lambda () (add-hook 'after-save-hook 'apply-prettier t t)))

;; ----------------------------------------------------------------------------
;; for WindowSystem
;; ----------------------------------------------------------------------------
(if window-system (progn
  (tool-bar-mode 0)                     ;; ツールバー非表示
  (scroll-bar-mode 0)                   ;; スクロールバー非表示

  ;; Colors
  (set-background-color "Black")
  (set-foreground-color "LightGray")
  (set-cursor-color "Gray")
  (set-frame-parameter nil 'alpha 85)

  ;;; ClipBorard
  (setq x-select-enable-clipboard t)))

;; EOF
