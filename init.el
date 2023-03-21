(cd "~/")

;; Packageの初期化
(require 'package)
(customize-set-variable
 'package-archives '(("org"   . "https://orgmode.org/elpa/")
                     ("melpa" . "https://melpa.org/packages/")
                     ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; パッケージがインストールされていなければ自動インストール
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

;; use-packageの初期化
(eval-when-compile
  (require 'use-package))

(defun find-or-install-package (x)
  (unless (package-installed-p x)
    (package-refresh-contents)
    (package-install x)))

;;-------------------------------------------------------------------------
;; basic
;;-------------------------------------------------------------------------
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

;;-------------------------------------------------------------------------
;; basic-keybind
;;-------------------------------------------------------------------------
(define-key global-map (kbd "C-z")     'undo)
(define-key global-map (kbd "C-x C-z") 'undo)

(define-key global-map (kbd "C-x C-a") 'mark-page)

(define-key global-map (kbd "M-p")     'previous-line)
(define-key global-map (kbd "M-n")     'next-line)

(define-key global-map (kbd "M-q")     'other-window)
(define-key global-map (kbd "C-<tab>") 'other-window)
(define-key global-map (kbd "C-x p")   'windmove-up)
(define-key global-map (kbd "C-x n")   'windmove-down)
(define-key global-map (kbd "C-x f")   'windmove-right)
(define-key global-map (kbd "C-x b")   'windmove-left)
(define-key global-map (kbd "C-^")     'enlarge-window)

(define-key global-map (kbd "C-u") 'backward-kill-word)      ; like bash
(define-key global-map (kbd "M-h") 'backward-kill-word)      ; like bash & default
(define-key global-map (kbd "C-k") 'kill-line)               ; like bash & default
(define-key global-map (kbd "C-h") 'delete-backward-char)    ; like bash
(define-key global-map (kbd "C-d") 'delete-char)             ; like bash & default
(define-key global-map (kbd "C-r") 'isearch-backward)        ; like bash & default
(define-key global-map (kbd "C-l") 'recenter)                ; like bash

(define-key global-map (kbd "C-w") 'kill-region)             ; original
(define-key global-map (kbd "M-w") 'kill-ring-save)          ; original
(define-key global-map (kbd "C-y") 'yank)                    ; original
(define-key global-map (kbd "M-y") 'yank-pop)                ; original

(define-key global-map (kbd "C-o") 'mark-sexp)
(define-key global-map (kbd "M-o") 'mark-sexp)
(define-key global-map (kbd "C-q") 'toggle-truncate-lines)

(define-key global-map (kbd "C-x l") 'linum-mode)
(define-key global-map (kbd "C-t")   'untabify)
(define-key global-map (kbd "C-x C-g") 'goto-line)

;; ------------------------------------------------------------------------
;; flycheck
;; ------------------------------------------------------------------------
(use-package flycheck
  :ensure t)

;; ------------------------------------------------------------------------
;; dimmer
;; ------------------------------------------------------------------------
(find-or-install-package 'dimmer)

(setq dimmer-fraction 0.4)
(setq dimmer-exclusion-regexp "^\\*helm\\|^ \\*Minibuf")
(dimmer-activate)

;;-------------------------------------------------------------------------
;; 補完検索
;;-------------------------------------------------------------------------
;; ファイル/バッファ/コマンド補完
(find-or-install-package 'vertico)
(use-package vertico
  :init
  (vertico-mode +1))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
        ("C-l" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-setup-minibuffer . vertico-directory-tidy))

;; 補完を中間一致で行う
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults '((file (styles basic partial-completion)))))

;; ミニバッファ詳細化
(find-or-install-package 'marginalia)
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; 検索系
(find-or-install-package 'consult)
(use-package consult
  :bind
  (:map global-map
        ("M-g" . consult-grep)
        ("C-s" . consult-line)
        ("M-s" . consult-line)
        ("C-M-s" . consult-line-multi)
        ("C-x C-b" . consult-buffer)))

;; minibuffer内でyank-popする場合はバッファの関係？でhelmでしかうまくいかない
(define-key global-map (kbd "C-M-y") 'helm-show-kill-ring)

;; ------------------------------------------------------------------------
;; undo-tree
;; ------------------------------------------------------------------------
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

;; ------------------------------------------------------------------------
;; lsp(eglog)
;; ------------------------------------------------------------------------
(find-or-install-package 'eglot)
(use-package eglot)

;; ------------------------------------------------------------------------
;; eshell-mode
;; ------------------------------------------------------------------------
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
   (define-key eshell-mode-map (kbd "C-r") 'consult-history)))

;; ------------------------------------------------------------------------
;; TypeScripts
;; ------------------------------------------------------------------------
(use-package typescript-mode
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.tsx\\'" . typescript-mode)

  :hook
  ; LSPはeglot
  (typescript-mode . eglot-ensure)
  ; flycheckでeslintを利用する
  (typescript-mode . add-node-modules-path)
  (typescript-mode . flycheck-mode)

  :config
  (auto-complete-mode nil)
  (setq typescript-indent-level 2)

  :bind
  ("C-x C-p" . flymake-goto-prev-error)
  ("C-x C-n" . flymake-goto-next-error)
  ("C-x C-e" . flymake-show-project-diagnostics)
  ("C-x e"   . flycheck-list-errors)

  ("C-x C-j" . xref-find-definitions)
  ("C-x C-h" . xref-go-back)
  ("C-x C-r" . xref-find-references)
  )

;; Company用の設定
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (global-company-mode))

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

;; ------------------------------------------------------------------------
;; for WindowSystem
;; ------------------------------------------------------------------------
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

(setq my-next-alpha 15)
(defun set-alpha-toggle () (interactive)
       (set-frame-parameter nil 'alpha (+ 30 my-next-alpha))
       (setq my-next-alpha (- 70 my-next-alpha))
       )
(define-key global-map (kbd "C-<return>") 'set-alpha-toggle)

;; EOF
