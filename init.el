;; TODO
;; for Windows
;; [ ] 2023/04/23 windows上でGitBashを介さずに起動したとき、git fetchなどのSSH接続ができない問題を解消したい
;; [ ] 2023/04/23 shell-modeでgit logしたときに結果が最後まで流れるのではなく１画面で止めたい(less)
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

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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
(setq ring-bell-function 'ignore)     ;; beep抑止

;; 文字コード設定
(set-language-environment 'Japanese)  ;; 言語設定
(set-language-environment "UTF-8")
(setq default-process-coding-system '(utf-8 . utf-8))
(prefer-coding-system 'utf-8)

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
(define-key global-map (kbd "M-g") 'grep-find)               ; like bash
(define-key global-map (kbd "C-l") 'recenter)                ; like bash

(define-key global-map (kbd "C-w") 'kill-region)             ; original
(define-key global-map (kbd "M-w") 'kill-ring-save)          ; original
(define-key global-map (kbd "C-y") 'yank)                    ; original

(define-key global-map (kbd "C-o") 'mark-sexp)
(define-key global-map (kbd "M-o") 'mark-sexp)
(define-key global-map (kbd "C-q") 'toggle-truncate-lines)

(define-key global-map (kbd "C-x l") 'linum-mode)
(define-key global-map (kbd "C-t")   'untabify)
(define-key global-map (kbd "C-c C-g") 'magit)

(find-or-install-package 'dimmer)
(use-package dimmer
  :config
  (setq dimmer-fraction 0.4)
  (setq dimmer-exclusion-regexp "^\\*helm\\|^ \\*Minibuf")
  (dimmer-activate))

;;-------------------------------------------------------------------------
;; 入力補完
;;-------------------------------------------------------------------------
;; 候補表示と絞り込み
(find-or-install-package 'vertico)
(use-package vertico
  :init
  (vertico-mode +1))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
        ("C-l" . vertico-directory-delete-word)
        ("C-m" . vertico-directory-enter))
  :hook
  (rfn-eshadow-setup-minibuffer . vertico-directory-tidy))

;; verticoと連携し候補表示を中間一致で行う
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

;; バッファ、検索機能など
(find-or-install-package 'swiper)
(use-package swiper
  :config
  :bind
  (:map global-map
        ("M-s" . swiper-thing-at-point)
        ("C-s" . swiper))
  (:map minibuffer-local-map
        ("M-y" . yank-pop)))

(find-or-install-package 'consult)
(use-package consult
  :config
  (setq consult-goto-line-numbers nil)
  :bind
  (:map global-map
        ("M-y" . consult-yank-pop)
        ("C-M-g" . consult-grep)
        ("C-M-s" . consult-line-multi)
        ("C-x C-b" . consult-buffer)
        ("C-x C-g" . consult-goto-line))
  (:map minibuffer-local-map
        ("M-y" . yank-pop)))

(find-or-install-package 'embark-consult)
(use-package embark-consult
  :bind
  (:map global-map
        ("C-x C-a" . embark-act)))

;; オートコンプリート
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (global-company-mode)
  :bind (("C-<return>" . company-complete))
  )

;; ------------------------------------------------------------------------
;; undo-tree
;; ------------------------------------------------------------------------
(find-or-install-package 'undo-tree)
(use-package undo-tree
  :init
  (defun undo-tree-visualize-start ()
    (interactive)
    (setq-default truncate-lines t)
    (undo-tree-visualize)
    (undo-tree-visualize-undo))
  (setq undo-tree-mode-lighter "")
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1)

  :bind
  (:map global-map
        ("C-z" . undo-tree-undo)
        ("C-X C-z" . undo-tree-visualize-start))
  (:map undo-tree-visualizer-mode-map
        ("C-z" . undo-tree-visualize-undo)
        ("C-y" . undo-tree-visualize-redo)
        ("C-g" . undo-tree-visualizer-quit)
        ("C-j" . undo-tree-visualizer-quit)
        ("C-m" . undo-tree-visualizer-quit)
        ("g"   . undo-tree-visualizer-quit)
        ("j"   . undo-tree-visualizer-quit)
        ("m"   . undo-tree-visualizer-quit)))

;; ------------------------------------------------------------------------
;; flycheck
;; ------------------------------------------------------------------------
(use-package flycheck
  :ensure t)

;; ------------------------------------------------------------------------
;; lsp(eglog)
;; ------------------------------------------------------------------------
(find-or-install-package 'eglot)
(use-package eglot)

;; ------------------------------------------------------------------------
;; TypeScripts
;; ------------------------------------------------------------------------
(find-or-install-package 'add-node-modules-path)
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
  (typescript-mode . hs-minor-mode)

  :config
  (setq typescript-indent-level 2)
  (setq flycheck-idle-change-delay 5)
  (setq-local company-backends '((company-capf)))

  (defun apply-prettier ()
    (interactive)
    (shell-command
     (format "%s --write %s"
             (shell-quote-argument (executable-find "prettier"))
             (shell-quote-argument (expand-file-name buffer-file-name))))
    (revert-buffer t t t))
  (add-hook 'typescript-mode-hook
            (lambda () (add-hook 'after-save-hook 'apply-prettier t t)))


  :bind
  ("C-x C-p" . flymake-goto-prev-error)
  ("C-x C-n" . flymake-goto-next-error)
  ("C-x C-e" . flymake-show-project-diagnostics)
  ("C-x e"   . flycheck-list-errors)

  ("C-x C-j" . xref-find-definitions)
  ("C-x C-h" . xref-go-back)
  ("C-x C-r" . xref-find-references)

  ("M-[" . hs-hide-block)
  ("M-]" . hs-show-block))

;; ------------------------------------------------------------------------
;; csv-mode
;; ------------------------------------------------------------------------
(find-or-install-package 'csv-mode)
(use-package csv-mode
  :ensure t
  :mode
  ("\\.csv\\'" . csv-mode))

;; ------------------------------------------------------------------------
;; shell-mode
;; ------------------------------------------------------------------------
;;
;; .bashrcに以下を記載しておく
;;
;;   # for emacs
;;   if [[ $TERM != "dumb" ]]; then
;;     export PS1='\w $ '
;;   fi
;;
;; FIXME: CTRL ALTの入れ替えをしたい
;; TODO: shellのgit diffで色分けをしたい

;; NOTE: use-packageのhookではなぜかうまくいかない
(add-hook 'shell-mode-hook
  (lambda ()
    (linum-mode 0)
    (font-lock-mode 0)
    (define-key shell-mode-map (kbd "C-l") 'my-git-diff-toggle)
    (define-key shell-mode-map (kbd "C-r") 'consult-history)
  ))

(use-package shell
  :ensure nil
  :config
  (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (setq shell-file-name "bash")
  (setq explicit-bash.exe-args '("--noediting" "-i"))
  :bind
  ("M-t" . shell))

(setq my-git-diff-toggle-status 0)

(defun my-git-diff-on ()
  (interactive)
  (highlight-regexp   "^\+.*$" 'compilation-info)
  (highlight-regexp   "^\-.*$" 'compilation-error)
  (highlight-regexp   "^Changes to be committed:" 'compilation-info)
  (highlight-regexp   "^Changes not staged for commit:" 'compilation-error)
  (highlight-regexp   "^Untracked files:" 'compilation-error)
  )

(defun my-git-diff-off ()
  (interactive)
  (unhighlight-regexp "^\+.*$")
  (unhighlight-regexp "^\-.*$")
  (unhighlight-regexp "^Changes to be committed:")
  (unhighlight-regexp "^Changes not staged for commit:")
  (unhighlight-regexp "^Untracked files:")
  )

(defun my-git-diff-toggle ()
  (interactive)
  (recenter)
  (if (eq my-git-diff-toggle-status 0) (my-git-diff-on) (my-git-diff-off))
  (setq my-git-diff-toggle-status (- 1 my-git-diff-toggle-status)))

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
(define-key global-map (kbd "C-M-<return>") 'set-alpha-toggle)

;; EOF
