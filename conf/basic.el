;;-----------------------------------------------------------------------------
;; 基本設定
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

;; ホワイトスペース警告
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode 1)

;; 行番号表示
(global-linum-mode 2)
(setq linum-format "%4d ")

;; 環境変数
(setenv "LANG" "ja_JP.UTF-8")
