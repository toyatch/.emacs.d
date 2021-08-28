(unless (require 'undo-tree nil t) (package-refresh-contents) (package-install 'undo-tree) (require 'undo-tree))
(setq undo-tree-mode-lighter "")
(global-undo-tree-mode 1)
(defun undo-tree-visualize-entrance () (interactive) (setq-default truncate-lines t) (undo-tree-visualize) (undo-tree-visualize-undo))

;; keybids
(bind-key*                                     "C-z"      'undo-tree-undo)
(define-key global-map                    (kbd "C-x C-z") 'undo-tree-visualize-entrance)
(define-key undo-tree-visualizer-mode-map (kbd "C-z")     'undo-tree-visualize-undo)
(define-key undo-tree-visualizer-mode-map (kbd "C-y")     'undo-tree-visualize-redo)

(define-key undo-tree-visualizer-mode-map (kbd "C-g")     'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "C-j")     'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "C-m")     'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "g")       'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "j")       'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "m")       'undo-tree-visualizer-quit)
