(defun mylib-wordwrap-is-double?  (text) (string-match "^\"[a-z0-9A-Z_\-]+\"$" text))
(defun mylib-wordwrap-is-single?  (text) (string-match "^\'[a-z0-9A-Z_\-]+\'$" text))
(defun mylib-wordwrap-is-simbole? (text) (string-match "^:[a-z0-9A-Z_\-]+$" text))

(defun mylib-wordwrap-l-search (transition)
  (re-search-forward "[^a-z0-9A-Z_:\'\"\-]+" nil nil -1)
  (re-search-forward "[a-z0-9A-Z_:\'\"\-]+" nil nil 1)
  (let ((case-fold-search nil) (s (buffer-substring (match-beginning 0) (match-end 0))))
    (delete-region (match-beginning 0) (match-end 0))
    (insert (funcall transition s))))

(defun mylib-wordwrap-l-change (text)
  (if (mylib-wordwrap-is-single? text )
      (concat "\"" (substring text 1 (- (length text) 1)) "\"")
    (if (mylib-wordwrap-is-double? text)
        (concat ":" (substring text 1 (- (length text) 1)))
      (if (mylib-wordwrap-is-simbole? text )
          (substring text 1)
        (concat "'" text "'")))))

(defun mylib-wordwrap-change () (interactive) (mylib-wordwrap-l-search (lambda (s) (mylib-wordwrap-l-change s))))

;; keybinds
(define-key global-map (kbd "M-8") 'mylib-wordwrap-change)
