;;-----------------------------------------------------------------------------
;; transition-word-case
;;-----------------------------------------------------------------------------

(unless (require 's nil t) (package-refresh-contents) (package-install 's) (require 's))

(defun is-lower-dashed-words? (text) (and (s-lowercase? text)
                                          (not (s-uppercase? text))
                                          (not (s-contains? "_" text))
                                          (s-contains? "-" text)
                                          (not (s-capitalized? (s-left 1 text)))))

(defun is-lower-snake-case? (text) (and (s-lowercase? text)
                                        (not (s-uppercase? text))
                                        (s-contains? "_" text)
                                        (not (s-contains? "-" text))
                                        (not (s-capitalized? (s-left 1 text)))))

(defun is-upper-dashed-words? (text) (and (not (s-lowercase? text))
                                          (s-uppercase? text)
                                          (not (s-contains? "_" text))
                                          (s-contains? "-" text)
                                          (s-capitalized? (s-left 1 text))))

(defun is-upper-snake-case? (text) (and (not (s-lowercase? text))
                                        (s-uppercase? text)
                                        (s-contains? "_" text)
                                        (not (s-contains? "-" text))
                                        (s-capitalized? (s-left 1 text))))

(defun is-lower-camel-case? (text) (and (not (s-lowercase? text))
                                        (not (s-uppercase? text))
                                        (not (s-contains? "_" text))
                                        (not (s-contains? "-" text))
                                        (not (s-capitalized? (s-left 1 text)))))

(defun is-upper-camel-case? (text) (and (not (s-lowercase? text))
                                        (not (s-uppercase? text))
                                        (not (s-contains? "_" text))
                                        (not (s-contains? "-" text))
                                        (s-capitalized? (s-left 1 text))))

(defun is-lower-only? (text) (and (s-lowercase? text)
                                  (not (s-uppercase? text))
                                  (not (s-contains? "_" text))
                                  (not (s-contains? "-" text))
                                  (not (s-capitalized? (s-left 1 text)))))

(defun is-upper-only? (text) (and (not (s-lowercase? text))
                                  (s-uppercase? text)
                                  (not (s-contains? "_" text))
                                  (not (s-contains? "-" text))
                                  (s-capitalized? (s-left 1 text))))

(defun to-lower-snake-case (text) (s-snake-case text))
(defun to-upper-snake-case (text) (s-upcase (s-snake-case text)))
(defun to-lower-camel-case (text) (s-lower-camel-case text))
(defun to-upper-camel-case (text) (s-upper-camel-case text))
(defun to-lower-dashed-words (text) (s-replace "_" "-" (s-snake-case text)))
(defun to-upper-dashed-words (text) (s-upcase (s-replace "_" "-" (s-snake-case text))))

(defun proc-word-case (transition)
  (re-search-forward "[^a-z0-9A-Z_\-]+" nil nil -1)
  (re-search-forward "[a-z0-9A-Z_\-]+" nil nil 1)
  (let ((case-fold-search nil) (s (buffer-substring (match-beginning 0) (match-end 0))))
    (delete-region (match-beginning 0) (match-end 0))
    (insert (funcall transition s))))

(defun transition-proc-next (text)
  (if (is-lower-dashed-words? text) (to-lower-snake-case text)
    (if (is-lower-snake-case? text ) (to-lower-camel-case text)
      (if (is-lower-camel-case? text ) (to-upper-camel-case text)
        (if (is-upper-camel-case? text) (to-upper-snake-case text)
          (if (is-upper-snake-case? text ) (to-upper-dashed-words text)
            (if (is-upper-dashed-words? text ) (to-lower-dashed-words text)
              (if (is-lower-only? text ) (to-upper-camel-case text)
                (if (is-upper-only? text ) (to-lower-camel-case text)
                  (to-lower-dashed-words text))))))))))

(defun transition-proc-prev (text)
  (if (is-lower-dashed-words? text) (to-upper-dashed-words text)
    (if (is-upper-dashed-words? text ) (to-upper-snake-case text)
      (if (is-upper-snake-case? text ) (to-upper-camel-case text)
        (if (is-upper-camel-case? text) (to-lower-camel-case text)
          (if (is-lower-camel-case? text ) (to-lower-snake-case text)
            (if (is-lower-snake-case? text ) (to-lower-dashed-words text)
              (if (is-lower-only? text ) (to-upper-dashed-words text)
                (if (is-upper-only? text ) (to-upper-camel-case text)
                  (to-lower-dashed-words text))))))))))

(defun mylib-wordcase-previous () (interactive) (proc-word-case (lambda (s) (transition-proc-prev s))))
(defun mylib-wordcase-next () (interactive) (proc-word-case (lambda (s) (transition-proc-next s))))

;; keybinds
(define-key global-map (kbd "M-9") 'mylib-wordcase-previous)
(define-key global-map (kbd "M-0") 'mylib-wordcase-next)
