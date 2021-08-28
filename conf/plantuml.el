;; plantuml support
(require 'cl)
(defun reverse_relation (text)
  (mapconcat
   (function (lambda (x)
               (if (string= (format "%c" x) "<") ">"
                 (if (string= (format "%c" x) ">") "<"
                   (if (string= (format "%c" x) "}") "{"
                     (if (string= (format "%c" x) "{") "}"
                       (format "%c" x)))))))
   (reverse text) ""))
(defun inverse_puml_relation (text)
  (setq pulm_relations (split-string text))
  (concat (nth 2 pulm_relations) " " (reverse_relation (nth 1 pulm_relations)) " " (nth 0 pulm_relations)))
(defun find-relation () (interactive)
       (re-search-forward "[\n]" nil nil -1)
       (re-search-forward "[a-z0-9A-Z_\-]+[\t ]+[\<}\|]?[o\|]?[-\.]+[o\|]?[\>{\|]?[\t ]+[a-z0-9A-Z_\-]+" nil nil 1)
       (let ((case-fold-search nil) (s (buffer-substring (match-beginning 0) (match-end 0))))
         (delete-region (match-beginning 0) (match-end 0))
         (insert (inverse_puml_relation s))))

(defun plantuml-template ()
  (interactive)
  (insert "@startuml\n\n")
  (insert "@enduml\n")
  (re-search-forward "\n@enduml" nil nil -1)
  )

(defun plantuml-save-png () (interactive)
       (setq cmd (concat "java -Djava.awt.headless=true -jar ~/.emacs.d/plantuml.jar -charset UTF-8 " (buffer-file-name)))
       (message cmd) (call-process-shell-command cmd nil 0))

;; keybinds
(define-key global-map (kbd "M-r") 'find-relation)
(define-key global-map (kbd "C-x p t") 'plantuml-template)
(define-key global-map (kbd "C-x p s") 'plantuml-save-png)
