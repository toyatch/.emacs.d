(autoload 'rspec-mode "rspec-mode" nil t)
(defun rspec-and-ruby-mode () (ruby-mode) (rspec-mode) )
(add-to-list 'auto-mode-alist '("_spec\\.rb$" . rspec-and-ruby-mode))
