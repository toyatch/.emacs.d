(unless (package-installed-p 'swiper) (package-refresh-contents) (package-install 'swiper))

; keybinds
;; 検索中: ("M-q") 'swiper-query-replace
(global-set-key (kbd "C-s")     'swiper)
(global-set-key (kbd "M-s")     'swiper-all-thing-at-point)
(global-set-key (kbd "C-M-s")   'swiper-all-thing-at-point)
