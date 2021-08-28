(unless (require 'mmm-mode nil t) (package-refresh-contents) (package-install 'mmm-mode) (require 'mmm-mode))

(use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 2)
  (set-face-background 'mmm-default-submode-face "black")
  (mmm-add-classes
   '((vue-embeded-web-mode
      :submode html-mode
      :front "^<template>"
      :back "^</template>")
     (vue-embeded-js-mode
      :submode js-mode
      :front "^<script>"
      :back "^</script>")
     (vue-embeded-css-mode
      :submode css-mode
      :front "^<style>"
      :back "^</style>")))

  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-web-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-js-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-scss-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-css-mode))
