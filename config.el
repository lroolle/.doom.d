;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Eric Wang"
      user-mail-address "wrqatw@gmail.com")

(setq doom-font (font-spec :family "Courier" :size 18)
      doom-variable-pitch-font (font-spec :family "Courier" :height 120)
      doom-unicode-font (font-spec :family "all-the-icons")
      doom-big-font (font-spec :family "Courier" :size 20))

;; https://github.com/hlissner/emacs-doom-themes/issues
(setq doom-theme 'doom-solarized-dark
      ns-auto-hide-menu-bar t
      tool-bar-mode 0
      org-directory "~/G/org/"
      display-line-numbers-type 'relative)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(after! company
  (setq company-idle-delay 0.1))

(use-package smart-input-source
  :config
  (setq smart-input-source-external-ism "im-select"
        smart-input-source-english-input-source "com.apple.keylayout.ABC"
        smart-input-source-other-input-source "com.apple.inputmethod.SCIM.ITABC")
  (add-hook 'text-mode-hook #'smart-input-source-mode)
  (add-hook 'prog-mode-hook #'smart-input-source-mode))

(use-package zoom
  :after-call pre-command-hook
  :config
  (remove-hook 'window-configuration-change-hook #'zoom)
  (add-hook 'doom-switch-window-hook #'zoom)
  (custom-set-variables
   ;; '(zoom-size '(0.618 . 0.618))
   '(zoom-ignored-major-modes '(dired-mode markdown-mode vterm-mode org-mode))
   '(zoom-ignored-buffer-names '("*doom:scratch*" "*info*"))
   '(zoom-ignored-buffer-name-regexps '("^*calc"))
   ;; '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20))))
   ))
