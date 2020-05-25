;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Eric Wang"
      user-mail-address "wrqatw@gmail.com")

(setq doom-font (font-spec :family "Courier" :size 20)
      doom-variable-pitch-font (font-spec :family "InputMono" :height 120)
      doom-unicode-font (font-spec :family "all-the-icons")
      doom-big-font (font-spec :family "InputMono" :size 20))

;; https://github.com/hlissner/emacs-doom-themes/issues
(setq doom-theme 'doom-solarized-dark
      org-directory "~/G/org/"
      display-line-numbers-type 'relative)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(after! company
  (setq company-idle-delay 0.1))

(use-package smart-input-source
  :config
  (setq smart-input-source-english-input-source
        "com.apple.keylayout.ABC")
  (setq smart-input-source-other-input-source
        "com.apple.inputmethod.SCIM.ITABC")
  (add-hook 'text-mode-hook #'smart-input-source-mode)
  (add-hook 'prog-mode-hook #'smart-input-source-mode))

(use-package zoom
  :after-call pre-command-hook
  :config
  (remove-hook 'window-configuration-change-hook #'zoom)
  (add-hook 'doom-switch-window-hook #'zoom))
