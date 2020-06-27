;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Eric Wang"
      user-mail-address "wrqatw@gmail.com")

(setq doom-localleader-key ",")

(setq doom-font (font-spec :family "Courier" :size 17)
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

;; Company
(use-package! company-tabnine
  :after company
  :config
  (set-company-backend! 'prog-mode 'company-tabnine 'company-capf 'company-yasnippet)
  (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
  (setq company-idle-delay 0
        company-show-numbers t))

;; FIXME Fix for emacs 27
;; https://github.com/emacs-lsp/lsp-mode/issues/1778
(setq lsp-gopls-codelens nil)

(use-package zoom
  :after-call pre-command-hook
  :config
  (remove-hook 'window-configuration-change-hook #'zoom)
  (add-hook 'doom-switch-window-hook #'zoom)
  (custom-set-variables
   '(zoom-size '(0.7 . 0.7))
   '(zoom-ignored-major-modes '(dired-mode markdown-mode vterm-mode org-mode helpful-mode))
   '(zoom-ignored-buffer-names '("*doom:scratch*" "*info*"))
   '(zoom-ignored-buffer-name-regexps '("^*calc"))
   ;; '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20))))
   ))


(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; (use-package smart-input-source
;;   :config
;;   (setq smart-input-source-external-ism "im-select")
;;   (setq smart-input-source-english
;;         "com.apple.keylayout.ABC")
;;   (setq-default smart-input-source-other
;;                 "com.apple.inputmethod.SCIM.ITABC")
;;   (smart-input-source-global-respect-mode t)
;;   ;; (smart-input-source-global-preserve-mode t)
;;   (add-hook 'text-mode-hook #'smart-input-source-follow-context-mode)
;;   (add-hook 'prog-mode-hook #'smart-input-source-follow-context-mode)
;;   (add-hook 'text-mode-hook #'smart-input-source-inline-english-mode)
;;   (add-hook 'prog-mode-hook #'smart-input-source-inline-english-mode))

(use-package smart-input-source
  :init
  ;; set the english input source
  (setq smart-input-source-english
        "com.apple.keylayout.ABC")

  ;; set the default other language input source for all buffer
  (setq-default smart-input-source-other
                "com.apple.inputmethod.SCIM.ITABC")

  :config
  ;; Input source specific cursor color
  (defvar original-cursor-background nil)
  (add-hook 'smart-input-source-set-english-hook
            (lambda ()
              (when original-cursor-background
                (set-cursor-color original-cursor-background))))
  (add-hook 'smart-input-source-set-other-hook
            (lambda ()
              (unless original-cursor-background
                (setq original-cursor-background
                      (or (cdr (assq 'cursor-color default-frame-alist))
                          (face-background 'cursor)
                          "Red")))
              (set-cursor-color "green")))

  ;; (push 'YOUR-COMMAND smart-input-source-preserve-save-triggers)
  ;; (push 'YOUR-COMMAND smart-input-source-preserve-M-x-commands)

  ;; enable the /respect/ mode
  (smart-input-source-global-respect-mode t)

  ;; enable the /follow context/ and /inline english/ mode for all buffers
  (smart-input-source-global-follow-context-mode t)
  (smart-input-source-global-inline-english-mode t)

  ;; enable the /follow context/ and /inline english/ mode for specific buffers
  ;; :hook
  ;; (((text-mode prog-mode) . smart-input-source-follow-context-mode)
  ;;  ((text-mode prog-mode) . smart-input-source-inline-english-mode))
  )


(load! "+bindings")
