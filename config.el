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
      display-line-numbers-type 'relative
      avy-all-windows t ;; gs SPC to search all windows
      )

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Company
(use-package! company-tabnine
  :after company
  :config
  (set-company-backend! 'prog-mode 'company-tabnine 'company-capf 'company-yasnippet)
  (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
  (setq company-idle-delay 0
        company-show-numbers t))

;; Evil snipe
(setq evil-snipe-scope 'visible)

;; FIXME Fix for emacs 27
;; https://github.com/emacs-lsp/lsp-mode/issues/1778
(setq lsp-gopls-codelens nil)

(use-package! zoom
  :after-call pre-command-hook
  :config
  (custom-set-variables
   '(zoom-size '(0.68 . 0.68))
   ;; '(zoom-ignored-major-modes '(dired-mode vterm-mode help-mode helpful-mode rxt-help-mode help-mode-menu org-mode))
   ;; '(zoom-ignored-buffer-names '("*info*"))
   ;; '(zoom-ignored-buffer-name-regexps '("^*calc" "\\*helpful variable: .*\\*"))
   ;; '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20))))
   )
  ;; (remove-hook! 'window-configuration-change-hook #'zoom)
  ;; (add-hook! 'doom-switch-window-hook #'zoom)
  ;; (add-hook! 'doom-switch-buffer-hook #'zoom)
  )


(use-package! dired-subtree :ensure t
              :after dired
              :config
              (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
              (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package! smart-input-source
  :init
  ;; set the english input source
  ;;(setq-default smart-input-source-english "com.apple.keylayout.ABC")
  (setq-default smart-input-source-english "com.apple.keylayout.ABC")
  ;; set the default other language input source for all buffer
  (setq-default smart-input-source-other "com.apple.inputmethod.SCIM.ITABC")

  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . smart-input-source-follow-context-mode)
  ;;  ((text-mode prog-mode) . smart-input-source-inline-mode))

  :config
  ;; enable the /cursor color/ mode
  (smart-input-source-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (smart-input-source-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (smart-input-source-global-follow-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (smart-input-source-global-inline-mode t)
  )

(use-package! disable-mouse
  :config
  (global-disable-mouse-mode)
  )


(load! "+bindings")
