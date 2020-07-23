;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq doom-localleader-key ",")

(setq user-full-name "Eric Wang"
      user-mail-address "wrqatw@gmail.com")

(setq doom-font (font-spec :family "Courier" :size 17)
      doom-variable-pitch-font (font-spec :family "Courier" :height 120)
      doom-unicode-font (font-spec :family "all-the-icons")
      doom-big-font (font-spec :family "Courier" :size 20))

;; https://github.com/hlissner/emacs-doom-themes/issues
(setq doom-theme 'doom-solarized-dark
      ns-auto-hide-menu-bar t
      tool-bar-mode 0
      display-line-numbers-type 'relative
      avy-all-windows t ;; gs SPC to search all windows
      evil-snipe-scope 'visible
      )

;; Org
(setq org-directory "~/G/org/"
      org-startup-with-inline-images t
      org-attach-dir-relative t
      )

(use-package org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )


;; Disable Mouse
(use-package! disable-mouse
  :config
  (global-disable-mouse-mode))


;; Company
(use-package! company-tabnine
  :after company
  :config
  (set-company-backend! 'prog-mode 'company-tabnine 'company-capf 'company-yasnippet)
  (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t))

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
              (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map)
              )


(use-package! sis
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-follow-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))
  :config
  (sis-ism-lazyman-config
   ;; "com.apple.keylayout.ABC"
   "com.apple.keylayout.ABC"
   ;; "im.rime.inputmethod.Squirrel.Rime"
   "com.apple.inputmethod.SCIM.ITABC")

  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (sis-global-follow-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )



(load! "+bindings")
