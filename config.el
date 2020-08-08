;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Why scroll slow?
;;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#why-is-scrolling-slow-in-emacsdoom

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq doom-localleader-key ","
      auto-save-visited-mode  +1)

(setq user-full-name "Eric Wang"
      user-mail-address "wrqatw@gmail.com")

(setq doom-font (font-spec :family "Courier" :size 17)
      doom-variable-pitch-font (font-spec :family "Courier" :height 18)
      doom-unicode-font (font-spec :family "all-the-icons")
      doom-big-font (font-spec :family "Courier" :size 18))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; https://github.com/hlissner/emacs-doom-themes/issues
(setq doom-theme 'doom-solarized-dark
      ns-auto-hide-menu-bar t
      tool-bar-mode 0
      hl-todo-mode t
      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil
      avy-all-windows t ;; gs SPC to search all windows
      evil-snipe-scope 'visible
      ;; org-image-actual-width '(200)
      +format-on-save-enabled-modes  '(not emacs-lisp-mode sql-mode tex-mode latex-mode, xml-mode)
      )

;; Org
;; It’s an aesthetic plugin that offers fancier bullets. Emacs seems to struggle
;; to display those characters with some fonts.
(defun +org-capture/create-notes-file ()
  "Create an org file in notes/."
  (interactive)
  (let ((filename (read-string "Filename: ")))
    (expand-file-name (format "%s.org" filename) org-notes-directory)))

(defun org-journal-find-location ()
  "Open today's journal, but specify a non-nil prefix argument in order to
  inhibit inserting the heading; org-capture will insert the heading. "
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))


(after! org
  (remove-hook 'org-mode-hook #'org-superstar-mode)
  (add-to-list 'org-capture-templates
               '("d" "Journal entry" entry (function org-journal-find-location)
                 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))
  (add-to-list 'org-capture-templates
               '("N" "New Notes" entry (file +org-capture/create-notes-file) ""))

  (setq org-todo-keywords
        '((sequence "TODO(t!)" "PROJ(p!)" "STRT(s!)" "WAIT(w!)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k!)")
          (sequence "[ ](T!)" "[-](S!)" "[?](W!)" "|" "[X](D)"))
        org-log-into-drawer t
        org-directory "~/G/org/"
        org-agenda-files '("~/G/org" "~/G/org/journal")
        org-archive-location (concat org-directory ".archive/%s::")
        org-roam-directory (concat org-directory "notes/")
        org-notes-directory (concat org-directory "notes/")
        ;; org-journal-encrypt-journal t
        org-attach-dir-relative t
        org-log-done 'time
        org-log-done-with-time t
        org-agenda-log-mode-items '(state)
        org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html)
        org-complete-tags-always-offer-all-agenda-tags t
        ))

(use-package org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:auto-category t))
        )
  (org-super-agenda-mode))


(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-file-format "%Y%m%d.org"))


(use-package! org-mind-map
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
  ;; :hook (doom-first-input . zoom-mode)
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

(use-package sis
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-follow-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))
  :config
  ;; (add-hook 'evil-insert-state-exit-hook #'sis-set-english)
  (sis-ism-lazyman-config
   ;; "com.apple.keylayout.ABC"
   "com.apple.keylayout.ABC"
   ;; "im.rime.inputmethod.Squirrel.Rime"
   "com.apple.inputmethod.SCIM.ITABC")

  (setq sis-auto-refresh-seconds nil)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (sis-global-follow-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )

;; (use-package esup
;;   :ensure t
;;   ;; To use MELPA Stable use ":pin mepla-stable",
;;   :pin melpa
;;   :commands (esup))
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Templates
(defvar private-file-templates-dir
  (expand-file-name "templates/" (file-name-directory load-file-name))
  "The path to a directory of yasnippet folders to use for file templates.")
(add-to-list 'yas-snippet-dirs 'private-file-templates-dir 'append #'eq)

(after! yasnippets
  ;; (set-file-template! "\\.vue$" ':trigger "__.vue" :mode 'web-mode)
  (set-file-template! "/README\\.org$" ':trigger "__readme" :Mode 'org-mode)
  (set-file-template! "\\.org$" ':trigger "__" :mode 'org-mode)
  (yas-reload-all))

(load! "+bindings")
(load! "+commands")
