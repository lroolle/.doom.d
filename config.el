;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Why scroll slow?
;;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#why-is-scrolling-slow-in-emacsdoom

(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(setq user-full-name "Eric Wang"
      user-mail-address "wrqatw@gmail.com"
      doom-localleader-key ",")

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890"))
      leetcode-prefer-language "golang"
      leetcode-prefer-sql "mysql")

;; (set-fontset-font "fontset-default" 'han '("Sarasa Gothic SC"))
(setq doom-font (font-spec :family "Courier" :size 18)
      doom-variable-pitch-font (font-spec :family "Courier" :size 16 :weight 'semi-bold)
      doom-unicode-font (font-spec :family "all-the-icons")
      doom-big-font (font-spec :family "Courier" :size 19))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t
      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)

(setq +doom-dashboard-functions
      '(eril-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded))

;; https://github.com/hlissner/emacs-doom-themes/issues
(setq ns-auto-hide-menu-bar t
      ;; doom-theme 'doom-solarized-dark
      tool-bar-mode 0
      hl-todo-mode t
      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil
      avy-all-windows t ;; gs SPC to search all windows
      evil-snipe-scope 'visible
      +format-on-save-enabled-modes  '(not org-mode sql-mode emacs-lisp-mode tex-mode latex-mode xml-mode)
      )

(add-hook 'after-save-hook #'delete-trailing-whitespace)

(custom-set-faces!
  '(font-lock-comment-face :foreground "#839496" :slant oblique))
;; '(font-lock-keyword-face :slant Oblique))

(use-package! doom-themes
  :custom-face ; for solarized dark
  (region                       ((t (:background "#274643"))))
  ;; (font-lock-constant-face      ((t (:foreground "#2aa198"))))                       ; solarized cyan
  ;; (highlight-numbers-number     ((t (:foreground "#2aa198"))))                       ; solarized cyan
  ;; (diff-hl-change               ((t (:background "#2aa198" :foreground "#2aa198")))) ; solarized cyan
  ;; (show-paren-match             ((t (:foreground "white" :background "#174652"))))
  :config
  (setq doom-themes-enable-bold t
        ;; doom-solarized-dark-brighter-comments t
        doom-solarized-dark-brighter-text t
        doom-solarized-dark-comment-bg t)
  (load-theme 'doom-solarized-dark t))

;; Org
;; It’s an aesthetic plugin that offers fancier bullets. Emacs seems to struggle
;; to display those characters with some fonts.
(defun +org-capture/create-notes-file ()
  "Create an org file in notes/."
  (interactive)
  (let ((filename (read-string "Filename: ")))
    (expand-file-name (format "%s.org" filename) org-notes-directory)))

;; (defun with-no-drawer (func &rest args)
;;   (interactive "P")
;;   (let ((org-log-into-drawer (not (car args))))
;;     (funcall func)))

;; (advice-add 'org-add-note :around #'with-no-drawer)

(defun org-journal-find-location ()
  "Open today's journal, but specify a non-nil prefix argument in order to
  inhibit inserting the heading; org-capture will insert the heading. "
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(after! org
  ;; (add-hook 'org-mode-hook #'valign-mode)
  (remove-hook 'org-mode-hook #'org-superstar-mode)
  ;; (add-to-list 'org-capture-templates
  ;;              '("d" "Journal entry" entry (function org-journal-find-location)
  ;;                "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))
  (add-to-list 'org-capture-templates
               '("N" "New Notes" entry (file +org-capture/create-notes-file) ""))

  (setq org-todo-keywords
        '((sequence "TODO(t!)" "PROJ(p!)" "STRT(s!)" "WAIT(w!)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k!)")
          (sequence "[ ](T!)" "[-](S!)" "[?](W!)" "|" "[X](D)"))
        org-log-into-drawer t
        org-directory "~/Dropbox/org/"
        ;; org-agenda-files '(org-directory (concat org-directory "journal/"))
        org-archive-location (concat org-directory ".archive/%s::")
        org-notes-directory (concat org-directory "notes/")
        ;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
        ;; org-journal-encrypt-journal t
        org-attach-dir-relative t
        org-log-done 'time
        org-log-done-with-time t
        org-agenda-log-mode-items '(state)
        org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html)
        org-complete-tags-always-offer-all-agenda-tags t
        org-image-actual-width 300
        geiser-default-implementation 'guile
        geiser-default-implementation 'guile
        geiser-active-implementations '(mit chicken guile racket chibi chez)
        ))

(after! org-journal
  (setq org-journal-file-format "W%V-%Y%m%d.org"
        org-journal-file-type 'weekly
        org-journal-start-on-weekday 0
        org-journal-time-format "%m/%d %R "
        ))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:auto-category t)))
  (org-super-agenda-mode))

;; Jupyter
;; (add-hook! '+org-babel-load-functions
;;   (defun +org-babel-load-jupyter-h (lang)
;;     (and (string-prefix-p "jupyter-" (symbol-name lang))
;;          (require 'ob-jupyter)
;;          (require lang nil t))))
(setq org-babel-default-header-args:jupyter-julia
      '((:session . "py")
        (:kernel . "python3")))

(after! spell-fu
  (setq spell-fu-idle-delay 1)
  (setq ispell-personal-dictionary "~/.doom.d/ispell/words.personal.pws"))

;; Company
(use-package! company-tabnine
  :after company
  :config
  (set-company-backend! 'prog-mode 'company-tabnine 'company-capf 'company-yasnippet)
  (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
  (setq company-idle-delay 0.4
        company-minimum-prefix-length 2
        company-show-numbers t))

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

;; FIXME Fix for emacs 27
;; https://github.com/emacs-lsp/lsp-mode/issues/1778
(setq lsp-gopls-codelens nil)

(use-package! zoom
  ;; :hook (doom-first-input . zoom-mode)
  :config
  (custom-set-variables
   '(zoom-size '(0.68 . 0.68))))

(use-package! dired-subtree
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package! sis
  :config
  (add-hook 'evil-insert-state-exit-hook #'sis-set-english)
  (sis-ism-lazyman-config
   "com.apple.keylayout.ABC"
   "com.apple.inputmethod.SCIM.ITABC")

  (setq sis-auto-refresh-seconds nil)
  (sis-global-cursor-color-mode t)
  ;; (sis-global-respect-mode t)
  (sis-global-follow-context-mode t)
  (sis-global-inline-mode t))

(use-package! evil-pinyin
  :after evil
  :config
  (global-evil-pinyin-mode t))

;; (use-package! cnfonts
;;   (cnfonts-enable))

;; Disable Mouse
(use-package! disable-mouse
  :config
  (global-disable-mouse-mode))

(after! elfeed
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  (setq elfeed-search-filter "@1-month-ago +unread"))

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package! org-mind-map
  :init
  (require 'ox-org)
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  ;; (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )


(load! "+bindings")
(load! "+commands")
