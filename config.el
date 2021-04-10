;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Why scroll slow?
;;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#why-is-scrolling-slow-in-emacsdoom

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq ns-auto-hide-menu-bar t)
(set-frame-position nil 0 -24)
(tool-bar-mode 0)

;; (with-eval-after-load "doom-modeline"
;;   (doom-modeline-def-modeline 'main
;;   '(misc-info bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
;;   '(objed-state persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker)))

(setq-default frame-title-format '(buffer-file-name "%f" "%b")) ; I already know this is Emacs

(setq user-full-name "Eric Wang"
      user-mail-address "wrqatw@gmail.com"
      doom-localleader-key ","
      leetcode-prefer-language "golang"
      leetcode-prefer-sql "mysql")

;; Proxy and melpa CN
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))
      url-proxy-services '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\|172\\..*\\)")
                           ("http" . "127.0.0.1:7890")
                           ("https" . "127.0.0.1:7890")))

;; TODO: ORGANIZE YOUR FUCKING CONFIGS
(setq lsp-eslint-quiet +1)
(setq +format-with-lsp nil)
;; (setq lsp-pyls-plugins-pycodestyle-ignore '("E221"))

;; (set-fontset-font "fontset-default" 'han '("Sarasa Gothic SC"))
(setq doom-font (font-spec :family "Courier" :size 15)
      doom-variable-pitch-font (font-spec :family "Courier" :size 15 :weight 'semi-bold)
      doom-unicode-font (font-spec :family "all-the-icons")
      doom-big-font (font-spec :family "Courier" :size 19))
(setq flycheck-global-modes '(go-mode))
(setq global-flycheck-mode nil)

;; Switch to the new window after splitting
(setq
 ;; evil-split-window-below t
 ;; evil-vsplit-window-right t
 ;; lsp-ui-sideline is redundant with eldoc and much more invasive, disable it by default.
 lsp-ui-sideline-enable nil
 lsp-enable-symbol-highlighting nil)

(setq +doom-dashboard-functions
      '(+eril/dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded))

;; https://github.com/hlissner/emacs-doom-themes/issues
(setq ns-auto-hide-menu-bar t
      ;; frame-title-format ""  ;; set blank with ugly title bar
      ;; doom-theme 'doom-solarized-dark
      tool-bar-mode 0
      hl-todo-mode t
      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-mode nil
      display-line-numbers-type nil
      inhibit-compacting-font-caches t
      avy-all-windows t ;; gs SPC to search all windows
      evil-snipe-scope 'visible
      ;; <2020-11-06 11:16:10> Update: Disable format +onsave
      ;; TODO: use-package
      ;; +format-on-save-enabled-modes  '(not org-mode sql-mode emacs-lisp-mode tex-mode latex-mode xml-mode)
      +format-with-lsp nil
      ;; (setq-hook! 'python-mode-hook +format-with 'html-tidy)
      ;; Or set it to `:none' to disable formatting
      ;; (setq-hook! 'python-mode-hook +format-with :none)
      )


(add-hook 'after-save-hook #'delete-trailing-whitespace)

(custom-set-faces!
  '(mode-line :family "Courier Prime Code" :weight semi-bold :height 0.9)
  '(mode-line-inactive :family "Courier Prime Code" :slant italic :height 0.8))

(use-package! doom-themes
  :custom-face ; for solarized dark
  ;; (region                       ((t (:background "#274643"))))
  ;; (font-lock-constant-face      ((t (:foreground "#2aa198"))))                       ; solarized cyan
  ;; (highlight-numbers-number     ((t (:foreground "#2aa198"))))                       ; solarized cyan
  ;; (diff-hl-change               ((t (:background "#2aa198" :foreground "#2aa198")))) ; solarized cyan
  (fg ;; '("#BBBBBB" "#BBBBBB" "brightwhite")
   '("#cbd5d6" "#bdc6c7" "#bdc6c7"))

  :config
  (setq doom-themes-enable-bold t
        ;; doom-solarized-dark-brighter-comments t
        ;; doom-solarized-dark-brighter-text t
        doom-solarized-dark-comment-bg t)
  ;; (show-paren-match             ((t (:foreground "white" :background "#174652"))))
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
  (setq org-hide-leading-stars nil
        ;; org-fontify-quote-and-verse-blocks nil
        ;; org-fontify-whole-heading-line nil
        org-hide-leading-stars-before-indent-mode nil
        ;; org-startup-indented nil
        )
  (remove-hook 'org-mode-hook #'org-superstar-mode)

  (remove-hook 'text-mode #'spell-fu-mode)
  (remove-hook 'xml-mode #'spell-fu-mode)

  ;; (add-to-list 'org-capture-templates
  ;;              '("d" "Journal entry" entry (function org-journal-find-location)
  ;;                "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))
  (add-to-list 'org-capture-templates
               '("N" "New Notes" entry (file +org-capture/create-notes-file) ""))
  (add-to-list 'org-capture-templates
               '("i" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %U %?\n%i\n%a" :prepend t))

  (require 'ox-confluence)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "PROJ(p!)" "STRT(s!)" "WAIT(w!)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k!)")
          (sequence "[ ](T!)" "[-](S!)" "[?](W!)" "|" "[X](D)"))
        org-log-into-drawer t
        org-directory "~/Dropbox/org/"
        ;; org-agenda-files '(org-directory (concat org-directory "journal/"))
        org-archive-location (concat org-directory ".archive/%s::")
        org-notes-directory (concat org-directory "notes/")
        deft-directory (concat org-directory "notes/")
        deft-extensions '("md" "org")
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
        geiser-active-implementations '(mit chicken guile racket chibi chez)
        ))

(after! org-journal
  (remove-hook 'org-mode-hook #'valign-mode)
  (setq org-journal-file-format "W%V-%Y%m%d.org"
        org-journal-file-type 'weekly
        org-journal-start-on-weekday 1
        org-journal-time-format "%m/%d %R "
        ))

(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (concat org-directory "notes/")
        +org-roam-open-buffer-on-find-file nil
        ))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:auto-category t)))
  (org-super-agenda-mode))

;; Jupyter
(add-hook! '+org-babel-load-functions
  (defun +org-babel-load-jupyter-h (lang)
    (and (string-prefix-p "jupyter-" (symbol-name lang))
         (require 'ob-jupyter)
         (require lang nil t))))

(setq org-babel-default-header-args:jupyter-julia
      '((:session . "py")
        (:kernel . "python3")))

(after! spell-fu
  (setq spell-fu-idle-delay 1)
  (setq ispell-personal-dictionary "~/.doom.d/ispell/words.personal.pws"))

;; Fix jump back to previous persp workspace buffer
(after! better-jumper
  :config
  (defun doom-prevent-persp-jump (orig-fn &rest args)
    "Ensure ORIG-FN doesn't set any jump points in buffers from other perspectives."
    (unless (and (markerp (car args))
                 (not (+workspace-contains-buffer-p (marker-buffer (car args)))))
      (apply orig-fn args)))
  (advice-add #'evil-set-jump :around #'doom-prevent-persp-jump)
  (advice-add #'better-jumper-set-jump :around #'doom-prevent-persp-jump))


(setq company-idle-delay 0.25
      company-minimum-prefix-length 3
      completion-ignore-case t)

;; Company
(use-package! company-tabnine
  :after company
  :when (featurep! :completion company)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 3
        completion-ignore-case t)

  ;; Avoid return to new line
  (define-key company-active-map (kbd "RET") 'company-complete-selection)

  ;; workaround for company-transformers
  (setq company-tabnine--disable-next-transform nil)
  (defun my-company--transform-candidates (func &rest args)
    (if (not company-tabnine--disable-next-transform)
        (apply func args)
      (setq company-tabnine--disable-next-transform nil)
      (car args)))

  (defun my-company-tabnine (func &rest args)
    (when (eq (car args) 'candidates)
      (setq company-tabnine--disable-next-transform t))
    (apply func args))


  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend)))) candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-1
            candidates-2)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate) 'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-2))
            (push candidate candidates-1)
            (puthash candidate t candidates-table)))
        (setq candidates-1 (nreverse candidates-1))
        (setq candidates-2 (nreverse candidates-2))
        (nconc (seq-take candidates-1 2)
               (seq-take candidates-2 2)
               (seq-drop candidates-1 2)
               (seq-drop candidates-2 2)))))

  (add-to-list 'company-transformers 'company//sort-by-tabnine t)
  (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
  (advice-add #'company-tabnine :around #'my-company-tabnine)
  (set-company-backend! 'prog-mode 'company-tabnine 'company-capf 'company-yasnippet)
  (set-company-backend! 'company-tabnine)
  (cl-pushnew 'company-tabnine (default-value 'company-backends))
  (add-to-list 'company-backends #'company-tabnine)
  (setq +lsp-company-backends '(company-lsp :with company-tabnine :separate))
  (setq +lsp-company-backends '(company-tabnine))
  )


;; ;; :separate 使得不同 backend 分开排序 (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))

(add-hook 'python-mode-hook
          (lambda() (which-function-mode 1)))

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
  (setq dired-hide-details-mode t)
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package! sis
  :config
  (add-hook 'evil-insert-state-exit-hook #'sis-set-english)
  (sis-ism-lazyman-config
   "com.apple.keylayout.ABC"
   "com.apple.inputmethod.SCIM.ITABC")

  (setq sis-auto-refresh-seconds nil
        sis-external-ism "im-select")
  (sis-global-cursor-color-mode t)
  ;; (sis-global-respect-mode t)
  (sis-global-context-mode t)
  (sis-global-inline-mode t))

;; Not needed if your input sources are the same with the default values
;; (use-package! sis
;;   :config
;;   (sis-ism-lazyman-config
;;    "com.apple.keylayout.ABC"
;;    "com.apple.inputmethod.SCIM.ITABC"))

(use-package! evil-pinyin
  :config
  (global-evil-pinyin-mode t)
  (setq evil-pinyin-start-pattern nil)
  )

;; (use-package! cnfonts
;;   (cnfonts-enable))

;; Disable Mouse
(use-package! disable-mouse
  :config
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map))
  (global-disable-mouse-mode))

(after! elfeed
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  (setq elfeed-search-filter "@1-month-ago +unread"))

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
;; (use-package! org-mind-map
;;   :init
;;   (require 'ox-org)
;;   ;; Uncomment the below if 'ensure-system-packages` is installed
;;   ;;:ensure-system-package (gvgen . graphviz)
;;   :config
;;   ;; (setq org-mind-map-engine "dot")       ; Default. Directed Graph
;;   (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
;;   ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
;;   ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
;;   ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
;;   ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
;;   ;; (setq org-mind-map-engine "circo")  ; Circular Layout
;;   )

;; (use-package! valign
;; :after org :config
;; (add-hook 'org-mode-hook #'valign-mode))

(use-package hl-line+
  :load-path "3rd" ; or wherever you put hl-line+.el
  :config
  (hl-line-when-idle-interval 0.3)
  (toggle-hl-line-when-idle 1))

(use-package! keyfreq
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          abort-recursive-edit
          forward-char
          backward-char
          previous-line
          next-line))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; (use-package! ranger
;;   (setq ranger-max-preview-size 10
;;         ranger-parent-depth 1
;;         ranger-width-parents 0.1
;;         ranger-width-preview 0.68
;;         ranger-show-literal nil
;;         ranger-hide-cursor nil))

;; (use-package! python-django)
;;


(use-package! super-save
  :config
  (super-save-mode +1))

(use-package! atomic-chrome
  :after-call focus-out-hook
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode
        atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-start-server))


(use-package! cnfonts)

(use-package! s)
(use-package! auto-complete)

;; setup with use-package
(use-package ejc-sql
  :commands
  (ejc-create-connection ejc-connect ejc-set-column-width-limit)
  :init
  (setq ejc-result-table-impl 'orgtbl-mode ;; 'ejc-result-mode
        ;; ejc-use-flx t
        ;; ejc-flx-threshold 3
        ejc-ring-length 3
        nrepl-sync-request-timeout 60
        ejc-org-mode-babel-wrapper t)

  (push 'ejc-company-backend company-backends)
  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (company-mode t)
              (ejc-set-column-width-limit 120)
              (ejc-set-fetch-size 50)
              (ejc-set-max-rows 100)
              (ejc-set-show-too-many-rows-message nil)
              (ejc-set-column-width-limit 125)
              (ejc-set-use-unicode t)
              (ejc-ac-setup))))

(defun my/ejc-setup-jdbc ()
  (interactive)
  (ejc-set-use-unicode t)
  (ejc-set-column-width-limit 120)
  (ejc-set-max-rows 1000)
  (ejc-set-show-too-many-rows-message nil)

  (ejc-create-connection
   "ssdch208"
   :dependencies [[ru.yandex.clickhouse/clickhouse-jdbc "0.2.6"]]
   :dbtype "clickhouse"
   :classname "ru.yandex.clickhouse.ClickHouseDriver"
   :connection-uri (concat "jdbc:clickhouse://172.16.200.208:8123/" "npm"))

  (ejc-create-connection
   "ch243"
   :dependencies [[ru.yandex.clickhouse/clickhouse-jdbc "0.2.6"]]
   :dbtype "clickhouse"
   :classname "ru.yandex.clickhouse.ClickHouseDriver"
   :connection-uri (concat "jdbc:clickhouse://10.1.4.243:8123/" "npmdata"))

  (ejc-create-connection
   "ch179"
   :dependencies [[ru.yandex.clickhouse/clickhouse-jdbc "0.2.6"]]
   :dbtype "clickhouse"
   :classname "ru.yandex.clickhouse.ClickHouseDriver"
   :connection-uri (concat "jdbc:clickhouse://10.1.4.179:8123/" "npmdata"))

  (ejc-create-connection
   "ch180"
   :dependencies [[ru.yandex.clickhouse/clickhouse-jdbc "0.2.6"]]
   :dbtype "clickhouse"
   :classname "ru.yandex.clickhouse.ClickHouseDriver"
   :connection-uri (concat "jdbc:clickhouse://10.1.4.180:8123/" "npm"))

  (ejc-create-connection
   "ch152"
   :dependencies [[ru.yandex.clickhouse/clickhouse-jdbc "0.2.6"]]
   :dbtype "clickhouse"
   :classname "ru.yandex.clickhouse.ClickHouseDriver"
   :connection-uri (concat "jdbc:clickhouse://10.1.4.152:8123/" "npm"))

  (ejc-create-connection
   "ch181"
   :dependencies [[ru.yandex.clickhouse/clickhouse-jdbc "0.2.6"]]
   :dbtype "clickhouse"
   :classname "ru.yandex.clickhouse.ClickHouseDriver"
   :connection-uri (concat "jdbc:clickhouse://10.1.4.181:8123/" "npm"))

  ;; (ejc-create-connection
  ;;  "npmweb178"
  ;;  :dependencies [[org.mongodb/mongodb-jdbc "1.0.3"]]
  ;;  :dbtype "mongodb"
  ;;  :classname "com.mongodb.jdbc.MongoDriver"
  ;;  :connection-uri (concat "jdbc:mongodb://172.16.13.178:27017/" "npmweb"))

  ;; (ejc-create-connection
  ;;  "u178"
  ;;  ;; :dependencies [[org.mongodb/mongodb-jdbc "1.0.3"]]
  ;;  :dbtype "mongodb"
  ;;  :classpath "/Users/eric/.m2/repository/com/dbschema/MongoDbJdbcDriver/mongojdbc3.1.jar"
  ;;  :classname "com.dbschema.MongoJdbcDriver"
  ;;  :connection-uri (concat "jdbc:mongodb://172.16.13.178:27017/" "npmweb"))

  ;; (ejc-create-connection
  ;;  "es177"
  ;;  :dependencies [[org.elasticsearch.plugin/x-pack-sql-jdbc "7.9.1"]]
  ;;  :dbtype "elasticsearch"
  ;;  :classname "org.elasticsearch.xpack.sql.jdbc.EsDriver"
  ;;  :connection-uri (concat "jdbc:es://172.16.13.177:9200/" ))

  ;; (ejc-create-connection
  ;;  "es178"
  ;;  :dependencies [[org.elasticsearch.plugin/x-pack-sql-jdbc "7.9.1"]]
  ;;  :dbtype "elasticsearch"
  ;;  :classname "org.elasticsearch.xpack.sql.jdbc.EsDriver"
  ;;  :connection-uri (concat "jdbc:es://172.16.13.178:9200/" ))
  )



;; (use-package! popwin
;;   (popwin-mode 1))
;; (use-package! es-mode
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages '((elasticsearch . t))))
;; (use-package! dumb-jump)

(load! "+bindings")
(load! "+commands")
(load! "ox-confluence-en")
