;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-
;;; Private Doom Bindings

(map!
 ;; Ensure there are no conflicts
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil

 ;; Easier window navigation
 :n "C-h"     #'evil-window-left
 :n "C-l"     #'evil-window-right
 ;; :n "C-j"     #'evil-window-down
 ;; :n "C-k"     #'evil-window-up
 :n "C-`"     #'+popup/toggle
 :n "C-<tab>" #'+popup/other
 ;; :n "<tab>"   #'+fold/toggle

 ;; Insert mode keep C-n/p as emacs
 :nvmoi "C-n"   #'next-line
 :nvmoi "C-p"   #'previous-line
 :i "C-k"   #'kill-line
 :i "M-k"   #'kill-sentence
 :i "C-M-k" #'kill-sexp

 ;; Short your life
 :v "~"       #'+eril/evil-title-case-region-or-line
 ;; :n [tab] (cmds! (and (featurep! :editor fold)
 ;;                      (save-excursion (end-of-line) (invisible-p (point)))
 ;;                      )
 ;;                 #'+fold/toggle
 ;;                 (fboundp 'evil-jump-item)
 ;;                 #'evil-jump-item)

 :n "zz"                #'+fold/toggle

 "s-N" #'restart-emacs-start-new-emacs

 (:map dired-mode-map
  (:n                                           [tab] #'dired-subtree-cycle)
  (:n                                           "o" #'+macos/open-in-default-program))

 (:map magit-mode-map
  (:desc "magit section toggle"                                           [tab] #'magit-section-toggle))

 ;; Python Mode
 (:map python-mode-map
  :n    [tab]   #'+fold/toggle
  :nmvo "C-S-j" #'+python/nav-down-class
  :nmvo "C-S-k" #'+python/nav-up-class
  :nmvo "C-j"   #'python-nav-forward-defun
  :nmvo "C-k"   #'python-nav-backward-defun
  ;; :nmvo "C-K"     #'python-nav-backward-defun
  ;; :nmvo "C-J"     #'python-nav-forward-defun
  )

 ;; Org Mode
 (:map org-mode-map
  ;; (:n                                           [tab] #'+fold/toggle)
  (:leader
   (:desc "Org Toggle Todo Done"  :n      "d"  #'+org/toggle-todo-done)

   (:desc "Org Export"            :prefix "e"
    (:desc "Org Export as Confluence"  :n      "c"  #'org-confluence-en-export-as-confluence))

   ;; Window bindings
   (:desc "Note"                     :prefix "n"
    :desc "Add note current entry"   :n "n" #'org-add-note)))

 ;; Leader
 (:leader
  (:desc "X-Up"                         "X"   #'doom/open-scratch-buffer
   :desc "X-Down"                       "x"   #'org-capture)

  (                                  :prefix "f"
   :desc "Flush Blank Lines"                 "L"   #'flush-blank-lines)

  ;; ssh deploy
  (:desc "SSH Deploy"                         :prefix  "r"
   :desc "Upload Forced"                      "u" #'ssh-deploy-upload-handler-forced
   :desc "Upload Just"                        "U" #'ssh-deploy-upload-handler
   )

  ;; Search for point
  (:desc "Swiper search buffer at point"      "*" #'swiper-isearch-thing-at-point)
  (:desc "Search"                             :prefix "s"
   :desc "Projectile search project at point" :nvmo   "*" #'+default/search-project-for-symbol-at-point
   )

  ;; magit +git & vc
  (:desc "+git"                     :prefix  "g"
   :desc "smerge-vc-next-conflict"  :invmo   "n" #'smerge-vc-next-conflict
   )

  ;; Workspace bindings
  (:desc "Workspace"                :prefix "TAB"
   :desc "Display tab bar"          :n "TAB" #'+workspace/other
   :desc "New workspace"            :n "N"   #'+workspace/new
   :desc "Next workspace"           :n "n"   #'+workspace/switch-right
   :desc "Prev workspace"           :n "p"   #'+workspace/switch-left
   :desc "Load workspace from file" :n "l"   #'+workspace/load
   :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
   :desc "Save workspace to file"   :n "s"   #'+workspace/save
   :desc "Autosave current session" :n "S"   #'+workspace/save-session
   :desc "Switch workspace"         :n "."   #'+workspace/switch-to
   :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
   :desc "Delete session"           :n "X"   #'+workspace/kill-session
   :desc "Delete this workspace"    :n "d"   #'+workspace/delete
   :desc "Load session"             :n "L"   #'+workspace/load-session
   :desc "Rename workspace"         :n "r"   #'+workspace:rename
   )
  ;; Window bindings
  (:desc "Window" :prefix "w"
   :desc "zoom"   :n "z" #'zoom
   :desc "which-function-mode"   :n "f" #'which-function-mode
   )

  ;; Window bindings
  (:desc "Toggle"         :prefix "t"
   :desc "Lsp UI Imenu(Tagbar)"   "t"     #'lsp-ui-imenu
   :desc "Zoom Mode Auto"         "o"     #'zoom-mode))

 ;; Localleader
 (:localleader
  (:desc "T"              :prefix "t"
   :desc "Translate Timestamp"    "t"  #'translate-timestamp-readable
   )

  (:map markdown-mode-map
   (:desc "Insert"        :prefix "i"
    :desc "Blockquote"    "q" 'markdown-insert-blockquote
    :desc "Bold"          "b" 'markdown-insert-bold
    :desc "Code"          "c" 'markdown-insert-code
    :desc "Emphasis"      "e" 'markdown-insert-italic
    :desc "Footnote"      "f" 'markdown-insert-footnote
    :desc "Code Block"    "s" 'markdown-insert-gfm-code-block
    :desc "Image"         "i" 'markdown-insert-image
    :desc "Link"          "l" 'markdown-insert-link
    :desc "List Item"     "n" 'markdown-insert-list-item
    :desc "Pre"           "p" 'markdown-insert-pre)
   (:desc "Headings" :prefix "h"
    :desc "One"       "1" 'markdown-insert-header-atx-1
    :desc "Two"       "2" 'markdown-insert-header-atx-2
    :desc "Three"     "3" 'markdown-insert-header-atx-3
    :desc "Four"      "4" 'markdown-insert-header-atx-4
    :desc "Five"      "5" 'markdown-insert-header-atx-5
    :desc "Six"       "6" 'markdown-insert-header-atx-6))

  (:map python-mode-map
   (:desc "Inserts"                  :prefix   "i"
    :desc "Insert Named Logger"      :n        "l" #'+python/insert-named-logger
    :desc "Insert Named Printer"     :n        "p" #'+python/insert-named-printer
    :desc "Insert PDB"               :n        "P" #'+python/insert-pdbpp
    :desc "Escape Unicode"           :n        "u" #'unicode-escape--unescape-region
    ))

  (:desc "window"               :prefix "s"
   :desc "Org Tags Sparse Tree" :n "t" #'org-tags-sparse-tree)

  (:desc "Code"                 :prefix "c"
   :desc "Format Region"        :nv     "f" #'+format:region
   :desc "Convert Ts Readable"  :nv     "t" #'+eril/translate-timestamp-readable)

  :desc "Evil NOH"             :n "<RET>" #'evil-ex-nohighlight)
 )


(provide '+bindings)
