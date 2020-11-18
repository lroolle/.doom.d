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

 ;; Insert mode keep C-n/p as emacs
 :i "C-n"   #'next-line
 :i "C-p"   #'previous-line
 :i "C-k"   #'kill-line
 :i "M-k"   #'kill-sentence
 :i "C-M-k" #'kill-sexp

 ;; Short your life
 :v "~"       #'+eril/evil-title-case-region-or-line

 ;; Python Mode
 (:map python-mode-map
  :nmvo "C-j"     #'+python/nav-down-class
  :nmvo "C-k"     #'+python/nav-up-class
  ;; :nmvo "C-K"     #'python-nav-backward-defun
  ;; :nmvo "C-J"     #'python-nav-forward-defun
  :n    "<tab>"   #'+fold/toggle
  )

 ;; Org Mode
 (:map org-mode-map
  (:leader
   ;; Window bindings
   (:desc "Note" :prefix "n"
    :desc "Add note current entry"   :n "n" #'org-add-note)))
 ;; (:localleader
 ;; (:desc "Org format src block"     :n "f" #'+org/format-src-block)
 ;; (:desc "Org insert footnotes"     :n "F" #'+org/org-footnote-new)))

 ;; Leader
 (:leader
  (:desc "X-Up"                         "X"   #'doom/open-scratch-buffer
   :desc "X-Down"                       "x"   #'org-capture)

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
   :desc "zoom"   :n "z" #'zoom)

  ;; Window bindings
  (:desc "Toggle"         :prefix "t"
   :desc "Lsp UI Imenu(Tagbar)"   "u"     #'lsp-ui-imenu))

 ;; Localleader
 (:localleader
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

  (:desc "window"               :prefix "s"
   :desc "Org Tags Sparse Tree" :n "t" #'org-tags-sparse-tree)

  (:desc "Code"                 :prefix "c"
   :desc "Format Region"        :nv     "f" #'+format:region)

  :desc "Evil NOH"             :n "<RET>" #'evil-ex-nohighlight)
 )


(provide '+bindings)
