;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map!
 ;; Ensure there are no conflicts
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil

 ;; Easier window navigation
 :n "C-h"   #'evil-window-left
 :n "C-j"   #'evil-window-down
 :n "C-k"   #'evil-window-up
 :n "C-l"   #'evil-window-right
 :n "C-`"      #'+popup/toggle
 :n "C-<tab>"  #'+popup/other

 ;; Leader
 (:leader
  ;; Workspace bindings
  (:desc "Workspace"                :prefix "TAB"
   :desc "Display tab bar"          :n "TAB" #'+workspace/other
   :desc "New workspace"            :n "n"   #'+workspace/new
   :desc "Load workspace from file" :n "l"   #'+workspace/load
   :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
   :desc "Save workspace to file"   :n "s"   #'+workspace/save
   :desc "Autosave current session" :n "S"   #'+workspace/save-session
   :desc "Switch workspace"         :n "."   #'+workspace/switch-to
   :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
   :desc "Delete session"           :n "X"   #'+workspace/kill-session
   :desc "Delete this workspace"    :n "d"   #'+workspace/delete
   :desc "Load session"             :n "L"   #'+workspace/load-session
   :desc "Next workspace"           :n "]"   #'+workspace/switch-right
   :desc "Previous workspace"       :n "["   #'+workspace/switch-left
   :desc "Rename workspace"         :n "r"   #'+workspace:rename
   )
  ;; Window bindings
  (:desc "window" :prefix "w"
   :desc "zoom"   :n "z" #'zoom))

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
    :desc "Six"       "6" 'markdown-insert-header-atx-6)))
 )


(provide '+bindings)
