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

 (:leader
  ;; Workspace bindings
  (:desc "workspace"                :prefix "TAB"
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
  )
 )


(provide '+bindings)
