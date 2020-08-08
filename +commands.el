;;; ~/.doom.d/+commands.el -*- lexical-binding: t; -*-

(defalias 'ex! 'evil-ex-define-cmd)

;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)

(defun +org/org-toc ()
  (interactive)
  (let ((headings (delq nil (loop for f in (f-entries "." (lambda (f) (f-ext? f "org")) t)
                  append
                  (with-current-buffer (find-file-noselect f)
                    (org-map-entries
                     (lambda ()
                       (when (> 2 (car (org-heading-components)))
                     (cons f (nth 4 (org-heading-components)))))))))))
    (switch-to-buffer (get-buffer-create "*toc*"))
    (erase-buffer)
    (org-mode)
    (loop for (file . heading) in headings
      do
      (insert (format "* [[%s::*%s]]\n" file heading)))))


(provide '+commands)
