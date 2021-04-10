;;; ~/.doom.d/+commands.el -*- lexical-binding: t; -*-

(defalias 'ex! 'evil-ex-define-cmd)

;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)


(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(defun flush-blank-lines ()
  "Removes all blank lines from buffer or region"
  (interactive)
  (save-excursion
    (let (min max)
      (if (equal (region-active-p) nil)
          (mark-whole-buffer))
      (setq min (region-beginning) max (region-end))
      (flush-lines "^ *$" min max t)
      )
    )
)


(defun +python/nav-backward-up-class ()
  "For nav between class block in Python-mode."
  (interactive)
  (let ((pos nil))
    (while (not (equal pos (point)))
      (setf pos (point))
      (python-nav-backward-up-list))))

(defun +python/nav-up-class ()
  "For nav up class block in Python-mode."
  (interactive)
  ;; Set jump point for jump back
  (better-jumper-set-jump)
  (+python/nav-backward-up-class)
  (beginning-of-defun))

(defun +python/nav-down-class ()
  "For nav down class block in Python-mode."
  (interactive)
  ;; Set jump point for jump back
  (better-jumper-set-jump)
  (+python/nav-backward-up-class)
  (end-of-defun)
  (end-of-defun)
  (+python/nav-backward-up-class))

(defun +python/insert-pdbpp ()
  (interactive)
  (evil-open-below 1)
  (insert "import pdb; pdb.pdb.set_trace()"))

(defun +python/insert-named-logger ()
  (interactive)
  (let ((logger-name (read-string "Logger Name: ")))
    (evil-open-below 1)
    (insert (format "logger.debug(\"%s<<<<<<<\\n%%s\\n\", %s)" logger-name logger-name))
    ;; (evil-open-below 1)
    ;;(insert (format "logger.debug(%s)" logger-name))
    ))

(defun +python/insert-named-printer ()
  (interactive)
  (let ((logger-name (read-string "Printer Name: ")))
    (evil-open-below 1)
    (insert (format "print \"%s<<<<<<<\"" logger-name))
    (evil-open-below 1)
    (insert (format "print %s" logger-name))))

(defun +org/insert-netis-jira-issue ()
  (interactive)
  (let ((issue-num (read-string "ISSUE: ")))
    (insert (format "[[https://jira.netisdev.com/browse/%s][%s]]" issue-num issue-num))
    ))

(defun translate-timestamp-readable (ms)
  (interactive)
  (format-time-string "%Y/%m/%d %H:%M:%S" (seconds-to-time (/ ms 1)) t))

(defun +eril/translate-timestamp-readable ()
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (translate-timestamp-readable (string-to-number  text))))))

(defun word-or-region-to-lcc ()
  "Convert word at point (or selected region) to lower camel case."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (s-lower-camel-case text)))))


(defun +eril/insert-current-date-time ()
  "Insert the current datetime using `insert-current-date-time-format'."
  (interactive)
  (insert "[")
  (insert (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time)))
  (insert "]"))

(defun +eril/insert-current-time ()
  "Insert the current time."
  (interactive)
  (insert "")
  (insert (format-time-string "%H:%M:%S" (current-time)))
  (insert " "))

(defun +org/toggle-todo-done ()
  "Mark to TODO for ordinary heading and toggle todos between todo/done"
  (interactive)
  (save-excursion
    ;; Make sure command works even if point is below target heading
    (org-back-to-heading t)
    (cond ((looking-at "\*+ \\(TODO\\|HOLD\\)")
           (org-todo "DONE")
           (hide-subtree))
          ((looking-at "\*+ ")
           (org-todo "TODO")
           (hide-subtree))
          (t (message "Cann't toggle at current point.")))))

(defun +org/mark-all-done ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-todo "DONE"))
   "/TODO" 'file))

(defun +org/insert-current-time-journal ()
  "Insert the current time in org journal."
  (interactive)
  (insert "")
  (insert (format-time-string "%m/%d %H:%M" (current-time)))
  (insert " "))

(defun +org/org-toc ()
  (interactive)
  (let ((headings (delq nil (loop for f in (f-entries "." (lambda (f) (f-ext? f "org")) t)
                                  append
                                  (with-current-buffer (find-file-noselect f)
                                    (org-map-entries (lambda ()
                                                       (when (> 2 (car (org-heading-components)))
                                                         (cons f (nth 4 (org-heading-components)))))))))))
    (switch-to-buffer (get-buffer-create "*toc*"))
    (erase-buffer)
    (org-mode)
    (loop for (file . heading) in headings
          do
          (insert (format "* [[%s::*%s]]\n" file heading)))))


(defun +eril/evil-title-case-region-or-line (@begin @end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or,
and, …}. If a word already contains cap letters such as HTTP, URL, they are left
as is.
When called in a elisp program, *begin *end are region boundaries.  URL
`http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           $p1
           $p2
           ($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward $skipChars (line-beginning-position))
         (setq $p1 (point))
         (skip-chars-forward $skipChars (line-end-position))
         (setq $p2 (point)))
       (list $p1 $p2))))
  (let* (
         ($strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (downcase-region (point-min) (point-max))
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
           $strPairs))))))

(defun +eril/reverse-string (beg end)
  "Reverse characters between BEG and END."
  (interactive "r")
  (let ((region (buffer-substring beg end)))
    (delete-region beg end)
    (insert (nreverse region))))


(defun +markdown/markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))



(defun +eril/dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-footer) " ")
            (insert "\n"))
          '("    ___       ___       ___       ___       ___   "
            "   /\\__\\     /\\  \\     /\\__\\     /\\__\\     /\\  \\  "
            "  /:/__/_   /::\\  \\   /:/  /    /:/  /    /::\\  \\ "
            " /::\\/\\__\\ /::\\:\\__\\ /:/__/    /:/__/    /:/\\:\\__\\"
            " \\/\\::/  / \\:\\:\\/  / \\:\\  \\    \\:\\  \\    \\:\\/:/  /"
            "   /:/  /   \\:\\/  /   \\:\\__\\    \\:\\__\\    \\::/  / "
            "   \\/__/     \\/__/     \\/__/     \\/__/     \\/__/  "
            "    ___       ___       ___       ___       ___   "
            "   /\\  \\     /\\  \\     /\\  \\     /\\__\\     /\\  \\  "
            "  /::\\  \\   /::\\  \\   _\\:\\  \\   /:/  /    /::\\  \\ "
            " /::\\:\\__\\ /::\\:\\__\\ /\\/::\\__\\ /:/__/    /:/\\:\\__\\"
            " \\:\\:\\/  / \\;:::/  / \\::/\\/__/ \\:\\  \\    \\:\\/:/  /"
            "  \\:\\/  /   |:\\/__/   \\:\\__\\    \\:\\__\\    \\::/  / "
            "   \\/__/     \\|__|     \\/__/     \\/__/     \\/__/  "))
    (when (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let ((image (create-image (fancy-splash-image-file))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +doom-dashboard--width
                                      (car (image-size image nil)))
                                   2))))
                   ? ))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0)
                           ?\n)))))

(provide '+commands)
