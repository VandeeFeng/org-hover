;;; org-block-hover.el --- Hover functionality for org INCLUDE blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Vandee
;;
;; Author: Vandee
;; URL: https://github.com/VandeeFeng/org-hover
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides hover functionality for org-mode INCLUDE blocks.
;; When hovering over a quote block containing an INCLUDE directive,
;; it shows the referenced file content in a popup.
;;
;;; Code:

(require 'org-hover-ui)

;;; Customization

(defcustom org-block-hover-enable t
  "Enable org block hover functionality."
  :type 'boolean
  :group 'org-hover)

(defcustom org-block-hover-context-lines 3
  "Number of context lines to show around location."
  :type 'number
  :group 'org-hover)

(defcustom org-block-hover-show-location nil
  "Show file location information in popup."
  :type 'boolean
  :group 'org-hover)

(defcustom org-block-hover-auto-insert nil
  "Auto-insert INCLUDE content into quote block.
When non-nil, simple INCLUDE without reference text will be replaced with file content.
When nil, only show popup without replacing content."
  :type 'boolean
  :group 'org-hover)

;;; Variables

(defconst org-block-hover-include-regex
  "^\\s-*#\\+INCLUDE:[ \t]*\"\\([^\"]+\\(?:::\\*[^\"]+\\)?\\)\"\\(?:[ \t]+:\\(lines\\)\\(?:[ \t]+\\([^\"\\n]*\\)\\)?\\)?"
  "Regex pattern to match org INCLUDE directives.
Matches:
1. file path with optional ::*header
2. parameter name (lines)
3. parameter value for lines")

;;; INCLUDE Parser

(defun org-block-hover--extract-reference-text (begin end)
  "Extract reference text from quote block between BEGIN and END."
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      ;; Skip INCLUDE line
      (when (looking-at "^\\s-*#\\+INCLUDE:")
        (forward-line 1))
      ;; Skip empty lines after INCLUDE
      (while (and (< (point) (point-max))
                  (looking-at "^\\s-*$"))
        (forward-line 1))
      ;; Extract remaining text as reference
      (when (< (point) (point-max))
        (buffer-substring-no-properties (point) (point-max))))))

;;; Content Extraction Functions

(defun org-block-hover--extract-full-file (file)
  "Extract entire content of FILE with maximum 1000 characters limit."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((content (buffer-string)))
      (if (> (length content) 1000)
          (concat (substring content 0 1000) "\n[...]")
        content))))

(defun org-block-hover--extract-lines (file line-start line-end)
  "Extract lines from LINE-START to LINE-END of FILE."
  (when (and line-start line-end)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (forward-line (1- line-start))
      (let ((start-pos (point)))
        (forward-line (1+ (- line-end line-start)))
        (buffer-substring-no-properties start-pos (point))))))

(defun org-block-hover--extract-section (file section-title)
  "Extract org-mode section with SECTION-TITLE from FILE, including the header."
  (when section-title
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      ;; Search for the exact section title, handling different heading levels
      (when (re-search-forward (format "^\\*+\\s-+%s\\s-*$" (regexp-quote section-title)) nil t)
        (let ((header-start (match-beginning 0))
              (header-end (match-end 0))
              (content-start (point)))
          (forward-line 1)
          (let ((content-end
                 (if (re-search-forward "^\\*+" nil t)
                     (match-beginning 0)
                   (point-max))))
            ;; Combine header and content
            (concat (buffer-substring-no-properties header-start header-end)
                    "\n"
                    (buffer-substring-no-properties content-start content-end))))))))

(defun org-block-hover--extract-content (file type params)
  "Extract content from FILE based on TYPE and PARAMS."
  (cond
   ((eq type 'origin)
    (org-block-hover--extract-full-file file))
   ((eq type 'lines)
    (org-block-hover--extract-lines file (car params) (cadr params)))
   ((eq type 'section)
    (org-block-hover--extract-section file params))))

;;; Location and Context Functions

(defun org-block-hover--locate-reference (file text)
  "Locate TEXT in FILE and return (line-number . position)."
  (when (and file text (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (search-forward text nil t)
        (cons (line-number-at-pos) (point))))))

(defun org-block-hover--get-context (file line-num)
  "Get context around LINE-NUM in FILE."
  (when (and file line-num (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let* ((start-line (max 1 (- line-num org-block-hover-context-lines)))
             (end-line (+ line-num org-block-hover-context-lines)))
        (forward-line (1- start-line))
        (let ((start-pos (point)))
          (forward-line (- end-line start-line))
          (let ((end-pos (point)))
            (when (>= end-pos start-pos)
              (buffer-substring-no-properties start-pos end-pos))))))))

;;; Content Replacement Functions

(defun org-block-hover--replace-quote-content (quote-start quote-end new-content)
  "Replace the content within quote block from QUOTE-START to QUOTE-END with NEW-CONTENT."
  (save-excursion
    (goto-char quote-start)
    ;; Find the end of the INCLUDE line
    (unless (re-search-forward "^\\s-*#\\+INCLUDE:.*$" quote-end t)
      (user-error "No INCLUDE line found in quote block"))
    (end-of-line)
    (let ((content-start (point)))
      (goto-char quote-end)
      (beginning-of-line)
      (let ((content-end (point)))
        (delete-region content-start content-end)
        (insert "\n\n" new-content)))))

;;; Popup Formatting and Display

(defun org-block-hover--format-popup-content (content file location)
  "Format popup CONTENT with FILE and LOCATION information."
  (let ((header (if org-block-hover-show-location
                    (format "📁 %s:%d\n%s"
                            (file-name-nondirectory file)
                            (or (car location) 1)
                            (make-string 30 ?─))
                  (format "📁 %s\n%s"
                          (file-name-nondirectory file)
                          (make-string 30 ?─)))))
    (concat header "\n" content)))

(defun org-block-hover--resolve-file-path (file-path)
  "Resolve FILE-PATH relative to current org file.
Strip any ::*section suffix before resolving the path."
  (let* ((clean-path (if (string-match "::\\*" file-path)
                         (substring file-path 0 (match-beginning 0))
                       file-path))
         (current-file (buffer-file-name)))
    (if current-file
        (expand-file-name clean-path (file-name-directory current-file))
      (expand-file-name clean-path))))

(defun org-block-hover-show-popup (include-info reference-info)
  "Show popup with INCLUDE-INFO and REFERENCE-INFO using org-hover-ui."
  (let* ((file-path (org-block-hover--resolve-file-path (alist-get 'file include-info)))
         (content (org-block-hover--extract-content
                   file-path
                   (alist-get 'type include-info)
                   (alist-get 'params include-info)))
         (location (when reference-info
                     (org-block-hover--locate-reference file-path reference-info)))
         (formatted-content (org-block-hover--format-popup-content content file-path location)))
    (when formatted-content
      (org-hover-ui-popup-show formatted-content))))

;;; Trigger Mechanism and Block Detection

(defun org-block-hover--detect-include-block ()
  "Detect if current point is within a quote block containing INCLUDE directive."
  (save-excursion
    ;; First try to find the nearest quote block
    (let ((quote-start (save-excursion
                         (re-search-backward "^#\\+begin_quote" nil t)))
          (quote-end (save-excursion
                       (re-search-forward "^#\\+end_quote" nil t))))
      (when (and quote-start quote-end (>= (point) quote-start) (<= (point) quote-end))
        ;; We're inside a quote block, now look for INCLUDE
        (save-restriction
          (narrow-to-region quote-start quote-end)
          (goto-char (point-min))
          (when (re-search-forward org-block-hover-include-regex nil t)
            (let* ((file-and-section (match-string 1))
                   (param-name (match-string 2))    ; :lines
                   (param-value (match-string 3))    ; parameter value
                   (file-parts (split-string file-and-section "::\\*"))
                   (file (car file-parts))
                   (section-name (cadr file-parts))) ; header-name may be nil
              (list
               (cons 'file file-and-section)  ; Keep full match for path resolution
               (cons 'type (cond
                            ((and section-name (not (string= section-name ""))) 'section)
                            ((string= param-name "lines") 'lines)
                            ((null param-name) 'origin)
                            (t 'origin)))
               (cons 'params (cond
                              ((and section-name (not (string= section-name ""))) section-name)
                              ((string= param-name "lines")
                               (when param-value
                                 (mapcar #'string-to-number (split-string param-value "-"))))
                              (t nil)))))))))))

(defun org-block-hover--replace-and-show ()
  "Handle INCLUDE block: show context for simple INCLUDE, replace for parameterized INCLUDE."
  (let* ((include-info (org-block-hover--detect-include-block))
         (include-type (alist-get 'type include-info))
         (file-path (org-block-hover--resolve-file-path (alist-get 'file include-info)))
         formatted-content)
    (when include-info
      (save-excursion
        (let ((quote-bounds (org-block-hover--find-quote-bounds)))
          (when quote-bounds
            (let* ((reference-text (org-block-hover--extract-from-quote quote-bounds)))
              (if (eq include-type 'origin)
                  (org-block-hover--handle-unpara-include file-path reference-text quote-bounds)
                (org-block-hover--handle-para-include file-path include-info reference-text quote-bounds))
              (setq formatted-content (org-block-hover--get-formatted-content
                                       include-type file-path include-info reference-text))))))
      (when formatted-content
        (org-hover-ui-popup-show formatted-content)))))

(defun org-block-hover--find-quote-bounds ()
  "Find quote block boundaries. Return (QUOTE-START . QUOTE-END) or nil."
  (let ((quote-start (re-search-backward "^#\\+begin_quote" nil t))
        (quote-end (re-search-forward "^#\\+end_quote" nil t)))
    (when (and quote-start quote-end)
      (cons quote-start quote-end))))

(defun org-block-hover--get-content-start (quote-start)
  "Get content start position after INCLUDE line."
  (goto-char quote-start)
  (forward-line 1)
  (when (looking-at "^\\s-*#\\+INCLUDE:")
    (forward-line 1))
  (point))

(defun org-block-hover--get-content-end (quote-end)
  "Get content end position before #+end_quote line."
  (goto-char quote-end)
  (forward-line -1)
  (end-of-line)
  (point))

(defun org-block-hover--extract-from-quote (quote-bounds)
  "Extract reference text from QUOTE-BOUNDS."
  (let* ((quote-start (car quote-bounds))
         (quote-end (cdr quote-bounds))
         (content-start (org-block-hover--get-content-start quote-start))
         (content-end (org-block-hover--get-content-end quote-end)))
    (org-block-hover--extract-reference-text content-start content-end)))

(defun org-block-hover--handle-unpara-include (file-path reference-text quote-bounds)
  "Handle unparameterized INCLUDE with FILE-PATH, REFERENCE-TEXT, and QUOTE-BOUNDS."
  (let* ((has-user-text (and reference-text (> (length reference-text) 0)))
         (location (when has-user-text
                     (org-block-hover--locate-reference file-path reference-text)))
         (context-content (when (and location (car location))
                            (org-block-hover--get-context file-path (car location))))
         (content-to-show (cond
                           (context-content context-content)
                           (has-user-text reference-text)
                           (t (org-block-hover--extract-full-file file-path)))))
    ;; Handle insertion based on user text and auto-insert setting
    (cond
     ;; Has user text: never insert
     (has-user-text nil)
     ;; No user text + auto-insert enabled: auto-insert
     (org-block-hover-auto-insert
      (org-block-hover--replace-quote-content (car quote-bounds) (cdr quote-bounds) content-to-show))
     ;; No user text + auto-insert disabled: ask user
     (t
      (if (y-or-n-p (format "Insert content from '%s'?" (file-name-nondirectory file-path)))
          (org-block-hover--replace-quote-content (car quote-bounds) (cdr quote-bounds) content-to-show)
        (message "Insertion cancelled."))))))

(defun org-block-hover--handle-para-include (file-path include-info reference-text quote-bounds)
  "Handle parameterized INCLUDE with FILE-PATH, INCLUDE-INFO, REFERENCE-TEXT, and QUOTE-BOUNDS."
  (let* ((include-type (alist-get 'type include-info))
         (content (org-block-hover--extract-content
                   file-path include-type (alist-get 'params include-info)))
         (location (org-block-hover--locate-reference file-path reference-text)))
    ;; Handle insertion based on auto-insert setting (parameterized INCLUDE never has user text)
    (if org-block-hover-auto-insert
        ;; Auto-insert enabled: insert directly
        (org-block-hover--replace-quote-content (car quote-bounds) (cdr quote-bounds) content)
      ;; Auto-insert disabled: ask user
      (if (y-or-n-p (format "Insert content from '%s'?" (file-name-nondirectory file-path)))
          (org-block-hover--replace-quote-content (car quote-bounds) (cdr quote-bounds) content)
        (message "Insertion cancelled.")))))

(defun org-block-hover--get-formatted-content (include-type file-path include-info reference-text)
  "Get formatted content for popup based on INCLUDE-TYPE, FILE-PATH, INCLUDE-INFO, and REFERENCE-TEXT."
  (if (eq include-type 'origin)
      ;; Simple INCLUDE content
      (let* ((has-user-text (and reference-text (> (length reference-text) 0)))
             (location (when has-user-text
                         (org-block-hover--locate-reference file-path reference-text)))
             (context-content (when (and location (car location))
                                (org-block-hover--get-context file-path (car location))))
             (content-to-show (cond
                               (context-content context-content)
                               (has-user-text reference-text)
                               (t (org-block-hover--extract-full-file file-path)))))
        (org-block-hover--format-popup-content content-to-show file-path location))
    ;; Parameterized INCLUDE content
    (let* ((content (org-block-hover--extract-content
                     file-path include-type (alist-get 'params include-info)))
           (location (org-block-hover--locate-reference file-path reference-text)))
      (org-block-hover--format-popup-content content file-path location))))

(defun org-block-hover-trigger ()
  "Main trigger function for org block hover functionality."
  (interactive)
  (when org-block-hover-enable
    (let ((include-info (org-block-hover--detect-include-block)))
      (if include-info
          ;; Automatically replace content and show popup
          (org-block-hover--replace-and-show)
        (message "No INCLUDE block found at point")))))

(defun org-block-hover-preview-only ()
  "Preview INCLUDE block content without replacing it."
  (interactive)
  (let ((include-info (org-block-hover--detect-include-block)))
    (when include-info
      ;; Extract reference text in a save-excursion context
      (let ((reference-info
             (save-excursion
               ;; Find the quote block boundaries for extracting reference text
               (let ((quote-start (re-search-backward "^#\\+begin_quote" nil t))
                     (quote-end (re-search-forward "^#\\+end_quote" nil t)))
                 (when (and quote-start quote-end)
                   (org-block-hover--extract-from-quote (cons quote-start quote-end)))))))
        ;; Show popup without save-excursion to preserve original context for cleanup timer
        (org-block-hover-show-popup include-info reference-info)))))

(provide 'org-block-hover)
;;; org-block-hover.el ends here
