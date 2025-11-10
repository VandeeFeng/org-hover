;;; org-hover.el --- Hover preview for org files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Vandee

;; Author: Vandee
;; URL: https://github.com/VandeeFeng/org-hover
;; Package-Requires: ((emacs "27.1") (org "9.0"))
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
;; This package provides hover preview functionality for org files.
;; It allows you to preview org file links in a popup frame.
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'org-hover-ui)

;;; Variables

(defvar org-hover--current-link nil
  "Currently displayed org link.")

(defvar org-hover--auto-hide-timer nil
  "Timer for auto-hiding the popup.")

;;; Customization

(defgroup org-hover nil
  "Settings for org-hover package."
  :group 'org
  :prefix "org-hover-")

(defcustom org-hover-max-size-ratio 0.7
  "Maximum size ratio of hover window relative to current window."
  :type 'float
  :group 'org-hover)

(defcustom org-hover-content-limit 10000
  "Maximum character count for displayed content."
  :type 'integer
  :group 'org-hover)

(defcustom org-hover-auto-hide nil
  "Whether to automatically hide the popup after a timeout."
  :type 'boolean
  :group 'org-hover)

(defcustom org-hover-auto-hide-delay 5
  "Delay in seconds before automatically hiding the popup."
  :type 'number
  :group 'org-hover)

;;; Link Detection Functions

(defun org-hover--link-at-point ()
  "Return org file link at point, or nil if none found."
  (let ((context (org-element-context)))
    (when context
      (if (eq (org-element-type context) 'link)
          (let ((link-type (org-element-property :type context))
                (link-path (org-element-property :path context)))
            (when (string= link-type "file")
              context))
        ;; If not directly on link, try to find link at current line
        (save-excursion
          (beginning-of-line)
          (let ((line-end (line-end-position)))
            (while (and (< (point) line-end)
                        (not (or (looking-at-p "\\[\\[.*\\]\\[.*\\]\\]")
                                 (looking-at-p "\\[\\[.*\\]\\]"))))
              (forward-char))
            (when (and (< (point) line-end)
                       (or (looking-at "\\[\\[\\(file:\\)?\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]")
                           (looking-at "\\[\\[\\(file:\\)?\\([^]]+\\)\\]\\]")))
              (let ((path (match-string 2)))
                ;; Return a plist directly instead of trying to mock org-element
                (list :type 'file
                      :path (expand-file-name path)
                      :raw-link (match-string 0))))))))))

(defun org-hover--extract-link-path (link)
  "Extract file path from org LINK element."
  (when link
    (let ((path (if (eq (car link) 'link)
                    (org-element-property :path link)
                  (plist-get link :path))))
      (when path
        (expand-file-name path)))))

(defun org-hover--validate-link (path)
  "Validate that PATH points to an existing file."
  (let ((expanded-path (expand-file-name path)))
    (and expanded-path
         (file-exists-p expanded-path))))

;;; Content Extraction Functions

(defun org-hover--extract-content (file-path)
  "Extract content from org FILE-PATH."
  (when (org-hover--validate-link file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (org-mode)
      (goto-char (point-min))
      (let ((content (buffer-string)))
        (if (> (length content) org-hover-content-limit)
            (substring content 0 org-hover-content-limit)
          content)))))

(defun org-hover--format-content (content)
  "Format CONTENT for display in popup."
  (propertize content 'face 'default))

;;; Display Functions

(defun org-hover--calculate-size (content)
  "Calculate appropriate size for displaying CONTENT."
  (let* ((lines (split-string content "\n"))
         (line-count (length lines))
         (max-line-length (apply #'max (mapcar #'length lines)))
         (char-width (frame-char-width))
         (char-height (frame-char-height))
         (frame-width (frame-width))
         (frame-height (frame-height))
         (max-width (round (* frame-width org-hover-max-size-ratio)))
         (max-height (round (* frame-height org-hover-max-size-ratio)))
         (calculated-width (min max-width (max 30 (+ max-line-length 4))))
         (calculated-height (min max-height (max 10 (+ line-count 2)))))
    (cons (* calculated-width char-width) (* calculated-height char-height))))

(defun org-hover--smart-position-function (width height)
  "Smart position function that considers content size and screen boundaries."
  (let* ((point-pos (org-hover-ui-popup--point-position-relative-to-native-frame))
         (x (car point-pos))
         (y (cdr point-pos))
         (frame-inner-width (frame-inner-width))
         (frame-inner-height (frame-inner-height))
         (em (frame-char-height)))
    (cons (if (< (- frame-inner-width width) x)
              (max 0 (- frame-inner-width width 16))
            x)
          (if (< (- frame-inner-height height) y)
              (max 0 (- y height))
            (+ y em)))))

(defun org-hover--get-popup-frame ()
  "Get the current popup frame if it exists."
  (when (boundp 'org-hover-ui-popup--frame)
    (and (framep org-hover-ui-popup--frame) (frame-live-p org-hover-ui-popup--frame) org-hover-ui-popup--frame)))

;;; Auto-hide Functions

(defun org-hover--setup-auto-hide ()
  "Setup auto-hide timer for the popup."
  (when org-hover--auto-hide-timer
    (cancel-timer org-hover--auto-hide-timer)
    (setq org-hover--auto-hide-timer nil))
  (when org-hover-auto-hide
    (setq org-hover--auto-hide-timer
          (run-with-timer org-hover-auto-hide-delay nil #'org-hover-hide))))

(defun org-hover--cancel-auto-hide ()
  "Cancel the auto-hide timer."
  (when org-hover--auto-hide-timer
    (cancel-timer org-hover--auto-hide-timer)
    (setq org-hover--auto-hide-timer nil)))

(defun org-hover--show-popup (content)
  "Display CONTENT in a popup frame with smart sizing."
  (let* ((formatted-content (org-hover--format-content content))
         (size (org-hover--calculate-size formatted-content))
         (org-hover-ui-popup-max-pixel-width (car size))
         (org-hover-ui-popup-max-pixel-height (cdr size))
         (org-hover-ui-popup--position-function #'org-hover--smart-position-function)
         (frame (org-hover-ui-popup-show formatted-content)))
    (when frame
      (org-hover--setup-auto-hide))
    frame))

;;; Main Interactive Functions

(defun org-hover--follow-link ()
  "Follow link at point in the popup buffer."
  (interactive)
  (let ((link (org-element-context)))
    (when (eq (org-element-type link) 'link)
      (let ((path (org-element-property :path link)))
        (when (file-exists-p path)
          (org-hover-hide)
          (find-file-other-window path))))))

(defun org-hover-at-point ()
  "Preview org file link at point."
  (interactive)
  (let ((link (org-hover--link-at-point)))
    (if (not link)
        (message "No org file link found at point")
      (let ((file-path (org-hover--extract-link-path link)))
        (if (not file-path)
            (message "Could not extract file path from link")
          (if (org-hover--validate-link file-path)
              (let* ((content (org-hover--extract-content file-path))
                     (frame (org-hover--show-popup content)))
                (setq org-hover--current-link file-path)
                frame)
            (message "Not a valid org file: %s" file-path)))))))

(defun org-hover-file (file)
  "Preview specified org FILE."
  (interactive "Hover org file: ")
  (if (org-hover--validate-link file)
      (let* ((content (org-hover--extract-content file))
             (frame (org-hover--show-popup content)))
        (setq org-hover--current-link file)
        frame)
    (message "Not a valid org file: %s" file)))

;;; Key Bindings

(define-key org-mode-map (kbd "C-c h") 'org-hover-at-point)
(global-set-key (kbd "C-c H f") 'org-hover-file)

(provide 'org-hover)
;;; org-hover.el ends here
