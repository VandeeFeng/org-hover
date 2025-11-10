;;; org-hover-ui.el --- UI library for org-hover popups (child-frames) -*- lexical-binding: t; -*-

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
;; This library provides UI functions for org-hover to create and manage popups
;; (child-frames) in Emacs.
;;
;;; Code:

;;; Variables

(defvar org-hover-ui-popup--frame nil
  "The child frame used for displaying popups.")

(defvar org-hover-ui-popup--buffer "*org-hover-ui-popup*"
  "The buffer used for displaying popup content.")

(defvar org-hover-ui-popup--position-function #'org-hover-ui-popup--default-at-point-position-function
  "This function is used to set the childframe's position.")

(defvar org-hover-ui--cleanup-timer nil
  "Timer for auto-cleanup of popup window.")

(defvar org-hover-ui--last-point nil
  "Last cursor position when popup was shown.")

(defvar org-hover-ui--last-buffer nil
  "Last buffer when popup was shown.")


;;; Customization

(defgroup org-hover-ui nil
  "UI components for org-hover."
  :group 'applications)

(defgroup org-hover-ui-popup nil
  "Settings for the org-hover UI popup frame."
  :group 'org-hover-ui
  :prefix "org-hover-ui-popup-")

(defcustom org-hover-ui-popup-max-pixel-width 800
  "Maximum width of popup frame in pixels."
  :type 'number
  :group 'org-hover-ui-popup)

(defcustom org-hover-ui-popup-max-pixel-height 700
  "Maximum height of popup frame in pixels."
  :type 'number
  :group 'org-hover-ui-popup)

(defcustom org-hover-ui-enable-auto-cleanup t
  "Enable auto-cleanup of popup window when user interacts elsewhere."
  :type 'boolean
  :group 'org-hover-ui-popup)

(defcustom org-hover-ui-cleanup-interval 0.2
  "Interval in seconds for checking auto-cleanup conditions."
  :type 'number
  :group 'org-hover-ui-popup)

(defvar org-hover-ui-popup-frame-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (desktop-dont-save . t))
  "Default frame parameters for the popup frame.")


;;; Popup Mode

(define-derived-mode org-hover-ui-popup-mode fundamental-mode "OrgHoverUIPopup"
  "Major mode for org-hover UI popup display with no mode line."
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local tab-line-format nil)
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (visual-line-mode 1))


;;; Popup Internals

(defun org-hover-ui--maybe-cleanup ()
  "Check if popup should be hidden due to user interaction elsewhere."
  (when (and org-hover-ui-enable-auto-cleanup
             org-hover-ui-popup--frame
             (frame-live-p org-hover-ui-popup--frame)
             (frame-visible-p org-hover-ui-popup--frame))
    (let ((current-buffer (current-buffer))
          (current-point (point))
          (current-frame (selected-frame))
          (mouse-pos (mouse-position)))
      (unless (or (eq current-frame org-hover-ui-popup--frame)
                  (and (car mouse-pos)
                       (eq (car mouse-pos) org-hover-ui-popup--frame)))
        (when (or (not (eq current-buffer org-hover-ui--last-buffer))
                  (not (eq current-point org-hover-ui--last-point)))
          (org-hover-ui-popup-hide))))))

(defun org-hover-ui--start-cleanup-timer ()
  "Start the cleanup timer when popup is shown."
  (org-hover-ui--cancel-cleanup-timer)
  (setq org-hover-ui--last-point (point))
  (setq org-hover-ui--last-buffer (current-buffer))
  (when org-hover-ui-enable-auto-cleanup
    (setq org-hover-ui--cleanup-timer
          (run-with-timer org-hover-ui-cleanup-interval
                         org-hover-ui-cleanup-interval
                         #'org-hover-ui--maybe-cleanup))))

(defun org-hover-ui--cancel-cleanup-timer ()
  "Cancel the cleanup timer when popup is hidden."
  (when org-hover-ui--cleanup-timer
    (cancel-timer org-hover-ui--cleanup-timer)
    (setq org-hover-ui--cleanup-timer nil)))

(defun org-hover-ui-popup--point-position-relative-to-native-frame (&optional point window)
  "Return (X . Y) as the coordinate of POINT in WINDOW relative to the native frame."
  (unless window (setq window (selected-window)))
  (unless point (setq point (window-point window)))
  (let* ((pos (pos-visible-in-window-p point window t))
         (x (car pos))
         (en (frame-char-width))
         (y (cadr pos))
         (edges (window-edges window nil nil t)))
    (cons (+ x (car edges) en)
          (+ y (cadr edges)))))

(defun org-hover-ui-popup--default-at-point-position-function (width height)
  "Default position function to place popup under point."
  (let* ((point-pos (org-hover-ui-popup--point-position-relative-to-native-frame))
         (x (car point-pos))
         (y (cdr point-pos))
         (em (frame-char-height)))
    (cons (if (< (- (frame-inner-width) width) x)
              (max 0 (- (frame-inner-width) width 16))
            x)
          (if (< (- (frame-inner-height) height) y)
              (max 0 (- y height))
            (+ y em)))))

(defun org-hover-ui-popup--update-childframe-geometry (frame window)
  "Update the size and the position of childframe FRAME."
  (let* ((parent-frame (frame-parent frame))
         (size
          (window-text-pixel-size
           window nil nil
           (if (functionp org-hover-ui-popup-max-pixel-width)
               (funcall org-hover-ui-popup-max-pixel-width)
             org-hover-ui-popup-max-pixel-width)
           (if (functionp org-hover-ui-popup-max-pixel-height)
               (funcall org-hover-ui-popup-max-pixel-height)
             org-hover-ui-popup-max-pixel-height)
           t))
         (width (car size))
         (height (cdr size))
         (width (+ width (frame-char-width frame))) ; add margin
         (width (if (eq (window-system) 'ns)
                    width
                  (min width (- (frame-pixel-width parent-frame) 32))))
         (height (if (eq (window-system) 'ns)
                     height
                   (min height (- (frame-pixel-height parent-frame) 32))))
         (frame-resize-pixelwise t)
         (pos (funcall org-hover-ui-popup--position-function width height)))
    (set-frame-size frame width height t)
    (set-frame-position frame (car pos) (cdr pos))))

(defun org-hover-ui-popup--get-frame (buffer)
  "Return a childframe displaying BUFFER."
  (let* ((parameter (append org-hover-ui-popup-frame-parameters
                            `((parent-frame . ,(selected-frame))
                              (minibuffer . ,(minibuffer-window)))))
         window frame)
    (if (and org-hover-ui-popup--frame (frame-live-p org-hover-ui-popup--frame))
        (progn
          (setq frame org-hover-ui-popup--frame)
          (setq window (frame-selected-window frame))
          (set-frame-parameter frame 'parent-frame (selected-frame)))
      (setq window (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,parameter))))
      (setq frame (window-frame window)))

    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (let ((border-color "gray"))
      (set-face-attribute 'internal-border frame :background border-color)
      (when (facep 'child-frame-border)
        (set-face-background 'child-frame-border border-color frame)))

    (org-hover-ui-popup--update-childframe-geometry frame window)
    (setq org-hover-ui-popup--frame frame)
    frame))

;;; Popup Public API

(defun org-hover-ui-popup-show (text)
  "Display TEXT in a popup near cursor."
  (interactive)
  (condition-case err
      (if (not (display-graphic-p))
          (message "%s" text)
        (let* ((doc-buffer (get-buffer-create org-hover-ui-popup--buffer))
               (frame (org-hover-ui-popup--get-frame doc-buffer)))
          (with-current-buffer doc-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert text)
              (goto-char (point-min))
              (org-hover-ui-popup-mode)))
          (let ((window (frame-selected-window frame)))
            (org-hover-ui-popup--update-childframe-geometry frame window))
          (make-frame-visible frame)
          (select-frame-set-input-focus (selected-frame) 'norecord)
          (org-hover-ui--start-cleanup-timer)
          frame))
    (error
     (message "Org-hover UI popup error: %s" (error-message-string err))
     nil)))

(defun org-hover-ui-popup-hide ()
  "Hide the popup frame."
  (interactive)
  (org-hover-ui--cancel-cleanup-timer)
  (when (and org-hover-ui-popup--frame (frame-live-p org-hover-ui-popup--frame))
    (make-frame-invisible org-hover-ui-popup--frame)))


(provide 'org-hover-ui)
;;; org-hover-ui.el ends here
