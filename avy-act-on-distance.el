;;; avy-act-on-distance.el --- Commands that let avy act from a distance  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Package-Requires: ((avy 20230420.404) (back-button 20220827.1733))
;; Version: 1.0
;; Keywords: tools, convenience
;; URL: https://github.com/nameiwillforget/Avy-act-on-Distance

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These commands let Avy act from a distance. There are four basic classes:

;; The commands avy-follow and avy-follow-in-new-buffer, which allow you to select a link to follow.

;; The commands avy-recenter-middle-at-line, avy-recenter-top-at-line and avy-recenter-bottom-at-line, which allow recentering at a line.

;; Extended functions, which allow you to do simple editing from a distance. These can be subdivided into four sub-classes:

;; The commands avy-act-on-position and avy-act-on-position-word-1, which act by selecting a position in the buffer through avy, then apply a marking command like er/mark-word or mark-sexp, then apply a command to the marked region.

;; The commands avy-act-on-region and avy-act-on-region-by-same-function, which act by marking a region through either two avy functions or the same function applied twice, then act on that region by applying a command.

;; The commands avy-act-to-point and avy-act-to-point-in-same-line, which mark a region up to the current position of point using one avy command, then apply a command to that region.

;; The commands that are applied in extended functions can be chosen through keyboard shortcuts as they are in the current buffer, or through the avy-selection-command-map. When using avy-act-on-position or avy-act-on-position-word-1, often either a whitespace is missing or one too much remains is at the position where the action was performed. For this, another command can be applied to that position from the avy-post-action-map, which by default contains commands to insert a whitespace or delete a character.

;; The default keys are chosen to be in line with the Daselt layout for which it was developed (https://gitlab.com/nameiwillforget/daselt). If you find them unfitting, remap them.

;;; Code:
(require 'avy)
(require 'back-button)

;; Keymaps
(defvar-keymap avy-function-map
  :doc "Map of functions for choosing avy-functions in extended avy-functions like avy-select-by-same-function-and-apply and its variants."
  "a" #'avy-goto-char-timer
  "," #'avy-goto-char-after-timer
  
  "t" #'avy-goto-word-1
  "c" #'avy-goto-char-after-word-1

  "e" #'avy-goto-char-in-line
  
  "r" #'avy-goto-line
  "l" #'avy-goto-end-of-line)


(defvar-keymap avy-selection-command-map
  :doc "Map of commands that act on a selection and are chosen by typing a letter in extended avy functions like act-by-same-function and its variants."
  "," #'backward-delete-char-untabify
  "w" #'kill-ring-save

  "i" #'overwrite

  "y" #'yank)


(defvar-keymap avy-position-selection-map
  :doc "Map of commands that act on a selection and are chosen by typing a letter in extended avy functions like act-by-same-function and its variants."
  "a" #'er/mark-word
  "t" #'mark-sexp

  "e" #'er/mark-sentence
  "r" #'mark-line
  
  "q" #'mark-paragraph)


(defvar-keymap avy-post-action-map
  :doc "Map of actions that can be taken post avy action commands like avy-act-by-same-function and avy-act-on-position."
  "C-," #'avy-act-delete-char-from-distance
  "C-<SPC>" #'avy-act-insert-space-from-distance)


;; Commands for marking
(defun avy-act-mark-nothing ()
  "This command does nothing."
  (interactive)
  (call-interactively #'set-mark-command))

(defun avy-act-mark-character ()
  "This command marks a character."
  (interactive)
  (call-interactively #'set-mark-command)
  (forward-char))



;; Action commands
(defun overwrite (string)
  "This command overwrites the marked region."
  (interactive "sOverwrite with: ")
  (call-interactively #'backward-delete-char-untabify)
  (insert string)
  ;; (back-button-local-backward)
  )


;; Post action commands
(defun avy-act-delete-char-from-distance ()
  "This command deletes a character at the position that was acted on."
  (interactive)
  (back-button-local-forward)
  (call-interactively #'delete-char)
  (back-button-local-backward))

(defun avy-act-insert-space-from-distance ()
  "This command inserts a space at the position that was acted on."
  (interactive)
  (back-button-local-forward)
  (insert '" ")
  (back-button-local-backward))


;; Commands opening links
(defun avy-act--follow-or-open ()
  "This command follows or opens a link at point."
  (interactive)
  (cond ((derived-mode-p 'Info-mode)
         ((derived-mode-p 'eww-mode)
          (goto-char pos)
          (goto-char pos)
          (call-interactively #'push-button))
         (t (org-open-at-point-global)))))

(defun avy-act--follow-or-open-new-buffer ()
  "This command follows or opens a link at point."
  (interactive)
  (cond ((derived-mode-p 'Info-mode)
         (call-interactively #'Info-follow-nearest-node t))
        ((derived-mode-p 'eww-mode)
         (call-interactively #'eww-follow-link))
        (t (org-open-at-point-global))))

(defun avy-follow ()
  "This command jumps to and opens a link using avy-follow-word-1."
  (interactive)
  (call-interactively #'avy-goto-word-1)
  (call-interactively #'avy-act--follow-or-open))

(defun avy-follow-in-new-buffer ()
  "This command jumps to and opens a link in a new buffer using avy-follow-word-1."
  (interactive)
  (call-interactively #'avy-goto-word-1)
  (call-interactively #'avy-act--follow-or-open))


;; Commands for recentering
(defun avy-recenter-middle-at-line ()
  "This function calls avy-goto-line and recenters the buffer with that line in the middle. If its starting position is still on the screen, it returns to it."
  (interactive)
  (let ((pos (point)) (midtoend (/ (- (window-end) (window-start)) 2)))
    (call-interactively #'avy-goto-line)
    (call-interactively #'recenter-top-bottom)
    (if (and (> pos (- (point) midtoend)) (< pos (+ (point) midtoend)))
        (goto-char pos))))

(defun avy-recenter-top-at-line ()
  "This function calls avy-goto-line, recenters the buffer with that line at the top. 
If its starting position is still on the screen, it returns to it."
  (interactive)
  (let ((pos (point)))
    (call-interactively #'avy-goto-line)
    (recenter-top-bottom 1)
    (if (> pos (point))
        (goto-char pos))))

(defun avy-recenter-bottom-at-line ()
  "This function calls avy-goto-line, recenters the buffer with that line at the bottom
If its starting position is still on the screen, it returns to it."
  (interactive)
  (let ((pos (point)))
    (call-interactively #'avy-goto-line)
    (recenter-top-bottom -1)
    (if (< pos (point))
        (goto-char pos))))


;; Extended commands
(defun avy-act-on-region-by-same-function (x y)
  "This command asks for a command and an avy function as chosen
using the avy-function-map, then uses the chosen avy function twice to
select an interval and apply the apply the input command to it. Input
commands are chosen through typing a key combination that is either in
the currently active maps or in the avy-selection-command-map, which
overrides the others. Through this method, simple editing of areas on
the screen can be done without having to move point. If you delete an
area surrounding point, have the first selected position be before the
second one."
  (interactive "kCommand: \nkAvy function: ")
  (let ((markon (bound-and-true-p visible-mark-mode)))
    (call-interactively (lookup-key avy-function-map y))
    (set-mark-command nil)
    (call-interactively (lookup-key avy-function-map y))
    (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t))) x))
    (deactivate-mark)
    (back-button-local-backward)
    (unless markon (visible-mark-mode 0))))

(defun avy-act-on-region (x y z)
  "This command asks for a command and two avy functions as chosen
using the avy-function-map, then uses the chosen avy functions to
select an interval and apply the apply the input command to it. Input
commands are chosen through typing a key combination that is either in
the currently active maps or in the avy-selection-command-map, which
overrides the others. Through this method, simple editing of areas on
the screen can be done without having to move point. If you delete an
area surrounding point, have the first selected position be before the
second one."
  (interactive "kCommand: \nkFirst avy function: \nkSecond avy function: ")
  (let ((markon (bound-and-true-p visible-mark-mode)))
    (call-interactively (lookup-key avy-function-map y))
    (set-mark-command nil)
    (call-interactively (lookup-key avy-function-map z))
    (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t))) x))
    (deactivate-mark)
    (back-button-local-backward)
    (unless markon (visible-mark-mode 0))))

(defun avy-act-to-point (x y)
  "This command asks for a command and an avy function as chosen
using the avy-function-map, then uses the chosen avy function to
select the interval between the current position of point and the
position chosen through avy, and then apply the apply the input
command to it. Input commands are chosen through typing a key
combination that is either in the currently active maps or in the
avy-selection-command-map, which overrides the others. Through this
method, simple editing of areas before or after point can be done
without having to move point."
  (interactive "kCommand: \nkAvy function: ")
  (let ((markon (bound-and-true-p visible-mark-mode)))
    (set-mark-command nil)
    (call-interactively (lookup-key avy-function-map y))
    (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t))) x))
    (exchange-point-and-mark)
    (unless markon (visible-mark-mode 0))))

(defun avy-act-to-point-in-same-line (x)
  "This command asks for a command and an avy function as chosen
using the avy-function-map, then uses the chosen avy function to
select the interval between the current position of point and the
position chosen through avy, and then apply the apply the input
command to it. Input commands are chosen through typing a key
combination that is either in the currently active maps or in the
avy-selection-command-map, which overrides the others. Through this
method, simple editing of areas before or after point can be done
without having to move point."
  (interactive "kCommand: ")
  (let ((markon (bound-and-true-p visible-mark-mode)))
    (set-mark-command nil)
    (call-interactively #'avy-goto-char-in-line)
    (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t)))  x))
    (exchange-point-and-mark)
    (unless markon (visible-mark-mode 0))))

(defun avy-act-on-position (cmd func size)
  "This command asks for a command and an avy function as chosen
using the avy-function-map, then uses the chosen avy function to
choose a position, and then apply the input command to it. Input
commands are chosen through typing a key combination that is either in
the currently active maps or in the avy-position-command-map, which
overrides the others. Through this method, simple editing of areas
before or after point can be done without having to move point."
  (interactive "kCommand: \nkAvy function: \nkSelection size: ")
  (call-interactively (lookup-key avy-function-map func))
  (push-mark)
  (deactivate-mark)
  (call-interactively (lookup-key avy-position-selection-map size))
  (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t)))  cmd))
  (deactivate-mark)
  (back-button-local-backward)
  (set-transient-map avy-post-action-map)
  (visible-mark-mode 0))

(defun avy-act-on-position-word-1 (cmd size)
  "This command asks for a command and an avy function as chosen
using the avy-function-map, then uses the chosen avy function to
choose a position, and then apply the input command to it. Input
commands are chosen through typing a key combination that is either in
the currently active maps or in the avy-position-command-map, which
overrides the others. Through this method, simple editing of areas
before or after point can be done without having to move point."
  (interactive "kCommand: \nkSelection size: ")
  (call-interactively #'avy-goto-word-1)
  (push-mark)
  (deactivate-mark)
  (call-interactively (lookup-key avy-position-selection-map size))
  (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t))) cmd))
  (deactivate-mark)
  (back-button-local-backward)
  (set-transient-map avy-post-action-map)
  (visible-mark-mode 0))

(provide 'avy-act-on-distance)
;;; avy-act-on-distance.el ends here
