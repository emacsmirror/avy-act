;;; avy-act-on-distance.el --- Commands that let avy act from a distance  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Keywords: tools, convenience

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

;; These commands let Avy act from a distance.
;;; Code:

(require 'avy)
(require 'back-button)

(defun avy-follow-url ()
  "This function uses avy to jump to an url, then follow that url."
  (interactive)
  (call-interactively #'avy-goto-char-timer)
  (call-interactively #'eww-follow-link))

(defun avy-follow-url-in-separate-tab ()
  "This function uses avy to jump to an url, then follow that url."
  (interactive)
  (call-interactively #'avy-goto-char-timer)
  (call-interactively #'eww-open-in-new-buffer))

(defun avy-word-follow-url ()
  "This function uses avy to jump to an url, then follow that url."
  (interactive)
  (call-interactively #'avy-goto-word-1)
  (call-interactively #'eww-follow-link))

(defun avy-word-follow-url-in-separate-tab ()
  "This function uses avy to jump to an url, then follow that url."
  (interactive)
  (call-interactively #'avy-goto-word-1)
  (call-interactively #'eww-open-in-new-buffer))

(defun avy-follow-url-within-same-line ()
  "This function uses avy to jump to an url, then follow that url."
  (interactive)
  (call-interactively #'avy-goto-char-in-line)
  (call-interactively #'eww-follow-link))

(defun avy-follow-url-within-same-line-in-separate-tab ()
  "This function uses avy to jump to an url, then follow that url."
  (interactive)
  (call-interactively #'avy-goto-char-in-line)
  (call-interactively #'eww-open-in-new-buffer))


(defun avy-goto-char-after-timer ()
  "This command goes to a position indicated by avy-goto-char-timer, then goes one character forward. This is useful when avy is used to, for instance, delete a letter in a word, so that one doesn't have to think about targeting the letter after to arrive at the right position."
  (interactive)
  (call-interactively #'avy-goto-char-timer)
  (forward-char))


(defun avy-goto-char-after-word-1 ()
  "This command goes to a position indicated by avy-goto-word-1, then goes one character forward. This is useful when avy is used to, for instance, delete a letter in a word, so that one doesn't have to think about targeting the letter after to arrive at the right position."
  (interactive)
  (call-interactively #'avy-goto-word-1)
  (forward-char))


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

  "y" #'yank
  )


(defvar-keymap avy-position-selection-map
  :doc "Map of commands that act on a selection and are chosen by typing a letter in extended avy functions like act-by-same-function and its variants."
  "a" #'er/mark-word
  "t" #'mark-sexp

  "e" #'er/mark-sentence
  "r" #'mark-line
  
  "q" #'mark-paragraph
  "v" #'mark-defun

  "<SPC>" #'forward-char)

;; (defun avy-act-mark-character ()
;;   "This command marks a character."
;;   (interactive)
;;   (call-interactively #'set-mark-command)
;;   (forward-char))


(defvar-keymap avy-post-action-map
  :doc "Map of actions that can be taken post avy action commands like avy-act-by-same-function and avy-act-on-position."
  "C-," #'avy-act-delete-char-from-distance
  "C-<SPC>" #'avy-act-insert-space-from-distance)

;; Overwrite
(defun overwrite (string)
  "This command overwrites the marked region."
  (interactive "sOverwrite with: ")
  (call-interactively #'backward-delete-char-untabify)
  (insert string)
  (back-button-local-backward))


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


;; Extended functions
(defun avy-act-by-same-function (x y)
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
  (call-interactively (lookup-key avy-function-map y))
  (call-interactively #'set-mark-command)
  (call-interactively (lookup-key avy-function-map y))
  (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t))) x))
  (call-interactively #'back-button-local-backward)
  (call-interactively #'back-button-local-backward))


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
  (call-interactively (lookup-key avy-function-map y))
  (call-interactively #'set-mark-command)
  (call-interactively (lookup-key avy-function-map z))
  (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t))) x))
  (call-interactively #'back-button-local-backward)
  (call-interactively #'back-button-local-backward))


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
  (call-interactively #'set-mark-command)
  (call-interactively (lookup-key avy-function-map y))
  (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t))) x))
  (call-interactively #'back-button-local-backward))


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
  (call-interactively #'set-mark-command)
  (call-interactively #'avy-goto-char-in-line)
  (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t)))  x))
  (call-interactively #'back-button-local-backward))


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
  (call-interactively (lookup-key avy-position-selection-map size))
  (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t)))  cmd))
  (defvar pos (point))
  (call-interactively #'back-button-local-backward)
  (set-transient-map avy-post-action-map))


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
  (push-mark)
  (call-interactively (lookup-key avy-position-selection-map size))
  (call-interactively (lookup-key (make-composed-keymap avy-selection-command-map (make-composed-keymap (current-active-maps t))) cmd))
  (back-button-local-backward)
  (set-transient-map avy-post-action-map)
  (visible-mark-mode 0))


(defun avy-recenter-middle-at-line ()
  "This function calls avy-goto-line and recenters the buffer with that line in the middle. If its starting position is still on the screen, it and returns to it."
  (interactive)
  (let ((pos (point)) (midtoend (/ (- (window-end) (window-start)) 2)))
    (call-interactively #'avy-goto-line)
    (call-interactively #'recenter-top-bottom)
    (if (and (> pos (- (point) midtoend)) (< pos (+ (point) midtoend)))
        (goto-char pos))))


(defun avy-recenter-top-at-line ()
  "This function calls avy-goto-line, recenters the buffer with that line at the top and returns to the starting position."
  (interactive)
  (let ((pos (point)))
    (call-interactively #'avy-goto-line)
    (recenter-top-bottom 1)
    (if (> pos (point))
        (goto-char pos))))


(defun avy-recenter-bottom-at-line ()
  "This function calls avy-goto-line, recenters the buffer with that line at the bottom and returns to the starting position."
  (interactive)
  (let ((pos (point)))
    (call-interactively #'avy-goto-line)
    (recenter-top-bottom -1)
    (if (< pos (point))
        (goto-char pos))))

(provide 'avy-act-on-distance)
;;; avy-act-on-distance.el ends here
