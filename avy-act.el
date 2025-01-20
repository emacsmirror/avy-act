;;; avy-act.el --- Commands that let avy act from a distance  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Package-Requires: ((avy "0.5.0") (emacs "29.1"))
;; Version: 1.0
;; Keywords: tools, convenience
;; URL: https://gitlab.com/nameiwillforget/avy-act

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

;; These commands let Avy act from a distance. There are three basic classes:

;; The commands `avy-act-follow' and `avy-act-follow-in-new-buffer', which allow
;; you to select a link to follow.

;; The commands `avy-recenter-middle-at-line', `avy-recenter-top-at-line' and
;; `avy-recenter-bottom-at-line', which allow recentering at a line.

;; Extended functions, which allow you to do simple editing from a distance.
;; These can be subdivided into three sub-classes:

;; The commands `avy-act-on-position' and `avy-act-on-position-word-1', which
;; act by selecting a position in the buffer through Avy, then apply a marking
;; command like `mark-word' or `mark-sexp', then apply a command to the marked
;; region.

;; The commands `avy-act-on-region' and `avy-act-on-region-by-same-function',
;; which act by marking a region through either two avy functions or the same
;; function applied twice, then act on that region by applying a command.

;; The commands `avy-act-to-point' and `avy-act-to-point-in-same-line', which
;; mark a region up to the current position of point using one avy command, then
;; apply a command to that region.

;; The commands that are applied in extended functions can be chosen through
;; keyboard shortcuts as they are in the current buffer, or through the
;; `avy-selection-command-map'. When using `avy-act-on-position' or
;; `avy-act-on-position-word-1', often either a whitespace is missing or one too
;; much remains is at the position where the action was performed. For this,
;; another command can be applied to that position from the
;; `avy-act-post-action-map', which by default contains commands to insert a
;; whitespace or delete a character.

;;; Code:
;;;; Preamble
(declare-function avy-act-functions-help "./avy-act.el" nil t)
(declare-function avy-act-selection-commands-help "./avy-act.el" nil t)
(declare-function avy-act-position-selection-help "./avy-act.el" nil t)
(declare-function help--help-screen "help-macro.el" (help-line help-text helped-map buffer-name) t)

(require 'avy)
(require 'dired)
(require 'eww)
(require 'info)
(require 'frame)
(require 'help-macro)

;;;; Keymaps
(eval-and-compile
  (defvar-keymap avy-act-function-map
    :doc "Map for choosing avy-functions in avy-act commands like
  avy-select-by-same-function-and-apply and its variants."
    "f" #'avy-goto-char-timer
    "j" #'avy-goto-word-1

    "d" #'avy-goto-char-in-line
    
    "g" #'avy-goto-line
    "h" #'avy-goto-end-of-line

    "?" #'avy-act-functions-help)

  (defvar-keymap avy-act-selection-command-map
    :doc "Map of commands that act on a selection and are chosen by typing a letter in
  extended avy functions like act-by-same-function and its variants."
    "e" #'delete-region
    "c" #'kill-ring-save

    "k" #'kill-region
    
    "i" #'avy-act-overwrite

    "y" #'yank

    "?" #'avy-act-selection-commands-help)


  (defvar-keymap avy-act-position-selection-map
    :doc "Map of commands that act on a selection and are chosen by typing a letter in
  extended avy functions like act-by-same-function and its variants."
    "f" #'mark-word
    "j" #'mark-sexp

    "d" #'mark-paragraph

    "v" #'avy-act-mark-nothing
    "n" #'avy-act-mark-character)

  (defvar-keymap avy-act-post-action-map
    :doc "Map of actions that can be taken post avy action commands like
  avy-act-by-same-function and avy-act-on-position."
    "C-<backspace>" #'avy-act-delete-char-from-distance
    "0" #'avy-act-insert-space-from-distance))

;;;; Customs
(defcustom avy-act-recenter-at-cur-line-keys
  (list (string-to-char (kbd "C-d")) (string-to-char (kbd "C-k")))
  "List of recenter key combinations for avy-act-recenter commands.
Typing one of these causes a command to recenter with the line point is on at
the top, middle or bottom."
  :type '(list character)
  :group 'convenience)


;;;; Variables
(defvar avy-act-frame)
(defvar avy-act-window)
(defvar avy-act-pos)

;;;; Help screens
;;;;; Help screen functions and macros
(eval-and-compile(defun avy-act--prepare-keymap-for-avy-act-help-screen (map)
                   "Prepare MAP for inclusion in an avy-act help-screen.
This means each command bound in MAP is converted into a function that throws an
exit-catch returning the command's name.
Might not work for all kinds of keymaps."
                   (mapcar (lambda (elt)
                                (if (consp elt)
                                      (if (keymapp elt)
                                          (avy-act--prepare-keymap-for-avy-act-help-screen elt)
                                      (cons (car elt) `(lambda () (interactive) (throw 'exit ',(cdr elt)))))
                                elt))
                            map)))

(defun avy-act-exit-function (fun)
  "Exit FUN with an exit catch around it."
  (catch 'exit (funcall fun)))

(defmacro avy-act-defun-name-help-screen-30 (fname help-line help-text helped-map
                                                   &optional buffer-name)
    "A variant of `make-help-screen'.
There is only difference to the original macro: instead of calling a selected
function it returns its name. See `make-help-screen' for an explanation of the
arguments."
    (declare (indent defun))
    `(defun ,fname ()
     "Help command."
     (interactive)
     (catch 'exit (help--help-screen ,help-line ,help-text
                                     (avy-act--prepare-keymap-for-avy-act-help-screen
                                      ,helped-map)
                                     ,buffer-name))))

(defmacro avy-act-defun-name-help-screen-29 (fname help-line help-text helped-map
                                                   &optional buffer-name)
    "A variant of `make-help-screen'.

There is only difference to the original macro: instead of calling a selected
function it returns its name. See `make-help-screen' for an explanation of the
arguments.

Note that, unlike with `avy-act-defun-make-help-screen-30', it is not possible
to choose commands that are not in the `avy-act-selection-command-map'."
    (declare (indent defun))
    `(progn
       (make-help-screen ,fname ,help-line ,help-text
         ',(avy-act--prepare-keymap-for-avy-act-help-screen
          (if (symbolp helped-map)
                  (symbol-value helped-map)
              helped-map))
         ,buffer-name)))

(defalias 'avy-act-defun-name-help-screen
  (if (< emacs-major-version 30)
      'avy-act-defun-name-help-screen-29
    'avy-act-defun-name-help-screen-30))

(defun avy-act--make-help-screens ()
              "Make help screens relevant to `avy-act'."
              (if (< emacs-major-version 30)
                              (progn (avy-act-defun-name-help-screen-29
               avy-act-functions-help "Function: (? for Help)"
               (format "%s" (substitute-command-keys "\\{avy-act-function-map}"))
               avy-act-function-map)

             (avy-act-defun-name-help-screen-29 avy-act-selection-commands-help "Function: (? for Help)"
               (format "%s"  (substitute-command-keys "\\{avy-act-selection-command-map}"))
               avy-act-selection-command-map)

             (avy-act-defun-name-help-screen-29 avy-act-position-selection-help "Function: (? for Help)"
               (format "%s"  (substitute-command-keys "\\{avy-act-position-selection-map}"))
               avy-act-position-selection-map))
    (avy-act-defun-name-help-screen-30
      avy-act-functions-help "Function: (? for Help)"
      (format "%s" (substitute-command-keys "\\{avy-act-function-map}"))
      avy-act-function-map)

    (avy-act-defun-name-help-screen-30 avy-act-selection-commands-help "Function: (? for Help)"
      (format "%s"  (substitute-command-keys "\\{avy-act-selection-command-map}"))
      (make-composed-keymap avy-act-selection-command-map (make-composed-keymap (current-active-maps t))))

    (avy-act-defun-name-help-screen-30 avy-act-position-selection-help "Function: (? for Help)"
      (format "%s"  (substitute-command-keys "\\{avy-act-position-selection-map}"))
      avy-act-position-selection-map))

              (advice-add 'avy-act-functions-help :around #'avy-act-exit-function)
              (advice-add 'avy-act-selection-commands-help :around #'avy-act-exit-function)
              (advice-add 'avy-act-position-selection-help :around #'avy-act-exit-function))

;;;; Commands
;;;;; Commands for marking
(defun avy-act-mark-nothing ()
  "This command does nothing."
  (interactive))

(defun avy-act-mark-character ()
  "This command marks a character."
  (interactive)
  (let ((pos (point)))
    (set-mark (1+ pos))))

;;;;; Action commands
(defun avy-act-overwrite (string)
  "This command overwrites the marked region with STRING."
  (interactive "sOverwrite with: ")
  (if (region-active-p)
      (delete-region (region-beginning) (region-end)))
  (insert string))

;;;;; Post action command generators

(defun avy-act-delete-char-from-distance (&optional arg)
  "This command deletes a character before the position that was acted on.
If a prefix ARG is given, delete the character after."
  (interactive "P")
  (let ((frame (selected-frame))
        (window (selected-window))
        (pos (point)))
    (select-frame avy-act-frame)
    (select-window avy-act-window)
    (goto-char avy-act-pos)
    (if arg
        (delete-char 1)
      (delete-char -1))
    (if (equal frame (selected-frame))
        (if (equal window (selected-window))
            (goto-char pos)
          (select-window window))
      (select-frame frame))))

(defun avy-act-insert-space-from-distance ()
  "This command inserts a space at the position that was acted on."
  (interactive)
  (let ((frame (selected-frame))
        (window (selected-window))
        (pos (point)))
    (select-frame avy-act-frame)
    (select-window avy-act-window)
    (goto-char avy-act-pos)
    (insert " ")
    (if (equal frame (selected-frame))
        (if (equal window (selected-window))
            (goto-char pos)
          (select-window window))
      (select-frame frame))))

;;;;; Commands opening links
(defun avy-act--follow-or-open ()
  "This command follows or opens a link at point."
  (interactive)
  (cond ((derived-mode-p 'Info-mode)
         (call-interactively #'Info-follow-nearest-node))
        ((derived-mode-p 'eww-mode)
         (call-interactively #'eww-follow-link))
        ((derived-mode-p 'help-mode)
         (call-interactively #'push-button))
        ((derived-mode-p 'dired-mode)
         (call-interactively #'dired-find-file))
        (t (org-open-at-point-global))))

(defun avy-act--follow-or-open-new-buffer ()
  "This command follows or opens a link at point."
  (interactive)
  (cond ((derived-mode-p 'Info-mode)
         (call-interactively #'Info-follow-nearest-node t))
        ((derived-mode-p 'eww-mode)
         (let ((eww-browse-url-new-window-is-tab nil))
           (call-interactively #'eww-open-in-new-buffer)))
        ((derived-mode-p 'dired-mode)
         (call-interactively #'dired-display-file))
        ((derived-mode-p 'help-mode)
         (clone-buffer)
         (call-interactively #'push-button))
        (t (org-open-at-point-global))))

(defun avy-act-follow ()
  "This command jumps to and opens a link using avy-act-follow-word-1."
  (interactive)
  (call-interactively #'avy-goto-word-1)
  (call-interactively #'avy-act--follow-or-open))

(defun avy-act-follow-in-new-tab ()
  "Jump to and open a link in a new tab using avy-act-follow-word-1."
  (interactive)
  (call-interactively #'tab-new)
  (call-interactively #'avy-goto-word-1)
  (call-interactively #'avy-act--follow-or-open-new-buffer))

(defun avy-act-follow-in-new-horizontal-window ()
  "Jump to and open a link in a new horizontal window using avy-act-follow-word-1."
  (interactive)
  (call-interactively #'split-window-below)
  (call-interactively #'avy-goto-word-1)
  (call-interactively #'avy-act--follow-or-open-new-buffer))

(defun avy-act-follow-in-new-vertical-window ()
  "Jump to and open a link in a new vertical window using avy-act-follow-word-1."
  (interactive)
  (call-interactively #'split-window-below)
  (call-interactively #'avy-goto-word-1)
  (call-interactively #'avy-act--follow-or-open-new-buffer))


;;;;; Commands for recentering
(defun avy-act--recenter (calcfun &optional num arg)
  "Helper function for avy-act-recenter-commands.

NUM is the number given to `recenter.' If left empty, `recenter' is
called without an argument and thus recenters at the middle.

CALCFUN is used to calculate when point can be moved back to its
original position (because it's still on the screen).

ARG works like for `avy-goto-line'.

If a member of AVY-ACT-RECENTER-AT-CUR-LINE-KEYS is entered during the
avy selection, just recenter at current line."
  (let ((pos (point))
        (window (selected-window))
        (frame (selected-frame))
        (midtoend (/ (- (window-end) (window-start)) 2)))
    (setq arg (or arg 1))
    (if (not (memq arg '(1 4)))
        (progn
          (goto-char (point-min))
          (forward-line (1- arg)))
      (avy-with avy-goto-line
        (let* ((avy-handler-old avy-handler-function)
               (avy-handler-function
                (lambda (char)
                  (if (or (cl-member char avy-act-recenter-at-cur-line-keys)
                          ;; To accomodate function keys.
                          (cl-member (vector char) avy-act-recenter-at-cur-line-keys
                                     :test #'equal))
                      (progn (if num
                                 (recenter-top-bottom num)
                               (recenter))
                             (throw 'done 'exit))
                    (if (or (< char ?0)
                            (> char ?9))
                        (funcall avy-handler-old char)
                      (let ((line (read-from-minibuffer
                                   "Goto line: " (string char))))
                        (when line
                          (avy-push-mark)
                          (save-restriction
                            (widen)
                            (goto-char (point-min))
                            (forward-line (1- (string-to-number line))))
                          (throw 'done 'exit)))))))
               (r (avy--line (eq arg 4))))
          (when (and (not (memq r '(t nil))) (eq avy-action #'identity))
            (avy-action-goto r)
            (if num
                (recenter num)
              (recenter))
            (if (equal (selected-frame) frame)
                (if (equal (selected-window) window)
                    (if (funcall calcfun pos (point) midtoend)
                        (goto-char pos))
                  (select-window window))
              (select-frame frame))))))))

(defun avy-act-recenter-middle-at-line (&optional arg)
  "Use `avy-goto-line' to choose a LINE.
Recenter the buffer LINE is in to so that LINE is in the middle.

If the entered character is in AVY-RECENTER-AT-CUR-LINE-KEYS, recenter with the
line point is in being in the middle and exit the selection process.

If the buffer containing LINE is in a different window or frame, return to the
original window or frame.

If LINE is in the same frame and window but still visible, return to that line.
If its starting position is still on the screen, it returns to it.

ARG acts the same way as for `avy-goto-line.'"
  (interactive "p")
  (avy-act--recenter
   (lambda (pos pnt midtoend) (and (> pos (- pnt midtoend)) (< pos (+ pnt midtoend))))
   nil
   arg))

(defun avy-act-recenter-top-at-line (&optional arg)
  "Use `avy-goto-line' to choose a LINE.
Recenter the buffer LINE is in to so that LINE is at the top.

If the entered character is in AVY-RECENTER-AT-CUR-LINE-KEYS, recenter
with the line point is in being in the top and exit the selection
process.

If the buffer containing LINE is in a different window or frame,
return to the original window or frame.

If LINE is in the same frame and window but still visible, return to
that line. If its starting position is still on the screen, it returns
to it.

ARG acts the same way as for `avy-goto-line."
  (interactive "p")
  (avy-act--recenter (lambda (pos pnt _midtoline) (> pos pnt))
                     1
                     arg))

(defun avy-act-recenter-bottom-at-line (&optional arg)
  "Use `avy-goto-line' to choose a LINE.
Recenter the buffer LINE is in to so that LINE is at the bottom.

If the entered character is in AVY-RECENTER-AT-CUR-LINE-KEYS, recenter
with the line point is in being in the bottom and exit the selection
process.

If the buffer containing LINE is in a different window or frame,
return to the original window or frame.

If LINE is in the same frame and window but still visible, return to
that line. If its starting position is still on the screen, it returns
to it.

ARG acts the same way as for `avy-goto-line."
  (interactive "p")
  (avy-act--recenter (lambda (pos pnt _midtoline) (< pos pnt))
                     -1
                     arg))

;;;;; Extended commands
;;;;;; Position commands
(defun avy-act-on-position (cmd avy size)
  "Use Avy to act on a position.

Ask for a command CMD, an avy function AVY as chosen using the
`avy-act-function-map' and a selecting function SIZE chosen using
`avy-act-position-selection-map', then use `avy-goto-word-1' to choose a
position, and then apply SIZE to mark a region and call CMD. Return to the
initial position POS.

CMD is chosen through typing a key combination that is in the
`avy-position-command-map'. In Emacs 30 and up you can also use any other bound
key combination that is not overwritten by one in the
`avy-position-command-map'.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (progn (avy-act--make-help-screens)
                            (avy-act-selection-commands-help))
                     (avy-act-functions-help)
                     (avy-act-position-selection-help)))
  (let ((window (selected-window))
        (frame (selected-frame)))
    (save-excursion (unwind-protect
                        (progn (call-interactively avy)
                               (call-interactively size)
                               (call-interactively cmd)
                               (deactivate-mark)
                               (setq-local avy-act-pos (point))
                               (setq avy-act-window (selected-window))
                               (setq avy-act-frame (selected-frame))
                               (set-transient-map avy-act-post-action-map))
                      (select-frame frame)
                      (select-window window)))))

(defun avy-act-on-position-word-1 (cmd size)
  "Use Avy to act on a position chosen by `avy-goto-word-1'.

Ask for a command CMD and a selecting function SIZE chosen using
`avy-act-position-selection-map', then use `avy-goto-word-1' to choose a
position, and then apply SIZE to mark a region and call CMD. Return to the
initial position POS.


CMD is chosen through typing a key combination that is in the
`avy-position-command-map'. In Emacs 30 and up you can also use any other bound
key combination that is not overwritten by one in the
`avy-position-command-map'.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (progn (avy-act--make-help-screens)
                            (avy-act-selection-commands-help))
                     (avy-act-position-selection-help)))
  (avy-act-on-position cmd #'avy-goto-word-1 size))

(defun avy-act-on-position-in-line (cmd size)
  "Use Avy to act on a position in the current line.

Ask for a command CMD and a selecting function SIZE chosen using
`avy-act-position-selection-map', then use `avy-goto-char-in-line' to choose a
position, and then apply SIZE to mark a region and call CMD. Return to the
initial position POS.


CMD is chosen through typing a key combination that is in the
`avy-position-command-map'. In Emacs 30 and up you can also use any other bound
key combination that is not overwritten by one in the
`avy-position-command-map'.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (progn (avy-act--make-help-screens)
                            (avy-act-selection-commands-help))
                     (avy-act-position-selection-help)))
  (avy-act-on-position cmd #'avy-goto-char-in-line size))

;;;;;; Region commands
(defun avy-act-on-region (cmd avy1 avy2)
  "Use Avy to act on a region.

Ask for a command CMD and two avy-functions AVY1 and AVY2 chosen by
`avy-act-function-map'. Use AVY1 to go to a position, set the mark, use AVY2 to
go to another position and call CMD. Return to the initial position POS.


CMD is chosen through typing a key combination that is in the
`avy-position-command-map'. In Emacs 30 and up you can also use any other bound
key combination that is not overwritten by one in the
`avy-position-command-map'.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (progn (avy-act--make-help-screens)
                            (avy-act-selection-commands-help))
                     (avy-act-functions-help)
                     (avy-act-functions-help)))
  (let ((window (selected-window))
        (frame (selected-frame)))
    (save-excursion (unwind-protect
                        (progn (call-interactively avy1)
                               (set-mark (point))
                               (call-interactively avy2)
                               (call-interactively cmd)
                               (deactivate-mark)
                               (setq-local avy-act-pos (point))
                               (setq avy-act-window (selected-window))
                               (setq avy-act-frame (selected-frame)))
                      (select-frame frame)
                      (select-window window)))
    (set-transient-map avy-act-post-action-map)))

(defun avy-act-on-region-by-same-function (cmd avy)
  "Use Avy to act on a region.

Ask for a command CMD and an avy-function AVY chosen by
`avy-act-function-map'. Use AVY to go to a position, set the mark, use AVY to
go to another position and call CMD. Return to the initial position POS.

CMD is chosen through typing a key combination that is in the
`avy-position-command-map'. In Emacs 30 and up you can also use any other bound
key combination that is not overwritten by one in the
`avy-position-command-map'.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (progn (avy-act--make-help-screens)
                            (avy-act-selection-commands-help))
                     (avy-act-functions-help)))
  (avy-act-on-region cmd avy avy))

;;;;;; Act-to-point commands
(defun avy-act-to-point (cmd avy)
  "Use Avy to act up to point.

Ask for a command CMD and an avy-function AVY chosen by `avy-act-function-map'.
Set the mark, use AVY to go to a position and call CMD. Return to the initial
position POS.

CMD is chosen through typing a key combination that is in the
`avy-position-command-map'. In Emacs 30 and up you can also use any other bound
key combination that is not overwritten by one in the
`avy-position-command-map'.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (progn (avy-act--make-help-screens)
                            (avy-act-selection-commands-help))
                     (avy-act-functions-help)))
  (save-excursion (set-mark (point))
                  (call-interactively avy)
                  (call-interactively cmd)
                  (setq-local avy-act-pos (point))
                  (setq avy-act-window (selected-window))
                  (setq avy-act-frame (selected-frame)))
  (set-transient-map avy-act-post-action-map))

(defun avy-act-to-point-in-same-line (cmd)
  "Use Avy to act up to point in the current line.

Ask for a command CMD. Set the mark, use `avy-goto-char-in-line' to go to a
position and call CMD. Return to the initial position POS.

CMD is chosen through typing a key combination that is in the
`avy-position-command-map'. In Emacs 30 and up you can also use any other bound
key combination that is not overwritten by one in the
`avy-position-command-map'.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (progn (avy-act--make-help-screens)
                            (avy-act-selection-commands-help))))
  (avy-act-to-point cmd #'avy-goto-char-in-line))

(provide 'avy-act)
;;; avy-act.el ends here
