;;; avy-act.el --- Commands that let avy act from a distance  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Package-Requires: ((avy "0.5.0") (back-button "0.6.8") (emacs "29.1"))
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

;; These commands let Avy act from a distance. There are four basic classes:

;; The commands `avy-act-follow' and `avy-act-follow-in-new-buffer', which allow you to select
;; a link to follow.

;; The commands `avy-recenter-middle-at-line', `avy-recenter-top-at-line' and
;; `avy-recenter-bottom-at-line', which allow recentering at a line.

;; Extended functions, which allow you to do simple editing from a distance. These can be
;; subdivided into four sub-classes:

;; The commands `avy-act-on-position' and `avy-act-on-position-word-1', which act by selecting
;; a position in the buffer through Avy, then apply a marking command like `mark-word' or
;; `mark-sexp', then apply a command to the marked region.

;; The commands `avy-act-on-region' and `avy-act-on-region-by-same-function', which act by
;; marking a region through either two avy functions or the same function applied twice,
;; then act on that region by applying a command.

;; The commands avy-act-to-point and `avy-act-to-point-in-same-line', which mark a region up
;; to the current position of point using one avy command, then apply a command to that
;; region.

;; The commands that are applied in extended functions can be chosen through keyboard
;; shortcuts as they are in the current buffer, or through the `avy-selection-command-map'.
;; When using `avy-act-on-position' or `avy-act-on-position-word-1', often either a whitespace
;; is missing or one too much remains is at the position where the action was performed.
;; For this, another command can be applied to that position from the
;; `avy-act-post-action-map', which by default contains commands to insert a whitespace or
;; delete a character.

;;; Code:
;;;; Requirements
(require 'avy)

;;;; Keymaps
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
  "0" #'avy-act-insert-space-from-distance)

;;;; Customs
(defcustom avy-act-recenter-at-cur-line-keys
  (list (string-to-char (kbd "C-f")) (string-to-char (kbd "C-j")))
  "List of recenter key combinations for avy-act-recenter commands.
Typing one of these causes a command to recenter with the line point is on at
the top, middle or bottom."
  :type '(list character)
  :group 'convenience)

;;;; Help screens
;;;;; Help screen functions and macros
(defmacro avy-act-make-function-select-help-screen (fname help-line help-text helped-map
                                                          &optional buffer-name)
  "A variant of `make-help-screen'.
There is only difference to the original macro: instead of calling a selected
function it returns its name. See `make-help-screen' for an explanation of the
arguments."
  (declare (indent defun))
  `(defun ,fname ()
     "Help command."
     (interactive)
     (avy-act--function-select-help-screen ,help-line ,help-text ,helped-map ,buffer-name)))

(defun avy-act--function-select-help-screen (help-line help-text helped-map buffer-name)
  "Helper function for `avy-act-make-function-select-help-screen'.
See `help--help-screen' for documentation of the arguments."
  (let ((line-prompt (substitute-command-keys help-line))
        (help-buffer-under-preparation t))
    (when three-step-help
      (message "%s" line-prompt))
    (let* ((help-screen help-text)
           ;; We bind overriding-local-map for very small
           ;; sections, *excluding* where we switch buffers
           ;; and where we execute the chosen help command.
           (local-map (make-sparse-keymap))
           (new-minor-mode-map-alist minor-mode-map-alist)
           (prev-frame (selected-frame))
           config new-frame key char)
      (when (string-match "%THIS-KEY%" help-screen)
        (setq help-screen
              (replace-match (help--key-description-fontified
                              (substring (this-command-keys) 0 -1))
                             t t help-screen)))
      (unwind-protect
          (prog1 (let ((minor-mode-map-alist nil))
                   (setcdr local-map helped-map)
                   (define-key local-map [t] #'undefined)
                   ;; Make the scroll bar keep working normally.
                   (define-key local-map [vertical-scroll-bar]
                               (lookup-key global-map [vertical-scroll-bar]))
                   (if three-step-help
                       (progn
                         (setq key (let ((overriding-local-map local-map))
                                     (read-key-sequence nil)))
                         ;; Make the HELP key translate to C-h.
                         (if (lookup-key function-key-map key)
                             (setq key (lookup-key function-key-map key)))
                         (setq char (aref key 0)))
                     (setq char ??))
                   (when (or (eq char ??) (eq char help-char)
                             (memq char help-event-list))
                     (setq config (current-window-configuration))
                     (pop-to-buffer (or buffer-name " *Metahelp*") nil t)
                     (and (fboundp 'make-frame)
                          (not (eq (window-frame)
                                   prev-frame))
                          (setq new-frame (window-frame)
                                config nil))
                     (setq buffer-read-only nil)
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (insert (substitute-command-keys help-screen)))
                     (let ((minor-mode-map-alist new-minor-mode-map-alist))
                       (help-mode)
                       (variable-pitch-mode)
                       (setq new-minor-mode-map-alist minor-mode-map-alist))
                     (goto-char (point-min))
                     (while (or (memq char (append help-event-list
                                                   (cons help-char '( ?? ?\C-v ?\s ?\177 ?\M-v ?\S-\s
                                                                      deletechar backspace vertical-scroll-bar
                                                                      home end next prior up down))))
                                (eq (car-safe char) 'switch-frame)
                                (equal key "\M-v"))
                       (condition-case nil
                           (cond
                            ((eq (car-safe char) 'switch-frame)
                             (handle-switch-frame char))
                            ((memq char '(?\C-v ?\s next end))
                             (scroll-up))
                            ((or (memq char '(?\177 ?\M-v ?\S-\s deletechar backspace prior home))
                                 (equal key "\M-v"))
                             (scroll-down))
                            ((memq char '(down))
                             (scroll-up 1))
                            ((memq char '(up))
                             (scroll-down 1)))
                         (error nil))
                       (let ((cursor-in-echo-area t)
                             (overriding-local-map local-map))
                         (frame-toggle-on-screen-keyboard (selected-frame) nil)
                         (setq key (read-key-sequence
                                    (format "Type one of listed options%s: "
                                            (if (pos-visible-in-window-p
                                                 (point-max))
                                                ""
                                              (concat  ", or "
                                                       (help--key-description-fontified (kbd "<PageDown>"))
                                                       "/"
                                                       (help--key-description-fontified (kbd "<PageUp>"))
                                                       "/"
                                                       (help--key-description-fontified (kbd "SPC"))
                                                       "/"
                                                       (help--key-description-fontified (kbd "DEL"))
                                                       " to scroll")))
                                    nil nil nil nil
                                    ;; Disable ``text conversion''.  OS
                                    ;; input methods might otherwise chose
                                    ;; to insert user input directly into
                                    ;; a buffer.
                                    t)
                               char (aref key 0)))

                       ;; If this is a scroll bar command, just run it.
                       (when (eq char 'vertical-scroll-bar)
                         (command-execute (lookup-key local-map key) nil key))))
                   ;; We don't need the prompt any more.
                   (message "")
                   ;; Mouse clicks are not part of the help feature,
                   ;; so reexecute them in the standard environment.
                   (if (listp char)
                       (setq unread-command-events
                             (cons char unread-command-events)
                             config nil)
                     (let ((defn (lookup-key local-map key)))
                       (if defn
                           (progn
                             (when config
                               (set-window-configuration config)
                               (setq config nil))
                             ;; Temporarily rebind `minor-mode-map-alist'
                             ;; to `new-minor-mode-map-alist' (Bug#10454).
                             (prog1
                                 (let ((minor-mode-map-alist new-minor-mode-map-alist))
                                   ;; Return the function name instead of calling it interactively
                                   defn)
                               (when new-frame
                                 ;; Do not iconify the selected frame.
                                 (unless (eq new-frame (selected-frame))
                                   (iconify-frame new-frame))
                                 (setq new-frame nil))))
                         (unless (equal (key-description key) "C-g")
                           (message (substitute-command-keys
                                     (format "No help command is bound to `\\`%s''"
                                             (key-description key))))
                           (ding))))))
            (progn (when config
                     (set-window-configuration config))
                   (when new-frame
                     (iconify-frame new-frame))
                   (setq minor-mode-map-alist new-minor-mode-map-alist)))))))

;;;;; Avy-act help screens
(avy-act-make-function-select-help-screen
  avy-act-functions-help "Function: (? for Help)"
  (format "%s"  (substitute-command-keys "\\{avy-act-function-map}"))
  avy-act-function-map)

(avy-act-make-function-select-help-screen avy-act-selection-commands-help "Function: (? for Help)"
  (format "%s"  (substitute-command-keys "\\{avy-act-selection-command-map}"))
  (make-composed-keymap avy-act-selection-command-map (make-composed-keymap (current-active-maps t))))

(avy-act-make-function-select-help-screen avy-act-position-selection-help "Function: (? for Help)"
                                                        (format "%s"  (substitute-command-keys "\\{avy-act-position-selection-map}"))
                                                        avy-act-position-selection-map)


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
        (t (progn (org-open-at-point-global)))))

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
                  (if (cl-member char avy-act-recenter-at-cur-line-keys)
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
  (avy-act--recenter (lambda (pos pnt midtoline) (> pos pnt))
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
  (avy-act--recenter (lambda (pos pnt midtoline) (< pos pnt))
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

CMD is chosen through typing a key combination that is either in the currently
active maps or in the `avy-position-command-map', which overrides the others.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (avy-act-selection-commands-help)
                     (avy-act-functions-help)
                     (avy-act-position-selection-help)))
  (let ((window (selected-window))
        (frame (selected-frame))
        (pos (point))
        (buflength (point-max)))
    (call-interactively avy)
    (let ((newpos (point)))
      (call-interactively size)
      (call-interactively cmd)
      (setq-local avy-act-pos (point))
      (setq avy-act-window (selected-window))
      (setq avy-act-frame (selected-frame))
      (if (equal frame (selected-frame))
          (if (equal window (selected-window))
              (if (< newpos pos) ; Calculate new position for changes before point.
                  (goto-char (+ pos (- (point-max) buflength)))
                (goto-char pos))
            (select-window window))
        (select-frame frame)))
    (set-transient-map avy-act-post-action-map)))

(defun avy-act-on-position-word-1 (cmd size)
  "Use Avy to act on a position chosen by `avy-goto-word-1'.

Ask for a command CMD and a selecting function SIZE chosen using
`avy-act-position-selection-map', then use `avy-goto-word-1' to choose a
position, and then apply SIZE to mark a region and call CMD. Return to the
initial position POS.

CMD is chosen through typing a key combination that is either in the currently
active maps or in the `avy-position-command-map', which overrides the others.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (avy-act-selection-commands-help)
                     (avy-act-position-selection-help)))
  (avy-act-on-position cmd #'avy-goto-word-1 size))

(defun avy-act-on-position-in-line (cmd size)
  "Use Avy to act on a position in the current line.

Ask for a command CMD and a selecting function SIZE chosen using
`avy-act-position-selection-map', then use `avy-goto-char-in-line' to choose a
position, and then apply SIZE to mark a region and call CMD. Return to the
initial position POS.

CMD is chosen through typing a key combination that is either in the currently
active maps or in the `avy-position-command-map', which overrides the others.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (avy-act-selection-commands-help)
                     (avy-act-position-selection-help)))
  (avy-act-on-position cmd #'avy-goto-char-in-line size))

(defun avy-act-on-region (cmd avy1 avy2)
  "Use Avy to act on a region.

Ask for a command CMD and two avy-functions AVY1 and AVY2 chosen by
`avy-act-function-map'. Use AVY1 to go to a position, set the mark, use AVY2 to
go to another position and call CMD. Return to the initial position POS.

CMD is chosen through typing a key combination that is either in the currently
active maps or in the `avy-position-command-map', which overrides the others.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (avy-act-selection-commands-help)
                     (avy-act-functions-help)
                     (avy-act-functions-help)))
  (let ((window (selected-window))
        (frame (selected-frame))
        (pos (point))
        (buflength (point-max)))
    (call-interactively avy1)
    (set-mark (point))
    (let ((newpos1 (point)))
      (call-interactively avy2)
      (call-interactively cmd)
      (deactivate-mark)
      (setq-local avy-act-pos (point))
      (setq avy-act-window (selected-window))
      (setq avy-act-frame (selected-frame))
      (if (equal frame (selected-frame))
          (if (equal window (selected-window))
              (if (and (< newpos1 pos)) ; Calculate new region for changes before point
                  (goto-char (+ pos (- (point-max) buflength)))
                (goto-char pos))
            (select-window window))
        (select-frame frame)))
    (set-transient-map avy-act-post-action-map)))

(defun avy-act-on-region-by-same-function (cmd avy)
  "Use Avy to act on a region.

Ask for a command CMD and an avy-function AVY chosen by
`avy-act-function-map'. Use AVY to go to a position, set the mark, use AVY to
go to another position and call CMD. Return to the initial position POS.

CMD is chosen through typing a key combination that is either in the currently
active maps or in the `avy-position-command-map', which overrides the others.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (avy-act-selection-commands-help)
                     (avy-act-functions-help)))
  (avy-act-on-region cmd avy avy))



(defun avy-act-to-point (cmd avy)
  "Use Avy to act up to point.

Ask for a command CMD and an avy-function AVY chosen by `avy-act-function-map'.
Set the mark, use AVY to go to a position and call CMD. Return to the initial
position POS.

CMD is chosen through typing a key combination that is either in the currently
active maps or in the `avy-position-command-map', which overrides the others.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (avy-act-selection-commands-help)
                     (avy-act-functions-help)))
  (let ((markon visible-mark-mode))
    (set-mark (point))
    (call-interactively avy)
    (call-interactively cmd)
    (exchange-point-and-mark)
    (unless markon (visible-mark-mode 0))
    (setq-local avy-act-pos (point))
    (setq avy-act-window (selected-window))
    (setq avy-act-frame (selected-frame))
    (set-transient-map avy-act-post-action-map)))

(defun avy-act-to-point-in-same-line (cmd)
  "Use Avy to act up to point in the current line.

Ask for a command CMD. Set the mark, use `avy-goto-char-in-line' to go to a
position and call CMD. Return to the initial position POS.

CMD is chosen through typing a key combination that is either in the currently
active maps or in the `avy-position-command-map', which overrides the others.

Through this method, simple editing of areas before or after point can be done
without having to move point.

If immediately after an action a key combination COMB in
`avy-act-post-action-map' is chosen, call command corresponding to COMB. This
command is supposed to use the variables `avy-act-frame', `avy-act-window' and
`avy-act-pos' to return to the position that was acted on, do something (mainly
delete whitespace) and return to POS."
  (interactive (list (avy-act-selection-commands-help)))
  (avy-act-to-point cmd #'avy-goto-char-in-line))

(provide 'avy-act)
;;; avy-act.el ends here
