;;; evil-layer.el --- A comprehensive evil mode layer for Quake Emacs -*- lexical-binding: t -*-

;; Author: Alexis Purslane <alexispurlsane@pm.me>
;; Keywords: emacs-configuration, emacs-distribution, doom-emacs, note-taking, writing, code

;; This file is not part of GNU Emacs.

;; Copyright (c) by Alexis Purslane 2024.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a comprehensive evil mode layer for
;; integrating evil mode deeply into Quake Emacs, including
;; providing keybindings for all the packages Quake Emacs
;; provides (in addition to `evil-collection' for everything
;; else), structural editing, and more. Unlike the other optional
;; layers for Quake, this one is provided in the core repository
;; because probably many people want an evil mode layer.

;;; Code:

(defmacro quake-evil-define-key (states keymaps &rest args)
    "Define the given keys in ARGS in STATES for the given KEYMAPS.
ARGS is a plist of the form (KEY DEF  KEY DEF). It must have an
even number of elements. If DEF is a symbol or function value,
that is what will be called when the key is pressed if the keymap
is active. If DEF is a `cons' cell, the car is the label that
will be shown in which-key, and the cadr is either a symbol or a
function."
    (declare (indent defun))
    (if (cl-evenp (length args))
            `(progn
                 ,@(cl-loop for (key def) on args by #'cddr
                            append (cl-loop
                                    for keymap in (if (symbolp keymaps) (list keymaps) keymaps)
                                    collect `(evil-define-key* ',states ,keymap (kbd ,key) ,def))))
        (error "Expected even number of arguments in ARGS so every key chord has a corresponding definition.")))

(defmacro quake-evil-create-keymap (states &rest args)
    "Create a new keymap with the given key definitions and return it.
Uses the same syntax and semantics as `quake-emacs-define-key'."
    (declare (indent defun))
    (cl-with-gensyms (keymap)
        `(let ((,keymap (make-keymap)))
             (quake-evil-define-key ,states ,keymap ,@args)
             ,keymap)))

(defcustom quake-evil-text-objects
    '(("f" . "function")
      ("s" . ("list" "conditional" "loop" "assignment" "call" "block" "statement"))
      ("t" . "class")
      ("c" . "comment")
      ("a" . "parameter")
      ("T" . "test"))
    "The text objects added to evil mode at startup.

A list of pairs, where the first element is a string, KEY, and
the second object is either a string or a list containing the
query to be made for that text object minus the .inner and .outer
qualifiers."
    :group 'quake)

(defun optional/evil-layer ()
    "'Emacs is a great OS, if only it had a good editor.' With
  the powerful text-object based command language of Vim, and the
  flexibility of Emacs, at your command, Evil is that editor.

  Loads:

  - `evil', the Emacs editor of choice"

;;;;; Evil mode itself (and associated integrations)
    (use-package evil
        :custom
        (evil-disable-insert-state-bindings t)
        (evil-want-integration t)
        (evil-want-minibuffer t)
        (evil-want-keybinding nil)
        (evil-want-C-u-scroll nil)
        (evil-want-C-d-scroll nil)
        (evil-want-C-i-jump t)
        (evil-undo-system 'undo-redo)
        (evil-kill-on-visual-paste nil) ;; oh thank god
        (evil-move-beyond-eol t) ;; so that it's easier to evaluate sexprs in normal mode
        :config
        (evil-mode 1)
        (setq evil-motion-state-modes (append evil-motion-state-modes
                                              evil-emacs-state-modes))
        (setq evil-emacs-state-modes nil)
        (quake-evil-define-key (motion) org-agenda-mode-map
                               "RET" org-agenda-switch-to)

        (remove-hook 'post-command-hook 'quake-god-mode-update-cursor-type)
        (quake-evil-define-key (motion god visual normal insert) override-global-map
                               "<escape>"      (defun quake--evil-escape-dwim ()
                                                   "Intuitive escape behavior.

Kills, exits, or escapes literally everything with successive
repetitions."
                                                   (interactive)
                                                   (when (and (fboundp 'corfu-quit) completion-in-region-mode) (corfu-quit))
                                                   (when overwrite-mode                                        (overwrite-mode -1))
                                                   (cond ((region-active-p)              (deactivate-mark))
                                                         ((not (evil-normal-state-p))    (evil-normal-state))
                                                         (god-local-mode                 (god-local-mode -1))
                                                         (isearch-mode                   (isearch-exit))
                                                         ((eq last-command 'mode-exited) nil)
                                                         ((> (minibuffer-depth) 0)       (abort-recursive-edit))
                                                         (current-prefix-arg             nil)
                                                         ((> (recursion-depth) 0)        (exit-recursive-edit))
                                                         (buffer-quit-function           (funcall buffer-quit-function))
                                                         ((string-match "^ \\*" (buffer-name (current-buffer)))
                                                          (bury-buffer)))))

;;;;; Custom evil mode key bindings
        ;; Make :q close the buffer and window, not quit the entire
        ;; Emacs application (we never leave Emacs!)
        (global-set-key [remap evil-quit] 'kill-buffer-and-window)

        ;; Override evil mode's exceptions to defaulting to normal-mode
        (evil-set-initial-state 'minibuffer-mode 'insert)

;;;;;; Emacs integration
        (add-hook 'evil-insert-state-entry-hook (lambda () (cua-mode 1)))
        (add-hook 'evil-normal-state-entry-hook (lambda () (cua-mode -1)))
;;;;;; Miscillanious useful keybindings for emacs capabilities
        (quake-evil-define-key (god) global-map
                               "C-w"      evil-window-map
                               "C-w C-u"  'winner-undo))

    (use-package evil-god-state
        :after (god-mode evil)
        :config
        (quake-evil-define-key (motion normal visual) override-global-map
                               "SPC"      'evil-execute-in-god-state)
;;;;; Make which-key for the top level keybindings show up when you enter evil-god-state
        (add-hook 'evil-local-mode-hook
                  (lambda () (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)))

        (add-hook 'evil-god-state-exit-hook
                  (lambda ()
                      (which-key--hide-popup)))
        (add-hook 'evil-god-state-entry-hook
                  (lambda ()
                      (which-key--create-buffer-and-show nil
                                                         nil
                                                         (lambda (x) (and (not (null (car x)))
                                                                          (not (null (cdr x)))))
                                                         "Top-level bindings")))))
