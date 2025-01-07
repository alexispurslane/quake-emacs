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

  - `evil', the Emacs editor of choice
  - `evil-collection', to integrate Evil mode with everything else
  - `evil-cleverparens', to give you proper S-expr editing
    capabilities, since you'll be doing a lot of that
  - `evil-org-mode', for good org mode keybindings for Evil
  - `treemacs-evil', for good treemacs keybindings for Evil
  - `evil-god-state', to use god mode as an automatic leader key
    for evil mode that supports all the existing emacs
    keybindings"

;;;;; Evil mode itself (and associated integrations)
    (use-package evil
        :custom
        (evil-want-integration t)
        (evil-want-minibuffer t)
        (evil-want-keybinding nil)
        (evil-want-C-u-scroll t)
        (evil-want-C-i-jump nil)
        (evil-undo-system 'undo-redo)
        (evil-kill-on-visual-paste nil) ;; oh thank god
        (evil-move-beyond-eol t) ;; so that it's easier to evaluate sexprs in normal mode
        :config
        (evil-mode 1)

        (remove-hook 'post-command-hook 'quake-god-mode-update-cursor-type)

;;;;; Custom evil mode key bindings
        ;; Make :q close the buffer and window, not quit the entire
        ;; Emacs application (we never leave Emacs!)
        (global-set-key [remap evil-quit] 'kill-buffer-and-window)
        (quake-evil-define-key (motion normal visual) override-global-map
                               "<escape>" 'escape-dwim)

        ;; Override evil mode's exceptions to defaulting to normal-mode
        (evil-set-initial-state 'enlight-mode 'motion)
        (evil-set-initial-state 'minibuffer-mode 'insert)

;;;;;; CUA integration
        (add-hook 'evil-insert-state-entry-hook (lambda () (cua-mode 1)))
        (add-hook 'evil-normal-state-entry-hook (lambda () (cua-mode -1)))
;;;;;; Miscillanious useful keybindings for emacs capabilities
        (quake-evil-define-key (normal visual) global-map
                               "g ."   'embark-act
                               "g RET" 'embark-dwim
                               ;; buffers
                               "gb"   'evil-switch-to-windows-last-buffer
                               ;; org mode
                               "gt"   'org-toggle-checkbox
                               ;; fill-region >> vim gqq
                               "gq"   'fill-region-as-paragraph
                               ;; Support for visual fill column mode and visual line mode
                               ;; Make evil-mode up/down operate in screen lines instead of logical lines
                               "j"    'evil-next-visual-line
                               "k"    'evil-previous-visual-line
                               ;; outline keybindings
                               "gh"   'outline-up-heading
                               "gj"   'outline-forward-same-level
                               "gk"   'outline-backward-same-level
                               "gl"   'outline-next-visible-heading
                               "gu"   'outline-previous-visible-heading)

        (quake-evil-define-key (normal motion) global-map
                               ;; tab bar mode
                               "gR" 'tab-rename
                               "gn" 'tab-bar-new-tab
                               "gx" 'tab-bar-close-tab
                               "gX" 'tab-bar-close-other-tabs
                               ;; Nice commenting
                               "gc" 'comment-region
                               "gC" 'uncomment-region)

        (quake-evil-define-key (normal motion) (outline-minor-mode-map org-mode-map outline-mode-map diff-mode-map)
                               "TAB" 'evil-toggle-fold)
        (quake-evil-define-key (normal motion) (org-mode-map)
                               "RET" 'evil-org-return)
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
                                                         "Top-level bindings"))))

    (use-package evil-collection
        :after (evil)
        :config
        (evil-collection-init))

    (use-package evil-cleverparens
        :after (evil)
        :hook ((lisp-mode . evil-cleverparens-mode)
               (emacs-lisp-mode . evil-cleverparens-mode)))

    (use-package treemacs-evil
        :after (treemacs evil)
        :ensure t)

    (use-package evil-org
        :after org
        :hook (org-mode . evil-org-mode))

;;;;; Evil mode text object support

    ;; NOTE: Eventually replace this *entire* boondoggle with
    ;; [[https://github.com/dvzubarev/evil-ts-obj/tree/master]],
    ;; which provides an even better set of text objects, and a
    ;; far more complete set of operations on them, instead of me
    ;; manually having to implement them. This would mean *all*
    ;; tree-sitter langauges get slurp/barf/convolute/etc,
    ;; meaning that we could eliminate evil-cleverparents and
    ;; just have a generalized structural editing system on our
    ;; hands. Currently however, it depends on Emacs 30.0.50.
    (use-package evil-textobj-tree-sitter
        :after (evil evil-collection)
        :config
        ;; Thanks to foxfriday/evil-ts for this one
        (defun evil-ts-expand-region ()
            "Expand selection to the closest tree-sitter parent node."
            (let* ((point (point))
                   (mark (or (mark t) point))
                   (start (min point mark))
                   (end (max point mark))
                   (node (treesit-node-at start))
                   (parent (treesit-parent-until node
                                                 (lambda (n) (and (> start (treesit-node-start  n))
                                                                  (< end (treesit-node-end n))))
                                                 nil))
                   (pstart (if parent (treesit-node-start parent) nil))
                   (pend (if parent (treesit-node-end parent) nil)))
                (when parent
                    (goto-char pstart)
                    (list pstart pend))))

        ;; make "tree sitter expand region" a manipulable text object
        (evil-define-text-object evil-ts-text-obj-expand-region (count &optional beg end type)
                                 (evil-ts-expand-region))

        ;; bind it to "x"
        (define-key evil-outer-text-objects-map "x" 'evil-ts-text-obj-expand-region)
        (define-key evil-inner-text-objects-map "x" 'evil-ts-text-obj-expand-region)

        ;; This being a macro is necessary because define-key does not
        ;; evaluate its arguments so it won't work in a regular loop.
        (eval-when-compile
            (defmacro define-textobjs ()
                "Loop through `quake-evil-text-objects' and construct
a flat list of the `define-key' expressions to set the text objects up."
                `(progn ,@(cl-loop for thing in quake-evil-text-objects
                                   for key = (car thing)
                                   for query = (if (listp (cdr thing)) (cdr thing) (list (cdr thing)))

                                   for outer-query = (mapcar (lambda (x) (concat x ".outer")) query)
                                   for inner-query = (mapcar (lambda (x) (concat x ".inner")) query)

                                   for docname-start = (concat "go-to-next-" (car query) "-start")
                                   for docname-end = (concat "go-to-next-" (car query) "-end")

                                   appending
                                   `((define-key evil-outer-text-objects-map ,key
                                                 (evil-textobj-tree-sitter-get-textobj ,outer-query))
                                     (define-key evil-inner-text-objects-map ,key
                                                 (evil-textobj-tree-sitter-get-textobj ,inner-query))
                                     (quake-evil-define-key (normal visual) global-map
                                                            ;; go to next start
                                                            ,(concat "[" key) (cons
                                                                               ,(concat "goto-textobj-" (car query) "-start")
                                                                               (lambda ()
                                                                                   (interactive)
                                                                                   (evil-textobj-tree-sitter-goto-textobj ,(car outer-query) t nil)))
                                                            ;; go to next end
                                                            ,(concat "]" key) (cons
                                                                               ,(concat "goto-textobj-" (car query) "-end")
                                                                               (lambda ()
                                                                                   (interactive)
                                                                                   (evil-textobj-tree-sitter-goto-textobj ,(car outer-query) nil t)))))
                                   into exprs

                                   finally (return exprs)))))
        (define-textobjs)))
