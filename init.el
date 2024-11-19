;; init.el --- implements the Quake Emacs distribution of Emacs -*- lexical-binding: t -*-
;; eval: (outline-hide-sublevels 4)

;; Author: Alexis Purslane <alexispurlsane@pm.me>
;; URL: https://github.com/alexispurslane/quake-emacs
;; Package-Requires: ((emacs "29.1") (cl-lib "1.0")) 
;; Version: 1.0.0-alpha
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

;; This is the initialization file for Quake Emacs, a self-contained
;; one-file Emacs distribution focused on leveraging modern vanilla
;; Emacs' built in capabities as much as possible and providing a
;; lean, fast and focused modern experience for code editing, writing,
;; and note-taking out of the box, while also serving as a good
;; starting point for further configuration.
;; 
;; Installation instructions:
;; git clone https://github.com/alexispurslane/quake-emacs.git ~/.emacs.d
;; mkdir -p ~/.quake.d/ && cp user.el ~/.quake.d/
;;
;; Update instructions:
;; git pull
;;
;; Switching to development branch instructions:
;; git checkout origin/develop
;; 
;; IF YOU DID NOT OBTAIN THIS FILE BY CLONING THE GIT REPOSITORY,
;; PLEASE DO SO INSTEAD OF USING THIS FILE DIRECTLY

;; For more information, see the README in the online repository.
(setq debug-on-error t)
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(run-with-timer 5 0 (lambda ()
                        (setq gc-cons-threshold gc-cons-threshold-original)
                        (message "Restored GC cons threshold")))
(profiler-start 'cpu)
;;; ======Prelude======
(require 'cl-lib)
(require 'rx)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(require 'use-package-ensure)
(setq use-package-always-ensure t
      package-enable-at-startup nil
      use-package-compute-statistics t)

;; Print a little message to the user that we're loading packages if this is a fresh install
(when (length< package-selected-packages 2)
    (with-current-buffer "*scratch*"
        (insert (propertize "Installing Quake Emacs..." 'face '(:height 400)))
        (insert "
Please wait patiently.

It is okay to close Emacs if you need to — installation
operations are idempotent. This process will take several
minutes (depending on the speed of your processor and internet
connection). You can watch the echo area for updates.

")
        (insert (propertize "Note: If Quake Emacs runs into any errors, restarting usually
fixes the problem." 'face 'bold))
        (goto-char 0))
    (redraw-display))

(defgroup quake nil
    "A customization group for the Quake Emacs distribution."
    :prefix "quake")

;;; ======User-Modifiable Variables======
(defcustom quake-org-home-directory
    "~/org"
    "The directory that your org capture templates, Denote notes, and
org-agenda files will be placed in by default."
    :type 'string
    :group 'quake)

(defcustom quake-enabled-layers
    (list
     #'core/usability-layer
     #'core/editor-layer
     #'optional/god-layer ;; or #'optional/devil-layer
     #'task/coding-layer
     #'task/writing-layer
     #'task/notes-layer
     #'core/aesthetic-layer
     #'optional/bling-layer
     )
    "The function symbols for the layers that Quake Emacs
should enable on startup.

This has a default value so that `init.el' will function without
`user.el' if the user just wants all-defaults. DO NOT CHANGE THIS
YOURSELF, IT MAY BREAK UPDATES."
    :group 'quake)

(defcustom quake-color-theme 'doom-gruvbox
    "The theme quake loads and uses at startup."
    :group 'quake
    :type 'symbol)

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

(defcustom quake-term-preferred-command 'eshell
    "Which Emacs command Quake Emacs's popup terminal will trigger.

If you pass in term, your environment shell will be
passed in as an argument."
    :group 'quake)

;;; ======Utility Functions======

(defun quake/set-aesthetics (frame)
    (let ((mode-bg (face-background 'mode-line))
          (main-bg (face-background 'default)))
        (when (not (frame-parent frame))
            (set-face-attribute 'internal-border frame :background main-bg)
            (modify-frame-parameters frame `((internal-border-width . 20))))
        (set-face-background 'line-number main-bg frame)
        (set-face-foreground 'vertical-border mode-bg frame)
        (setq window-divider-default-right-width 1)
        (window-divider-mode 1)
        (set-face-attribute 'mode-line frame :inherit 'variable-pitch
                            :height 120
                            :box `(:line-width 5 :color ,mode-bg :style nil))
        (set-face-attribute 'mode-line-inactive frame :inherit 'mode-line
                            :box `(:line-width 5 :color ,(face-background 'mode-line-inactive) :style nil))))

;;; ======Load User Script======
;; we load the user script at the beginning so that some of their
;; config can run *before* layer initialization happens, and
;; their custom layers can run during and after, thus producing a
;; nice clean

(when (file-exists-p "~/.quake.d/user.el")
    (load "~/.quake.d/user.el"))

;;; ======Vanilla Emacs======
(use-package emacs
    :init
;;;; Setting up Emacs to behave in a more familiar and pleasing way
    (setq inhibit-startup-message t ; we're going to have our own dashboard
	      visible-bell t         ; nobody likes being beeped at
	      make-backup-files nil ; don't litter all over the place
	      lisp-body-indent 4    ; four space tabs
	      vc-follow-symlinks t ; we'll always want to follow symlinks
	      warning-minimum-level :emergency ; don't completely shit the bed on errors
	      display-line-numbers 'relative ; whether you use evil or not, these are useful
	      custom-file "~/.emacs.d/custom.el") ; dump all the shit from custom somewhere else
    (setq-default fill-column 65) ; this will be used in reading modes, so set it to something nice
    (setq tab-always-indent 'complete) ; more modern completion behavior
    (setq read-file-name-completion-ignore-case t ; ignore case when completing file names
          read-buffer-completion-ignore-case t ; ignore case when completing buffer names
          completion-ignore-case t) ; fucking ignore case in general!
    (setopt use-short-answers t) ; so you don't have to type out "yes" or "no" and hit enter
    (setopt initial-buffer-choice #'enlight)
    (setq eldoc-idle-delay 1.0) ; w/ eldoc-box/an LSP, idle delay is by default too distracting
    (setq display-line-numbers-width-start t) ; when you open a file, set the width of the linum gutter to be large enough the whole file's line numbers
    (setq-default indent-tabs-mode nil) ; prefer spaces instead of tabs
;;;;; Disabling ugly and largely unhelpful UI features 
    (menu-bar-mode 1)
    (tool-bar-mode -1)
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;;;; Enable some modes that give nicer, more modern behavior
    (setq pixel-scroll-precision-interpolate-mice t
	      pixel-scroll-precision-interpolate-page t)
    (pixel-scroll-precision-mode 1)   ; smooth scrolling
    (winner-mode 1)                ; better window manipulation
    (savehist-mode 1)              ; remember commands
    (column-number-mode) ; keep track of column number for the useful modeline readout
    (global-visual-line-mode)     ; wrap lines at end of window
;;;;; A basic programmming mode to build off of that adds some expected things
    (add-hook 'prog-mode-hook (lambda ()
			                      (prettify-symbols-mode 1)
			                      (display-line-numbers-mode 1)
			                      (setq display-line-numbers 'relative)
			                      (hl-line-mode t)
			                      (electric-pair-mode)))
;;;;; Performance tuning
    (setq gc-cons-percentage 0.2)
;;;;;; Optimize font-locking for greater responsiveness
    (setq jit-lock-stealth-time 0.2
          jit-lock-defer-time 0.0
          jit-lock-context-time 0.2
          jit-lock-stealth-load 200)
;;;;;; Optimize for long lines. 
    (setq-default bidi-paragraph-direction 'left-to-right ; assume we're using LtR text unless explicitly told otherwise
                  bidi-inhibit-bpa t) ; turn off bidirectional paren display algorithm, it is expensive
;;;;;; Faster minibuffer
    (defun setup-fast-minibuffer ()
        (setq gc-cons-threshold most-positive-fixnum))
    (defun close-fast-minibuffer ()
        (setq gc-cons-threshold (* 8 1024 1024)))

    (add-hook 'minibuffer-setup-hook 'setup-fast-minibuffer)
    (add-hook 'minibuffer-exit-hook 'close-fast-minibuffer)
;;;; Fonts
    (set-display-table-slot
     standard-display-table
     'selective-display
     (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
         (vconcat (mapcar (lambda (c) (+ face-offset c)) " ")))))
;;;; Customizing the built in tab-bar
(use-package tab-bar
    :commands (tab-bar-new-tab tab-bar-mode)
    :init
    (setq tab-bar-button-relief 0)
    :config
    (add-hook 'tab-bar-mode-hook
              (lambda ()
                  (set-face-attribute 'tab-bar nil :inherit 'variable-pitch)
                  (set-face-attribute 'tab-bar-tab nil :box `(:line-width 5 :color ,(face-background 'tab-bar-tab) :style nil))
                  (set-face-attribute 'tab-bar-tab-inactive nil
                                      :box `(:line-width 5 :color ,(face-background 'tab-bar-tab-inactive) :style nil))))
    (setq tab-bar-auto-width t
          tab-bar-auto-width-max '(200 20)
          tab-bar-auto-width-min '(200 20))
    (setq tab-bar-new-tab-choice "*enlight*")
    (setq tab-bar-show t))
;;;; Vertico-style IComplete
(use-package icomplete
    :demand t
    :bind (:map icomplete-minibuffer-map
	            ("RET"    . icomplete-force-complete-and-exit)
	            ("M-RET"  . icomplete-fido-exit)
	            ("TAB"    . icomplete-force-complete)
	            ("DEL"    . icomplete-fido-backward-updir)
	            ("M-."    . embark-act) ; mostly useful in case you don't have evil mode enabled (if you do, just do {ESC g .})
	            ("<down>" . icomplete-forward-completions)
	            ("<up>"   . icomplete-backward-completions))
    :config
    ;; remove arbitrary optimization limits that make icomplete
    ;; feel old-fashioned
    (setq icomplete-delay-completions-threshold 0)
    (setq icomplete-max-delay-chars 0)
    (setq icomplete-compute-delay 0)
    (setq icomplete-show-matches-on-no-input t)
    (setq icomplete-hide-common-prefix nil)
    (setq icomplete-prospects-height 15)
    (setq icomplete-with-completion-tables t)
    (icomplete-vertical-mode 1))
;;;; Recentf
(use-package recentf
    :custom
    (recentf-max-menu-items 25)
    (recentf-max-saved-items 25)
    (recentf-auto-cleanup 'never)
    :config
    (recentf-mode))
;;; ======Basic Packages======
(defun core/usability-layer ()
    "Loads the core packages needed to make Emacs more usable in the
  modern day.

  Loads:
  - `which-key' to show what keys can be pressed next at each stage of a
  key combination
  - `helpful' to give slower, but better, documentation for Emacs Lisp
  - `icomplete' with careful configuration (thanks to Prot!) to
    make it work just as nicely as Vertico
  - `marginalia', to add crucial metadata to icomplete completion
    candidates
  - `consult', for the ability to use icomplete to find things in
  minibuffers (useful for xref)
  - `elisp-def', `elisp-demos', and `highlight-defined' to make
  the experience of configuring your editor much nicer.
  - `embark' to offer a powerful Hyperbole-like experience with
    better integration."

;;;; Minibuffer completion and searching improvement packages
    (use-package marginalia
        :after icomplete
        :init
        (marginalia-mode))

    (use-package orderless
        :after icomplete
        :init
        (setq completion-styles '(orderless flex substring)
	          orderless-component-separator "-"
              orderless-matching-styles '(orderless-literal orderless-regexp)
	          completion-category-defaults nil
	          completion-category-overrides '((file (styles partial-completion)))))

    (use-package consult
        :commands (consult-grep consult-ripgrep consult-man consult-theme)
        :bind (("M-g i"   . #'consult-imenu) ;; override regular imenu
               ("M-s r"   . #'consult-ripgrep)
               ("M-s f"   . #'consult-grep)
               ("C-x C-r" . #'consult-recent-file))
        :config
        ;; We also want to use this for in-buffer completion, which icomplete can't do alone
        (setq xref-show-xrefs-function #'consult-xref
	          xref-show-definitions-function #'consult-xref)
        (setq completion-in-region-function
	          (lambda (&rest args)
                  (apply (if fido-mode
                                 #'consult-completion-in-region
		                     #'completion--in-region)
                         args))))
;;;; Better help messages and popups
    (use-package helpful
        :commands (helpful-key helpful-callable helpful-command helpful-variable))

    (use-package which-key
        :init (which-key-mode)
        :diminish which-key-mode
        :custom
        (which-key-idle-delay 0.1)
        (which-key-idle-secondary-delay nil)
        (which-key-sort-order #'which-key-key-order-alpha)
        :config
        (which-key-enable-god-mode-support))
;;;; Better Emacs Lisp editing experience

    (use-package elisp-def
        :hook (emacs-lisp-mode . elisp-def-mode))

    (use-package elisp-demos
        :after (helpful)
        :config
        (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

    (use-package highlight-defined
        :hook (emacs-lisp-mode . highlight-defined-mode))

    (use-package outline
        :hook ((prog-mode . outline-minor-mode)
	           (text-mode . outline-minor-mode))
        :config
        (quake-emacs-define-key global-map
            "C-c C-o" (cons "Outline/Folding..." outline-mode-prefix-map))
        (add-hook 'outline-minor-mode-hook (lambda ()
					                           (outline-show-all))))
;;;; Better peformance using asynchronous processing with subordinate Emacs processes 
    (use-package async
        :commands (async-start async-start-process))
;;;; Org-Mode improvements
    (use-package htmlize
        :after (org-mode))
    (use-package toc-org
        :hook (org-mode . toc-org-mode))
;;;; Embark
    (use-package embark
        :commands (embark-act embark-dwim)
        :bind (:map embark-general-map
		            ("G" . embark-internet-search)
		            ("O" . embark-default-action-in-other-window))
        :config
;;;;; Add useful Hyperbole-style actions to Embark
;;;;;; Search DuckDuckGo for the given term
        (defun embark-internet-search (term)
            (interactive "sSearch Term: ")
            (browse-url
             (format "https://duckduckgo.com/search?q=%s" term)))
;;;;;; Run default action in another Emacs window
        (defun embark-default-action-in-other-window ()
            "Run the default embark action in another window."
            (interactive))

        (cl-defun run-default-action-in-other-window
	            (&rest rest &key run type &allow-other-keys)
            (let ((default-action (embark--default-action type)))
	            (split-window-below) ; or your preferred way to split
	            (funcall run :action default-action :type type rest)))

        (setf (alist-get 'embark-default-action-in-other-window
		                 embark-around-action-hooks)
	          '(run-default-action-in-other-window))
;;;;;; GNU Hyperbole-style execute textual representation of keyboard macro
        (defun embark-kmacro-target ()
            "Target a textual kmacro in braces."
            (save-excursion
	            (let ((beg (progn (skip-chars-backward "^{}\n") (point)))
	                  (end (progn (skip-chars-forward "^{}\n") (point))))
	                (when (and (eq (char-before beg) ?{) (eq (char-after end) ?}))
	                    `(kmacro ,(buffer-substring-no-properties beg end)
		                         . (,(1- beg) . ,(1+ end)))))))

        (add-to-list 'embark-target-finders 'embark-kmacro-target)

        (defun embark-kmacro-run (arg kmacro)
            (interactive "p\nsKmacro: ")
            (kmacro-call-macro arg t nil (kbd kmacro)))

        (defun embark-kmacro-name (kmacro name)
            (interactive "sKmacro: \nSName: ")
            (let ((last-kbd-macro (kbd kmacro)))
	            (kmacro-name-last-macro name)))

        (defvar-keymap embark-kmacro-map
            :doc "Actions on kmacros."
            :parent embark-general-map
            "RET" #'embark-kmacro-run
            "n" #'embark-kmacro-name)

        (add-to-list 'embark-keymap-alist '(kmacro . embark-kmacro-map))))


;;; ======Non-Emacs Keybindings======
;;;; Custom keybinding macros, to replace `general'
(defmacro quake-emacs-define-key (keymaps &rest args)
    "Define the given keys in ARGS for the given KEYMAPS.
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
                                    collect `(define-key ,keymap (kbd ,key) ,def))))
        (error "Expected even number of arguments in ARGS so every key chord has a corresponding definition.")))
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
(defmacro quake-emacs-create-keymap (&rest args)
    "Create a new keymap with the given key definitions and return it.
Uses the same syntax and semantics as `quake-emacs-define-key'."
    (declare (indent defun))
    (cl-with-gensyms (keymap)
        `(let ((,keymap (make-keymap)))
             (quake-emacs-define-key ,keymap ,@args)
             ,keymap)))
(defmacro quake-evil-create-keymap (states &rest args)
    "Create a new keymap with the given key definitions and return it.
Uses the same syntax and semantics as `quake-emacs-define-key'."
    (declare (indent defun))
    (cl-with-gensyms (keymap)
        `(let ((,keymap (make-keymap)))
             (quake-evil-define-key ,states ,keymap ,@args)
             ,keymap)))
;;;; Core largely unopinionated evil-mode layer
(defun core/editor-layer ()
    "'Emacs is a great OS, if only it had a good editor.' With
  the powerful text-object based command language of Vim, and the
  flexibility of Emacs, at your command, Evil is that editor.

  Loads:

  - `evil', the Emacs editor of choice
  - `evil-collection', to integrate Evil mode with everything else
  - `evil-cleverparens', to give you proper S-expr editing
    capabilities, since you'll be doing a lot of that"

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
        
;;;;; Custom evil mode key bindings
        ;; Make :q close the buffer and window, not quit the entire
        ;; Emacs application (we never leave Emacs!)
        (global-set-key [remap evil-quit] 'kill-buffer-and-window)

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
            "gC" 'uncomment-region
            ;; keybindings for outline mode
            "TAB" 'evil-toggle-fold))

    (use-package evil-collection
        :after (evil)
        :config
        (evil-collection-init))

    (use-package evil-cleverparens
        :after (evil)
        :hook ((lisp-mode . evil-cleverparens-mode)
               (emacs-lisp-mode . evil-cleverparens-mode))) 

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
;;;; God-Mode based leader key layer
(make-obsolete 'optional/devil-layer "an obsolete keybinding layer that required too much maintinence to be practical." "Jun 1st, 2024")

(defun optional/god-layer ()
    "This layer sets up and configures `god-mode' to act like a leader
key, so that you can take advantage of all existing Emacs
keybindings and documentation, while having something close to
the ergonomics of a true leader key. This is the recommended
configuration"
    (use-package god-mode :after (evil))
    (use-package evil-god-state
        :after (god-mode)
        :config
;;;;; Make which-key for the top level keybindings show up when you enter evil-god-state
        (add-hook 'evil-god-state-exit-hook
                  (lambda ()
                      (which-key--hide-popup)))
        (add-hook 'evil-god-state-entry-hook
                  (lambda ()
                      (which-key--create-buffer-and-show nil
                                                         nil
                                                         (lambda (x) (and (not (null (car x)))
                                                                          (not (null (cdr x)))))
                                                         "Top-level bindings")))
;;;;; Notes
        (quake-emacs-define-key global-map
            "C-c n" (cons "Notes"
                          (quake-emacs-create-keymap
                              "s"     'denote-silo
                              "c"     'org-capture
                              "l"     'org-store-link
                              "n"     'consult-notes
                              "i"     'denote-link-global
                              "S-I"   'denote-link-after-creating
                              "r"     'denote-rename-file
                              "k"     'denote-keywords-add
                              "S-K"   'denote-keywords-remove
                              "b"     'denote-backlinks
                              "S-B"   'denote-find-backlink
                              "S-R"   'denote-region)))
;;;;; Yasnippet
        (quake-emacs-define-key global-map
            "C-c &"    (cons "Code Snippets..."
                             (quake-emacs-create-keymap
                                 "n"   'yas-new-snippet
                                 "s"   'yas-insert-snippet
                                 "v"   'yas-visit-snippet-file)))
;;;;; General Quake-recommended keybindings
        (quake-emacs-define-key global-map
            "C-c p"   (cons "Profile..."
                            (quake-emacs-create-keymap
                                "t" 'consult-theme
                                "f" (cons "Open framework config"
                                          (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
                                "u" (cons "Open user config"
                                          (lambda () (interactive) (find-file "~/.quake.d/user.el")))
                                "r" 'restart-emacs
                                "l" (cons "Reload user config"
                                          (lambda () (interactive) (load-file "~/.emacs.d/init.el")))))
;;;;;; Opening things
            "C-c o"     (cons "Open..."
                              (quake-emacs-create-keymap
                                  "w"   'eww
                                  "a"   'org-agenda
                                  "="   'calc
                                  "s"   (cons "Open new shell"
                                              (lambda () (interactive)
                                                  (let ((new-shell-frame (make-frame)))
                                                      (select-frame new-shell-frame)
                                                      (funcall quake-term-preferred-command 'new))))
                                  "-"   'dired
                                  "t"   'toggle-frame-tab-bar
                                  "m"   'gnus-other-frame
                                  "d"   'word-processing-mode
                                  "S"   'scratch-window-toggle))
;;;;;; Top-level keybindings for convenience
            "C-~" 'shell-toggle
            "C-:" 'pp-eval-expression
            "C-;" 'execute-extended-command
;;;;;; File and directory manpulation
            "C-x C-x"   'delete-file
            "C-x C-S-x" 'delete-directory
;;;;;; Buffer manipulation
            "C-x S-K" 'kill-current-buffer
            "C-x B"   'ibuffer
;;;;;; Eglot
            "C-c l"   (cons "LSP Server..."
                            (quake-emacs-create-keymap
                                "s" 'eglot
                                "a" 'eglot-code-actions
                                "r" 'eglot-rename
                                "h" 'eldoc
                                "f" 'eglot-format
                                "F" 'eglot-format-buffer
                                "R" 'eglot-reconnect))

;;;;;; Helpful
            "C-h v" 'helpful-variable
            "C-h f" 'helpful-callable
            "C-h k" 'helpful-key
            "C-h x" 'helpful-command
            )
;;;;; Core keybindings that make all this work
        (defun escape-dwim ()
            "Kill, exit, escape, stop, everything, now, and put me back in the
current buffer in Normal Mode."
            (interactive)
            (evil-god-state-bail)
            (evil-normal-state)
            (keyboard-escape-quit))
        (global-set-key (kbd "<escape>") 'escape-dwim)
        (quake-evil-define-key (god) global-map
            "C-w"      evil-window-map
            "C-w C-u"  'winner-undo)
        (quake-evil-define-key (motion normal visual) override-global-map
            "<escape>" 'escape-dwim
            "SPC"      'evil-execute-in-god-state)))
;;; ======Task Specific Layers======
;;;; Coding layer
(defun task/coding-layer ()
    "All the basic components needed for a Visual Studio Code-style
  IDE-lite experience in Emacs... but better.

  Loads:
  - `magit', the powerful Git user interface that lets you do
  anything from trivial to complex git commands with just a few
  mnemonic keypresses, all with helpful command palettes to guide
  you on your way, and `diff-hl' so you can see what's changed in-editor
  - `treemacs' to get a high-level overview of your project
    without leaving whatever you're doing
  - `treesit-auto', to automatically install and use the tree-sitter mode
  for any recognized language, so you have IDE-class syntax
  highlighting for nearly anything, out of the box.
  - `corfu', the faster, slimmer, yet more featureful
  universal (available anywhere) auto-completion UI for Emacs
  - `apheleia', as an auto-formatter, so you never need to worry about your
  formatting not matching a project's again.
  - `yasnippet' and `yasnippet-corfu', so you don't have to type all
  that rote boilerplate"

;;;;; Version-control and project management
    ;; Emacs' entire selling point right here!
    (use-package magit
        :commands (magit-status magit-get-current-branch)
        :custom
        (magit-define-global-keybindings 'recommended)
        (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
        :config
                                        ; Escape quits magit help mode like I expect
        (quake-emacs-define-key transient-base-map
            "<escape>" #'transient-quit-one))

    (use-package diff-hl
        :hook ((prog-mode . diff-hl-mode)
               (dired-mode . diff-hl-dired-mode)))

    (use-package treemacs
        :commands (treemacs)
        :config
        (dolist (face (custom-group-members 'treemacs-faces nil))
            (set-face-attribute (car face) nil :inherit 'variable-pitch :height 120)))

    (use-package treemacs-evil
        :after (treemacs evil)
        :ensure t)
;;;;; Treesit and Eglot (LSP) configuration
    (customize-set-variable 'treesit-font-lock-level 4)

    (use-package treesit-auto
        :custom
        (treesit-auto-install 'prompt)
        :config
        ;; TODO: If we could get tree-sitter support for
        ;; emacs-lisp working out of the box (automatically
        ;; installed), then we could do away with
        ;; `evil-cleverparens' entirely, and just have a
        ;; consistent, generalized way of manipulating the AST of
        ;; basically every langauge. The only other thing needed
        ;; would be to implement generalized slurp/barf for tree
        ;; sitter text objects. See [[file://~/.emacs.d/init.el::493]]
        ;; 
        ;; (define-derived-mode elisp-ts-mode emacs-lisp-mode "ELisp[ts]"
        ;;     "Tree-sitter major mode for editing Emacs Lisp."
        ;;     :group 'rust
        ;;     (when (treesit-ready-p 'elisp)
        ;;         (treesit-parser-create 'elisp)
        ;;         (treesit-major-mode-setup)))
        ;; (setq elisp-tsauto-config
        ;;       (make-treesit-auto-recipe
        ;;        :lang 'elisp
        ;;        :ts-mode 'elisp-ts-mode
        ;;        :remap '(emacs-lisp-mode)
        ;;        :url "https://github.com/Wilfred/tree-sitter-elisp"
        ;;        :revision "main"
        ;;        :source-dir "src"
        ;;        :ext "\\.el\\'"))

        ;; (add-to-list 'treesit-auto-recipe-list elisp-tsauto-config)
        ;; (add-to-list 'treesit-auto-langs 'elisp)
        (treesit-auto-add-to-auto-mode-alist 'all)
        (global-treesit-auto-mode))

    (use-package eglot
        :commands (eglot eglot-ensure)
        :preface
        (add-hook 'prog-mode-hook (lambda ()
                                      (interactive)
                                      (unless (ignore-errors
                                                  (command-execute #'eglot-ensure))
                                          (message "Info: no LSP found for this file.")))) 
        :config
        (setq eglot-autoshutdown t
              eglot-sync-connect nil))

;;;;; Eglot-compatible Debug Adapter Protocol client (for more IDE shit)
    (use-package dape
        :commands (dape)
        :preface
        (setq dape-key-prefix nil)
        :init
        ;; To use window configuration like gud (gdb-mi)
        (setq dape-buffer-window-arrangement 'right)
        :config
        ;; To not display info and/or buffers on startup
        (remove-hook 'dape-on-start-hooks 'dape-info)
        (remove-hook 'dape-on-start-hooks 'dape-repl)

        ;; To display info and/or repl buffers on stopped
        (add-hook 'dape-on-stopped-hooks 'dape-info)
        (add-hook 'dape-on-stopped-hooks 'dape-repl)

        ;; Kill compile buffer on build success
        (add-hook 'dape-compile-compile-hooks 'kill-buffer)

        ;; Save buffers on startup, useful for interpreted languages
        (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t))))
;;;;; An actually good completion-at-point UI for completion inside buffers
    (defun corfu-enable-in-minibuffer ()
        "Enable Corfu in the minibuffer."
        (when (local-variable-p 'completion-at-point-functions)
            (setq-local corfu-echo-delay nil) ;; Disable automatic echo and popup
            (corfu-mode 1)))

    (use-package corfu
        :after (orderless)
        :hook ((prog-mode . corfu-mode)
	           (comint-mode . corfu-mode)
               (eshell-mode . corfu-mode)
               (latex-mode . corfu-mode)
               (org-mode . corfu-mode)
               (markdown-mode . corfu-mode)
	           (minibuffer-setup . corfu-enable-in-minibuffer))
        ;; Optional customizations
        :custom
        (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
        (corfu-auto t)  ;; Enable auto completion
        (corfu-separator ?\s) ;; Orderless field separator
        (corfu-quit-no-match 'separator)
        (corfu-auto-delay 0.15)
        (corfu-auto-prefix 2)
        (corfu-popupinfo-delay 0.3)
        (corfu-popupinfo-direction 'right)
        :config
;;;; Turn off return accepting completions!!
        (define-key corfu-map (kbd "RET") nil)
        (defun corfu-popupinfo-start ()
            (require 'corfu-popupinfo)
            (set-face-attribute 'corfu-popupinfo nil :inherit 'variable-pitch)
            (corfu-popupinfo-mode))
        (add-hook 'corfu-mode-hook 'corfu-popupinfo-start)) 

;;;;; Project- and language-aware autoformatting
    ;; Global autoformatting
    (use-package apheleia
        :config
        (setf (alist-get 'prettier apheleia-formatters)
	          '("prettier" file))
        (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
        (add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier))
        :hook (prog-mode . apheleia-mode))

    (use-package editorconfig :hook (prog-mode . editorconfig-mode))
;;;;; Snippets
    (use-package yasnippet
        :hook (prog-mode . yas-minor-mode)
        :config
        (yas-reload-all))

    (use-package yasnippet-capf
        :after (corfu yasnippet)
        :config
        (add-to-list 'completion-at-point-functions #'yasnippet-capf)))
;;;; Writing layer
(defun task/writing-layer ()
    "If you're like me and you use Emacs to write blog posts and/or
  fiction, a good focus mode is priceless.

  Loads:
  - `pandoc-mode' one mode to rule them all for managing
    conversions and compilations of all your files!
  - `visual-fill-column' for dealing with those line-paragraphs
  - `darkroom', the focus mode of your dreams
  - `flymake-proselint', to help you improve your prose
  - `latex-preview-pane', so if you're writing LaTeX, you can see
  what it will produce"
;;;;; Set up org mode and evil-org
    (use-package org
        :commands (org-mode)
        :config
        (add-to-list 'org-agenda-files quake-org-home-directory)

        (set-face-attribute 'org-level-1 nil :height 2.0)
        (set-face-attribute 'org-level-2 nil :height 1.7)
        (set-face-attribute 'org-level-3 nil :height 1.4)
        (set-face-attribute 'org-level-4 nil :height 1.1)
        (set-face-attribute 'org-level-5 nil :height 1.0)

        (setq org-ellipsis "  " ;; folding symbol
	          org-startup-indented t
	          org-image-actual-width (list 300) ; no one wants gigantic images inline
	          org-hide-emphasis-markers t         
              org-pretty-entities nil ; part of the benefit of lightweight markup is seeing these 
	          org-agenda-block-separator ""
	          org-fontify-whole-heading-line t ; don't fontify the whole like, so tags don't look weird
	          org-fontify-done-headline t
	          org-fontify-quote-and-verse-blocks t)

        (setq org-capture-templates
              '(("t" "Todo" entry (file+headline (file-name-concat quake-org-home-directory "todo.org") "Tasks")
                 "* TODO %?\n  %i\n  %a")
                ("j" "Journal" entry (file+datetree (file-name-concat quake-org-home-directory "journal.org"))
                 "* %?\nEntered on %U\n  %i\n  %a"))))

    (use-package evil-org
        :after org
        :hook (org-mode . evil-org-mode))
;;;;; Typesetting packages (Latex, pandoc)
    (use-package pandoc-mode
        :hook ((markdown-mode . pandoc-mode)
	           (org-mode . pandoc-mode)
	           (latex-mode . pandoc-mode)
	           (doc-view-mode . pandoc-mode)))

    (use-package latex-preview-pane
        :commands (latex-preview-pane-mode latex-preview-pane-enable))
;;;;; Fully-fledged word processing minor mode
    (use-package flymake-proselint
        :hook (word-processing-mode . flymake-proselint-setup))

    (use-package darkroom
        :commands (darkroom-mode darkroom-tentative-mode))

    (define-minor-mode word-processing-mode
        "Toggle Word Processing mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
disables it. From Lisp, argument omitted or nil enables the mode,
`toggle' toggles the state.

When Word Processing mode is enabled, `darkroom-mode' is
triggered for a distraction-free writing experience. In addition,
column numbers, ligatures, prettified symbols, and fringes are
disabled, `buffer-face-mode' is enabled to set the current buffer
face to iA Writer Quattro V or your choice of writing-specific
faces, and the flymake `proselint' backend is enabled."
        :init-value nil
        :lighter " Word Processing"
        (setq line-spacing 0.1)
        (column-number-mode -1)
        (ligature-mode -1)
        (prettify-symbols-mode -1)
        ;; Less distracting UI
        (setq left-fringe-width 0)
        (setq right-fringe-width 0)
        (if (and (boundp 'darkroom-mode) darkroom-mode)
	            (progn
	                (darkroom-mode -1)
	                (buffer-face-mode -1))
            (progn
	            (darkroom-mode 1)
	            (buffer-face-mode 1)))

        ;; Proselint
        (when (fboundp 'flymake-proselint-setup)
            (flymake-mode))
        
        ;; Spellcheck
        (flyspell-mode)))

;;;; Zettelkasten note-taking layer
(defun task/notes-layer ()
    "For those who take notes in Emacs without being tied down to any
  one markup language or program.

  - `denote', for a simple, fast but feature-complete zettelkesten
  note-taking solution that optionally integrates well with org
  mode, that is more general than org-roam and uses only
  plain-text files.
  - `consult-notes' to have a metadata-rich, integrated way to find
  your notes."
    (use-package denote
        :commands (denote consult-notes denote-link-global denote-link-after-creating denote-region denote-silo)
        :custom
        (denote-known-keywords '())
        (denote-infer-keywords t)
        (denote-prompts-with-history-as-completion t)
        (denote-prompts '(title keywords))
        (denote-backlinks-show-context t)
        :preface
        (setq denote-directory quake-org-home-directory)

        ;; NOTE: I initially had a much simpler implementation, but
        ;; Prot suggested this was safer, since I'm defining my own
        ;; link function instead of just fucking with core denote
        ;; sanity-checking functions. See:
        ;; https://github.com/protesilaos/denote/issues/364
        ;; 
        ;; FIXME 2: this *does not* buttonize Denote links in
        ;; non-markup-language buffers automatically, except for
        ;; right after you insert that link. This is fine for now
        ;; though, because you can use `embark' on org-mode links
        ;; whether or not they are buttonized.
        (defun denote-link-global (file file-type description &optional id-only)
            "Like the `denote-link', but works in any buffer.
The FILE, FILE-TYPE, DESCRIPTION, and ID-ONLY have the same meaning as
in `denote-link'."
            (interactive
             (let* ((file (denote-file-prompt nil "Link to FILE"))
	                (file-type (denote-filetype-heuristics buffer-file-name))
	                (description (when (file-exists-p file)
			                         (denote--link-get-description file))))
                 (list file file-type description current-prefix-arg)))
            (unless (file-exists-p file)
                (user-error "The linked file does not exists"))
            (let ((beg (point)))
                (denote--delete-active-region-content)
                (insert (denote-format-link file description file-type id-only))
                (unless (derived-mode-p 'org-mode)
	                (make-button beg (point) 'type 'denote-link-button))))

        ;; Integrate Denote with org-capture
        (with-eval-after-load 'org-capture
            (add-to-list 'org-capture-templates
                         '("n" "New note (with Denote)" plain
                           (file denote-last-path)
                           #'denote-org-capture
                           :no-save t
                           :immediate-finish nil
                           :kill-buffer t
                           :jump-to-captured t)))
        
        ;; Declare directories with ".dir-locals.el" as a project so
        ;; we can use project.el to manage non-version controlled
        ;; projects, such as [Denote
        ;; silos](https://protesilaos.com/emacs/denote#h:15719799-a5ff-4e9a-9f10-4ca03ef8f6c5)
        (setq project-vc-extra-root-markers '(".dir-locals.el"))

        (defun denote-silo (dir)
            "Create a new directory DIR to function as a `denote' silo and add it to the project list."
            (interactive (list (read-directory-name "Silo directory: ")))
            (unless (not (make-directory dir t))
                (with-temp-file (file-name-concat dir ".dir-locals.el")
	                (insert (format (concat 
			                         ";;; Directory Local Variables            -*- no-byte-compile: t -*-"
			                         ";;; For more information see (info \"(emacs) Directory Variables\")"
			                         "((nil . ((denote-directory . \"%s\"))))")
			                        (expand-file-name dir)))))
            (add-to-list 'project--list `(,(expand-file-name dir)))
            (project--write-project-list)))

    (use-package consult-notes
        :after (denote)
        :config
        (consult-notes-denote-mode 1)
        ;; search only for text files in denote dir
        (setq consult-notes-denote-files-function (function denote-directory-text-only-files))))


;;; ======Aesthetic Packages======
;;;; Core Aesthetic Packages
(defun core/aesthetic-layer ()
    "If you're going to be staring at your editor all day, it might as well look nice.

  Loads:
  - `doom-themes', for an unparalleled collection of excellent
  themes, so you never have to go searching for a theme again
  - `mood-line', for an incredibly fast and lightweight emacs modeline
  that offers just the features you need for a great experience
  - `enlight', because a launchpad is always welcome
  - `eldoc-box', because documentation needs to look nice and appear
  next to your cursor so you don't have to move your eyes
  - `breadcrumb', so you don't get lost"
    ;; Although this is big and relatively slow (one of the slowest
    ;; things I include) doom's themes are just too good to pass up
    (use-package doom-themes
        :config
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t
	          doom-themes-enable-italic t))

    (advice-add 'load-theme :after (lambda (&rest args) (quake/set-aesthetics nil)))
    (add-hook 'after-make-frame-functions 'quake/set-aesthetics)
    (load-theme quake-color-theme t)

    ;; A super-fast modeline that also won't make me wish I didn't have eyes at least
    (use-package mood-line
        :after (evil doom-themes)
        :custom
        (mood-line-glyph-alist mood-line-glyphs-unicode)
        (mood-line-segment-modal-evil-state-alist 
         '((normal . ("Ⓝ" . font-lock-variable-name-face))
           (insert . ("Ⓘ" . font-lock-string-face))
           (visual . ("Ⓥ" . font-lock-keyword-face))
           (replace . ("Ⓡ" . font-lock-type-face))
           (motion . ("Ⓜ" . font-lock-constant-face))
           (operator . ("Ⓞ" . font-lock-function-name-face))
           (god . ("Ⓖ" . font-lock-function-name-face))
           (emacs . ("Ⓔ" . font-lock-builtin-face))) )
        (mood-line-format
         (mood-line-defformat
          :left
          (((mood-line-segment-modal) . "\t")
           ((or (mood-line-segment-buffer-status) " ") . " ")
           ((mood-line-segment-buffer-name) . "\t")
           ((mood-line-segment-cursor-position) . " ")
           (mood-line-segment-scroll))
          :right
          (((mood-line-segment-vc) . "\t")
           ((mood-line-segment-major-mode) . "\t")
           ((mood-line-segment-checker) . "\t"))))
        :config
        (mood-line-mode))

    (use-package enlight
        :custom
        (enlight-content
         (concat
          "    "
          (propertize "QUAKE EMACS" 'display (create-image "~/.emacs.d/banner-quake.png"))
          "\n"
          "\n"
          (propertize  (format "Started in %s\n" (emacs-init-time)) 'face '(:inherit 'font-lock-comment-face))
          (enlight-menu
           '(("Org Mode"
	          ("Org-Agenda (current day)" (org-agenda nil "a") "a")
	          ("Org-Agenda (TODOs)" (org-agenda nil "t") "o")
              ("" (consult-notes) "n"))
             ("Files"
              ("Recent Files" (consult-recent-file) "r")
              ("Writing" (read-file-name "Writing: " "~/Sync/Private/") "c")
              ("Documents" (read-file-name "Document: " "~/Documents/") "d"))
             ("Other"
	          ("Projects" project-switch-project "p")))))))

    (use-package eldoc-box
        :config
        (set-face-attribute 'eldoc-box-body nil :inherit 'variable-pitch)
        (set-face-foreground 'border (face-background 'mode-line))
        :hook (eldoc-mode . eldoc-box-hover-at-point-mode))

    ;; Pretty markdown formatting for eldoc-box
    (use-package markdown-mode
        :after (eldoc-box)
        :commands (markdown-mode)
        :custom
        (markdown-header-scaling t)
        (markdown-enable-math t)
        (markdown-fontify-code-blocks-natively t))

    (use-package breadcrumb
        :hook (eglot-managed-mode . breadcrumb-local-mode)))

;;;; Optional Aesthetic Packages
(defun optional/bling-layer ()
    "If you want your editor to wow the hipsters, or you just like
  looking at the fancy pretty colors, you need some bling.

  Loads:
  - `hl-todo', so you never miss those TODOs and FIXMEs
  - `nerd-icons', `nerd-icons-completion', and `nerd-icons-corfu',
  because there's really nothing better than a nice set of icons to
  spice things up, and we want integration *everywhere*
  - `ligature', because ligatures are cool"
    (use-package hl-todo
        :commands (hl-todo-mode)
        :init (add-hook 'prog-mode-hook #'hl-todo-mode))

    ;; Icons are nice to have! Nerd icons is faster and better
    ;; integrated (so less icon duplication between packages) with the
    ;; packages I'm using than all-the-icons
    (use-package nerd-icons
        :if (display-graphic-p)
        :custom (nerd-icons-font-family "Symbols Nerd Font Mono"))

    ;; Integrate these icons with marginalia
    (use-package nerd-icons-completion
        :after (marginalia nerd-icons)
        :config
        (nerd-icons-completion-mode)
        (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

    ;; Integrate them with corfu
    (use-package nerd-icons-corfu
        :after (corfu nerd-icons)
        :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

    ;; This assumes you've installed the package via MELPA.
    (use-package ligature
        :config
        ;; Enable traditional ligature support in eww-mode, if the
        ;; `variable-pitch' face supports it
        (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
        ;; Enable all Cascadia Code ligatures in programming modes
        (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
					                         ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
					                         "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "======" "-<<"
					                         "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
					                         "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
					                         "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
					                         "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
					                         "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
					                         ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
					                         "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
					                         "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
					                         "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
					                         "\\\\" "://"))
        :hook (prog-mode . ligature-mode)))

;;; ======Appendix: Togglable Shell======
(defvar quake--existing-shell nil)

(defun shell-toggle ()
    (interactive)
    (unless (buffer-live-p quake--existing-shell)
        (setq quake--existing-shell nil))
    (let* ((existing-window (and quake--existing-shell (get-buffer-window quake--existing-shell))))
        (if existing-window
                (delete-window existing-window)
            (let ((display-buffer-alist `((,(rx "\*" (or "terminal" "shell" "eshell" "vterm") "\*")
                                           (display-buffer-in-side-window)
                                           (side . top)
                                           (window-height . 10)))))
                (unless quake--existing-shell
                    (setq quake--existing-shell (funcall quake-term-preferred-command
					                                     (when (eq quake-term-preferred-command 'term)
					                                         (getenv "SHELL")))))
                (display-buffer quake--existing-shell)
                (select-window (get-buffer-window quake--existing-shell))))))

(defun scratch-window-toggle ()
    (interactive)
    (let* ((scratch (get-scratch-buffer-create))
	       (existing-window (get-buffer-window scratch)))
        (if existing-window
	            (delete-window existing-window)
            (progn
	            (display-buffer-at-bottom scratch '((window-height . 25)))
	            (other-window 1)))))

;;; ======Load Layers======
;; no garbage collection during startup — we can amortize it later
;; Enable layers
(dolist (layer quake-enabled-layers)
    (setq start-time (current-time))
    (funcall layer)
    (message "Finished enabling layers %s in %.2f seconds" layer (float-time (time-since start-time))))

;; Fix the aesthetics
(quake/set-aesthetics nil)
(put 'erase-buffer 'disabled nil)
(profiler-stop)
