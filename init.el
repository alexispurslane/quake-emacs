;;; init.el --- implements the Quake Emacs distribution of Emacs -*- lexical-binding: t -*-
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

;;; ======Imported Packages======
(require 'cl-lib)
(require 'rx)
;;; ======Package Setup======
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(require 'use-package-ensure)
(setq use-package-always-ensure t
      package-enable-at-startup nil
      use-package-compute-statistics t)

(defgroup quake nil
    "A customization group for the Quake Emacs distribution."
    :prefix "quake")

;;; ======User-Modifiable Variables======
(defcustom quake-enabled-layers
    '(core/usability-layer
      core/editor-layer
      task/coding-layer
      task/writing-layer
      task/notes-layer
      core/aesthetic-layer
      optional/bling-layer
      ;; optional/blog-layer
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
      ("s" . ("conditional" "loop" "assignment" "call" "block" "statement"))
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
    (setq inhibit-startup-message t               ; we're going to have our own dashboard
	  visible-bell t                          ; nobody likes being beeped at
	  make-backup-files nil                   ; don't litter all over the place
	  lisp-body-indent 4                      ; four space tabs
	  vc-follow-symlinks t                    ; we'll always want to follow symlinks
	  warning-minimum-level :emergency        ; don't completely shit the bed on errors
	  display-line-numbers 'relative          ; whether you use evil or not, these are useful
	  custom-file "~/.emacs.d/custom.el")     ; dump all the shit from custom somewhere else
    (setq-default fill-column 65)                 ; this will be used in reading modes, so set it to something nice
    (setq tab-always-indent 'complete)            ; more modern completion behavior
    (setq read-file-name-completion-ignore-case t ; ignore case when completing file names
          read-buffer-completion-ignore-case t    ; ignore case when completing buffer names
          completion-ignore-case t)               ; fucking ignore case in general!
    (setopt use-short-answers t)                  ; so you don't have to type out "yes" or "no" and hit enter
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; people are used to ESC quitting things
    (setq eldoc-idle-delay 0.8)                   ; w/ eldoc-box/an LSP, idle delay is by default too distracting
    (setq display-line-numbers-width-start t)     ; when you open a file, set the width of the linum gutter to be large enough the whole file's line numbers
    (setq-default indent-tabs-mode nil)           ; prefer spaces instead of tabs
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))) ; show dashboard in new frames
;;;;; Disabling ugly and largely unhelpful UI features 
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;;;; Enable some modes that give nicer, more modern behavior
    (setq pixel-scroll-precision-interpolate-mice t
	  pixel-scroll-precision-interpolate-page t)
    (pixel-scroll-precision-mode 1)        ; smooth scrolling
    (cua-mode t)                           ; Ctrl-C, Ctrl-V, etc
    (winner-mode 1)                        ; better window manipulation
    (savehist-mode 1)                      ; remember commands
    (column-number-mode)                   ; keep track of column number for the useful modeline readout
    (global-visual-line-mode)              ; wrap lines at end of window
;;;;; A basic programmming mode to build off of that adds some expected things
    (add-hook 'prog-mode-hook (lambda ()
				  (prettify-symbols-mode 1)
				  (display-line-numbers-mode 1)
				  (setq display-line-numbers 'relative)
				  (hl-line-mode t)
				  (electric-pair-mode)))
;;;;; Customizing the built-in tab-bar to look nice
    (setq tab-bar-auto-width nil)
    (setq tab-bar-new-tab-choice "*dashboard*")
    (setq tab-bar-button-relief 0)
    (setq tab-bar-show nil)
    (tab-bar-mode 1)
    (defun core/current-tab-name ()
	(alist-get 'name (tab-bar--current-tab)))
;;;;; Performance tuning
    (setq gc-cons-percentage 0.2)
;;;;;; Optimize font-locking for greater responsiveness
    (setq jit-lock-stealth-time 0.2
          jit-lock-defer-time 0.0
          jit-lock-context-time 0.2
          jit-lock-stealth-load 200)
;;;;;; Optimize for long lines. 
    (setq-default bidi-paragraph-direction 'left-to-right ; assume we're using LtR text unless explicitly told otherwise
                  bidi-inhibit-bpa t)                     ; turn off bidirectional paren display algorithm, it is expensive
;;;;;; Faster minibuffer
    (defun setup-fast-minibuffer ()
        (setq gc-cons-threshold most-positive-fixnum))
    (defun close-fast-minibuffer ()
        (setq gc-cons-threshold (* 8 1024 1024)))

    (add-hook 'minibuffer-setup-hook #'setup-fast-minibuffer)
    (add-hook 'minibuffer-exit-hook #'close-fast-minibuffer)
;;;; Fonts
    (set-display-table-slot
     standard-display-table
     'selective-display
     (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
	 (vconcat (mapcar (lambda (c) (+ face-offset c)) " ")))))
;;;; Vertico-style IComplete
(use-package icomplete
    :demand t
    :bind (:map icomplete-minibuffer-map
		("RET"    . icomplete-force-complete-and-exit)
		("M-RET"  . icomplete-fido-exit)
		("TAB"    . icomplete-force-complete)
		("DEL"    . icomplete-fido-backward-updir)
		("C-RET"  . embark-act)
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
    (recentf-auto-cleanup 10))
(use-package tramp
    :commands (tramp-mode))
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
	(setq completion-styles '(orderless basic)
	      orderless-component-separator "-"
	      completion-category-defaults nil
	      completion-category-overrides '((file (styles partial-completion)))))

    (use-package consult
        :commands (consult-grep consult-ripgrep consult-man)
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
	(which-key-sort-order #'which-key-key-order-alpha))
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
	(add-hook 'outline-minor-mode-hook (lambda ()
					       (outline-show-all))))
;;;; Better peformance using asynchronous processing with subordinate Emacs processes 
    (use-package async
	:commands (async-start async-start-process))
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


;;; ======Evil Mode Layer======
(defun core/editor-layer ()
    "'Emacs is a great OS, if only it had a good editor.' With
  the powerful text-object based command language of Vim, and the
  flexibility of Emacs, at your command, Evil is that editor.

  Loads:

  - `evil', the Emacs editor of choice
  - `evil-collection', to integrate Evil mode with everything else
  - `general', because I would otherwise have to invent my own
  keybinding system, and an extensive set of leader key
  keybindings, so you can control Emacs from the comfort of your
  leader key"

;;;; Evil mode itself (and associated integrations)
    (use-package evil
        :custom
        (evil-want-integration t)
        (evil-want-keybinding nil)
        (evil-want-C-u-scroll t)
        (evil-want-C-i-jump nil)
        (evil-undo-system 'undo-redo)
	(evil-kill-on-visual-paste nil) ;; oh thank god
        :config
        (evil-mode 1)
        
        ;; Make :q close the buffer and window, not quit the entire
        ;; Emacs application (we never leave Emacs!)
        (global-set-key [remap evil-quit] 'kill-buffer-and-window)

        ;; Override evil mode's exceptions to defaulting to normal-mode
        (evil-set-initial-state 'messages-buffer-mode 'normal)
        (evil-set-initial-state 'dashboard-mode 'normal)

        ;; set leader key in all states
        (evil-set-leader nil (kbd "C-SPC"))

        ;; set leader key in normal state
        (evil-set-leader 'normal (kbd "SPC")))

    (use-package evil-collection
        :after (evil)
        :config
        (evil-collection-init))
;;;; Custom evil mode key bindings
    (use-package general
        ;; PERF: Loading `general' early make Emacs very slow on startup.
        :after (evil evil-collection)
        :config
        ;; Advise `define-key' to automatically unbind keys when necessary.
        (general-auto-unbind-keys)
        ;; Set up some basic equivalents (like `general-nmap') with short named
        ;; aliases (like `nmap') for VIM mapping functions.
        (general-evil-setup t)

	;;;;; CUA integration
        ;; We want to be able to use ctrl-v and ctrl-c just for
        ;; convenience/user-friendliness, especially since ctrl-shift-v
        ;; doesn't work in evil, unlike (terminal) vim
        (general-imap
	    "C-c"    #'cua-copy-region
	    "C-v"    #'cua-paste
	    "C-x"    #'cua-cut-region
	    "C-z"    #'undo
	    "C-y"    #'undo-redo
	    "M-RET"  #'outline-insert-heading)
;;;;; Miscillanious useful keybindings for emacs capabilities
        (general-nvmap
	    "ga"   #'embark-act
	    "g RET" #'embark-dwim
	    ;; fill-region >> vim gqq
	    "gq"   #'fill-region-as-paragraph
	    ;; Support for visual fill column mode and visual line mode
	    ;; Make evil-mode up/down operate in screen lines instead of logical lines
	    "j"    #'evil-next-visual-line
	    "k"    #'evil-previous-visual-line
	    ;; outline keybindings
	    "gh"   #'outline-up-heading
	    "gj"   #'outline-forward-same-level
	    "gk"   #'outline-backward-same-level
	    "gl"   #'outline-next-visible-heading
	    "gu"   #'outline-previous-visible-heading)

        (general-nmap
	    ;; tab bar mode
	    "gR" #'tab-rename
	    "gn" #'tab-bar-new-tab
	    "gx" #'tab-bar-close-tab
	    "gX" #'tab-bar-close-other-tabs
	    ;; Nice commenting
	    "gc" #'comment-region
	    "gC" #'uncomment-region
	    ;; keybindings for outline mode
	    "TAB" #'evil-toggle-fold)
;;;;; Create the mode-specific leader key mapping function
        (general-create-definer +core--internal-local-map!
	    :states '(insert emacs visual normal)
	    :keymaps 'override              
	    :prefix "SPC m"      
	    :global-prefix "M-SPC m")
;;;;; Add mode-specific keybindings
        (add-hook 'emacs-lisp-mode-hook
                  (lambda ()
		      (+core--internal-local-map!
                          "e" #'eval-last-sexp
                          "d" #'eval-defun
                          "b" #'eval-buffer
                          "r" #'eval-region)))

	(add-hook 'pandoc-mode-hook
		  (lambda ()
		      (+core--internal-local-map!
			  "p" #'pandoc-main-hydra/body)))

	(add-hook 'outline-minor-mode
		  (lambda ()
		      (+core--internal-local-map!
			  "j"  #'outline-move-subtree-down
			  "k"  #'outline-move-subtree-up
			  "h"  #'outline-promote
			  "l"  #'outline-demote)))

	(add-hook 'org-mode-hook
		  (lambda ()
		      (+core--internal-local-map!
			  "p"     #'org-static-blog-publish-async
			  "L"     #'org-insert-link)))
;;;;; Spacemacs/Doom-like evil mode leader key keybindings
        ;; gobal keybindgs that are truly global
        (general-create-definer tyrant-def
	    :states '(normal insert motion emacs)
	    :keymaps 'override
	    :prefix "SPC"
	    :non-normal-prefix "M-SPC")

        ;; Define the built-in global keybindings — this is the heart of this editor!
        (tyrant-def
;;;;;; Top level functions
	    "SPC"  '(execute-extended-command :wk "M-x")
	    ":"    '(pp-eval-expression :wk "Eval expression")
	    ";"    #'project-find-file
	    "u"    '(universal-argument :wk "C-u")
	    "C"    #'universal-coding-system-argument
	    "O"    #'other-window-prefix
	    "r"    #'restart-emacs
	    "~"    #'shell-toggle

;;;;;; Quit/Session
	    "q"    '(nil :wk "quit/session")
	    "qq"   #'save-buffers-kill-terminal
	    "qQ"   #'kill-emacs
	    "qS"   #'server-start
	    "qR"   #'recover-session
	    "qd"   #'desktop-read
	    "qs"   #'desktop-save
	    "qr"   #'restart-emacs

;;;;;; Files 
	    "f"    '(nil :wk "file") "fS"   '(write-file :wk "Save as ...")
	    "fi"   #'auto-insert
	    "ff"   #'find-file
	    "fs"   #'save-buffer
	    "ft"   #'recover-this-file
	    "fT"   #'recover-file
	    "fr"   #'consult-recent-file
	    "fa"   #'outline-show-all
	    "fh"   (lambda () (interactive) (outline-hide-sublevels 1))
	    "ft"   #'tab-switch

;;;;;; Personal Profile 
	    "P"    '(nil :wk "profile")
	    "Pf"   `(,(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
		     :wk "Open framework config")
	    "Pu"   `(,(lambda () (interactive) (find-file "~/.quake.d/user.el"))
		     :wk "Open user config")

;;;;;; Buffers 
	    "b"    '(nil :wk "buffer")
	    "bB"   #'switch-to-buffer-other-tab
	    "bb"   #'consult-buffer
	    "bI"   #'ibuffer
	    "bx"   #'bury-buffer
	    "bS"   #'save-some-buffers
	    "bM"   #'view-echo-area-messages
	    "bk"   `(,(lambda () (interactive) (kill-buffer (current-buffer)))
		     :wk "Kill this buffer")
	    "br"   '(revert-buffer :wk "Revert")
	    "bR"   '(rename-buffer :wk "Rename")
	    "bn"    '(switch-to-next-buffer :wk "Next buffer")
	    "bp"    '(switch-to-prev-buffer :wk "Previous buffer")
;;;;;;; Lines
	    "bl"   '(nil :wk "line")
	    "blk"  #'keep-lines ;; Will be overwritten with `consult-keep-lines'
;;;;;;; Bookmarks
	    "bm"   '(nil :wk "bookmark")
	    "bmm"  #'bookmark-set
	    "bmd"  #'bookmark-delete
;;;;;;; Files / Local variables
	    "bv"   '(nil :wk "locals")
	    "bvv"  '(add-file-local-variable :wk "Add")
	    "bvV"  '(delete-file-local-variable :wk "Delete")
	    "bvp"  '(add-file-local-variable-prop-line :wk "Add in prop line")
	    "bvP"  '(delete-file-local-variable-prop-line :wk "Delete from prop line")
	    "bvd"  '(add-dir-local-variable :wk "Add to dir-locals")
	    "bvD"  '(delete-dir-local-variable :wk "Delete from dir-locals")

;;;;;; Insert
	    "i"    '(nil :wk "insert")
	    "iu"   '(insert-char :wk "Unicode char")
	    "ip"   #'yank-pop ;; Will be overwritten with `consult-yank-pop'
	    "iei"  #'emoji-insert
	    "ie+"  #'emoji-zoom-increase
	    "ie-"  #'emoji-zoom-decrease
	    "ie0"  #'emoji-zoom-reset
	    "ied"  #'emoji-describe
	    "iel"  #'emoji-list
	    "ier"  #'emoji-recent
	    "iee"  #'emoji-search

;;;;;; Window
	    "w"    '(nil :wk "window")
	    "wd"   #'delete-window
	    "wD"   #'delete-windows-on
	    "wo"   #'delete-other-windows
	    "wm"   #'maximize-window
	    "wu"   #'winner-undo
	    "wU"   #'winner-redo
	    "wj"   #'evil-window-down
	    "wk"   #'evil-window-up
	    "wh"   #'evil-window-left
	    "wl"   #'evil-window-right
	    "ws"   #'split-window-vertically
	    "wv"   #'split-window-horizontally
	    "ww"   #'other-window

;;;;;; Applications (Open)
	    "o"    '(nil :wk "open")
	    "o-"   '(dired :wk "Dired") ;; Will be overwritten if dirvish is used
	    "ot"   #'treemacs
	    "oT"   #'toggle-frame-tab-bar
	    "od"   #'word-processing-mode
	    "op"   #'pandoc-mode
	    "o="   #'calc
	    "ow"   #'scratch-window-toggle
            "os" `(,(lambda () (interactive)
                        (let ((new-shell-frame (make-frame)))
                            (select-frame new-shell-frame)
                            (funcall quake-term-preferred-command 'new)))
                   :wk "Open new shell")

;;;;;; Search
	    "s"    '(nil :wk "search")
	    "si"    #'consult-imenu
	    "sr"    #'consult-ripgrep
	    "sf"    #'consult-find

;;;;;; Mode specific a.k.a. "local leader" 
	    "m"    '(nil :wk "mode-specific")

;;;;; Version Control
	    "g"    '(nil :wk "git/vc")
	    "gg"   #'magit

;;;;;; Toggle
	    "t"    '(nil :wk "toggle")
	    "td"   #'toggle-debug-on-error
	    "tr"   #'read-only-mode
	    "tl"   #'follow-mode
	    "tv"   #'visible-mode
	    "tf"   #'flymake-mode

;;;;;; Language Server
	    "l"    '(nil :wk "lsp and flymake")
	    "le"    #'eglot
	    "lr"    #'eglot-rename
	    "la"    #'eglot-code-actions
	    "lx"    #'eglot-code-action-extract
	    "lf"    #'eglot-code-action-quickfix
	    "l!"    #'consult-flymake
	    "ln"    #'flymake-goto-next-error
	    "lp"    #'flymake-goto-prev-error

;;;;;; Debug
	    "d"    '(nil :wk "debug")
	    "dG"   #'gdb
	    "dd"   #'dape

;;;;;; Notes
	    "n"    '(nil :wk "notes")
	    "ns"   #'denote-silo
	    "nc"   #'denote
	    "nn"   #'consult-notes
	    "ni"   #'denote-link-global
	    "nI"   #'denote-link-after-creating
	    "nr"   #'denote-rename-file
	    "nk"   #'denote-keywords-add
	    "nK"   #'denote-keywords-remove
	    "nb"   #'denote-backlinks
	    "nB"   #'denote-find-backlink
	    "nR"   #'denote-region

;;;;;; Help
	    "h"    '(nil :wk "help")
	    "hi"   #'info
	    "hg"   #'general-describe-keybindings

	    "he"   '(nil :wk "elisp/emacs")
	    "hes"  #'elisp-index-search
	    "hem"  #'info-emacs-manual
	    "hei"  #'Info-search

	    "hv"  #'helpful-variable
	    "hk"  #'helpful-key
	    "hc"  #'helpful-command
	    "hf"  #'helpful-callable
	    "hm"  #'describe-keymap
	    "hb"  #'describe-bindings
	    "hs"  #'describe-symbol
	    "hp"  #'describe-package

;;;;;; Project
	    "p"    '(nil :wk "project")
	    "pp"  #'project-switch-project
	    "pc"  #'project-compile
	    "pd"  #'project-find-dir
	    "pf"  #'project-find-file
	    "pk"  #'project-kill-buffers
	    "pb"  #'project-switch-to-buffer
	    "p-"  #'project-dired
	    "px"  #'project-execute-extended-command
;;;;;;; Compile / Test
	    "pc" #'project-compile
;;;;;;; Run
	    "pr"  '(nil :wk "run")
	    "prc" #'project-shell-command
	    "prC" #'project-async-shell-command
;;;;;;; Forget
	    "pF"  '(nil :wk "forget/cleanup")
	    "pFp" #'project-forget-project
	    "pFu" #'project-forget-projects-under
;;;;;;; Search / Replace
	    "ps"  '(nil :wk "search/replace")
	    "pss" #'project-search
	    "psn" '(fileloop-continue :wk "Next match")
	    "psr" #'project-query-replace-regexp
	    "psf" #'project-find-regexp))

;;;; Evil mode text object support
    (use-package evil-textobj-tree-sitter
        :after (evil evil-collection general)
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
				     (general-nvmap
                                         ;; go to next start
                                         ,(concat "[" key) '((lambda ()
                                                                 (interactive)
                                                                 (evil-textobj-tree-sitter-goto-textobj ,(car outer-query) t nil))
							     :which-key ,(concat "goto-textobj-" (car query) "-start"))
                                         ;; go to next end
                                         ,(concat "]" key) '((lambda ()
                                                                 (interactive)
                                                                 (evil-textobj-tree-sitter-goto-textobj ,(car outer-query) nil t))
							     :which-key ,(concat "goto-textobj-" (car query) "-end"))))
                                   into exprs

                                   finally (return exprs)))))
	(define-textobjs)))

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
        (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
        :config
                                        ; Escape quits magit help mode like I expect
        (general-define-key
         :keymaps 'transient-base-map
         "<escape>" 'transient-quit-one))

    (use-package diff-hl :hook (prog-mode . diff-hl-mode))
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
        (treesit-auto-add-to-auto-mode-alist 'all)
        (global-treesit-auto-mode))

    (with-eval-after-load 'eglot
        (setq eglot-autoshutdown t
	      eglot-events-buffer-size 0
	      eglot-sync-connect nil)
	(add-hook 'prog-mode-hook (lambda ()
				      (message "Tip: Press `SPC l e' to activate your LSP if you have one!"))))

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
	       (shell-mode . corfu-mode)
               (eshell-mode . corfu-mode)
	       (minibuffer-setup . corfu-enable-in-minibuffer))
        ;; Optional customizations
        :custom
        (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
        (corfu-auto t)                 ;; Enable auto completion
        (corfu-separator ?\s)          ;; Orderless field separator
        (corfu-quit-no-match 'separator)
        (corfu-auto-delay 0.15)
        (corfu-auto-prefix 2)
        (corfu-popupinfo-delay 0.3)
        (corfu-popupinfo-direction 'right)
        :config

        (defun corfu-popupinfo-start ()
	    (require 'corfu-popupinfo)
	    (set-face-attribute 'corfu-popupinfo nil :inherit 'variable-pitch)
	    (corfu-popupinfo-mode))
        (add-hook 'corfu-mode-hook #'corfu-popupinfo-start)) 

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
        :config (yas-reload-all))

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
	(set-face-attribute 'org-level-1 nil :height 2.0)
	(set-face-attribute 'org-level-2 nil :height 1.7)
	(set-face-attribute 'org-level-3 nil :height 1.4)
	(set-face-attribute 'org-level-4 nil :height 1.1)
	(set-face-attribute 'org-level-5 nil :height 1.0)

	(setq org-ellipsis "  " ;; folding symbol
	      org-startup-indented t
	      org-image-actual-width (list 300)      ; no one wants gigantic images inline
	      org-hide-emphasis-markers t         
              org-pretty-entities nil                  ; part of the benefit of lightweight markup is seeing these 
	      org-agenda-block-separator ""
	      org-fontify-whole-heading-line t     ; don't fontify the whole like, so tags don't look weird
	      org-fontify-done-headline t
	      org-fontify-quote-and-verse-blocks t))

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
        :config
        (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
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
  - `spacious-padding', so your user interface feels less like a
  cramped TTY and more like a modern editor. We can afford the
  screen real-estate
  - `mood-line', for an incredibly fast and lightweight emacs modeline
  that offers just the features you need for a great experience
  - `dashboard', because a launchpad is always welcome
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

    (load-theme quake-color-theme t)

    (use-package spacious-padding
        :after (doom-themes)
        :config
        (unless spacious-padding-mode 
            (spacious-padding-mode 1)))

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
           ((mood-line-segment-checker) . "\t")
	   (core/current-tab-name))))
        :config
        (mood-line-mode))

    (use-package dashboard
        :custom
        (dashboard-banner-logo-title "Lean, fast, focused, based on the latest tech. Welcome to Quake Emacs")
        (dashboard-startup-banner "~/.emacs.d/banner-quake.png")
        (dashboard-center-content t)
        (dashboard-vertically-center-content t)
        (dashboard-items '((recents   . 5)
                           (projects  . 5)))
        (dashboard-item-shortcuts '((recents   . "r")
				    (projects  . "p")))
        (dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
        (dashboard-set-heading-icons t)
        (dashboard-set-file-icons t)
        :config
        (dashboard-setup-startup-hook))

    (use-package eldoc-box
        :config
        (set-face-attribute 'eldoc-box-body nil :inherit 'variable-pitch)
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

;;;; A simple, lightweight static site generator layer, with added asynchronicity
(defun optional/blog-layer ()
    "Blog writing is one of the most common writing tasks there
is. With the existing `task/writing-layer' you should have all
you need to write blog entries, but in case you want to fully
generate and manage your entire blog from inside Emacs, then this
layer is for you.

Loads:
- `org-static-blog': A beautifully simple SSG for org. No
external programs, no templating languages, nothing to fiddle
with to procrastinate, just org-mode, Emacs, and Emacs Lisp."
    (use-package org-static-blog
	:commands (org-static-blog-publish org-static-blog-publish-file org-static-blog-mode))
    
    (define-minor-mode org-static-blog-watch-mode
	"Re-run `org-static-blog-publish-async' whenever the current file is saved."
	:init-value nil
	:lighter " Org-Static-Blog-Watch"
        (add-hook 'after-save-hook #'org-static-blog-publish-async nil t))

    (defun org-static-blog-publish-async ()
	(interactive)
	(async-start
	 `(lambda ()
	      (let ((start-time (current-time)))
		  (setq load-path ',load-path)
		  (require 'org-static-blog)
		  transfer variables without having to load this entire init.el file again
		  ,@(cl-loop for x in (cl-remove 'custom-group (custom-group-members group nil)
						 :key #'cadr)
			     for variable = (car x)
			     for value = (symbol-value variable)
			     collect `(setq ,variable ,value))
		  (org-static-blog-publish)
		  (format "%.2f" (float-time (time-since start-time)))))
	 (lambda (publish-time)
	     (message (format "Finished publishing blog after %s seconds" publish-time))))))

;;; ======Load Layers======
;; no garbage collection during startup — we can amortize it later
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer
 5 nil
 (lambda ()
     (setq gc-cons-threshold gc-cons-threshold-original)
     (makunbound 'gc-cons-threshold-original)
     (message "gc-cons-threshold and file-name-handler-alist restored")))

;; Enable layers
(dolist (layer quake-enabled-layers)
    (message (format "Enabling the %s layer" layer))
    (funcall (symbol-function layer)))

;; Set some final settings that should always take precidence

(defun quake/fix (frame)
    (interactive)
    (set-frame-parameter frame 'undecorated t)
    (set-face-attribute 'mode-line frame :inherit 'variable-pitch :height 120)
    (set-face-attribute 'mode-line-inactive frame :inherit 'mode-line))
(add-hook 'after-make-frame-functions #'quake/fix)
(quake/fix nil)

;;; ======Appendix: Togglable Shell======
(defvar existing-shell nil)

(defun shell-toggle ()
    (interactive)
    (let* ((existing-window (and existing-shell (get-buffer-window existing-shell))))
        (if existing-window
                (delete-window existing-window)
            (let ((display-buffer-alist `((,(rx "\*" (or "terminal" "shell" "eshell" "vterm") "\*")
                                           (display-buffer-in-side-window)
                                           (side . top)
                                           (window-height . 10)))))
                (unless existing-shell
                    (setq existing-shell (funcall quake-term-preferred-command
					          (when (eq quake-term-preferred-command 'term)
					              (getenv "SHELL")))))
                (display-buffer existing-shell)
                (select-window (get-buffer-window existing-shell))))))

(defun scratch-window-toggle ()
    (interactive)
    (let* ((scratch (get-scratch-buffer-create))
	   (existing-window (get-buffer-window scratch)))
	(if existing-window
		(delete-window existing-window)
	    (progn
		(display-buffer-at-bottom scratch '((window-height . 25)))
		(other-window 1)))))
