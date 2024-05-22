;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-compute-statistics t
      package-enable-at-startup nil)

(defvar quake-color-theme
    'doom-gruvbox
    "The theme quake loads and uses at startup.")

(defvar quake-evil-text-objects
    '(("f" . "function")
      ("s" . ("conditional" "loop" "assignment" "call" "block" "statement"))
      ("t" . "class")
      ("c" . "comment")
      ("a" . "parameter")
      ("T" . "test"))
    "The text objects added to evil mode at startup. A list of pairs,
where the first element is a string, KEY, and the second object is
either a string or a list containing the query to be made for
that text object minus the .inner and .outer qualifiers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BASE EMACS CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A few more useful configurations... 
(use-package emacs
    :init
    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete)

    ;; Less annoying emacs
    (setq inhibit-startup-message t
          visible-bell t
          make-backup-files nil
          lisp-body-indent 4
          vc-follow-symlinks t
          warning-minimum-level :emergency
          display-line-numbers 'relative
	  custom-file "~/.emacs.d/custom.el"
          fill-column 65)
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

    ;; Possibly cargo-cult optimization settings
    (setq jit-lock-stealth-time 0.2
	  jit-lock-defer-time 0.0
	  jit-lock-context-time 0.2
	  jit-lock-stealth-load 200)
    ;; Optimize for long lines. If you use a RtL language, just unset
    ;; these
    (setq-default bidi-paragraph-direction 'left-to-right
		  bidi-inhibit-bpa t)
    (setopt use-short-answers t)
    (customize-set-variable 'treesit-font-lock-level 4)

    ;; Fonts
    (when (find-font (font-spec :name "JetBrains Mono"))
        (set-face-attribute 'default nil :font "JetBrains Mono" :height 120))
    (when (find-font (font-spec :name "Cantarell"))
        (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120))
    (when (find-font (font-spec :name "iA Writer Quattro V"))
	(setq buffer-face-mode-face '(:family "iA Writer Quattro V")))

    ;; Nicer appearance
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (set-frame-parameter nil 'undecorated t)
    
    (column-number-mode)

    ;; Basic programmming mode to build off of
    (add-hook 'prog-mode-hook (lambda ()
				  (display-line-numbers-mode 1)
				  (setq display-line-numbers 'relative)
				  (hl-line-mode t)
				  (electric-pair-mode)))

    ;; Nicer behavior
    (cua-mode t)

    (pixel-scroll-precision-mode 1)
    (setq pixel-scroll-precision-use-momentum t
	  pixel-scroll-precision-large-scroll-height 40.0)

    (winner-mode 1)

    (recentf-mode 1)
    (savehist-mode 1)
    (setq recentf-max-menu-items 25
	  recentf-max-saved-items 25)

    ;; Ignore case!
    (setq read-file-name-completion-ignore-case t
	  read-buffer-completion-ignore-case t
	  completion-ignore-case t)

    (defun setup-fast-minibuffer ()
	(setq gc-cons-threshold most-positive-fixnum))
    (defun close-fast-minibuffer ()
	(setq gc-cons-threshold 800000))

    (add-hook 'minibuffer-setup-hook #'setup-fast-minibuffer)
    (add-hook 'minibuffer-exit-hook #'close-fast-minibuffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CORE USABILITY PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/usability-layer ()
    "Loads the core packages needed to make Emacs more usable in the
  modern day.

  Loads:
  - `which-key' to show what keys can be pressed next at each stage of a
  key combination
  - `helpful' to give slower, but better, documentation for Emacs Lisp
  - `icomplete' with careful configuration (thanks to Prot!) to
    make it work just as nicely as Vertico
  - `marginalia', to add crucial metadata to icomplete completion candidates
  - `consult', for the ability to use icomplete to find things in
  minibuffers (useful for xref)
  - `elisp-def', `elisp-demos', and `highlight-defined' to make
  the experience of configuring your editor much nicer. "
    (use-package which-key
        :init (which-key-mode)
        :diminish which-key-mode
        :custom
        (which-key-idle-delay 0.1)
        (which-key-idle-secondary-delay nil)
        (which-key-sort-order #'which-key-key-order-alpha))

    ;; Better docs are always good
    (use-package helpful
	:commands (helpful-key helpful-callable helpful-command helpful-variable))

    ;; Now that icomplete has a vertical interface, with a little
    ;; customization, Emacs's built-in completion capabilities can
    ;; feel just as modern and fast as Vertico! No need for external
    ;; dependencies for this!
    (use-package icomplete
	:hook (pre-command . fido-mode)
	:bind (:map icomplete-minibuffer-map
		    ("RET" . icomplete-force-complete-and-exit)
		    ("M-RET" . icomplete-fido-exit)
		    ("TAB" . icomplete-force-complete)
		    ("<down>" . icomplete-forward-completions)
		    ("C-n" . icomplete-forward-completions)
		    ("<up>" . icomplete-backward-completions)
		    ("C-p" . icomplete-backward-completions))
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

    (use-package marginalia
        :after icomplete
        ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
        ;; available in the *Completions* buffer, add it to the
        ;; `completion-list-mode-map'.
        :bind (:map minibuffer-local-map
                    ("M-A" . marginalia-cycle))

        ;; The :init section is always executed.
        :init

        ;; Marginalia must be activated in the :init section of use-package such that
        ;; the mode gets enabled right away. Note that this forces loading the
        ;; package.
        (marginalia-mode))

    (use-package orderless
        :after icomplete
        :init
	(setq completion-styles '(orderless basic)
	      completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))))

    ;; Vertical completion UI superpowers like grep!
    (use-package consult
	:commands (consult-grep consult-ripgrep consult-man)
        :config
        ;; We also want to use this for in-buffer completion, which vertico can't do alone
        (setq xref-show-xrefs-function #'consult-xref
              xref-show-definitions-function #'consult-xref)
        (setq completion-in-region-function
              (lambda (&rest args)
                  (apply (if fido-mode
                                 #'consult-completion-in-region
                             #'completion--in-region)
                         args))))

    ;; Emacs Lisp
    (use-package elisp-def
        :hook (emacs-lisp-mode . elisp-def-mode))

    (use-package elisp-demos
	:after (helpful)
        :config
        (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

    (use-package highlight-defined
        :hook (emacs-lisp-mode . highlight-defined-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CORE EDITING PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ;; Join the vim side :evil_grin:
    (use-package evil
        :custom
        (evil-want-integration t)
        (evil-want-keybinding nil)
        (evil-want-C-u-scroll t)
        (evil-want-C-i-jump nil)
        (evil-undo-system 'undo-redo)
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

    ;; A basic keymap for managing emacs using an evil mode leader key instead of
    ;; pure repetitive strain injury (copied from MinEmacs originally :))
    (use-package general
	;; PERF: Loading `general' early make Emacs very slow on startup.
	:after (evil evil-collection)
	:config
	;; Advise `define-key' to automatically unbind keys when necessary.
	(general-auto-unbind-keys)
	;; Set up some basic equivalents (like `general-nmap') with short named
	;; aliases (like `nmap') for VIM mapping functions.
	(general-evil-setup t)

        ;; We want to be able to use ctrl-v and ctrl-c just for
        ;; convenience/user-friendliness, especially since ctrl-shift-v
        ;; doesn't work in evil, unlike (terminal) vim
	(general-imap
	    "C-c" #'cua-copy-region
	    "C-v" #'cua-paste
	    "C-x" #'cua-cut-region
	    "C-z" #'undo
	    "C-y" #'undo-redo)

	(general-nvmap
	    ;; fill-region >> vim gqq
	    "gq" #'fill-region-as-paragraph
            ;; Support for visual fill column mode and visual line mode
            ;; Make evil-mode up/down operate in screen lines instead of logical lines
	    "j" #'evil-next-visual-line
	    "k" #'evil-previous-visual-line)

	(general-nmap
	    ;; Centaur tab (optional) supporj
	    "gt" #'centaur-tabs-forward
	    "gT" #'centaur-tabs-backward
	    ;; Nice commenting
	    "gc" #'comment-region
	    "gC" #'uncomment-region)

	(general-create-definer +core--internal-local-map!
	    :states '(insert emacs visual normal)
	    :keymaps 'override              
	    :prefix "SPC m"      
	    :global-prefix "M-SPC m")

	(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
		      (+core--internal-local-map!
			  "e" #'eval-last-sexp
			  "d" #'eval-defun
			  "b" #'eval-buffer
			  "r" #'eval-region)))

	;; gobal keybindgs that are truly global
	(general-create-definer tyrant-def
	    :states '(normal insert motion emacs)
	    :keymaps 'override
	    :prefix "SPC"
	    :non-normal-prefix "M-SPC")

	;; Define the built-in global keybindings
	(tyrant-def
	    ;; ====== Top level functions ======
	    "SPC"  '(execute-extended-command :wk "M-x")
	    ":"    '(pp-eval-expression :wk "Eval expression")
	    ";"    #'project-find-file
	    "u"    '(universal-argument :wk "C-u")
	    "C"    #'universal-coding-system-argument
	    "O"    #'other-window-prefix
	    "r"    #'restart-emacs
	    "~"    #'shell-toggle

	    ;; ====== Quit/Session ======
	    "q"    '(nil :wk "quit/session")
	    "qq"   #'save-buffers-kill-terminal
	    "qQ"   #'kill-emacs
	    "qS"   #'server-start
	    "qR"   #'recover-session
	    "qd"   #'desktop-read
	    "qs"   #'desktop-save
	    "qr"   #'restart-emacs

	    ;; ====== Files ======
	    "f"    '(nil :wk "file")
	    "fS"   '(write-file :wk "Save as ...")
	    "fi"   #'auto-insert
	    "ff"   #'find-file
	    "fs"   #'save-buffer
	    "ft"   #'recover-this-file
	    "fT"   #'recover-file
	    "fr"   #'consult-recent-file

	    ;; ====== Personal Profile ======
	    "P"    '(nil :wk "profile")
	    "Pf"   `(,(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
		     :wk "Open framework config")
	    "Pu"   `(,(lambda () (interactive) (find-file "~/.quake.d/user.el"))
		     :wk "Open user config")

	    ;; ====== Buffers ======
	    "b"    '(nil :wk "buffer")
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

	    ;; Lines
	    "bl"   '(nil :wk "line")
	    "blk"  #'keep-lines ;; Will be overwritten with `consult-keep-lines'
	    ;; Bookmarks
	    "bm"   '(nil :wk "bookmark")
	    "bmm"  #'bookmark-set
	    "bmd"  #'bookmark-delete
	    ;; Files / Local variables
	    "bv"   '(nil :wk "locals")
	    "bvv"  '(add-file-local-variable :wk "Add")
	    "bvV"  '(delete-file-local-variable :wk "Delete")
	    "bvp"  '(add-file-local-variable-prop-line :wk "Add in prop line")
	    "bvP"  '(delete-file-local-variable-prop-line :wk "Delete from prop line")
	    "bvd"  '(add-dir-local-variable :wk "Add to dir-locals")
	    "bvD"  '(delete-dir-local-variable :wk "Delete from dir-locals")

	    ;; ====== Insert ======
	    "i"    '(nil :wk "insert")
	    "iu"   '(insert-char :wk "Unicode char")
	    "ip"   #'yank-pop ;; Will be overwritten with `consult-yank-pop'
	    "ie"   #'emojify-insert-emoji

	    ;; ====== Window ======
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

	    ;; ====== Applications (Open) ======
	    "o"    '(nil :wk "open")
	    "o-"   '(dired :wk "Dired") ;; Will be overwritten if dirvish is used
	    "ot"   #'treemacs
	    "oT"   #'centaur-tabs-mode
	    "od"   #'darkroom-mode
	    "o="   #'calc

	    ;; ====== Search ======
	    "s"    '(nil :wk "search")
	    "si"    #'consult-imenu
	    "sr"    #'consult-ripgrep
	    "sf"    #'consult-find

	    ;; ======  Mode specific a.k.a. "local leader" ======
	    "m"    '(nil :wk "mode-specific")

	    ;; ====== VC ======
	    "g"    '(nil :wk "git/vc")
	    "gg"   #'magit

	    ;; ====== Toggle ======
	    "t"    '(nil :wk "toggle")
	    "td"   #'toggle-debug-on-error
	    "tr"   #'read-only-mode
	    "tl"   #'follow-mode
	    "tv"   #'visible-mode
	    "tf"   #'flymake-mode

	    ;; ====== Code ======
	    "l"    '(nil :wk "lsp and flymake")
	    "le"    #'eglot
	    "lr"    #'eglot-rename
	    "la"    #'eglot-code-actions
	    "lx"    #'eglot-code-action-extract
	    "lf"    #'eglot-code-action-quickfix
	    "l!"    #'consult-flymake
	    "ln"    #'flymake-goto-next-error
	    "lp"    #'flymake-goto-prev-error

	    ;; ====== Debug ======
	    "d"    '(nil :wk "debug")
	    "dG"   #'gdb

	    ;; ====== Notes ======
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

	    ;; ====== Help ======
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

	    ;; ====== Extras ======
	    "e"    '(nil :wk "extras")

	    ;; ====== Project ======
	    "p"    '(nil :wk "project")
	    "pp"  #'project-switch-project
	    "pc"  #'project-compile
	    "pd"  #'project-find-dir
	    "pf"  #'project-find-file
	    "pk"  #'project-kill-buffers
	    "pb"  #'project-switch-to-buffer
	    "p-"  #'project-dired
	    "px"  #'project-execute-extended-command
	    ;; compile/test
	    "pc" #'project-compile
	    ;; run
	    "pr"  '(nil :wk "run")
	    "prc" #'project-shell-command
	    "prC" #'project-async-shell-command
	    ;; forget
	    "pF"  '(nil :wk "forget/cleanup")
	    "pFp" #'project-forget-project
	    "pFu" #'project-forget-projects-under
	    ;; search/replace
	    "ps"  '(nil :wk "search/replace")
	    "pss" #'project-search
	    "psn" '(fileloop-continue :wk "Next match")
	    "psr" #'project-query-replace-regexp
	    "psf" #'project-find-regexp))

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
a flat list of the `define-key' expressions needed to set the text
objects up with the correct values already substituted in for further
macroexpansion."
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK-SPECIFIC PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun task/coding-layer ()
    "All the basic components needed for a Visual Studio Code-style
  IDE-lite experience in Emacs... but better.

  Loads:
  - `magit', the powerful Git user interface that lets you do
  anything from trivial to complex git commands with just a few
  mnemonic keypresses, all with helpful command palettes to guide
  you on your way, and `diff-hl' so you can see what's changed in-editor
  - `treesit-auto', to automatically install and use the tree-sitter mode
  for any recognized language, so you have IDE-class syntax
  highlighting for nearly anything, out of the box.
  - `corfu', the faster, slimmer, yet more featureful
  universal (available anywhere) auto-completion UI for Emacs
  - `apheleia', as an auto-formatter, so you never need to worry about your
  formatting not matching a project's again.
  - `yasnippet' and `yasnippet-corfu', so you don't have to type all
  that rote boilerplate"
    ;; This wouldn't be Quake emacs without the preconfigured ability
    ;; to have a Quake style dropdown terminal!
    (add-to-list 'display-buffer-alist
		 '("\\*terminal\\*"
		   (display-buffer-in-side-window)
		   (side . top)
		   (window-height . 10)))

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

    ;; We like syntax highlighting in this house
    (use-package treesit-auto
	:custom
	(treesit-auto-install 'prompt)
	:config
	(treesit-auto-add-to-auto-mode-alist 'all)
	(global-treesit-auto-mode))

    (defun corfu-enable-in-minibuffer ()
	"Enable Corfu in the minibuffer."
	(when (local-variable-p 'completion-at-point-functions)
	    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
			corfu-popupinfo-delay nil)
	    (corfu-mode 1)))

    (use-package corfu
	:hook ((prog-mode . corfu-mode)
	       (shell-mode . corfu-mode)
	       (minibuffer-setup . corfu-enable-in-minibuffer))
	;; Optional customizations
	:custom
	(corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
	(corfu-auto t)                 ;; Enable auto completion
	(corfu-separator ?\s)          ;; Orderless field separator
	(corfu-quit-no-match 'separator)
	(corfu-auto-delay 0.12)
	(corfu-auto-prefix 3)
	(corfu-popupinfo-delay 0.22)
	(corfu-popupinfo-direction 'right)
	:config

	(defun corfu-popupinfo-start ()
	    (require 'corfu-popupinfo)
	    (set-face-attribute 'corfu-popupinfo nil :inherit 'variable-pitch)
	    (corfu-popupinfo-mode))
	(add-hook 'corfu-mode-hook #'corfu-popupinfo-start))

    (with-eval-after-load 'eglot
	(setq eglot-autoshutdown t
	      eglot-events-buffer-size 0
	      eglot-sync-connect nil)
	(add-hook 'eglot-connect-hook (lambda (server)
					  (message "Server connected")))
	(add-to-list 'eglot-server-programs
		     '((typescript-ts-mode js-ts-mode) . ("typescript-language-server" "--stdio")))
	(add-to-list 'eglot-server-programs
		     '((rust-ts-mode) . ("rust-analyzer"))))

    ;; Global autoformatting
    (use-package apheleia
	:config
	(setf (alist-get 'prettier apheleia-formatters)
	      '("prettier" file))
	(add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
	(add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier))
	:hook (prog-mode . apheleia-mode))

    (use-package editorconfig :hook (prog-mode . editorconfig-mode))

    ;; Snippets are useful for an IDE-lite experience!
    (use-package yasnippet
	:hook (prog-mode . yas-minor-mode)
	:config (yas-reload-all))

    (use-package yasnippet-capf
	:after (corfu yasnippet)
	:config
	(add-to-list 'completion-at-point-functions #'yasnippet-capf)))

(defun task/writing-layer ()
    "If you're like me and you use Emacs to write blog posts and/or
  fiction, a good focus mode is priceless.

  Loads:
  - `visual-fill-column' for dealing with those line-paragraphs
  - `darkroom', the focus mode of your dreams
  - `flymake-proselint', to help you improve your prose
  - `latex-preview-pane', so if you're writing LaTeX, you can see
  what it will produce"
    ;; Ability to fill words into the width of the screen as proper
    ;; WYSIWYG editors do
    (use-package visual-fill-column
	:commands (visual-fill-column-mode))

    (defun flymake-proselint-setup ()
	"Enable flymake backend."
	(message "Initializing proselint flymake backend")
	(add-hook 'flymake-diagnostic-functions #'flymake-proselint-backend nil t))

    (defun distraction-free-writing-mode ()
	"Enhance `darkroom-mode' with more things for writing."
	;; Faster performance on long lines
	(column-number-mode -1)
	(ligature-mode -1)
	(prettify-symbols-mode -1)
	;; Less distracting fringe
	(setq left-fringe-width 0)
	(setq right-fringe-width 0)
	(visual-fill-column-mode)
	(if darkroom-mode
		(buffer-face-mode 1) ; enable variable pitch in buffer if entering
	    (buffer-face-mode -1)) ; disable it if exiting
	;; Proselint
	(flymake-mode)
	(flymake-proselint-setup))
    
    ;; Distraction free writing mode
    (use-package darkroom
	:commands (darkroom-mode darkroom-tentative-mode)
	:config
	(add-hook 'darkroom-mode-hook #'distraction-free-writing-mode))

    (use-package latex-preview-pane
	:commands (latex-preview-pane-mode latex-preview-pane-enable)))

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
	(denote-file-type 'markdown-yaml)
	(denote-backlinks-show-context t)
	:config
	(add-hook 'find-file-hook #'denote-link-buttonize-buffer)
	;; NOTE: I initially had a much simpler implementation, but
	;; Prot suggested this was safer, since I'm defining my own
	;; link function instead of just fucking with core denote
	;; sanity-checking functions. See:
	;; https://github.com/protesilaos/denote/issues/364
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
	    "Create a new directory DIR to function as a `denote' silo. Adds it to the project list."
	    (interactive (list (read-directory-name "Silo directory: ")))
	    (if (not (make-directory dir t))
		    (progn
			(with-temp-file (file-name-concat dir ".dir-locals.el")
			    (insert (format "
;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info \"(emacs) Directory Variables\")
((nil . ((denote-directory . \"%s\"))))" (expand-file-name dir))))
			(project-remember-project (expand-file-name dir))))))

    (use-package consult-notes
	:after (denote)
	:config
	(consult-notes-denote-mode 1)
	;; search only for text files in denote dir
	(setq consult-notes-denote-files-function (function denote-directory-text-only-files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RECOMMENDED AESTHETIC PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	(spacious-padding-mode 1)
	(set-face-attribute 'mode-line nil :inherit 'variable-pitch :height 120)
	(set-face-attribute 'mode-line-inactive nil :inherit 'mode-line))

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
	  (((mood-line-segment-modal) . " ")
	   ((or (mood-line-segment-buffer-status) " ") . " ")
	   ((mood-line-segment-buffer-name) . " ")
	   ((mood-line-segment-cursor-position) . " ")
	   (mood-line-segment-scroll))
	  :right
	  (((mood-line-segment-vc) . " ")
	   ((mood-line-segment-major-mode) . " ")
	   ((mood-line-segment-checker) . " ")
	   ((mood-line-segment-process) . " "))))
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
	(markdown-asymmetric-header nil)
	(markdown-header-scaling t)
	(markdown-marginalize-headers t)
	(markdown-enable-math t)
	(markdown-fontify-code-blocks-natively t))

    (use-package breadcrumb
	:hook (eglot-managed-mode . breadcrumb-local-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OPTIONAL AESTHETIC BLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun optional/bling-layer ()
    "If you want your editor to wow the hipsters, or you just like
  looking at the fancy pretty colors, you need some bling.

  Loads:
  - `hl-todo', so you never miss those TODOs and FIXMEs
  - `nerd-icons', `nerd-icons-completion', and `nerd-icons-corfu',
  because there's really nothing better than a nice set of icons to
  spice things up, and we want integration *everywhere*
  - `emojify', because emojis are cool
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

    (use-package emojify
	:config
	(setq emojify-display-style 'unicode)
	(setq emojify-emoji-styles '(unicode))
	:hook (after-init-hook . global-emojify-mode))

    ;; This assumes you've installed the package via MELPA.
    (use-package ligature
	:config
	;; Enable the "www" ligature in every possible major mode
	(ligature-set-ligatures 't '("www"))
	;; Enable traditional ligature support in eww-mode, if the
	;; `variable-pitch' face supports it
	(ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
	;; Enable all Cascadia Code ligatures in programming modes
	(ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
					     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
					     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
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

(defun optional/ide-layer ()
    "If you want your editor to feel even more like a GUI-based IDE,
  but faster and more flexible, this layer is for you.

  Loads:
  - `treemacs' and `treemacs-evil', for a fully graphical project tree
    explorer sidebar
  - `centaur-tabs', because nothing screams 'this isn't a regular
  text editor, this is an IDE' like fully-GUI tabs, curved in a
  way text could never emulate"
    ;; Integrate nerd icons with dired (I never use dired)

    (use-package treemacs
	:commands (treemacs)
	:config
	(dolist (face '(treemacs-root-face
			treemacs-git-unmodified-face
			treemacs-git-modified-face
			treemacs-git-renamed-face
			treemacs-git-ignored-face
			treemacs-git-untracked-face
			treemacs-git-added-face
			treemacs-git-conflict-face
			treemacs-directory-face
			treemacs-directory-collapsed-face
			treemacs-file-face
			treemacs-tags-face))
            (set-face-attribute face nil :inherit 'variable-pitch :height 120)))

    (use-package treemacs-evil
	:after (treemacs evil)
	:ensure t)


    (use-package centaur-tabs
	:commands (centaur-tabs-mode)
	:custom
	(centaur-tabs-style "wave")
	(centaur-tabs-height 32)
	(centaur-tabs-set-icons t)
	(centaur-tabs-gray-out-icons 'buffer)
	(centaur-tabs-set-modified-marker t)
	(centaur-tabs-label-fixed-length 15)
	:config
	(centaur-tabs-change-fonts "Cantarell" 120)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RUN LAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.quake.d/user.el")

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
(dolist (layer (user/enabled-layers))
    (message (format "Enabling the %s layer" layer))
    (funcall (symbol-function layer)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;; CODE MOVED IN-TREE FOR PERFORMANCE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local flymake-proselint-backend--proc nil)

;; See https://www.gnu.org/software/emacs//manual/html_node/flymake/An-annotated-example-backend.html
(defun flymake-proselint-backend (report-fn &rest _args)
    ;; Not having a proselint interpreter is a serious problem which
    ;; should cause the backend to disable itself, so an error is
    ;; signaled.
    (unless (executable-find "proselint") 
	(error "Cannot find a suitable proselint executable. Try installing it with pip?"))

    ;; If a live process launched in an earlier check was found, that
    ;; process is killed.  When that process's sentinel eventually runs,
    ;; it will notice its obsoletion, since it have since reset
    ;; `flymake-proselint-backend-proc' to a different value
    ;;
    (when (process-live-p flymake-proselint-backend--proc)
	(kill-process flymake-proselint-backend--proc))

    ;; Save the current buffer, the narrowing restriction, remove any
    ;; narrowing restriction.
    (let ((source (current-buffer)))
	(save-restriction
	    (widen)
	    ;; Reset the `flymake-proselint-backend--proc' process to a new process
	    ;; calling the proselint tool.
	    (setq
	     flymake-proselint-backend--proc
	     (make-process
	      :name "proselint-flymake" :noquery t :connection-type 'pipe
	      ;; Make output go to a temporary buffer.
	      ;;
	      :buffer (generate-new-buffer " *proselint-flymake*")
	      :command '("proselint")
	      :sentinel
	      (lambda (proc _event)
		  ;; Check that the process has indeed exited, as it might
		  ;; be simply suspended.
		  (when (memq (process-status proc) '(exit signal))
		      (unwind-protect
			      ;; Only proceed if `proc' is the same as
			      ;; `flymake-proselint-backend--proc', which indicates that
			      ;; `proc' is not an obsolete process.
			      (if (with-current-buffer source (eq proc flymake-proselint-backend--proc))
				      (with-current-buffer (process-buffer proc)
					  (goto-char (point-min))
					  ;; Parse the output buffer for diagnostic's
					  ;; messages and locations, collect them in a list
					  ;; of objects, and call `report-fn'.
					  ;;
					  (cl-loop
					   while (search-forward-regexp
						  "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.+\\)$"
						  nil t)
					   for lnum = (string-to-number (match-string 1))
					   for lcol = (string-to-number (match-string 2))
					   for msg = (match-string 3)
					   for (beg . end) = (flymake-diag-region source lnum lcol)
					   when (and beg end)
					   collect (flymake-make-diagnostic source
									    beg
									    end
									    :warning
									    msg)
					   into diags
					   finally (funcall report-fn diags)))
				  (flymake-log :warning "Canceling obsolete check %s"
					       proc))
			  ;; Cleanup the temporary buffer used to hold the
			  ;; check's output.
			  (kill-buffer (process-buffer proc)))))))
	    ;; Send the buffer contents to the process's stdin, followed by
	    ;; an EOF.
	    (process-send-region flymake-proselint-backend--proc (point-min) (point-max))
	    (process-send-eof flymake-proselint-backend--proc))))

(defvar existing-shell nil)

(defun shell-toggle ()
    (interactive)
    (let* ((existing-window (get-buffer-window existing-shell)))
	(cond
	 ((and existing-shell existing-window) (delete-window existing-window))
	 ((and existing-shell (not existing-window)) (display-buffer existing-shell))
	 (t (setq existing-shell (term (getenv "SHELL")))))))
