(defmacro +cmdfy! (&rest body)
    "Convert BODY to an interactive command."
    `(lambda () (interactive) ,@body))

(require 'cl-lib)

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(require 'use-package-ensure)
(setq use-package-always-ensure t) ; we care about performance here!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; USER TUNABLE PARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar quake-color-theme
    'doom-gruvbox
    "The theme quake loads and uses at startup.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BASE EMACS CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A few more useful configurations...
(use-package emacs
    :init
    ;; TAB cycle if there are only few candidates
    ;; (setq completion-cycle-threshold 3)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete)

    ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
    ;; mode. Corfu commands are hidden, since they are not used via M-x. This
    ;; setting is useful beyond Corfu.
    ;; (setq read-extended-command-predicate #'command-completion-default-include-p)

    ;; Less annoying emacs
    (setq inhibit-startup-message t
	  visible-bell t
	  vc-follow-symlinks t
	  warning-minimum-level :emergency
	  display-line-numbers 'relative
	  fill-column 65)
    (setopt use-short-answers t)   ;; Since Emacs 29, `yes-or-no-p' will use `y-or-n-p'
    (customize-set-variable 'treesit-font-lock-level 4)

    ;; Fonts
    (when (find-font (font-spec :name "JetBrains Mono"))
	(set-face-attribute 'default nil :font "JetBrains Mono" :height 120))
    (when (find-font (font-spec :name "Cantarell"))
	(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120))
    (when (find-font (font-spec :name "iA Writer Quattro V"))
	(setq buffer-face-mode-face '(:family "iA Writer Quattro V")))

    ;; Nicer appearance
    (menu-bar-mode -1)  ; Leave this one on if you're a beginner!
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (set-frame-parameter nil 'undecorated t)
    (column-number-mode)

    ;; Basic programmming mode to build off of
    (add-hook 'prog-mode-hook (lambda ()
				  (display-line-numbers-mode 1)
				  (setq display-line-numbers 'relative)
				  (global-hl-line-mode t)
				  (electric-pair-mode)))

    ;; Nicer behavior
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
    (cua-mode t)

    (pixel-scroll-precision-mode 1)
    (setq pixel-scroll-precision-use-momentum t
	  pixel-scroll-precision-large-scroll-height 40.0)

    (winner-mode 1)

    (recentf-mode 1)
    (setq recentf-max-menu-items 25
	  recentf-max-saved-items 25)

    ;; Ignore case!
    (setq read-file-name-completion-ignore-case t
	  read-buffer-completion-ignore-case t
	  completion-ignore-case t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CORE USABILITY PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/usability-layer ()
    ;; Where to next, boss?
    (use-package which-key
	:init (which-key-mode)
	:diminish which-key-mode
	:custom
	(which-key-idle-delay 0.1)
	(which-key-idle-secondary-delay nil)
	(which-key-sort-order #'which-key-key-order-alpha))

    ;; Better docs are always good
    (use-package helpful
	:commands (helpful-variable
		   helpful-key
		   helpful-command))

    ;; Hijack every prompt and completion in all of Emacs and make them *good*
    (use-package vertico
	:hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
	:custom
	(vertico-cycle t)
	:init
	(vertico-mode)

	;; Nice directory behavior please
	(keymap-set vertico-map "RET" #'vertico-directory-enter)
	(keymap-set vertico-map "M-DEL" #'vertico-directory-delete-char)
	(keymap-set vertico-map "DEL" #'vertico-directory-delete-word))

    ;; Enable rich annotations using the Marginalia package
    (use-package marginalia
	:after vertico
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

    ;; Optionally use the `orderless' completion style for proper fuzzy searching
    ;; in vertico
    (use-package orderless
	:after vertico
	:init
	(setq completion-styles '(substring orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))))

    ;; Vertico completion superpowers like grep
    (use-package consult
	:after vertico
	:config
	;; We also want to use this for in-buffer completion, which vertico can't do alone
	(setq xref-show-xrefs-function #'consult-xref
	      xref-show-definitions-function #'consult-xref)
	(setq completion-in-region-function
	      (lambda (&rest args)
		  (apply (if vertico-mode
				 #'consult-completion-in-region
			     #'completion--in-region)
			 args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CORE EDITING PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/editor-layer ()
    ;; Join the vim side :evil_grin:
    ;; TODO: build list of places where evil mode isn't good and add a hook to
    ;; avoid them
    (use-package evil
	:init
	(setq evil-want-integration t)
	(setq evil-want-keybinding nil)
	(setq evil-want-C-u-scroll t)
	(setq evil-want-C-i-jump nil)
	(setq evil-undo-system 'undo-redo)
	:config
	(evil-mode 1)
	(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
	(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

	;; We want to be able to use ctrl-v and ctrl-c just for
	;; convenience/user-friendliness, especially since ctrl-shift-v
	;; doesn't work in evil, unlike (terminal) vim
	(define-key evil-insert-state-map (kbd "C-c") 'cua-copy-region)
	(define-key evil-insert-state-map (kbd "C-v") 'cua-paste)
	(define-key evil-insert-state-map (kbd "C-x") 'cua-cut-region)
	(define-key evil-insert-state-map (kbd "C-z") 'undo-tree-undo)
	(define-key evil-insert-state-map (kbd "C-y") 'undo-tree-redo)

	;; Centaur tab (optional) support
	(define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
	(define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)

	;; Nice commenting
	(define-key evil-normal-state-map (kbd "g c") 'comment-region)
	(define-key evil-normal-state-map (kbd "g C") 'uncomment-region)

	;; Support for visual fill column mode and visual line mode
	;; Make evil-mode up/down operate in screen lines instead of logical lines
	(define-key evil-motion-state-map "j" 'evil-next-visual-line)
	(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
	;; Also in visual mode
	(define-key evil-visual-state-map "j" 'evil-next-visual-line)
	(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

	;; Override evil mode's exceptions to defaulting to normal-mode
	(evil-set-initial-state 'messages-buffer-mode 'normal)
	(evil-set-initial-state 'dashboard-mode 'normal)
	;; set leader key in all states
	(evil-set-leader nil (kbd "C-SPC"))

	;; set leader key in normal state
	(evil-set-leader 'normal (kbd "SPC")))

    (defun core/dont-arrow-me-sis ()
	(interactive)
	(message "Arrow keys are bad, you know?"))

    (use-package evil-collection
	:after evil
	:config
	(evil-collection-init)

	;; Disable arrow keys in normal and visual modes
	(define-key evil-normal-state-map (kbd "<left>") 'core/dont-arrow-me-sis)
	(define-key evil-normal-state-map (kbd "<right>") 'core/dont-arrow-me-sis)
	(define-key evil-normal-state-map (kbd "<down>") 'core/dont-arrow-me-sis)
	(define-key evil-normal-state-map (kbd "<up>") 'core/dont-arrow-me-sis)
	(evil-global-set-key 'motion (kbd "<left>") 'core/dont-arrow-me-sis)
	(evil-global-set-key 'motion (kbd "<right>") 'core/dont-arrow-me-sis)
	(evil-global-set-key 'motion (kbd "<down>") 'core/dont-arrow-me-sis)
	(evil-global-set-key 'motion (kbd "<up>") 'core/dont-arrow-me-sis))

    ;; A basic keymap for managing emacs using an evil mode leader key instead of
    ;; pure repetitive strain injury (copied from MinEmacs :))

    (use-package general
	;; PERF: Loading `general' early make Emacs very slow on startup.
	:after (evil evil-collection)
	:config
	;; Advise `define-key' to automatically unbind keys when necessary.
	(general-auto-unbind-keys)
	;; Set up some basic equivalents (like `general-nmap') with short named
	;; aliases (like `nmap') for VIM mapping functions.
	(general-evil-setup t)

	(general-create-definer +core--internal-local-map!
	    :states '(insert emacs visual normal)
	    :keymaps 'override              
	    :prefix "SPC m"      
	    :global-prefix "C-SPC m")

    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (+core--internal-local-map!
                  "e" #'eval-last-sexp
                  "d" #'eval-defun
                  "b" #'eval-buffer
                  "r" #'eval-region)))

	;; Define the built-in global keybindings
	(general-nmap
	    :prefix "SPC"
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
	    "Pf"   `(,(+cmdfy! (find-file "~/.emacs.d/init.el"))
		     :wk "Open framework config")
	    "Pu"   `(,(+cmdfy! (find-file "~/.quake.d/user.el"))
		     :wk "Open user config")

	    ;; ====== Buffers ======
	    "b"    '(nil :wk "buffer")
	    "bb"   #'consult-buffer
	    "bI"   #'ibuffer
	    "bx"   #'bury-buffer
	    "bS"   #'save-some-buffers
	    "bM"   #'view-echo-area-messages
	    "bk"   `(,(+cmdfy! (kill-buffer (current-buffer)))
		     :wk "Kill this buffer")
	    "bK"   `(,(+cmdfy! (+kill-buffer-and-its-windows (current-buffer)))
		     :wk "Kill this buffer and its windows")
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
	    "bvr"  '(nil :wk "reload dir-locals for...")
	    "bvrr" '(+dir-locals-reload-for-this-buffer :wk "This buffer")
	    "bvrd" '(+dir-locals-reload-for-all-buffers-in-this-directory :wk "All buffers in this directory")

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
	    "wh"   #'split-window-horizontally
	    "wv"   #'split-window-vertically

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

	    ;; ======  Mode specific a.k.a. "local leader" ======
	    "m"    '(nil :wk "mode-specific")

	    ;; ====== VC ======
	    "g"    '(nil :wk "git/vc")
	    "gg"   #'magit

	    ;; ====== Workspaces ======
	    "TAB"  '(nil :wk "workspace")

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

    (use-package hydra
	:after evil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CORE CODE PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/coding-layer ()
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

    ;; We like syntax highlighting in this house
    (use-package treesit-auto
	:custom
	(treesit-auto-install 'prompt)
	:config
	(treesit-auto-add-to-auto-mode-alist 'all)
	(global-treesit-auto-mode))

    (use-package corfu
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
	(global-corfu-mode)
	(defun corfu-enable-in-minibuffer ()
	    "Enable Corfu in the minibuffer."
	    (when (local-variable-p 'completion-at-point-functions)
		(setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
			    corfu-popupinfo-delay nil)
		(corfu-mode 1)))
	(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

	(defun corfu-popupinfo-start ()
	    (require 'corfu-popupinfo)
	    (set-face-attribute 'corfu-popupinfo nil :inherit 'variable-pitch)
	    (corfu-popupinfo-mode))
	(add-hook 'corfu-mode-hook #'corfu-popupinfo-start))

    (with-eval-after-load 'eglot
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

    ;; Snippets are useful for an IDE-lite experience!
    (use-package yasnippet
	:hook (prog-mode . yas-minor-mode)
	:config (yas-reload-all))
    
    (use-package yasnippet-capf
	:after (corfu yasnippet)
	:config
	(add-to-list 'completion-at-point-functions #'yasnippet-capf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CORE AESTHETIC PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/aesthetic-layer ()
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
	:after (doom-themes)
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
	(set-face-attribute 'eldoc-box-body nil :font "Cantarell-12")
	:hook (eldoc-mode . eldoc-box-hover-at-point-mode))

    ;; Pretty markdown formatting for eldoc-box
    (use-package markdown-mode
	:after (eldoc-box)
	:commands (markdown-mode))

    (use-package breadcrumb
	:hook (eglot-managed-mode . breadcrumb-local-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OPTIONAL AESTHETIC BLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/bling-layer ()
    (use-package hl-todo
	:commands (hl-todo-mode)
	:init (add-hook 'prog-mode-hook #'hl-todo-mode))

    ;; I like being able to distinguish parenthesis
    (use-package rainbow-delimiters
	:hook (prog-mode . rainbow-delimiters-mode))


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

(defun core/ide-layer ()
    ;; Integrate nerd icons with dired (I never use dired)
    (use-package nerd-icons-dired
	:after (nerd-icons)
	:hook (dired-mode . nerd-icons-dired-mode))

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
	(centaur-tabs-change-fonts (face-attribute 'variable-pitch :font) 120)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LANGUAGE SPECIFIC PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/writing-layer ()
    ;; Ability to fill words into the width of the screen as proper
    ;; WYSIWYG editors do
    (use-package visual-fill-column
	:commands (visual-fill-column-mode))

    ;; Distraction free writing mode
    (use-package darkroom
	:commands (darkroom-mode darkroom-tentative-mode)
	:config
	(add-hook 'darkroom-mode-hook (lambda ()
					  (if (fboundp 'visual-fill-column-mode)
						  (visual-fill-column-mode)
					      (visual-line-mode))
					  (if darkroom-mode
						  (buffer-face-mode 1)
					      (buffer-face-mode -1)))))

    (defun flymake-proselint-setup ()
	"Enable flymake backend."
	(message "Initializing proselint flymake backend")
	(add-hook 'flymake-diagnostic-functions #'flymake-proselint-backend nil t))

    (add-hook 'darkroom-mode-hook (lambda ()
				      (flymake-mode)
				      (flymake-proselint-setup)))

    (use-package latex-preview-pane
	:commands (latex-preview-pane-mode latex-preview-pane-enable)))

(defun core/markdown-layer ()
    "Tree-Sitter markdown requires more setup than the other tree-sitter modes"
    ;; Main markdown mode
    (use-package markdown-ts-mode
	:mode ("\\.md\\'" . markdown-ts-mode)
	:config
	(add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
	(add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))))

(defun core/emacs-lisp-layer ()
    "Although the goal of Quake Emacs is to rely exclusively on
Tree-Sitter and Eglot for language support, obviating the need
for language-specific layers, there is no such support for Emacs
Lisp, and if you're using Emacs, you must use Elisp, so an
exception must be made."
    ;; Emacs Lisp
    (use-package elisp-def
	:hook (emacs-lisp-mode-hook . elisp-def-mode))

    (use-package elisp-demos
	:config
	(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

    (use-package highlight-defined
	:hook (emacs-lisp-mode . highlight-defined-mode))

    (setq lisp-body-indent 4))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RUN LAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.quake.d/user.el")
(defvar enabled-layers (user/enabled-layers))

(defun core/enable-layer (layer)
    (message (format "Enabling the %s layer" layer))
    (funcall (symbol-function layer)))

(mapcar #'core/enable-layer enabled-layers)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" default))
 '(mini-frame-show-parameters '((top . 10) (width . 0.7) (left . 0.5)))
 '(package-selected-packages
   '(esup latex-preview-pane yasnippet-capf which-key visual-fill-column vertico treesit-auto spacious-padding rainbow-delimiters orderless nerd-icons-corfu nerd-icons-completion mood-line markdown-ts-mode markdown-mode marginalia magit ligature hydra hl-todo highlight-defined helpful gruvbox-theme general flymake-quickdef flymake-proselint evil-collection equake emojify elisp-demos elisp-def eldoc-box doom-themes doom-modeline dashboard darkroom corfu consult breadcrumb apheleia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
