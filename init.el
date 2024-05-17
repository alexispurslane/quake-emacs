(defmacro +cmdfy! (&rest body)
    "Convert BODY to an interactive command."
    `(lambda () (interactive) ,@body))

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

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
    (setq read-extended-command-predicate #'command-completion-default-include-p)
    ;; Less annoying emacs
    (setq inhibit-startup-message t
	  visible-bell t
	  vc-follow-symlinks t
	  warning-minimum-level :emergency
	  display-line-numbers 'relative)
    (customize-set-variable 'treesit-font-lock-level 4)

    ;; Nicer appearance
    (set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
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
	(setq evil-respect-visual-line-mode t)
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
	:after evil
	:config
	;; Advise `define-key' to automatically unbind keys when necessary.
	(general-auto-unbind-keys)
	;; Set up some basic equivalents (like `general-nmap') with short named
	;; aliases (like `nmap') for VIM mapping functions.
	(general-evil-setup t)

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
	    "Pu"   `(,(+cmdfy! (find-file "~/.emacs.d/user.el"))
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
	    "l"    '(nil :wk "eglot lsp")
	    "le"    #'eglot
	    "lr"    #'eglot-rename
	    "la"    #'eglot-code-actions
	    "lx"    #'eglot-code-action-extract
	    "lf"    #'eglot-code-action-quickfix

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
	    "pre" #'project-eshell
	    "prs" #'project-shell
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
	:after evil)

    ;; Emacs' entire selling point right here!
    (use-package magit
	:commands (magit-status magit-get-current-branch)
	:custom
	(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CORE CODE PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/coding-layer ()
    ;; We like syntax highlighting in this house
    (use-package treesit-auto
	:custom
	(treesit-auto-install 'prompt)
	:config
	(treesit-auto-add-to-auto-mode-alist 'all)
	(global-treesit-auto-mode))

    (use-package corfu
	;;:config
	;;(set-face-attribute 'corfu-popupinfo nil :family "Cantarell" :height 120)
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
	:init
	(global-corfu-mode)
	(defun corfu-enable-in-minibuffer ()
	    "Enable Corfu in the minibuffer."
	    (when (local-variable-p 'completion-at-point-functions)
		;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
		(setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
			    corfu-popupinfo-delay nil)
		(corfu-mode 1)))
	(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
	(add-hook 'corfu-mode-hook #'corfu-popupinfo-mode))


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
	:hook (prog-mode . apheleia-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CORE AESTHETIC PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/aesthetic-layer ()
    ;; Some non-insane themes, please
    (use-package doom-themes
	:config
	;; Global settings (defaults)
	(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	      doom-themes-enable-italic t) ; if nil, italics is universally disabled
	(load-theme 'doom-gruvbox t)

	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config))

    ;; A modeline that won't make me wish I didn't have eyes
    (use-package doom-modeline
	:init (doom-modeline-mode 1)

	:custom
	(doom-modeline-major-mode-icon t)
	(doom-modeline-height 33)

	:config
	(set-face-attribute 'mode-line nil :family "Cantarell" :height 120)
	(set-face-attribute 'mode-line-inactive nil :family "Cantarell" :height 120))

    (use-package dashboard
	:custom
	(dashboard-banner-logo-title "Lean, fast, focused, based on the latest tech. Welcome to Quake Emacs")
	(dashboard-startup-banner "~/.emacs.d/load/banner-quake.png")
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

    (use-package breadcrumb
	:hook (eglot-managed-mode . breadcrumb-local-mode))

    (use-package spacious-padding))

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
	    (set-face-attribute face nil :family "Cantarell" :height 120)))

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LANGUAGE SPECIFIC PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/markdown-layer ()
    ;; Markdown
    (use-package markdown-ts-mode
	:mode ("\\.md\\'" . markdown-ts-mode)
	:config
	(add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
	(add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

    ;; In case you want something fancier/slower (also needed for eldoc-box)
    (use-package markdown-mode
	:commands (markdown-mode)))

(defun core/emacs-lisp-layer ()
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

(load "~/.emacs.d/user.el")
(defvar enabled-layers (user/enabled-layers))

(defun core/enable-layer (layer)
    (message (format "Enabling the %s layer" layer))
    (funcall (symbol-function layer)))

(mapcar #'core/enable-layer enabled-layers)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" default))
 '(mini-frame-show-parameters '((top . 10) (width . 0.7) (left . 0.5)))
 '(package-selected-packages '(centaur-tabs treemacs esup markdown-ts-mode doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
