;; init.el --- implements the Quake Emacs distribution of Emacs -*- lexical-binding: t -*-
;; eval: (outline-hide-sublevels 4)

;; Author: Alexis Purslane <alexispurlsane@pm.me>
;; URL: https://github.com/alexispurslane/quake-emacs
;; Package-Requires: ((emacs "29.1") (cl-lib "1.0"))
;; Version: 1.0.0
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

;;; Code:

;;; ======Prelude======

(profiler-start 'cpu)
(require 'cl-lib)
(require 'rx)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(require 'use-package-ensure)
(setq use-package-always-ensure t
      package-quickstart t
      package--init-file-ensured t
      package-enable-at-startup nil)
(defvar quake--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist quake--file-name-handler-alist)))

;; Print a little message to the user that we're loading packages if this is a fresh install

(when (let ((place (file-name-concat user-emacs-directory "elpa/")))
          (or (directory-empty-p place) (not (file-directory-p place))))
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

;;;; Garbage Collector Optimization
;; Until scratch/igc gets merged and Emacs gets a multithreaded
;; generational GC, GC pauses when the user is trying to interact
;; will be an issue. Quake 1.0 solved this by:
;;
;; 1. setting `gc-cons-percentage' to 20% during normal usage,
;; which was high enough to eliminate most GC pauses during
;; normal usage
;;
;; 2. turning the GC off during startup and then turning it back
;; on five seconds after startup
;;
;; 3. turning GC off when the minibuffer is open and then back on
;; when it closes
;;
;; However, all of these ad-hoc solutions are basically unified
;; under one concept: not doing GC when Lisp is actively running,
;; instead putting it off till some presumably more idle time
;; when responsiveness is less important. Thus, a strategy that
;; unifies all these ideas into a single system, and also times
;; when those "catch-up" GCs happen more intelligently, is
;; probably better. That's GCMH. See: [[https://akrl.sdf.org/]]
(use-package gcmh
    :custom
    (gcmh-idle-delay 'auto) ; this means that it will take longer to activate a GC if GCs are taking longer and happening at non-idle times. However, with the default `gcmh-auto-idle-delay-factor' it'll still happen very often usually, <1s probably, unlike the 15s of the normal setting.
    (gcmh-high-cons-threshold (* 200 1000 1000)) ; this fixes [[https://gitlab.com/koral/gcmh/-/issues/6]] and addresses Eli Zaretski (lead Emacs maintainer)'s [[https://old.reddit.com/r/emacs/comments/bg85qm/garbage_collector_magic_hack/][concerns]] about large packages/files/jobs creating a huge amount of memory without it ever being collected thanks to GCMH that then hangs Emacs when idle time arrives
    :config
    (when (version< emacs-version "31.0.50") ; scratch/igc should land in 31, so don't use GCMH, we won't need it anymore
        (gcmh-mode 1)))

;;; ======User-Modifiable Variables======
(defcustom quake-org-home-directory
    "~/org/"
    "The directory that your org capture templates and
org-agenda files will be placed in by default. Always ends in a slash."
    :type 'string
    :group 'quake)

(defcustom quake-enabled-layers
    (list
     'core/usability-layer
     'core/keys-layer
     'task/coding-layer
     'task/writing-layer
     'core/aesthetic-layer
     'optional/bling-layer)
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

(defcustom quake-term-preferred-command 'eshell
    "Which Emacs command Quake Emacs's popup terminal will trigger.

If you pass in term, your environment shell will be
passed in as an argument."
    :group 'quake)


(defun quake--insert-into-list (list el n
                                     )
    "Insert into list LIST an element EL at index N.

If N is 0, EL is inserted before the first element.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid."
    (let* ((padded-list (cons nil list))
           (c (nthcdr n padded-list)))
        (setcdr c (cons el (cdr c)))
        (cdr padded-list)))

(cl-defmacro quake-layers (&key (enable (list)) (disable (list)))
    "Remove DISABLE layers from, and add ENABLE layers to the end of,
`quake-enabled-layers'. DISABLE is a list of symbols, ENABLE is a
list of symbols or a list of conses specifying a symbol and the
index at which to insert it.

A Warning from the Ancients: Use this instead of directly
modifying `quake-enabled-layers' unless you're willing to update
your user.el with every single update to Quake."
    `(progn
         ,@(mapcar (lambda (layer) `(delete ',layer quake-enabled-layers))
                   disable)
         ,@(mapcar (lambda (layer)
                       (cond
                        ((and (consp layer)
                              (symbolp (car layer))
                              (numberp (cdr layer))
                              (fboundp (car layer)))
                         `(quake--insert-into-list quake-enabled-layers ',(car layer) ,(cdr layer)))
                        ((and (symbolp layer) (fboundp layer))
                         `(add-to-list 'quake-enabled-layers ',layer t))))
                   enable)))

;;; ======Load User Script======
;; we load the user script at the beginning so that some of their
;; config can run *before* layer initialization happens, and
;; their custom layers can run during and after, thus producing a
;; nice clean [sic]?

(when (file-exists-p "~/.quake.d/user.el")
    (load "~/.quake.d/user.el"))

;;; ======Vanilla Emacs======
(use-package emacs
    :init
;;;; Setting up Emacs to behave in a more familiar and pleasing way
    (setq auth-sources '("~/.authinfo.gpg"))      ; don't store my passwords and keys in plaintext, please
    (setq inhibit-startup-message t               ; we're going to have our own dashboard
          visible-bell t                          ; nobody likes being beeped at
          backup-by-copying t                     ; backing up a file by moving it is insane
          backup-directory-alist `((".*" . ,temporary-file-directory))
          auto-save-file-name-transforms `((".*" ,temporary-file-directory t)) ; don't litter dammit
          delete-old-versions t                   ; delete excess backup files
          lisp-body-indent 4                      ; four space tabs
          vc-follow-symlinks t                    ; we'll always want to follow symlinks
          warning-minimum-level :emergency        ; don't completely shit the bed on errors
          display-line-numbers 'relative          ; whether you use evil or not, these are useful
          enable-recursive-minibuffers t          ; allow you to have multiple minibuffers open
          search-nonincremental-instead nil         ; don't degrade to nonincremental search if the user tries to exit out of isearch, nobody fucking wants that
          custom-file "~/.emacs.d/custom.el")     ; dump all the shit from custom somewhere else
    (setq split-width-threshold 0 split-height-threshold nil) ; always split vertically, because most people's displays have more horizontal width than vertical width
    (setq-default fill-column 65)                 ; this will be used in reading modes, so set it to something nice
    (global-auto-revert-mode t)                      ; automatically update files when they change
    (add-hook 'before-save-hook 'whitespace-cleanup) ; remove trailling whitespace
    (setq tab-always-indent 'complete)               ; more modern completion behavior
    (setq read-file-name-completion-ignore-case t    ; ignore case when completing file names
          read-buffer-completion-ignore-case t       ; ignore case when completing buffer names
          completion-ignore-case t)                  ; fucking ignore case in general!
    (setopt use-short-answers t)                     ; so you don't have to type out "yes" or "no" and hit enter
    (setq eldoc-idle-delay 1.0)                      ; w/ eldoc-box/an LSP, idle delay is by default too distracting
    (setq display-line-numbers-width-start t)        ; when you open a file, set the width of the linum gutter to be large enough for the whole file's line numbers
    (setq-default indent-tabs-mode nil)              ; prefer spaces instead of tabs
;;;;; Disabling ugly and largely unhelpful UI features
    (tool-bar-mode -1) ; the menu bar is quite useful, but the tool bar is painfully ugly and adds little
;;;;; Enable some modes that give nicer, more modern behavior

    (minibuffer-depth-indicate-mode 1)
    (setq pixel-scroll-precision-interpolate-mice t
          pixel-scroll-precision-interpolate-page t)
    (pixel-scroll-precision-mode 1)                  ; smooth scrolling
    (savehist-mode 1)                                ; remember commands
    (column-number-mode)                             ; keep track of column number for the useful modeline readout
    (global-visual-line-mode)                        ; wrap lines at end of window

;;;;; Enable better window management
    (winner-mode 1)                ; so you can undo modifications to your window layout
    (windmove-mode 1)              ; so you can move to windows directionally like in Vim, instead of only having other-window
;;;;; Performance tuning
;;;;;; Optimizations copied from DOOM
    (setq frame-inhibit-implied-resize t)
    (setq initial-major-mode 'fundamental-mode)
;;;;;; Optimize font-locking for greater responsiveness
    (setq jit-lock-stealth-time 0.2
          jit-lock-defer-time 0.0
          jit-lock-context-time 0.2
          jit-lock-stealth-load 200)
;;;;;; Optimize for long lines.
    (setq-default bidi-paragraph-direction 'left-to-right ; assume we're using LtR text unless explicitly told otherwise
                  bidi-inhibit-bpa t) ; turn off bidirectional paren display algorithm, it is expensive
;;;; Fonts
    (set-display-table-slot
     standard-display-table
     'selective-display
     (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
         (vconcat (mapcar (lambda (c) (+ face-offset c)) " ")))))
;;; ======Basic Packages======
(defun core/usability-layer ()
    "Loads the core packages needed to make Emacs more usable in the
  modern day.

  Loads:
  - `recentf' so you can open recent files quickly,
  - `which-key' to show what keys can be pressed next at each stage of a
  key combination,
  - `repeat-mode', so that Emacs prefix key chords are much less onerous,
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
    better integration.
  - `wgrep', so that you get the power of writable grep buffers,
- `emacs-async' by jwiegley, so that sending mail, opening
    org-agendas, running dired commands, and more can be done
    asynchronously if you need to."

;;;; More DWIM-y C-x o behavior, to avoid the long keybindings for window movement
    (advice-add 'other-window :before
                (defun quake--other-window-split-if-single (&rest _)
                    "Split the frame if there is a single window."
                    (when (one-window-p)
                        (split-window-sensibly))))

;;;; Recentf
    (use-package recentf
        :commands (recentf-mode)
        :custom
        (recentf-auto-cleanup 'never) ; automatic cleanup checks for dead files, which was causing it to try to visit files in recentf that were over TRAMP, which caused massive inexplicable slowdowns
        :init (add-hook 'pre-command-hook (lambda () (recentf-mode 1))))
;;;; Vertico-style minibuffer completion UI using icomplete
    (use-package icomplete
        :commands (icomplete-vertical-mode)
        :init (add-hook 'pre-command-hook (lambda () (icomplete-vertical-mode 1)))
        :bind (:map icomplete-minibuffer-map
                    ("RET"    . icomplete-force-complete-and-exit)
                    ("TAB"    . icomplete-force-complete)
                    ("DEL"    . icomplete-fido-backward-updir)
                    ("<down>" . icomplete-forward-completions)
                    ("<up>"   . icomplete-backward-completions)
                    ("M-RET"  . embark-act)
                    ("C-RET"  . embark-dwim)
                    ("C-j"    . icomplete-fido-exit))
        :custom
        ;; remove arbitrary optimization limits that make icomplete
        ;; feel old-fashioned
        (icomplete-delay-completions-threshold 0)
        (icomplete-max-delay-chars 0)
        (icomplete-compute-delay 0)
        (icomplete-show-matches-on-no-input t)
        (icomplete-hide-common-prefix nil)
        (icomplete-prospects-height 15)
        (icomplete-with-completion-tables t))
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
        :commands (which-key-mode)
        :init (add-hook 'pre-command-hook (lambda () (which-key-mode 1)))
        :custom
        (which-key-idle-delay 0.1)
        (which-key-idle-secondary-delay nil)
        (which-key-sort-order #'which-key-key-order-alpha)
        :config
        (which-key-enable-god-mode-support))

    (use-package repeat
        :after (which-key)
        :config
        (repeat-mode 1) ; so we don't have to re-enter long prefixes
        (quake-repeatize 'ctl-x-4-map) ; the other-window prefix map is useful when repeated
        (quake-repeatize 'ctl-x-5-map) ; the frame management prefix map also
        (with-eval-after-load 'outline-minor-mode
            (quake-repeatize 'outline-minor-mode-map)) ; also the outline map (useful in org mode navigation)
        (quake-repeatize 'window-prefix-map) ; and of course the regular window management map
        ;; use which key to show repeat map
        (advice-add 'repeat-post-hook :after
                    (defun repeat-help--which-key-popup ()
                        (if-let ((cmd (or this-command real-this-command))
                                 (keymap (or repeat-map
                                             (repeat--command-property 'repeat-map))))

                                (run-at-time
                                 0 nil
                                 (lambda ()
                                     (which-key--create-buffer-and-show
                                      nil (symbol-value keymap))))
                            (which-key--hide-popup))))
        (custom-set-variables 'repeat-echo-function #'ignore))
;;;; Better Emacs Lisp editing experience

    (use-package elisp-def
        :commands (elisp-def-mode)
        :hook (emacs-lisp-mode . elisp-def-mode))

    (use-package elisp-demos
        :after (helpful)
        :config
        (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

    (use-package highlight-defined
        :commands (highlight-defined-mode)
        :hook (emacs-lisp-mode . highlight-defined-mode))

    (use-package outline
        :commands (outline-minor-mode)
        :hook ((prog-mode . outline-minor-mode)
               (text-mode . outline-minor-mode))
        :config
        (quake-emacs-define-key global-map
            "C-c C-o" (cons "Outline/Folding..." outline-mode-prefix-map))
        (add-hook 'outline-minor-mode-hook (lambda () (outline-show-all))))
;;;; Dired improvements
    (setq dired-listing-switches "-alFh")
    (use-package dired-subtree ; (this is already brought in by dired-sidebar later, so imo it doesn't count as an extra dep)
        :after dired
        :config
        (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
        (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))
;;;; Better peformance using asynchronous processing with subordinate Emacs processes
    (use-package async
        :demand t
        :hook (dired-mode . dired-async-mode)
        :config
        (add-hook 'message-mode-hook (lambda () (require 'smtpmail-async))))
    ;;;; Embark
    (use-package embark
        :commands (embark-act embark-dwim)
        :bind (:map embark-general-map
                    ("G" . embark-internet-search)
                    ("O" . embark-default-action-in-other-window))
        :config
;;;;; Use `which-key' to display possible actions instead of a separate buffer
        ;; This is a good idea because:
        ;;
        ;; 1. Interface consistency. Now every prompt for what
        ;; further actions can be taken is laid out basically the
        ;; same, using which-key or hydras.
        ;;
        ;; 2. Compactness. There are usually so many embark
        ;; actions that they go right off the screen when
        ;; displayed one per line with their docstrings next to
        ;; them, which is extremely annoying to deal with
        ;;
        ;; 3. Eliminating redundancy. We don't need the
        ;; docstrings as a first approximation anyway because of
        ;; embark's help feature (which displays the same
        ;; information as its default action buffer but better
        ;; anyway because it's fuzzy-searchable) and because most
        ;; command names are reasonably self-explanatory.
        (defun embark-which-key-indicator ()
            "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
            (lambda (&optional keymap targets prefix)
                (if (null keymap)
                        (which-key--hide-popup-ignore-command)
                    (which-key--show-keymap
                     (if (eq (plist-get (car targets) :type) 'embark-become)
                             "Become"
                         (format "Act on %s '%s'%s"
                                 (plist-get (car targets) :type)
                                 (embark--truncate-target (plist-get (car targets) :target))
                                 (if (cdr targets) "…" "")))
                     (if prefix
                             (pcase (lookup-key keymap prefix 'accept-default)
                                 ((and (pred keymapp) km) km)
                                 (_ (key-binding prefix 'accept-default)))
                         keymap)
                     nil nil t (lambda (binding)
                                   (not (string-suffix-p "-argument" (cdr binding))))))))

        (setq embark-indicators
              '(embark-which-key-indicator
                embark-highlight-indicator
                embark-isearch-highlight-indicator))

        (defun embark-hide-which-key-indicator (fn &rest args)
            "Hide the which-key indicator immediately when using the completing-read prompter."
            (which-key--hide-popup-ignore-command)
            (let ((embark-indicators
                   (remq #'embark-which-key-indicator embark-indicators)))
                (apply fn args)))

        (advice-add #'embark-completing-read-prompter
                    :around #'embark-hide-which-key-indicator)
;;;;; Add useful Hyperbole-style actions to Embark
;;;;;; Search DuckDuckGo for the given term
        (defun embark-internet-search (term)
            (interactive "sSearch Term: ")
            (browse-url
             (format "https://duckduckgo.com/search?q=%s" term)))
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
            (message kmacro)
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

        (define-key embark-general-map "a" 'flymake-vale-add-vocab)
        (define-key embark-general-map "r" 'flymake-vale-remove-vocab)
        (add-to-list 'embark-keymap-alist '(kmacro . embark-kmacro-map)))
;;;;; Embark-Consult integration package
    (use-package embark-consult
        :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;;; Writable Grep is a necessity
    (use-package wgrep
        :defer t
        :custom
        (wgrep-auto-save-buffer t)
        (wgrep-enable-key "i")))

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

(defmacro quake-emacs-create-keymap (&rest args)
    "Create a new keymap with the given key definitions and return it.
Uses the same syntax and semantics as `quake-emacs-define-key'."
    (declare (indent defun))
    (cl-with-gensyms (keymap)
        `(let ((,keymap (make-keymap)))
             (quake-emacs-define-key ,keymap ,@args)
             ,keymap)))
;;;; Core Keybindings
(defun quake-repeatize (keymap)
    "Add `repeat-mode' support to a KEYMAP."
    (map-keymap
     (lambda (_key cmd)
         (when (symbolp cmd)
             (put cmd 'repeat-map keymap)))
     (symbol-value keymap)))

(defun ispell-autocorrect-word ()
    "Replace the misspelled word under point with ispell's best-guess
alternative. Useful for fixing simple typos. Uses
`flyspell-auto-correct-word' if flyspell-mode is active."
    (interactive)
    (if flyspell-mode
            (flyspell-auto-correct-word)
        (cl-letf (((symbol-function 'ispell-command-loop)
                   (lambda (miss guess word start end) (car miss))))
            (ispell-word))))

(defun core/keys-layer ()
    "Defines all the custom keybindings for Quake Emacs and pull
in god mode, since both evil and non-evil users willprobably want it.

External Packages:

- `god-mode' - God mode provides a mode where you can run all
  normal Emacs commands without modifier keys. A useful way to
  bring the ergonomics of modal editors to Emacs without changing
  vanilla behavior as much as possible.

- `expreg' - A version of the famous `expand-region' package that
  incorporates treesit.el support and behaves in a slightly more
  simple and predictable way

- `puni' - Puni provides Smartparens-style structural editing
  based on Emacs's already exising structural editing commands,
  instead of reinventing the wheel, meaning that it can take
  advantage of the syntax tables already available for those
  commands in most language modes instead of having to write
  custom language specific logic itself, and integrate better
  with existing Emacs behavior and commands. It also means that
  it can automatically take advantage of tree sitter, because
  Emacs's built in structural editing commands do, instead of
  having to maintian its own library of queries."

;;;; Embark Should Always Work
    (quake-emacs-define-key override-global-map
        "M-RET" (lambda ()
                    (interactive)
                    (if (and (eq major-mode 'org-mode) (looking-at-p "^\\|$"))
                            (org-meta-return)
                        (embark-act)))
        "C-<return>" #'embark-dwim)
;;;;; Window Management
    (quake-emacs-define-key window-prefix-map
        "b"    #'windmove-left
        "n"    #'windmove-down
        "p"    #'windmove-up
        "f"    #'windmove-right
        "u"    #'winner-undo
        "r"    #'winner-redo)
;;;;; God Mode
    (use-package god-mode
        :config
        (defun quake-god-mode-update-cursor-type ()
            (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

        (add-hook 'post-command-hook 'quake-god-mode-update-cursor-type)
        (defun quake-god-mode-toggle-on-overwrite ()
            "Toggle god-mode on overwrite-mode."
            (if (bound-and-true-p overwrite-mode)
                    (god-local-mode-pause)
                (god-local-mode-resume)))

        (add-hook 'overwrite-mode-hook #'quake-god-mode-toggle-on-overwrite)

        (define-key god-local-mode-map (kbd ".") #'repeat)
        (define-key god-local-mode-map (kbd "i") #'god-local-mode)
        (global-set-key (kbd "<escape>") #'(lambda () (interactive) (god-local-mode 1))))

;;;;; Structural editing with Puni Mode
    ;; Use puni-mode globally and disable it for term-mode.
    (use-package puni
        :bind (("C-)" . puni-slurp-forward)
               ("C-}" . puni-barf-forward)
               ("C-(" . puni-slurp-backward)
               ("C-{" . puni-barf-backward)
               ("C-%" . puni-split)
               ("C-&" . puni-convolute)
               ("C-^" . puni-raise)
               ("C-$" . puni-splice))
        :hook ((text-mode . puni-mode)
               (prog-mode . puni-mode)))

;;;;; Define useful editing keys
    (quake-emacs-define-key global-map
        "C-x C-S-T"  'transpose-regions
        "C-M-i"      'ispell-autocorrect-word
        "M-S-U"      'upcase-dwim
        "C-S-U"      'upcase-char
        "C-S-R"      'overwrite-mode
        "M-F"        'forward-to-word
        "M-B"        'backward-to-word
        "M-A"        'align-regexp
        "M-S"        'sort-lines
        "M-R"        'reverse-region
        "M-j"        'join-line
        "C-."        'repeat
        "C-'"        'puni-expand-region
        "C-\""       'puni-contract-region
        "C-x @"      'rectangle-mark-mode
        "C-="        'indent-region)

;;;;; Yasnippet
    (quake-emacs-define-key global-map
        "C-c &"    (cons "Code Snippets..."
                         (quake-emacs-create-keymap
                             "n"   'yas-new-snippet
                             "s"   'yas-insert-snippet
                             "v"   'yas-visit-snippet-file)))
;;;;; General Quake-recommended keybindings
    (quake-emacs-define-key global-map
;;;;;; Org Notes
        "C-c a"                 'org-agenda
        "C-c P"                 'org-publish
        "C-c c"                 'org-capture
        "C-c l"                 'org-store-link
        "C-c L"                 'org-insert-link-global
        "C-c n"                 'quake-org-new-note-file
        "C-c A"                 'consult-org-agenda
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
        "C-c w"                 'eww
;;;;;; Top-level keybindings for convenience
        "C-~"                   'shell-toggle
        "C-c C-j"               'eval-print-last-sexp
;;;;;; Buffer manipulation
        "C-x S-K"               'kill-current-buffer
        "C-x B"                 'ibuffer
;;;;;; Project.el
        "C-x p E"               'flymake-show-project-diagnostics
;;;;;; Eglot
        "C-c e"   (cons "LSP Server..."
                        (quake-emacs-create-keymap
                            "E" 'flymake-show-buffer-diagnostics
                            "f" 'consult-flymake
                            "e" 'eglot
                            "a" 'eglot-code-actions
                            "r" 'eglot-rename
                            "h" 'eldoc
                            "f" 'eglot-format-buffer
                            "R" 'eglot-reconnect))

;;;;;; Helpful
        "C-h v"                 'helpful-variable
        "C-h f"                 'helpful-callable
        "C-h k"                 'helpful-key
        "C-h x"                 'helpful-command))

;;; ======Task Specific Layers======
;;;; Coding layer
(defun task/coding-layer ()
    "All the basic components needed for a Visual Studio Code-style
  IDE-lite experience in Emacs... but better.

  External Packages:
  - `magit', the powerful Git user interface that lets you do
  anything from trivial to complex git commands with just a few
  mnemonic keypresses, all with helpful command palettes to guide
  you on your way, and `diff-hl' so you can see what's changed in-editor
  - `dired-sidebar', to provide a project sidebar that builds on
  the powerful file manager already built into Emacs, instead of
  reinventing the wheel by creating its own file management
  system.
  - `treesit-auto', to automatically install and use the tree-sitter mode
  for any recognized language, so you have IDE-class syntax
  highlighting for nearly anything, out of the box.
  - `corfu', the faster, slimmer, yet more featureful
  universal (available anywhere) auto-completion UI for Emacs
  - `apheleia', as an auto-formatter, so you never need to worry about your
  formatting not matching a project's again.
  - `yasnippet' and `yasnippet-corfu', so you don't have to type all
  that rote boilerplate"
;;;;; A basic programming mode using built in things
    (electric-pair-mode 1)
    (add-hook 'prog-mode-hook (defun progify-mode ()
                                  (prettify-symbols-mode 1)
                                  (display-line-numbers-mode 1)
                                  (setq display-line-numbers 'relative)
                                  (hl-line-mode t)
                                  (eglot-ensure)
                                  (flymake-mode)))
    (add-hook 'flymake-mode-hook (lambda ()
                                     (setq next-error-function 'flymake-goto-next-error)))
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

    (use-package dired-sidebar
        :commands (dired-sidebar-toggle-sidebar)
        :custom
        (dired-sidebar-should-follow-file t)
        (dired-sidebar-use-custom-font t)
        :init
        (add-hook 'dired-sidebar-mode-hook
                  (lambda ()
                      (unless (file-remote-p default-directory)
                          (auto-revert-mode))))
        :config
        (setq dired-sidebar-theme 'nerd-icons)
        (set-face-attribute 'dired-sidebar-face nil :inherit 'variable-pitch))

;;;;; Treesit and Eglot (LSP) configuration
    (customize-set-variable 'treesit-font-lock-level 4)

    (use-package treesit-auto
        :custom
        (treesit-auto-install 'prompt)
        :config
        (treesit-auto-add-to-auto-mode-alist 'all)
        (global-treesit-auto-mode))

    (use-package eglot
        :commands (eglot eglot-ensure)
        :init
        (unless debug-on-error
            (fset #'jsonrpc--log-event #'ignore)
            (setq eglot-events-buffer-size 0))
        :config
        (add-hook 'eglot-managed-mode-hook
                  (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
        (setq eglot-autoshutdown t
              eglot-sync-connect nil))

;;;;; Eglot-compatible Debug Adapter Protocol client (for more IDE shit)
    (use-package dape
        :commands (dape)
        :preface
        (setq dape-key-prefix nil)
        :init
        (setq dape-buffer-window-arrangement 'right)
        :config
        ;; Kill compile buffer on build success
        (add-hook 'dape-compile-compile-hooks 'kill-buffer))
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
        (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
        (corfu-auto t)                 ;; Enable auto completion
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
(defun quake-org-add-note (file)
    "Adds an org file to your agenda text search and consult note files."
    (interactive "FNote file: ")
    (add-to-list 'consult-notes-org-headings-files file)
    (add-to-list 'org-agenda-text-search-extra-files file))

(defun quake-org-add-note-directory (dir)
    "Adds a whole directory of notes using `quake-org-add-note'."
    (interactive "DNotes directory: ")
    (mapcar #'quake-add-note (directory-files dir)))

(defun quake-org-new-note-file (file)
    "Creates a new note file in your `quake-org-home-directory'"
    (interactive (list (read-file-name "Note file name: " quake-org-home-directory)))
    (with-temp-file file
        (insert ""))
    (quake-org-add-note file))

(defun quake--org-mode-pretty ()
    "Enables a large amount of prettification for `org-mode' files."
    (org-indent-mode 1)
    (setq *previous-line-spacing* line-spacing)
    (setq line-spacing 0.1)
    (setq-local visual-fill-column-width 65)
    (column-number-mode -1)
    (buffer-face-mode 1)
    (ligature-mode -1)
    (flymake-vale-load)
    (flymake-mode 1)
    (prettify-symbols-mode -1))

(defun flymake-vale-add-vocab ()
    (interactive)
    (let ((word (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
        (with-temp-buffer
            (newline)
            (insert word)
            (write-region (point-min) (point-max) "~/.config/vale/styles/config/vocabularies/wordlist/accept.txt" t))
        (flymake-after-change-function (car bounds) (cdr bounds) (- (cdr bounds) (car bounds)))))
(defun flymake-vale-remove-vocab ()
    (interactive)
    (let ((word (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
        (with-temp-buffer
            (newline)
            (insert word)
            (write-region (point-min) (point-max) "~/.config/vale/styles/config/vocabularies/wordlist/reject.txt" t))
        (flymake-after-change-function (car bounds) (cdr bounds) (- (cdr bounds) (car bounds)))))
(defun task/writing-layer ()
    "Adds a focus mode with prose linting, pandoc, and a latex preview
pane, as well as prose-appropriate line wrapping. Also configures
org mode for vanilla-org zettelkesten note-taking based on
[[https://github.com/egh/org-mode-zettelkasten][this guide]].

  External Packages:
  - `htmlize', to convert org buffers into html buffers for e.g.
    sending mail,
  - `flymake-vale', so that you can get fast, accurate,
    markup-aware prose linting and fast(er than `flyspell-mode')
    spell checking in all your text, latex, org mode, markdown,
    and email buffers (to correct misspelt words, use ispell,
    since vale doesn't offer that),
  - `org-toc', for generating tables of contents inside an org
    buffer instead of during export,
  - `pandoc-mode' one mode to rule them all for managing
    conversions and compilations of all your files!
  - `visual-fill-column' for dealing with those line-paragraphs
  - `latex-preview-pane', so if you're writing LaTeX, you can see
    what it will produce.
  - `wucuo-mode' so that flyspell only runs within the currently
    visible region of a file, significantly speeding up what is
    otherwise an extremely slow process which is notorious for
    locking Emacs up"
;;;;; Set up org mode
    (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
    (unless (fboundp 'flymake-vale-load)
        ;; Suggested Vale config:
        ;;
        ;; StylesPath = /var/home/<username>/.config/vale/styles
        ;;
        ;; MinAlertLevel = suggestion
        ;;
        ;; Packages = proselint
        ;; Vocab = wordlist
        ;;
        ;; [*.{md,org}]
        ;;
        ;; BasedOnStyles = Vale, proselint
        (package-vc-install "https://github.com/tpeacock19/flymake-vale"))
    (use-package org
        :commands (org-mode consult-org-agenda org-agenda org-insert-link-global org-publish)
        :custom
        (org-use-speed-commands t)
        (org-agenda-inhibit-startup t)
        ;; TODO: if I can figure out a good cache invalidation scheme, (org-refile-use-cache t)
        (org-agenda-window-setup 'other-window)
        (org-complete-tags-always-offer-all-agenda-tags t)
        (org-id-link-to-org-use-id t)
        (org-id-extra-files 'org-agenda-text-search-extra-files)
        (org-refile-use-outline-path t)
        (org-outline-path-complete-in-steps nil)
        (org-refile-targets '((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 5)))
        :config
        (defun quake-org-add-ids-to-headlines-in-file ()
            "Add ID properties to all headlines in the current file which
do not already have one."
            (interactive)
            (org-map-entries 'org-id-get-create))

        (add-hook 'org-mode-hook (defun quake--org-mode-hooks ()
                                     ;; Cooperate with imenu. This should exist by default, dammit!
                                     (imenu-add-to-menubar "Imenu")
                                     ;; Prettify
                                     (quake--org-mode-pretty)
                                     ;; Add IDs to everything after save.
                                     (add-hook 'before-save-hook 'quake-org-add-ids-to-headlines-in-file)))


        ;; Zettelkasten configuration
        (defun org-id-complete-link ()
            "Create an id: link using completion."
            (concat "id:" (org-id-get-with-outline-path-completion org-refile-targets)))

        (org-link-set-parameters "id" :complete 'org-id-complete-link)

        ;; TODO: Replace this with
        ;; [[https://github.com/toshism/org-super-links]] when
        ;; Emacs 30 rolls around and we have use-package :vc
        (defun quake-org-backlinks ()
            "Search for backlinks to current entry."
            (interactive)
            (let ((link (condition-case nil
                                (org-id-store-link)
                            (error "Unable to create a link to here"))))
                (org-occur-in-agenda-files (regexp-quote link))))

        (setq org-directory quake-org-home-directory)
        (setq org-agenda-files (cons quake-org-home-directory
                                     (cl-remove-if-not #'file-directory-p
                                                       (directory-files-recursively quake-org-home-directory "" t (lambda (dir) (length= (directory-files dir nil "\\.agenda-ignore" t 1) 0)) t))))

        ;;(org-id-update-id-locations)
        (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

        (quake-repeatize 'org-mode-map)

        (custom-set-faces
         '(org-default ((t (:inherit variable-pitch :height 160))))
         '(org-level-1 ((t (:height 2.0))))
         '(org-level-2 ((t (:height 1.7))))
         '(org-level-3 ((t (:height 1.4))))
         '(org-level-4 ((t (:height 1.1))))
         '(org-level-5 ((t (:height 1.0))))
         '(org-block ((t (:inherit fixed-pitch))))
         '(org-code ((t (:inherit fixed-pitch))))
         '(org-verbatim ((t (:inherit fixed-pitch)))))

        (setq org-ellipsis "  " ;; folding symbol
              org-startup-indented t
              org-image-actual-width (list 300)      ; no one wants gigantic images inline
              org-pretty-entities t                  ; part of the benefit of lightweight markup is seeing these
              org-agenda-block-separator ""
              org-fontify-done-headline t
              org-fontify-quote-and-verse-blocks t)

        (defun quake-org-prompt-note-file ()
            (interactive)
            (find-file
             (completing-read-default "Note file: "
                                      (let ((files (directory-files-recursively quake-org-home-directory ".*.org")))
                                          (lambda (str pred flag)
                                              (pcase flag
                                                  ('metadata `(metadata (category . file)))
                                                  (_ (all-completions str files pred)))))))
            (goto-char (point-min))
            (org-end-of-subtree))
        (setq org-capture-templates
              `(("t" "Todo" entry (function quake-org-prompt-note-file)
                 "* TODO %?\n  %i\n  %a")
                ("n" "Note" entry (function quake-org-prompt-note-file)
                 "* %?\n  %i\n  ")
                ("j" "Journal" entry (function quake-org-prompt-note-file)
                 "* %?\nEntered on %U\n  %i\n  %a"))))
    (use-package htmlize
        :after (org-mode))
    (use-package toc-org
        :hook (org-mode . toc-org-mode))
    (use-package pandoc-mode
        :hook ((markdown-mode . pandoc-mode)
               (org-mode . pandoc-mode)
               (latex-mode . pandoc-mode)
               (doc-view-mode . pandoc-mode))))

;;; ======Aesthetic Packages======
;;;; Core Aesthetic Packages
(defun get-lines (string)
    (thread-last string
                 string-lines
                 (mapcar #'string-trim)
                 (cl-remove-if (lambda (x) (string-empty-p x)))))
(defun quake-check-for-updates ()
    (interactive)
    (let* ((default-directory "~/.emacs.d/")
           (buffer (generate-new-buffer "*git version check*"))
           (process (start-process-shell-command
                     "*git version check*"
                     buffer
                     ;; we merge these into one command using
                     ;; shell because I hate callback hell and we
                     ;; don't have promises in elisp yet :)
                     "git --no-pager tag -l --no-color; echo \"---\"; git --no-pager ls-remote --tags origin")))
        (set-process-sentinel
         process
         (lambda (proc event)
             (with-current-buffer buffer
                 (pcase (process-status proc)
                     ('exit
                      (when-let* ((halves (mapcar #'get-lines (string-split (buffer-string) "---")))
                                  (current-version (caar halves))
                                  (remote-version-line (caadr halves))
                                  (remote-version (save-match-data
                                                      (string-match
                                                       (rx (group "v" num "." num "." num (optional "-alpha." num)))
                                                       remote-version-line)
                                                      (match-string 1))))
                          (if (not (string= remote-version current-version))
                                  (message "There is an update available for Quake Emacs: %s"
                                           (propertize (format "%s -> %s" current-version remote-version)
                                                       'face `(:foreground "orange")))
                              (message "%s" (propertize "Quake Emacs is up to date"
                                                        'face `(:foreground "green"))))
                          (kill-buffer buffer)))))))))
(defun quake-update ()
    (let ((default-directory "~/.emacs.d/")
          (process (start-process "*quake update process*"
                                  (get-buffer-create "*quake update process*")
                                  "git"
                                  "pull")))
        (set-process-sentinel
         process
         (lambda (proc event)
             (pcase event
                 ('exit (if (> (process-exit-status proc) 0)
                                (message "%s" (propertize "There was an error updating Quake Emacs. See *quake update process* for more details" 'face `(:foreground "red")))
                            (message "%s%s"
                                     (propertize "Quake Emacs has successfully been updated, please restart with " 'face `(:foreground "green"))
                                     (key-description (vector "C-c" "p" "r"))))))))))
(defun core/aesthetic-layer ()
    "If you're going to be staring at your editor all day, it might as well look nice.

  External Packages:
  - `doom-themes', for an unparalleled collection of excellent
  themes, so you never have to go searching for a theme again
  - `eldoc-box', because documentation needs to look nice and appear
  next to your cursor so you don't have to move your eyes
  - `breadcrumb', so you don't get lost
  - `visual-fill-column', so that text is centered in buffers and
    pleasingly visually wrapped to a reasonable size. Makes
    writing and even code-editing nicer."
;;;;; Make Info manuals nicer to read
    (add-hook 'Info-mode-hook 'buffer-face-mode)
;;;;; Fill Columns and Center
    (use-package visual-fill-column
        :config
        (setq-default visual-fill-column-center-text t)
        (setq-default visual-fill-column-width 120)
        (global-visual-fill-column-mode))
;;;;; Themes
    ;; Although this is big and relatively slow (one of the slowest
    ;; things I include) doom's themes are just too good to pass up
    (use-package doom-themes
        :config
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t
              doom-themes-enable-italic t)
        (load-theme quake-color-theme t))

    ;; Make things roomier and more minimal/austere
    (use-package spacious-padding
        :after (doom-themes)
        :config
        (setq spacious-padding-widths `( :internal-border-width 20
                                         :header-line-width 4
                                         :mode-line-width 6
                                         :tab-width 4
                                         :right-divider-width 30
                                         :scroll-bar-width 8
                                         :fringe-width 8))
        (setq spacious-padding-subtle-mode-line
              '(:mode-line-active variable-pitch :mode-line-inactive variable-pitch))
        (advice-add 'spacious-padding-set-face-box-padding
                    :filter-return
                    (lambda (res)
                        (append '(:inherit variable-pitch :weight normal) res)))
        (spacious-padding-mode 1))

    ;; Keep as much information as possible from the vanilla
    ;; modeline, but make it a bit cleaner/more comprehensible
    ;; write a function to do the spacing

    (setq-default mode-line-format
                  `("%e"
                    (:eval
                     (cond
                      ((boundp 'evil-mode-line-tag) evil-mode-line-tag)
                      ((and (fboundp 'god-mode) god-local-mode) "<G>")
                      ((and (not god-local-mode))
                       (propertize "<I>" 'face
                                   '(:foreground ,(face-background 'region))))
                      (t "")))
                    "    "
                    mode-line-front-space
                    (:eval (if (buffer-modified-p) "● " "   "))
                    mode-line-buffer-identification
                    " %o    "
                    (:eval (system-name))
                    "    "
                    vc-mode
                    "    "
                    mode-line-end-spaces
                    "    "
                    mode-line-modes
                    (:eval (propertize " "
                                       'display
                                       `(space :align-to (- (+ right right-fringe right-margin) 20))))
                    global-mode-string))

    ;; A lightweight dashboard
    (setq initial-scratch-message
          (apply #'concat (mapcar (lambda (string)
                                      (concat (propertize string 'display `(space :align-to (- center  ,(/ (string-width string) 2)))) string))
                                  (list
                                   (propertize "----QUAKE EMACS----" 'display (create-image "~/.emacs.d/banner-quake.png")) "\n\n"
                                   (propertize (format "Started in %s" (emacs-init-time "%.2f seconds")) 'face `(:inherit font-lock-comment-face)) "\n\n"
                                   (concat "To open a file press " (propertize "{C-x C-f}" 'face 'bold)) "\n"
                                   (concat "To open a project press " (propertize "{C-x p p}" 'face 'bold)) "\n"
                                   (concat "To open a recent file press " (propertize "{C-x C-r}" 'face 'bold)) "\n"
                                   (concat "To close Emacs press " (propertize "{C-x C-c}" 'face 'bold)) "\n"
                                   "\n"
                                   "You can also enter lisp forms and evaluate them here." "\n\n" " "))))
    (add-hook 'post-self-insert-hook (defun scratch-buffer-enable-lisp ()
                                         (when (and (not (eq major-mode 'emacs-lisp-mode))
                                                    (string= (buffer-name (current-buffer))
                                                             "*scratch*"))
                                             (emacs-lisp-mode))))

    (use-package eldoc-box
        :config
        (set-face-attribute 'eldoc-box-body nil :inherit 'variable-pitch :weight 'normal)
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

External Packages:
- `hl-todo', so you never miss those TODOs and FIXMEs
- `nerd-icons', `nerd-icons-completion', and `nerd-icons-corfu',
because there's really nothing better than a nice set of icons to
spice things up, and we want integration *everywhere*
- `ligature', because ligatures are cool"
;;;;; Tab Bar
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

;;;;; Highlight TODOs, FIXMEs, etc
    (use-package hl-todo
        :commands (hl-todo-mode)
        :hook (prog-mode . hl-todo-mode))

;;;;; Icons
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

    ;; Integrate them with dired
    (use-package nerd-icons-dired
        :commands (nerd-icons-dired-mode)
        :hook (dired-mode . nerd-icons-dired-mode))
;;;;; Ligatures
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

;;; ======Appendix======
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

(autoload 'optional/evil-layer "~/.emacs.d/evil-layer.el")

;;; ======Load Layers======
;; Enable layers
(dolist (layer quake-enabled-layers)
    (setq start-time (current-time))
    (condition-case-unless-debug err
            (funcall layer)
        (error
         (display-warning :error (format "Error occured in layer %S: %s" layer (error-message-string err)))))
    (message "Finished enabling layers %s in %.2f seconds" layer (float-time (time-since start-time))))
(add-hook 'emacs-startup-hook #'quake-check-for-updates)
(profiler-stop)
