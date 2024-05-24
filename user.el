;; This is a template designed to be copied to ~/.quake.d and
;; then manually modified by you to have all your customizations
;; in it.

(defun user/before-layer-load ()
    "Use this function for anything you want to run before Quake Emacs
loads its layers and does its own configuration. Necessary for
things like setting your `denote-directory' and your
`org-static-blog' settings."
    )

(setq quake-enabled-layers
      '(core/usability-layer
	core/editor-layer
	task/coding-layer
	task/writing-layer
	task/notes-layer
	core/aesthetic-layer
	optional/bling-layer
	;; optional/blog-layer
	))
