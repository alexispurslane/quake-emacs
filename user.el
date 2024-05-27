;; This is a template designed to be copied to ~/.quake.d and
;; then manually modified by you to have all your customizations
;; in it.

;; Top level expressions are evaluated before the layers run, if
;; you want to run something after the layers, create a layer and
;; put it after in the layer order

(setq quake-enabled-layers
      (list
       #'core/usability-layer
       #'core/editor-layer
       #'task/coding-layer
       #'task/writing-layer
       #'task/notes-layer
       #'core/aesthetic-layer
       #'optional/bling-layer
       ;; optional/blog-layer
       ))
