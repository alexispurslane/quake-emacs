;;; user.el --- a template for user-private customization of Quake Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; This is a template designed to be copied to ~/.quake.d and
;; then manually modified by you to have all your customizations
;; in it.

;; Top level expressions are evaluated before the layers run, if
;; you want to run something after the layers, create a layer and
;; put it after in the layer order

;;; Code:

;; DO NOT directly modify `quake-enabled-layers' unless you are
;; willing to manually update it every time Quake Emacs is
;; updated. Instead use `quake-layers' to remove built in layers
;; and add them. Here's a possible example:
;;
;; (quake-layers
;;  :enable ((user/prometheus-layer . 2)
;;           user/eshell-layer
;;           user/communication-layer
;;           user/containers-layer
;;           user/ebooks-layer
;;           user/syncthing-layer)
;;  :disable (optional/bling-layer))
