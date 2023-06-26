;;; modules.el -*- lexical-binding: t; -*-

;; This file can be used to override `minemacs-modules'
;; and `minemacs-core-modules'

;; Ordered list of enabled core modules
(setq minemacs-core-modules
      '(me-splash        ; Simple splash screen
        me-keybindings   ; Keybinding (general, which-key, hydra, ...)
        me-evil          ; Emacs as Vim (evil, evil-collection, evil-escape, evil-snipe, evil-numbers, ...)
        me-core-ui       ; Core UI (doom-themes, modus-themes, doom-modeline, ...)
        me-completion))  ; Completion (vertico, marginalia, corfu, cape, consult, embark, ...)

;; List of enabled modules
(setq minemacs-modules
      '(me-ui            ; User interface (focus, writeroom-mode, mixed-pitch, ...)
        me-editor        ; Editing (tempel, smartparens, unicode-fonts, ligature, ...)
        me-undo          ; Better undoing (undo-fu, undo-fu-session, vundo, ...)
        me-multi-cursors ; Multi-cursors editing (iedit, evil-mc, evil-iedit-state, ...)
        me-vc            ; Version control (magit, forge, core-review, diff-hl, ...)
        me-project       ; Project management (project, consult-project-extra, ...)
        me-prog          ; Programming stuff (tree-sitter, eglot, eldoc, eldoc-box, apheleia, editorconfig, ...)
        me-checkers      ; Static checkers (flymake, flymake-easy, ...)
        ;; me-debug         ; Debugging tools (gdb-mi, realgud, disaster, ...)
        ;; me-lsp        ; LSP and DAP (lsp-mode, dap-mode, consult-lsp, lsp-pyright, ccls, ...)
        ;; me-lisp          ; Lisps development (parinfer-rust, sly, macrostep, geiser, elisp, helpful, eros, ...)
        me-data          ; Data file formats (csv, yaml, toml, json, plantuml-mode, ...)
        me-extra         ; Extra features (better-jumper, crux, ...)
        me-natural-langs ; Natural language stuff (spell-fu, go-translate, eglot-ltex, ...)
        me-files         ; Files and directories (dirvish, treemacs, vlf, ...)
        me-workspaces ; Workspace separation (tabspaces, tab-bar, ...). NOTE: This is a WIP
        me-natural-langs ; Natural language stuff (spell-fu, go-translate, eglot-ltex, ...)
        me-window))      ; Frame & window tweaks

;; You can set `minemacs-disabled-packages' to disable some packages.
(push '(tempel-collection treemacs googles eglot-ltex) minemacs-disabled-packages)
