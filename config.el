;;; config.el -*- lexical-binding: t; -*-

;;
;; Personal info
;;

(setq user-full-name "azzamsa"
      ;; prevent search engines from indexing the user's email address
      user-mail-address (concat "vcs" "@" "azzamsa" "." "com"))
;;
;; Better defaults
;;

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; Sort by modified time
(setq dired-listing-switches "-alhFt")
;; Ranger doesn't pick up `dired-listing-switches'
(setq ranger-listing-switches "-althFt")
;; Both don't work in Ranger. I need to set the sorting
;; manually and make it persists.
(setq ranger-persistent-sort t)
;; Delete files to trash, as an extra layer of precaution against
;; accidentally deleting wanted files.
(setq delete-by-moving-to-trash t)
;; scratch buffer mode
(setq initial-major-mode 'markdown-mode)

;;
;; Keybindings
;;

(use-package evil-colemak-basics
  :demand t
  :straight t
  ;; :after evil
  :init
  (setq evil-colemak-basics-layout-mod `mod-dh)
  (global-evil-colemak-basics-mode))

;;
;; UI
;;

;;
;; Why <current theme name>?
;; 1.  `catpuccin-*' has so manny issue I need to fix myself
;; - [active region almost unnoticable · Issue #112 · catppuccin/emacs](https://github.com/catppuccin/emacs/issues/112)
;; - [No iedit faces specified (used by evil-multiedit) · Issue #108 · catppuccin/emacs](https://github.com/catppuccin/emacs/issues/108)
;; doesn't have noticeable region color during evil multi-cursor.
;; 2. `doom-palenight' too muted for me.
;; 2. `doom-material' has she same issue as catppuccin. The highlighted text is unnoticable.
(setq minemacs-theme 'doom-dracula)
;; Why <current font name>?
;; - `VictorMono Nerd Font' is too thin
(setq minemacs-fonts'(:font-family "Iosevka Nerd Font"
                                   :font-size 18
                                   :variable-pitch-font-family "Iosevka Nerd Font"
                                   :variable-pitch-font-size 18))

;;
;; Buit-in
;;

(with-eval-after-load 'company
  (setq company-idle-delay 1))

(with-eval-after-load 'eldoc
  (setq eldoc-idle-delay 3))

(use-package abbrev
  :defer 1
  :init
  (setq-default abbrev-mode t)
  :config
  (setq abbrev-file-name (concat minemacs-root-dir "abbrevs.el"))
  (setq save-abbrevs 'silently))

;;
;; Minemacs
;;

(setq +scratch-initial-major-mode 'markdown-mode)

(+deferred!
 (display-battery-mode -1)
 (display-time-mode -1))

(with-eval-after-load 'me-defaults
  (setq display-line-numbers-type t))

;;
;; Bring back favorite Doom's behavior
;;

;; https://github.com/doomemacs/doomemacs/blob/master/modules/ui/indent-guides/config.el
(use-package highlight-indent-guides
  :straight t
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  ;; optional. I need it to be brighther
  (set-face-foreground 'highlight-indent-guides-character-face (face-foreground 'font-lock-comment-face)))

;; SPC RET: bookmark-jump
(with-eval-after-load 'me-general-ready
  (+map!
    "RET" #'consult-bookmark
    ))

;; SPC /: project search
(with-eval-after-load 'me-completion
  (+map!
    "/"  #'consult-ripgrep
    ))

;; SPC SPC: find file in project
;; SPC x: persistent scratch buffer
(with-eval-after-load 'me-keybindings
  (+map!
    "SPC"  #'project-find-file
    "w x" #'scratch-buffer!
    ))

;; highlight yanked line
(use-package evil-goggles
  :straight t
  :init
  (evil-goggles-mode))

;;
;; Adopt MinEmacs default
;;

;; SPC w o: delete other windows
;; SCP /: search in project => SPC s s
;; SPC SPC: find a file in project => SPC f f

;;
;; Modules
;;

;; Emacs doesn't play well with fish
(setq shell-file-name "/bin/bash")

(setq projectile-project-search-path '("~/projects" "~/office" "~/playground"))

(use-package exec-path-from-shell
  :straight t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; It is the 21st century, should I save file manually?
(use-package super-save
  :straight t
  :defer 3
  :config
  (add-to-list 'super-save-triggers 'vertico)
  (add-to-list 'super-save-triggers 'magit)
  (add-to-list 'super-save-triggers 'find-file)
  (add-to-list 'super-save-triggers 'winner-undo)
  ;; Need to explicitly load the mode
  (super-save-mode +1))

(use-package all-the-icons :straight t)

;; reference: https://github.com/doomemacs/doomemacs/blob/master/modules/ui/neotree/config.el
(use-package neotree
  :straight t
  :init
  (+map! "op" '(neotree-project-dir :wk "Side panel"))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq neo-autorefresh t)
  ;;work with projectile
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(?:git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
          ;; generated files, caches or local pkgs
          ;; `cdata': container data
          ;; `target': rust generated directory
          "^\\(?:node_modules\\|target\\|cdata\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(?:sync\\|export\\|attach\\)$"
          ;; temp files
          "~$"
          "^#.*#$")))

(defun neotree-project-dir ()
  "Always open NeoTree in project root."
  (interactive)
  (let ((project-dir (ignore-errors (project-root (project-current))))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (if file-name
                  (neotree-find file-name))))
      (message "Could not find project root."))))

;;; :tools magit
(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk 'all)
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

(use-package web-mode
  :defer t
  :mode "\\.njk\\'")

(use-package lsp-tailwindcss
  :straight t
  :after web-mode
  :config
  ;; LSP-mode doesn't know what is njk producing `Unable to calculate the languageId for buffer …'
  (add-to-list 'lsp-language-id-configuration '(".*\\.njk$" . "html")))

 ;;; :tools lsp
(+deferred!
 ;; Auto enable Eglot in modes `+eglot-auto-enable-modes' using
 ;; `+eglot-auto-enable' (from the `me-prog' module). You can use
 (+eglot-auto-enable)

 (with-eval-after-load 'eglot
   ;; You can use this to fill `+eglot-auto-enable-modes' with all supported
   ;; modes from `eglot-server-programs'
   (+eglot-use-on-all-supported-modes eglot-server-programs)))

;; Module: `me-natural-langs' -- Package: `spell-fu'
(with-eval-after-load 'spell-fu
  ;; We can use MinEmacs' helper macro `+spell-fu-register-dictionaries!'
  ;; to enable multi-language spell checking.
  (+spell-fu-register-dictionaries! "en" "id"))

;; With default `rust-mode' or `rust-ts-mode' I need to setup `eglot-server-programs'
;; to the location of ".local/share/rustup/toolchains/<target>/bin". It creates many more errors.
;; Because we need to set every binary such as `cargo' to the same directory.
;; I just went to `rustic-mode' and solves everytings.
(use-package rustic
  :straight t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(use-package just-mode :straight t :defer t)

(use-package ron-mode
  :straight t
  :defer t
  :mode "\\.ron\\'")

(use-package pest-mode :straight t :defer t)

(use-package hurl-mode
  :straight (hurl-mode
             :pin "b5e7256"
             :host github :repo "Orange-OpenSource/hurl" :files ("contrib/emacs/*.el")))
;;
;; Misc
;;

;; If you installed Emacs from the source, you can add the source code
;; directory to enable jumping to symbols defined in Emacs' C code.
(setq source-directory "~/opt/emacs/")

;;
;; My tools
;;

(use-package aza-scripts
  :defer t
  :straight (scripts :local-repo "~/projects/aza-scripts/"))

;;;###autoload
(defun file-manager-here ()
  "Open current directory with default file manager"
  (interactive)
  (message "Opening file manager in current directory...")
  ;; `xdg-open' will pick the default file manager
  (start-process "" nil "flatpak-spawn" "--host" "xdg-open" "."))

;;;###autoload
(defun terminal-here ()
  "Open a new terminal with the current directory as PWD"
  (interactive)
  (message "Opening terminal in %s" default-directory)
  ;; Need to use `expand-file-name` to expand `~` into a full path
  ;; Otherwise, termhere fallback to `$HOME`
  ;; The Rust version of `termhere' only works with `call-process-shell-command',
  ;; `async-shell-command', and `shell-command'. But the (b)ash version works
  ;; out of the box. Including with `start-process'
  (call-process-shell-command (concat "termhere " (expand-file-name default-directory))))

(defun scratch-buffer! ()
  "Toggle persistent scratch buffer"
  (interactive)
  (let ((filename "~/.local/share/**scratch**.md"))
    (if-let ((buffer (find-buffer-visiting filename)))
        (if (eq (selected-window) (get-buffer-window buffer))
            (delete-window)
          (if (get-buffer-window buffer)
              (select-window (get-buffer-window buffer))
            (pop-to-buffer buffer)))
      (progn
        (split-window-vertically)
        (other-window 1)
        (find-file filename)))))
