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
(setq initial-major-mode 'text-mode)

;;
;; Keybindings
;;

(use-package evil-colemak-basics
  :after evil
  :demand t
  :straight t
  :init
  (setq evil-colemak-basics-layout-mod `mod-dh)
  (global-evil-colemak-basics-mode))

;;
;; UI
;;

;;
;; Why <current theme name>?
;; - Unlike `doom-dracula', `catpuccin-*' doesn't have noticeable region color during evil multi-cursor
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

;; IMO, modern editors have trained a bad habit into us all: a burning need for
;; completion all the time -- as we type, as we breathe, as we pray to the
;; ancient ones -- but how often do you *really* need that information? I say
;; rarely. So opt for manual completion:
(with-eval-after-load 'company
  (setq company-idle-delay 0.5))

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

(setq +scratch-initial-major-mode 'text-mode)

(+deferred!
 (display-battery-mode -1)
 (display-time-mode -1))

;;
;; Bring back favorite Doom's behavior
;;

;; https://github.com/doomemacs/doomemacs/blob/master/modules/config/default/config.el
(use-package drag-stuff
  :straight t
  :defer t
  :bind (("<M-up>"   . 'drag-stuff-up)
         ("<M-down>"  . 'drag-stuff-down)
         ("<M-left>"  . 'drag-stuff-left)
         ("<M-right>" . 'drag-stuff-right)))

;; auto comment next line
;; https://github.com/doomemacs/doomemacs/blob/master/modules/config/default/config.el#L3
;; I don't think it is possible. The code is too complex. It needs to know every possible language mode

;; https://github.com/doomemacs/doomemacs/blob/master/modules/ui/indent-guides/config.el
(use-package highlight-indent-guides
  :straight t
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character))

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
(with-eval-after-load 'me-keybindings
  (+map!
    "SPC"  #'project-find-file
    ))

;; highlight yanked line

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

;; Load environments variables.
;; Need to set manually, because using `exec-path-from-shell' doesn't work

(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.local/share/cargo/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/.local/share/cargo/bin"))))

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
 ;; `+lsp-auto-enable' instead to automatically enable LSP mode in supported
 ;; modes (from the `me-lsp' module).
 (+eglot-auto-enable)

 (with-eval-after-load 'eglot
   ;; You can use this to fill `+eglot-auto-enable-modes' with all supported
   ;; modes from `eglot-server-programs'
   (+eglot-use-on-all-supported-modes eglot-server-programs)))

;; Module: `me-natural-langs' -- Package: `spell-fu'
(with-eval-after-load 'spell-fu
  ;; We can use MinEmacs' helper macro `+spell-fu-register-dictionaries!'
  ;; to enable multi-language spell checking.
  (+spell-fu-register-dictionaries! "en" "fr"))

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
