;;; config.el -*- lexical-binding: t; -*-

;; Personal info
(setq user-full-name "azzamsa"
      user-mail-address (concat "vcs" "@" "azzamsa" "." "com"))
;;
;; Better defaults
;;

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; Sort by modified time
(setq dired-listing-switches "-alhFt")
;; Ranger doesn't pickup `dired-listing-switches'
(setq ranger-listing-switches "-althFt")
;; Both doesn't work in Ranger. I need to set the sorting
;; manually and make it persists.
(setq ranger-persistent-sort t)
;; Delete files to trash , as an extra layer of precaution against
;; accidentally deleting wanted files.
(setq delete-by-moving-to-trash t)

;;
;; Keybindings
;;

(use-package evil-colemak-basics
  :demand t
  :straight t
  :init
  (setq evil-colemak-basics-layout-mod `mod-dh)
  :config
  (global-evil-colemak-basics-mode))

;;
;; UI
;;

;;
;; Why <current theme name>?
;; - Unlike `doom-dracula', `catpuccin-*' doesn't have noticable region color during evil multi-cursor
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
  ;; (setq abbrev-file-name (concat doom-user-dir "abbrevs.el"))
  (setq save-abbrevs 'silently))

;;
;; Minemacs
;;

(+deferred!
 (display-battery-mode -1)
  (display-time-mode -1))

;;
;; Modules
;;

;; Emacs doesn't play well with fish
(setq shell-file-name "/bin/bash")

(setq projectile-project-search-path '("~/projects" "~/office" "~/playground"))

;; It is 21st century, should I save file manually?
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
(use-package neotree
  :straight t
  ;; :bind ("o p" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  ;;work with projectile
  (setq projectile-switch-project-action 'neotree-projectile-action))

(with-eval-after-load 'treemacs
  (add-to-list 'treemacs-litter-directories '("target"))
  (setq treemacs-workspace-switch-cleanup t))

;;; :tools magit
(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk 'all)
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

(use-package web-mode
  :defer t
  :mode "\\.njk\\'")

;; (use-package lsp-tailwindcss
;;   :straight t
;;   :after web-mode
;;   :config
;;   ;; lsp-mode doen't khow what is njk producing `Unable to calculate the languageId for buffer …'
;;   (add-to-list 'lsp-language-id-configuration '(".*\\.njk$" . "html")))

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

;; (use-package hurl-mode :pin "b5e7256" :recipe
;;   :straight t
;;   (:host github
;;    :repo "Orange-OpenSource/hurl"
;;    :files ("contrib/emacs/*.el")))

;;
;; Misc
;;

;; If you installed Emacs from source, you can add the source code
;; directory to enable jumping to symbols defined in Emacs' C code.
(setq source-directory "~/opt/emacs/")

;; I use Brave, and never use Chrome, so I replace chrome program with "brave"
(setq browse-url-chrome-program (or (executable-find "brave") (executable-find "chromium")))
