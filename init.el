;;; Package --- init.el
;;; Commentary:
;;;  My Emacs dotfile

;;; Code:

(setenv "LANG" "en_US.UTF-8")

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-check-signature nil)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable JIT
(setq native-comp-deferred-compilation t)
(setq package-native-compile t)

;; Set Korean
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(global-set-key (kbd "<S-kana>") 'toggle-input-method)
;; (global-set-key (kbd "TAB") 'indent-relative)

(global-display-line-numbers-mode)
(electric-pair-mode)
(recentf-mode 1)
(show-paren-mode t)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist '(font . "Hack-12"))
(set-face-attribute 'default t :font "Hack-12")
(set-fontset-font t 'hangul (font-spec :name "D2Coding-12"))

(setq gc-cons-threshold 10000000)
(setq read-process-output-max (* 1024 1024))
(setq warning-minimum-level :emergency)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq scroll-conservatively 101)
(setq tab-always-indent nil)
(setq-default recentf-max-menu-items 25)
(setq-default recentf-max-saved-items 25)
(setq-default show-paren-delay 0)
(setq-default show-paren-style 'parenthesis)
(setq-default show-paren-when-point-inside-paren t)
(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default comint-move-point-for-output t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default display-line-numbers-type 'list)
(setq-default display-fill-column-indicator-column 80)
(setq-default c-tab-always-indent nil)
;; Remove all whitespaces when type "#"
(setq-default c-electric-pound-behavior '(alignleft))

(setq inhibit-startup-screen t)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "my-c-style")

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode)
            (flycheck-mode t)))
(add-hook 'makefile-mode-hook 'my-makefile-config)
(add-hook 'makefile-gmake-mode-hook 'my-makefile-config)

(add-hook 'c-mode-common-hook
          (lambda ()
            (display-fill-column-indicator-mode)
            (my-set-c-style)))

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

;; Disable for speed
;; (use-package rainbow-identifiers
;;   :hook (prog-mode . rainbow-identifiers-mode))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Using vterm package when operating system is not Windows
(if (not (eq system-type 'windows-nt))
    (use-package vterm
      :custom
      (vterm-shell "/bin/zsh"))
  (when (executable-find "plink")
    (setq-default tramp-default-method "plink")))

(use-package ivy
  :config
  (ivy-mode 1))

(use-package magit)

(use-package golden-ratio
  :config
  (golden-ratio-mode t)
  (setq golden-ratio-auto-scale t)
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  buf-move-left
                  buf-move-right
                  buf-move-up
                  buf-move-down
                  window-number-select
                  select-window
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  select-window-6
                  select-window-7
                  select-window-8
                  select-window-9)))
  (setq golden-ratio-exclude-modes
        (append golden-ratio-exclude-modes
                '(term-mode))))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-minibuffer t)
  :config
  (evil-mode 1))
(use-package undo-tree
  :config
  (global-undo-tree-mode))
(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; (use-package powerline-evil
;;   :config
;;   (powerline-evil-vim-color-theme))

(use-package powerline
  :config
  (defpowerline powerline-minor-modes nil))

(use-package airline-themes
  :after powerline
  :custom
  (airline-cursor-colors nil)
  (airline-helm-colors nil)
  (airline-display-directory 'airline-directory-shortend)
  :config
  (load-theme 'airline-onedark t))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-find-usages-bindings t)
  :config (evil-collection-init))

(defun my-cargo-process-run-release()
  "Run cargo run --relase."
  (interactive)
  (cargo-process--start "Run" "run --release"))

(defun my-cargo-process-test()
  "Run cargo test."
  (interactive)
  (cargo-process--start "Test" "test --all"))

(defun my-open-dot-file()
  "Open the Emacs dotfile."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun my-open-repos()
  "Reload configurations from the Emacs dotfile."
  (interactive)
  (dired "~/repos"))

(defun my-load-dot-file()
  "Open the Emacs dotfile."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun my-makefile-config()
  "Reload the Emacs dotfile."
  (interactive)
  (setq tab-width 8)
  (setq indent-tabs-mode t))

(use-package general
  :config
  (general-define-key
   :keymaps 'lsp-command-map
   :wk-full-keys nil

   "h f"    '(lsp-ui-doc-focus-frame :which-key "focus document")
   "h u"    '(lsp-ui-doc-unfocus-frame :which-key "unfocus document"))

  ;; (general-define-key
  ;;  :keymaps 'c-mode-base-map

  ;;  "\C-m"   'newline-and-indent
  ;;  [ret]    'newline-and-indent)

  (general-define-key
   :states 'normal
   :prefix "SPC"

   ""      '(nil :which-key "my lieutenant general prefix")

   "b"     '(:ignore t :which-key "buffer prefix")
   "b d"   'kill-buffer

   "w"     '(:ignore t :which-key "window prefix")
   "w d"   'quit-window

   "f"     '(:ignore t :which-key "file prefix")
   "f e"   'find-file
   "f d"   'my-open-dot-file
   "f w"   'my-open-repos
   "f r"   'my-load-dot-file

   "s"     'vterm
   "g"     'magit
   "r"     'recentf-open-files
   "<tab>" 'mode-line-other-buffer

   "t"     'treemacs

   "p"     '(:ignore t :which-key "projectile prefix")
   "p r"   'projectile-ripgrep
   "p f"   'projectile-find-file
   "p s"   'projectile-run-vterm

   "c"     '(:ignore t :which-key "cargo prefix")
   "c n"   'cargo-process-new

   "h"     '(:ignore t :which-key "hl prefix")
   "h n"   'hl-todo-next
   "h p"   'hl-todo-previous

   "l"     '(:package lsp-mode :keymap lsp-command-map :which-key "lsp-mode prefix")))

(use-package lsp-mode
  :hook
  (((c-mode
     c++-mode
     cmake-mode) . lsp)
   (lsp-mode . (lambda () (let ((lsp-keymap-prefix "SPC l"))
                            (lsp-enable-which-key-integration)))))
  :commands lsp
  :custom
  (lsp-diagnostic-provider :flycheck)
  (lsp-completion-provider :capf)
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-auto-execute-action t)
  (setq lsp-completion-enable t)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-snippet t)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-text-document-color t)
  (setq lsp-enable-xref t)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "clangd")
    :major-modes '(c-mode c++-mode)
    :remote? t
    :server-id 'clangd-remote))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "cmake-language-server")
    :major-modes '(cmake-mode)
    :remote? t
    :server-id 'cmake-language-server-remote)))

(use-package which-key
  :config
  (which-key-setup-side-window-right)
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package yasnippet
  :config
  (add-hook 'lsp-mode-hook 'yas-minor-mode-on))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-symbol t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-doc-frame-mode 1)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company
  :config
  (push 'company-capf company-backends)
  (define-key company-mode-map (kbd "C-SPC") 'company-complete-common)
  (define-key company-active-map (kbd "C-l")   'company-complete-selection)
  (define-key company-active-map (kbd "<return>")   'company-complete-selection)
  (define-key company-active-map (kbd "C-h")   'company-abort)
  (define-key company-active-map (kbd "C-j")   'company-select-next)
  (define-key company-active-map (kbd "C-k")   'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (add-hook 'prog-mode-hook 'global-company-mode))

(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package popwin
  :config
  (popwin-mode 1))

(use-package all-the-icons)

(use-package projectile)

(use-package treemacs
  :hook
  (treemacs-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (progn
    (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
          treemacs-project-follow-cleanup t
          treemacs-workspace-switch-cleanup t)
    (treemacs-follow-mode t)
    (treemacs-fringe-indicator-mode t)
    (treemacs-git-mode 'deferred)))
(use-package treemacs-evil
  :after treemacs evil)
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit)
(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

;; Emacs Lisp
(use-package company
  :config
  (push 'company-elisp company-backends))

;; Rust
;; (use-package lsp-mode
;;   :hook
;;   (rust-mode . lsp)
;;   :custom
;;   (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
;;   (lsp-rust-analyzer-display-parameter-hints t)
;;   (lsp-rust-analyzer-proc-macro-enable t)
;;   (lsp-rust-analyzer-server-display-inlay-hints t))

(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb))

(use-package flycheck-rust
  :hook
  (rust-mode . flycheck-mode))
(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode))
(use-package toml-mode)

(use-package cmake-mode)

(use-package dtrt-indent
  :config
  (dtrt-indent-global-mode t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-input-method "korean-hangul")
 '(package-selected-packages
   '(dtrt-indent cmake-mode toml-mode cargo flycheck-rust dap-mode treemacs-icons-dired treemacs-magit treemacs-projectile treemacs-evil treemacs projectile all-the-icons popwin hl-todo company-box company-shell company lsp-ui yasnippet which-key lsp-mode general evil-collection airline-themes powerline undo-fu undo-tree evil golden-ratio magit ivy vterm auto-package-update dracula-theme use-package))
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'lsp-format-buffer nil t)
     (dtrt-indent-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((min-colors 16777216)) (:background "#282a36" :foreground "#f8f8f2")) (t (:background "#000000" :foreground "#f8f8f2")))))

(provide 'init)
;;; init.el ends here
