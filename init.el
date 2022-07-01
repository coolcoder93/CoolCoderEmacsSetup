;;; init.el --- Emacs config for coders

;; Author: CoolCoder93
;; Homepage: https://github.com/coolcoder93/CoolCoderEmacsSetup
;; Keywords: emacs-config
;; Version: 0.2 - alpha
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This is a simple Emacs config focused on
;; developers and people who code
;;
;; This is still in very early stages
;; of development and I'm still learning
;; about Emacs so I don't recommend using
;; this config (yet).
;;
;; Features (planned to be) offered:
;; * ivy and counsel enabled by default
;; * Clean interface
;; * lsp-mode enabled
;; * Code completion
;; * Integrated shell(terminal)
;;
;; TODO:
;; * Clean up code
;; * Organize code
;; * Add NerdTree like functionality
;; * Add installation instructions
;; * Use org
;;
;; Installation:
;; *

;;; code:

;; setq declarations
(setq inhibit-startup-message 1)



;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Use M-x recentf-open-files
(recentf-mode 1)

;; Disable modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; TODO: (menu-bar-mode -1)

(global-display-line-numbers-mode 1)

;; Create custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Disable the line numbers for certain file types
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Activate repositories
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
			 
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; On first install run M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Run M-x package-install RET which-key and the uncomment the two following lines and the two lines in the lsp mode use-package config
;;(require 'which-key)
;;(which-key-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;;:config
  ;;(lsp-enable-which-key-integration t)
  )

;; Run M-x package-install RET lsp-java RET for java suppot and uncomment the two following lines
;;(require 'lsp-java)
;;(add-hook 'java-mode-hook #'lsp)

;; C/C++ hooks
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; For code completion run M-x package-install RET company RET
;; enable company-mode by uncomenting the following line
;;(add-hook 'after-init-hook 'global-company-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; init.el ends here
