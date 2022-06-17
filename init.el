;;; init.el --- A lightweight emacs init file for coders

;; Author: CoolCoder93
;; Homepage: https://github.com/coolcoder93/CoolCoderEmacsSetup
;; Keywords: emacs-setup
;; Version: 0.1 - alpha
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This is a simple emacs config focused on
;; developers and people who code
;;
;; This is still in very early stages
;; of development and I'm still leraning
;; about emacs so I don't recommend using
;; this config. Please leave feedback.
;;
;; Features (planned to be) offered:
;; * ivy and counsel enabled by default
;; * Clean interface
;; * Major language support
;;
;; TODO:
;; * Clean up code
;; * Organize code
;; * Add a mode bar
;; * Add code completion
;; * Add NerdTree like functionality
;; * Add installation instructions
;;
;; Prerequisites:
;; * Solargraph (for ruby support)
;; *
;; 
;; Installation:
;; * 

;;; code:

;; setq declarations
(setq inhibit-startup-message t)

;;(setq mode-line-format
;;          (list
;;           "%m: "
;;           "buffer: %b, "
;;           "(%l,%c)"
;;           "-- user: "
;;           (getenv "USER")
;;	   "%s"))



(setq mode-line-format
  (list "-"
   'mode-line-mule-info
   'mode-line-modified
   'mode-line-frame-identification
   "%b--"
   (getenv "HOST")
   ":"
   'default-directory
   "   "
   'global-mode-string
   "   %[("
   '(:eval (format-time-string "%F"))
   'mode-line-process
   'minor-mode-alist
   "%n"
   ")%]--"
   '(which-function-mode ("" which-func-format "--"))
   '(line-number-mode "L%l--")
   '(column-number-mode "C%c--")
   '(-3 "%p")))


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Enable the emacs server
(server-start)

;; Use M-x recentf-open-files
(recentf-mode 1)

;; Disable modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; TODO: (menu-bar-mode -1)

(global-display-line-numbers-mode 1)

;; Add custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(counsel-load-theme)

;; Create custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Activate repositories
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages")))
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



;;; init.el ends here
