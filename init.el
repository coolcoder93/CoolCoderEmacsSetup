;; setq declarations
(setq inhibit-startup-message t
      visible-bell nil)

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

;; Activate MELPA repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Add custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
