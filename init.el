;; setq declarations
(setq inhibit-startup-message t)

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

;; Create custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
