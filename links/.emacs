
;; emacs init file

;; a bit darker and adjust font size)
(load-theme 'tango-dark t)
(set-face-attribute 'default nil :height 110)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; refresh the cache of the contents in packages
(unless package-archive-contents
  (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-collection helm helm-ag ac-slime evil-surround paredit-everywhere rainbow-mode smartparens auto-complete-exuberant-ctags evil-paredit evil exwm slime magit notmuch)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; slime config
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-quicklisp slime-asdf))

;; we want evil by default
(require 'evil)
(evil-mode 1)
