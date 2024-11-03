(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- requires v%s or higher" minver)))

(require 'package)
(setq package-archives
  '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
    ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 基础配置
(use-package emacs
  :config
  (global-display-line-numbers-mode)
  (setq-default auto-fill-function 'do-auto-fill)
  (setq make-backup-files nil)
  (global-set-key (kbd "C-j") nil))

;; 一些常用插件
(use-package evil
  ;:load-path "/home/zhugy/.emacs.d/evil"
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :hook (after-init . evil-mode))

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer)))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(delete-consecutive-dups
			       company-sort-by-occurrence))
  (setq company-backends '((company-capf
			    company-dabbrev-code
			    company-keywords
			    company-semantic
			    company-files
			    company-etags))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company which-key avy ivy evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
