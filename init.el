;; 检查 Emacs 版本
(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- requires v%s or higher" minver)))

;; 设定源码目录
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 自动下载并启用 use-package 插件
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
(electric-pair-mode t)                          ; 自动补全括号
(global-display-line-numbers-mode)              ; 全局显示行号
(setq-default auto-fill-function 'do-auto-fill) ; 全局自动换行

;; 快捷键配置
(global-set-key (kbd "C-j") nil) ; 将 C-j 作为一个前缀键

;; 一些常用插件
(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode))

(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer)))

(use-package company
  :ensure t
  :config
  (global-company-mode)
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

(use-package python
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
