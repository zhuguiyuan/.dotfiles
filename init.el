;; 检查 Emacs 版本
(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- requires v%s or higher" minver)))

;; 设定源码目录
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 自动下载并启用 use-package 插件
(require 'package)
; list the packages you want
(setq package-list '(use-package))
; list the repositories containing them
(add-to-list 'package-archives
	     '("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
(add-to-list 'package-archives
	     '("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))
(add-to-list 'package-archives
	     '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
; activate all the packages (in particular autoloads)
(package-initialize)
; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; 基础配置
(electric-pair-mode t)                          ; 自动补全括号
(global-display-line-numbers-mode)              ; 全局显示行号
(setq-default auto-fill-function 'do-auto-fill) ; 全局自动换行

;; 快捷键配置
(global-set-key (kbd "C-j") nil) ; 将 C-j 作为一个前缀键
(global-set-key (kbd "C-j C-k") 'kill-whole-line) ; 删除光标所在的行

;; 一些常用插件
(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  (evil-mode))

(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer)))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

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
