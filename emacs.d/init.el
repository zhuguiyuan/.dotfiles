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

(use-package elisp-mode
  :config
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table))

(use-package cc-mode
  :config
  (modify-syntax-entry ?_ "w" c-mode-syntax-table))

(use-package python
  :config
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package verilog-ts-mode
  :ensure t
  :mode ("\\.[ds]?va?h?\\'" . verilog-ts-mode)
  :bind (:map verilog-mode-map ("\r" . nil))
  :bind (:map verilog-mode-map (":" . nil))
  :bind (:map verilog-mode-map ("`" . nil))
  :bind (:map verilog-mode-map (";" . nil))
  :bind (:map verilog-mode-map ("C-;" . nil))
  :hook (verilog-ts-mode . (lambda () (setq indent-tabs-mode nil)))
  :init
  (unless (treesit-language-available-p 'verilog)
    (let ((url "https://github.com/zhuguiyuan/tree-sitter-systemverilog")
	  (revision "debian12"))
      (add-to-list 'treesit-language-source-alist `(verilog ,url ,revision))
      (treesit-install-language-grammar 'verilog))))

;; 一些常用插件
(use-package evil
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
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
