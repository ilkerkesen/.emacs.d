;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  enb  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))
(defvar root-dir (file-name-directory load-file-name))
(defun load-settings (name)
  (load-file (concat root-dir name ".el")))

(defvar my-packages
  '(expand-region
    flycheck
    isend-mode
    json-mode
    julia-mode
    julia-shell
    lua-mode
    magit
    markdown-mode
    org
    projectile
    racket-mode
    smartparens
    undo-tree
    yaml-mode
    yasnippet
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p)) (package-install p)))

;; prevent custom.el modifications to init.el
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; customize interface
(setq inhibit-splash-screen t)
;; (setq initial-buffer-choice "~/repos/ilkerkesen/mudur/index.org")
(transient-mark-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode)
(add-to-list 'default-frame-alist '(font . "Dejavu Sans Mono-10" ))
(set-face-attribute 'default t :font "Dejavu Sans Mono-10" )
(show-paren-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)
(add-hook 'org-agenda-mode-hook (lambda () (setq global-hl-line-mode nil)))
(setq pop-up-frames t)

;; (load-theme 'zenburn t)
;; (load-theme 'solarized t)
;; (load-theme 'doneburn t)
;; (load-theme 'anti-zenburn t)
;; (load-theme 'leuven t)
;; (load-theme 'gotham t)
;; (load-theme 'ample-light t)
;; (load-theme 'hemisu-dark t)
(load-theme 'spacemacs-light t)
;; (load-theme 'ubuntu)
;; (load-theme 'solarized-light t)
;; (load-theme 'gruvbox)
;; (load-theme 'cyberpunk)
;; (load-theme 'dracula)

;; general keybindings
(global-set-key (kbd "C-x c r") 'comment-or-uncomment-region)

;; ido-mode
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; projectile
(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(counsel-projectile-mode)

;; smartparens
(require 'smartparens-config)
(add-hook 'programming-mode-hook #'smartparens-mode)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; magit-tramp setup
(require 'tramp)
(setq explicit-shell-file-name "/bin/bash")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; handle trailing whitespaces
(add-hook'prog-mode-hook
 (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(global-set-key (kbd "C-c C-y i") 'yas-insert-snippet)
(global-set-key (kbd "C-c C-y n") 'yas-new-snippet)
(global-set-key (kbd "C-c C-y v") 'yas-visit-snippet)

;; Octave
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; CUDA mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; Racket
(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

;; undo-tree
(global-undo-tree-mode)

;; rcirc settings
(defvar rcirc-settings-file "~/.emacs.d/rcirc-settings.el")
(load-file rcirc-settings-file)

(put 'downcase-region 'disabled nil)

;; javascript
(setq js-indent-level 2)
(put 'narrow-to-region 'disabled nil)

;; org-mode config
(load-settings "org-mode")

;; webjump
(setq webjump-sites
      '(("julia" . [simple-query
			 "docs.julialang.org"
			 "docs.julialang.org/en/v1/search/?q="
			 ""])
      ("pytorch" . [simple-query
			 "pytorch.org/docs/stable/"
			 "pytorch.org/docs/stable/search.html?check_keywords=yes&area=default&q="
			 ""])
      (""   . [simple-query
	       "eksisozluk.com"
	       <nowiki>"eksisozluk.com/"</nowiki>
	       ""])))


;; python-mode
(defun python-insert-self () (interactive)
       (insert "self."))
(defun python-insert-ipdb () (interactive)
       (progn (back-to-indentation)
	      (insert "import ipdb; ipdb.set_trace()")
	      (newline-and-indent)))
(add-hook 'python-mode-hook
	  (lambda () (local-set-key (kbd "M-s i") 'python-insert-ipdb)))
(add-hook 'python-mode-hook
	  (lambda () (local-set-key (kbd "M-s s") 'python-insert-self)))


;; elfeed
(require 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)
(load-settings "feeds")
