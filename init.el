;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

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
(transient-mark-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode)
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10" ))
(set-face-attribute 'default t :font "DejaVu Sans Mono-10" )
(show-paren-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode)
(setq pop-up-frames t)

;; general keybindings
(global-set-key (kbd "C-x c r") 'comment-or-uncomment-region)

;; ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; projectile
(projectile-mode)

;; smartparens
(require 'smartparens-config)
(add-hook 'programming-mode-hook #'smartparens-mode)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; magit-tramp setup
(require 'tramp)
(add-to-list 'tramp-remote-path "/KUFS/apps/git/2.15.1/bin")

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

;; orgmode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

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

;; flycheck
(global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

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

;; tramp additions
(setq explicit-shell-file-name "/bin/bash")

;; rcirc settings
(defvar rcirc-settings-file "~/.emacs.d/rcirc-settings.el")
(load-file rcirc-settings-file)

(setq explicit-shell-file-name "/bin/bash")
(put 'downcase-region 'disabled nil)
