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
    julia-mode
    julia-shell
    magit
    markdown-mode
    org
    projectile
    racket-mode
    smartparens
    swiper
    undo-tree
    yasnippet)
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
(load-theme 'leuven t)
(set-frame-font "Ubuntu Mono-11" nil t)
(show-paren-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; general keybindings
(global-set-key (kbd "C-x c r") 'comment-or-uncomment-region)

;; ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; perspective
(persp-mode)

;; projectile
(projectile-mode)

;; smartparens
(require 'smartparens-config)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(global-set-key (kbd "C-c C-y i") 'yas-insert-snippet)
(global-set-key (kbd "C-c C-y n") 'yas-new-snippet)
(global-set-key (kbd "C-c C-y v") 'yas-visit-snippet)

;; Julia
(load "~/.emacs.d/ESS/lisp/ess-site")
(defvar inferior-julia-program-name "/usr/bin/julia")
(add-hook 'julia-mode-hook #'smartparens-mode)

;; orgmode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files
      (list "~/source/github/ilkerkesen/notes/todo/research.org"
	    "~/source/github/ilkerkesen/notes/todo/comp508.org"
	    "~/source/github/ilkerkesen/notes/todo/comp529.org"
	    "~/source/github/ilkerkesen/notes/todo/math504.org"
	    "~/source/github/ilkerkesen/notes/todo/comp200.org"
	    "~/source/github/ilkerkesen/notes/todo/other.org"))
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

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
