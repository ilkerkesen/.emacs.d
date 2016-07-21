;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

(defvar my-packages
  '(auto-complete
    expand-region
    flycheck
    google-translate
    helm
    helm-projectile
    julia-mode
    julia-shell
    magit
    markdown-mode
    multiple-cursors
    org
    org-page
    org-pdfview
    org-pomodoro
    projectile
    smartparens
    solarized-theme
    undo-tree
    yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p)) (package-install p)))

;; prevent custom.el modifications to init.el
(setq custom-file (expand-file-name "custom.el" "."))
(load-file custom-file)

;; customize interface
(setq inhibit-splash-screen t)
(transient-mark-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode)
(load-theme 'solarized-dark t)
(set-frame-font "DejaVu Sans Mono-9" nil t)

;; helm configuration
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (defvar helm-google-suggest-use-curl-p t))

(defvar helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(projectile-global-mode)
(defvar helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(defvar helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

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
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Octave
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; flycheck
(global-flycheck-mode)
