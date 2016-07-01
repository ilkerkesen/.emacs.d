;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

(defvar my-packages
  '(auto-complete
    flycheck
    google-translate
    helm
    helm-projectile
    julia-mode
    julia-shell
    magit
    org
    org-page
    org-pdfview
    org-pomodoro
    projectile
    solarized-theme
    undo-tree
    yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p)) (package-install p)))

;; customize interface
(setq inhibit-splash-screen t)
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
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(projectile-global-mode)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; yasnippet
(yas-global-mode 1)
