;; orgmode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq calendar-week-start-day 1)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-agenda-exporter-settings
      '((ps-number-of-columns 2)
	(ps-landscape-mode t)
	(org-agenda-add-entry-text-maxlines 5)
	(htmlize-output-type 'css)))

(setq org-startup-with-inline-images t)
(setq org-pretty-entities t)
(setq org-agenda-window-setup 'current-window)
(org-toggle-sticky-agenda)
;; (require 'org-super-agenda)
(setq org-startup-indented t)

;; (require 'org-alert)
;; (org-alert-enable)
;; (setq alert-default-style 'libnotify)
;; (setq org-alert-interval 600)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; priorities
(setq org-highest-priority 65)
(setq org-lowest-priority 66)
(setq org-default-priority 66)

(setq org-ref-completion-library 'org-ref-ivy-cite)
(require 'org-ref)
(setq org-ref-bibliography-notes "~/repos/me/research/reading.org"
      org-ref-default-bibliography '("~/repos/me/research/references.bib"))
(setq org-ref-bibtex-hydra-key-binding "\C-cj")


(setq org-todo-keywords
      '((sequence "TODO" "SOMEDAY" "|" "DONE" "DELEGATED" "POSTPONED")))

(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil)

(require 'org-habit)
