;;; copy from http://milkbox.net/note/single-file-master-emacs-configuration/
;;; init.el --- Milkmacs configuration file

;;source directory
(setq source-directory "~/project/emacs")

;;format time display
(setq display-time-24hr-format t)
(display-time-mode t)

;;show paren
(show-paren-mode t)

;;backup file
(setq make-backup-files nil)

;;ido
(require 'ido)
(ido-mode t)

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

;(ignore-errors (server-start))

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
;; (add-to-list 'package-archives
;;              '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives '("melpa-local" . "/Users/dcurtis/src/melpa/packages/") t)
(package-initialize)

(defun mp-install-rad-packages ()
  "Install only the sweetest of packages."
  (interactive)
  (package-refresh-contents)
  (mapc '(lambda (package)
	   (unless (package-installed-p package)
	     (package-install package)))
	'(browse-kill-ring
	  ;ido-ubiquitous
	  ;magit
	  paredit
	  smex
	  undo-tree
	  google-c-style
	  python-mode
	  go-mode
	  go-complete
	  go-autocomplete
	  auto-complete
	  yasnippet
	  ggtags)))

;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;;; global key bindings

(after "paredit-autoloads"
       (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
       (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
       (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
       (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
       (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
       (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
       (add-hook 'scheme-mode-hook           #'enable-paredit-mode))


(after "smex-autoloads"
  (autoload 'smex "smex" "Smex is a M-x enhancement for Emacs, it provides a convenient interface to your recently and most frequently used commands.")
  (global-set-key (kbd "M-x") 'smex)
  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
	   `(lambda ()
	      (interactive)
	      (if (string= " " (this-command-keys))
		  (insert ?-)
		(funcall ,ido-cannot-complete-command)))))
      ad-do-it)))

(after "google-c-style-autoloads"
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(after 'yasnippet
  (yas-reload-all)
  (add-hook 'c-mode-common-hook 'yas-minor-mode)
  (add-hook 'go-mode-hook 'yas-minor-mode))

(after "python-mode-autoloads"
  (add-to-list 'auto-mode-alist '("\.py\'" . python-mode))
  (setq py-ipython-command-args '("--pprint" "--automagic" )))

(after "go-mode-autoloads"
  (add-hook 'go-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'gofmt-before-save)
	      (setq tab-width 4)
	      (setq indent-tabs-mode 1))))

(after "go-complete-autoloads"
  (add-hook 'completion-at-point-functions 'go-complete-at-point))

(after "go-autocomplete-autoloads"
  (require 'go-autocomplete))


;;after-save-hook
(defun gtags-root-dir ()
  "Return GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
	(buffer-substring (point-min) (1- (point-max)))
      nil)))
(defun gtags-update ()
  "Make GTAGS incremental update"
  (call-process "global" nil nil nil "-u"))
(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))

(after "ggtags-autoloads"
  (add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode)
	      (ggtags-mode 1))))
  (add-hook 'after-save-hook #'gtags-update-hook))

(after 'auto-complete
  (ac-config-default))



;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)






;;; init.el ends here

