(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'theme-changer)
(straight-use-package 'solarized-theme)
(straight-use-package 'helm)
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-all-the-icons)
(straight-use-package 'treemacs-perspective)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'treemacs-tab-bar)
(straight-use-package 'treemacs-magit)
(straight-use-package 'treemacs-icons-dired)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-credo)
(straight-use-package 'flycheck-elixir)
(straight-use-package 'flycheck-dialyxir)

(straight-use-package 'company)
(straight-use-package 'projectile)
(straight-use-package 'elixir-ts-mode)
(straight-use-package 'multiple-cursors)
(straight-use-package 'magit)

(require 'theme-changer)
(require 'eglot)
(require 'multiple-cursors)

(setq calendar-location-name "Tallinn, Estonia")
(setq calendar-latitude 58.5953)
(setq calendar-longitude 25.0136)

(add-to-list 'default-frame-alist
             '(font . "Monospace 17"))

;; Add elixir-ls to Eglot's server programs list
(add-to-list 'eglot-server-programs
	     '(elixir-ts-mode "~/.emacs.d/elixir-ls/release/language_server.sh"))

(add-to-list 'eglot-server-programs
	     '(java-mode "~/.emacs.d/jdtls/target/repository/bin/jdtls -configuration ~/.cache/jdtls -data ~/.emacs.d/jdtls/data"))

(change-theme 'solarized-light 'solarized-dark)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; To get out of multiple-cursors-mode, press <return> or C-g.

