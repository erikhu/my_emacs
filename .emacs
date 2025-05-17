(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
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
(straight-use-package 'flycheck-credo)
(straight-use-package 'flycheck-elixir)
(straight-use-package 'flycheck-dialyxir)
(straight-use-package 'company)
(straight-use-package 'projectile)
;; elixir should come out of the box at emacs 30
(straight-use-package 'multiple-cursors)
(straight-use-package 'magit)
(straight-use-package 'code-review)
(straight-use-package 'forge)
(straight-use-package 'exunit)
(straight-use-package
 '(yml-mode :type git :host github :repo "yoshiki/yaml-mode"))
(straight-use-package 'mermaid-mode)
(straight-use-package 'ellama)
(straight-use-package 'use-package)
;;crystal
(straight-use-package 'crystal-mode)
;; lsp
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'helm-lsp)
(straight-use-package 'dap-mode)
;;minibuffer
(straight-use-package 'vertico)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'wgrep)
(straight-use-package 'transient)
(straight-use-package 'savehist)

(require 'theme-changer)
(require 'eglot)
(require 'multiple-cursors)

(setq calendar-location-name "Tallinn, Estonia")
(setq calendar-latitude 58.5953)
(setq calendar-longitude 25.0136)

(setq code-review-fill-column 80)
(setq code-review-new-buffer-window-strategy #'switch-to-buffer)
(setq code-review-download-dir "/tmp/code-review/")
(setq code-review-auth-login-marker 'forge)

(setq treesit-language-source-alist
      '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	(heex "https://github.com/phoenixframework/tree-sitter-heex")))

(setq column-number-mode t)

(add-to-list 'default-frame-alist
             '(font . "Monospace 17"))

;; Add elixir-ls to Eglot's server programs list
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;; ;; 	     '(elixir-ts-mode . (eglot-alternatives
;; ;; 				  '(("~/git/elixir-ls/language_server.sh")
;; ;; ;;				    ("npx" "tabby-agent" "--lsp" "--stdio")
;; ;; 				    )))
;;  	       '(elixir-ts-mode . (eglot-alternatives . '(("npx" "tabby-agent" "--lsp" "--stdio"))))
;; 	       '(heex-ts-mode "~/git/elixir-ls/language_server.sh")
;; 	       ))


;;(add-to-list 'eglot-server-programs
;;	     '(java-ts-mode "~/.emacs.d/jdtls/bin/jdtls" "-configuration" "~/.cache/jdtls" "-data" ;;"~/.emacs.d/jdtls/data"))

(change-theme 'solarized-light 'solarized-dark)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)

(add-hook 'elixir-ts-mode-hook #'exunit-mode)
;;(add-hook 'elixir-ts-mode-hook #'tree-sitter-hl-mode)
(add-hook 'elixir-ts-mode-hook #'flycheck-mode)
;;(add-hook 'elixir-ts-mode-hook #'eglot-ensure)
(add-hook 'elixir-ts-mode-hook #'dap-mode)
;;(add-hook 'elixir-ts-mode-hook #'tabby-mode)

(add-hook 'code-review-mode-hook #'emojify-mode)
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (elixir-ts-mode . lsp))
         ;; if you want which-key integration
         ;;(lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil))

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; minibuffer
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; to switch buffers
(use-package ace-window
  :bind ("M-o" . ace-window))

;; optionally if you want to use debugger
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; (use-package dap-mode)
;; (dap-register-debug-provider
;;  "elixir"
;;  (lambda (conf)
;;    (plist-put conf :dap-server-path '("~/.emacs.d/.cache/lsp/elixir-ls/debug_adapter.sh"))
;;    conf))

;; (dap-register-debug-template
;;   "Elixir::Phoenix"
;;   (list :type "elixir"
;;         :request "launch"
;; 	:task "phx.server"
;;         :name "phx.server"
;; 	))

;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))

;; TABBY ML
;; (with-eval-after-load 'lsp-mode
;;   (lsp-register-client
;;     (make-lsp-client  :new-connection (lsp-stdio-connection '("npx" "tabby-agent" "--stdio"))
;;                       ;; you can select languages to enable Tabby language server
;;                       :activation-fn (lsp-activate-on "elixir" "typescript" "javascript" "toml")
;;                       :priority 1
;;                       :add-on? t
;;                       :server-id 'tabby-agent)))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-.") 'mc/mark-all-like-this)
;; To get out of multiple-cursors-mode, press <return> or C-g.

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
