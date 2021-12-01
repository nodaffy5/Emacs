(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
(url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
(eval-buffer)
(quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package general
  :config
  (general-create-definer jacob/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (jacob/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package undo-tree
  :config
  (turn-on-undo-tree-mode))
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(setq inhibit-startup-message t)

(global-undo-tree-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(load-theme 'doom-vibrant t)

(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
	  term-mode-hook
	  shell-mode-hook
	  treemacs-mode-hook
	  eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))


(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-frame-font "Roboto Mono":style=Light:size=12)

(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular)

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
 ("f" nil "finished" :exit t))

 (jacob/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(setq ispell-extra-args '("--sug-mode=fast"))

(add-to-list 'load-path "~/src/org-mode/lisp" )
(add-to-list 'load-path "~/src/org-mode/contrib/lisp" t)
(defun jacob/org-mode-setup ()
(variable-pitch-mode 1)
(visual-line-mode 1))

(setq org-src-preserve-indentation t)

(use-package org
 :hook (org-mode . jacob/org-mode-setup)
 :config
 (setq org-ellipsis " ▾")
 (setq org-src-block-indentation nil)
 (setq org-agenda-start-with-log-mode t)
 (setq org-log-done 'time)
 (setq org-log-into-drawer t)
 (setq org-agenda-files
       '("~/org/Tasks.org"
	 "~/org/Habits.org"
	 "~/org/Birthdays.org"))
 (setq org-file-apps
  '((auto-mode . emacs)
 ("\\.pdf::\\([0-9]+\\)?\\'" . "zathura %s -P %1")
 ("\\.pdf\\'" . "zathura %s")
 (directory . emacs)))
 (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
 (require 'ox-latex)
  ; (require 'ox-groff)
 (require 'org-habit)
 (add-to-list 'org-modules 'org-habit)
 (jacob/org-font-setup))


(setq org-todo-keywords
     '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))

(setq org-refile-targets
   '(("archive.org" :maxlevel . 1)
     ("Tasks.org" :maxlevel . 1)))
(setq org-adapt-indentation t)
	 ;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-capture-templates
   `(("t" "Task" entry (file+olp "~/org/Tasks.org" "Inbox")
	  "* TODO %?\n" :empty-lines 1)
     ("j" "Journal" entry (file+datetree "~/org/Journal.org")
	"* %?\n")
     ("m" "Metrics Capture")
     ("mb" "Bands" table-line (file+headline "~/org/Metrics.org" "Weight")
      "| %U | %^{Number} |" :kill-buffer t)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)

  :custom
  (org-bullets-bullet-list '("○" "●" "○" "●" "○" "●")))

(defun jacob/org-font-setup ()
	 ;; Replace list hyphen with dot
 (font-lock-add-keywords 'org-mode
			 '(("^ *\\([-]\\) "
			    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

	 ;; Set faces for heading levels
 (dolist (face '((org-level-1 . 1.2)
		 (org-level-2 . 1.1)
		 (org-level-3 . 1.05)
		 (org-level-4 . 1.0)
		 (org-level-5 . 1.1)
		 (org-level-6 . 1.1)
		 (org-level-7 . 1.1)
		 (org-level-8 . 1.1)))
 (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
	 ;; Ensure that anything that should be fixed-pitch in Org files appears that way
 (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
 (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
 (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
 (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
 (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
 (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun jacob/org-mode-visual-fill ()
 (setq visual-fill-column-width 150
       visual-fill-column-center-text t)
 (visual-fill-column-mode 1))

(use-package visual-fill-column
 :hook (org-mode . jacob/org-mode-visual-fill))

(with-eval-after-load 'org
 (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
     (python . t))))

(with-eval-after-load 'org
 ;; This is needed as of Org 9.2
 (require 'org-tempo)

 (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
 (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
 (add-to-list 'org-structure-template-alist '("py" . "src python")))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(defun jacob/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.emacs.d/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jacob/org-babel-tangle-config)))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
      ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

      ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
 :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(defun jacob/configure-eshell ()
    ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

    ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

    ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . jacob/configure-eshell))
  (with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim")))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
"h" 'dired-up-directory
"l" 'dired-find-file))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-single)

    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer)

(use-package no-littering)
(setq auto-save-file-name-transforms
  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package openwith
  :config
  (setq openwith-associations
  (list
   (list (openwith-make-extension-regexp
	  '("mpg" "mpeg" "mp3" "mp4"
	    "avi" "wmv" "wav" "mov" "flv"
	    "ogm" "ogg" "mkv"))
	  "mpv"
	  '(file))
    (list (openwith-make-extension-regexp
	  '("xbm" "pbm" "pgm" "ppm" "pnm"
	    "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
	    ;; causing feh to be opened...
	    "feh"
	    '(file))
    (list (openwith-make-extension-regexp
	  '("pdf"))
	  "zathura"
	  '(file)))))

(global-set-key (kbd "C-x w") 'elfeed)
(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
  '(
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCOMku8U1zFKsBudd3BcgSAw"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCcAy1o8VUCkdowxRYbc0XRw"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCsnGwSIHyoYN0kiINAGUKxg"
     "https://lukesmith.xyz/peertube"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCJetJ7nDNLlEzDLXv7KIo0w"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ"
     "http://thephilosophicalsalon.com/author/sz/feed/"
     "https://blog.emoryadmission.com/feed/"
     "https://feeds.soundcloud.com/users/soundcloud:users:339644074/sounds.rss")))

(use-package emms
  :commands emms
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (emms-mode-line-disable)
  (require 'emms-player-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(setq emms-source-file-default-directory "~/Music/Music")
(jacob/leader-keys
"m"  '(:ignore t :which-key "media")
"mp" '(emms-pause :which-key "play / pause")
"mf" '(emms-play-file :which-key "play file")))

(setq snake-tick-period '0.15)

(use-package mu4e
  :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/mail"))



(use-package mentor)

(use-package transmission)

(use-package matrix-client
  :quelpa (matrix-client :fetcher github :repo "alphapapa/matrix-client.el"
                         :files (:defaults "logo.png" "matrix-client-standalone.el.sh")))
