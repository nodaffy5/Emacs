	   (defun jacob/run-in-background (command)
	     (let ((command-parts (split-string command "[ ]+")))
		 (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

	   (defun jacob/exwm-init-hook ()
	   (display-time-mode 1)
	     ;; Make workspace 1 be the one where we land at startup
	   (exwm-workspace-switch-create 1))
	     ;; Open eshell by default
	     (eshell)


	       (defun jacob/exwm-update-class ()
	 (exwm-workspace-rename-buffer exwm-class-name))

     (defun jacob/window-classes ()
     (interactive)
    (pcase exwm-class-name
	("mpv" (exwm-floating-toggle-floating)
	 (exwm-layout-toggle-mode-line)
	("Tk" (exwm-floating-toggle-floating)
	 (exwm-layout-toggle-mode-line)))))

	       (use-package exwm
		 :config
	       (setq exwm-workspace-number 5)
	       (add-hook 'exwm-update-class-hook #'jacob/exwm-update-class)

	       (add-hook 'exwm-init-hook #'jacob/exwm-init-hook)

	       (setq exwm-input-prefix-keys
	     '(?\C-x
	       ?\C-u
	       ?\C-h
	       ?\M-x
	       ?\M-`
	       ?\M-&
	       ?\M-:
	       ?\C-\M-j  ;; Buffer list
	       ?\C-\ ))  ;; Ctrl+Space
  (add-hook 'exwm-manage-finish-hook #'jacob/window-classes)
		 ;; Ctrl+Q will enable the next key to be sent directly
		 (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

		 ;; Set up global key bindings.  These always work, no matter the input state!
		 ;; Keep in mind that changing this list after EXWM initializes has no effect.
		 (setq exwm-input-global-keys
		 `(
		   ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
		   ([?\s-r] . exwm-reset)

		   ;; Move between windows
		   ([s-left] . windmove-left)
		   ([s-right] . windmove-right)
		   ([s-up] . windmove-up)
		   ([s-down] . windmove-down)

		   ;; Launch applications via shell command
		   ([?\s-&] . (lambda (command)
				(interactive (list (read-shell-command "$ ")))
				(start-process-shell-command command nil command)))

		   ;; Switch workspace
		   ([?\s-w] . exwm-workspace-switch)

		   ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
		   ,@(mapcar (lambda (i)
			       `(,(kbd (format "s-%d" i)) .
				 (lambda ()
				   (interactive)
				   (exwm-workspace-switch-create ,i))))
			     (number-sequence 0 9))))
       (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
       (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)

		 (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(use-package edwina
  :ensure t
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  (edwina-mode 1))


