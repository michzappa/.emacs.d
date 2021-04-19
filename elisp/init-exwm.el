;;; init-exwm

;; should exwm be enabled?
(setq exwm-enabled (and (eq window-system 'x)
                        (seq-contains command-line-args "--use-exwm")))

;; package which allows emacs to be a full X11 window manager
(use-package exwm
  :if exwm-enabled
  :init
  ;; package to manage bluetooth from emacs
  (use-package bluetooth)
  ;; enhanced firefox support in exwm
  (use-package exwm-firefox-core
    :if exwm-enabled
    :config
    (require 'exwm-firefox))
  ;; mode to bind media keys
  (use-package desktop-environment
    :custom
    ;; for some reason the default volume commands do not work
    (desktop-environment-volume-toggle-command       "amixer -D pulse set Master toggle")
    (desktop-environment-volume-set-command          "amixer -D pulse set Master %s")
    (desktop-environment-volume-get-command          "amixer -D pulse get Master")
    ;; brightness change amount
    (desktop-environment-brightness-normal-increment "5%+")
    (desktop-environment-brightness-normal-decrement "5%-")
    (desktop-environment-brightness-small-increment  "2%+")
    (desktop-environment-brightness-small-decrement  "2%-"))
  :custom
  (exwm-workspace-number 2)
  (exwm-randr-workspace-monitor-plist
   '(0 "eDP-1" ;; laptop
       1 "DP-3")) ;; external monitor via HDMI which is for some reason named DP-3
  ;; these keys should always pass through to emacs
  (exwm-input-prefix-keys
   '(?\C-x
     ?\C-u
     ?\C-h
     ?\C-g
     ?\M-x
     ?\M-!))
  ;; set up global key bindings.  these always work, no matter the input state!
  ;; keep in mind that changing this list after EXWM initializes has no effect.
  (exwm-input-global-keys
   `(
     ;; reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
     ([?\s-r] . exwm-reset)

     ;; general app launcher
     ;; got rid of counsel
     ;; ([?\s-/] . (lambda ()
     ;; (interactive)
     ;; (counsel-linux-app)))

     ;; shortcut for firefox
     ([?\s-x] . (lambda ()
                  (interactive)
                  (shell-command "firefox")))

     ;; shortcut for terminal emulator
     ([s-return] . (lambda ()
                     (interactive)
                     (vterm-other-window)))))
  :config
  (desktop-environment-mode)
  ;; when window "class" updates, use it to set the buffer name
  (defun mz/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))
  (add-hook 'exwm-update-class-hook #'mz/exwm-update-class)

  ;; enable the next key to be sent directly, for things like copy and paste from x windows
  (define-key exwm-mode-map [?\C-m] 'exwm-input-send-next-key))

;; function to turn on all the exwm stuff
(defun mz/enable-exwm ()
  "Enables the features of EXWM."

  ;; ensure screen updates with xrandr will refresh EXWM frames
  (require 'exwm-randr)
  (exwm-randr-enable)

  ;; use default super+shift keybindings
  (windmove-swap-states-default-keybindings)

  ;; remap capsLock to ctrl
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/xmodmap")

  ;; display time
  (setq display-time-default-load-average nil)
  (display-time-mode t)

  ;; Show battery status in the mode line
  (display-battery-mode 1)

  ;; systray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; enhanced support for firefox
  (exwm-firefox-mode)

  (exwm-enable)
  (exwm-init))

(if exwm-enabled (mz/enable-exwm) ())

(provide 'init-exwm)
;;; init-exwm.el ends here
