;;; -*- lexical-binding: t -*-

;;; Package management and init file benchmark

(require 'package)
(require 'use-package)

(setopt package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setopt package-selected-packages
        '(bash-completion
          benchmark-init
          bluetooth
          company
          consult
          consult-company
          consult-eglot
          coterm
          crux
          debbugs
          devicetree-ts-mode
          doom-themes
          dts-mode
          easy-kill
          easy-kill-extras
          eat
          echo-bar
          ednc
          elfeed
          engine-mode
          enwc
          evil
          expand-region
          exwm
          gcmh
          ggtags
          google-translate
          hackernews
          helm
          helm-company
          helm-gtags
          helm-themes
          howdoyou
          ibuffer-vc
          imenu-list
          insecure-lock
          keycast
          kkp
          llm
          lsp-mode
          magit
          marginalia
          md4rd
          move-dup
          no-littering
          notmuch
          orderless
          org-gcal
          osm
          pdf-tools
          pdfgrep
          project-shells
          projectile
          pulseaudio-control
          repo
          rotate
          shell-command-x
          shx
          sly
          spacemacs-theme
          speed-type
          sx
          telega
          tldr
          transmission
          urgrep
          vertico
          vterm
          wgrep
          winum
          zenburn-theme))

(dolist (p package-selected-packages)
  (unless (package-installed-p p)
   (package-install p)))

(setopt use-package-always-defer t
        use-package-compute-statistics t
        use-package-expand-minimally t)

;; (use-package benchmark-init
;;   :demand t
;;   :hook (after-init . benchmark-init/deactivate))

(use-package no-littering :demand t)

;;; C source or configs that don't have feature to require

(use-package emacs
  :custom
  ;; C source
  (bidi-display-reordering nil)
  (bidi-inhibit-bpa t)
  (completion-ignore-case t)
  (delete-by-moving-to-trash t)
  (enable-recursive-minibuffers t)
  (fast-but-imprecise-scrolling t)
  (history-delete-duplicates t)
  (history-length t)
  (large-hscroll-threshold 1000)
  (long-line-threshold 1000)
  (mode-line-format '((vc-mode vc-mode) "  "
                      mode-line-buffer-identification "  "
                      mode-line-position))
  (read-buffer-completion-ignore-case t)
  (ring-bell-function #'ignore)
  (scroll-preserve-screen-position t)
  (use-short-answers t)
  (user-full-name "BooAA")
  (visible-cursor nil)

  ;; startup.el
  (inhibit-startup-screen t)
  (user-mail-address "s930054123yaoyao@gmail.com")
  :bind
  ("M-R" . raise-sexp))

;;; Configuration for each package

(use-package apropos
  :bind
  ("C-h /" . apropos)
  ("C-h u" . apropos-user-option))

(use-package autorevert
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :hook
  (after-init . global-auto-revert-mode))

(use-package bash-completion
  :custom (bash-completion-use-separate-processes t)
  :hook (after-init . bash-completion-setup))

(use-package battery
  :custom (battery-load-low 20))

(use-package bluetooth
  :custom (bluetooth-bluez-bus :system))

(use-package browse-url
  :custom
  (browse-url-browser-function #'browse-url-chrome)
  (browse-url-chrome-arguments '("--new-window"))
  :bind
  ("C-c z ." . browse-url-at-point)
  ("C-c z b" . browse-url-of-buffer)
  ("C-c z r" . browse-url-of-region)
  ("C-c z u" . browse-url)
  ("C-c z v" . browse-url-of-file))

(use-package c-ts-mode
  :custom
  (c-ts-mode-indent-offset 8)
  (c-ts-mode-indent-style 'linux))

(use-package cc-mode
  :custom
  (c-default-style
   '((c-mode . "linux")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))

(use-package comint
  :init
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output))

(use-package company
  :custom
  (company-backends '(company-capf
                      company-files
                      (company-dabbrev-code company-keywords)
                      company-dabbrev))
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-files-chop-trailing-slash nil)
  (company-idle-delay nil)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotation t)
  :bind
  ("C-M-/" . company-manual-begin)
  :hook
  (after-init . global-company-mode)
  (company-mode . company-tng-mode))

(use-package consult
  :init
  (define-prefix-command 'consult-command-map)
  :bind
  (:map consult-command-map
        ("l" . consult-locate)
        ("m" . consult-man)
        ("/" . consult-find)
        ("i" . consult-imenu)
        ("I" . consult-imenu-multi)
        ("C-x r b" . consult-bookmark)
        ("M-y" . consult-yank-from-kill-ring)
        ("M-x" . consult-mode-command)
        ("M-s o" . consult-line)
        ("M-g a" . consult-ripgrep)
        ("C-c f" . consult-recentf)
        ("C-x C-b" . consult-buffer)
        ("C-x r i" . consult-register)
        ("C-M-i" . consult-company)))

(use-package consult-company
  :bind (:map consult-command-map ("C-M-i" . consult-company)))

(use-package consult-eglot)

(use-package coterm
  :hook (after-init . coterm-mode))

(use-package crux
  :bind
  ("C-S-<return>" . crux-smart-open-line-above)
  ("S-<return>" . crux-smart-open-line))

(use-package custom
  :init (setq custom--inhibit-theme-enable nil))

(use-package debbugs)

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package devicetree-ts-mode)

(use-package dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

(use-package doom-themes)

(use-package dts-mode)

(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

(use-package easy-kill-extra)

(use-package eat)

(use-package echo-bar
  :custom
  (echo-bar-format '((:eval (ednc-top-notification)) " "
                     "[ "
                     ;; (:eval (battery-format "%b%p%%%" (battery-upower)))
                     ;; system-name
                     ;; " | "
                     (:eval (format-time-string "%a %b %d | %H:%M"))
                     " ]"))
  (echo-bar-minibuffer nil)
  (echo-bar-right-padding 1)
  (echo-bar-update-interval 2)
  :hook (after-init . echo-bar-mode))

(use-package ediff
  :custom
  (ediff-window-setup-function #'ediff-setup-window-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package ednc
  :preface
  (defun ednc-top-notification ()
    (if-let ((notification (car (ednc-notifications))))
        (ednc-format-notification notification)
      ""))
  :hook
  (after-init . ednc-mode))

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-connect-timeout 10)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilites
   '(:inlayHintProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider))
  (eglot-sync-connect nil))

(use-package eglot-hierarchy
  :vc (:url "https://github.com/dolmens/eglot-hierarchy"))

(use-package eldoc
  :custom
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p nil))

(use-package elec-pair
  :custom (electric-pair-open-newline-between-pairs t)
  :hook (after-init . electric-pair-mode))

(use-package elfeed
  :custom
  (elfeed-feeds '(("https://planet.lisp.org/rss20.xml" lisp)
                  ("https://sachachua.com/blog/category/emacs-news/feed/" emacs)
                  ("https://lwn.net/headlines/rss" linux)))
  (elfeed-search-filter "@3days-ago +unread"))

(use-package engine-mode
  :hook
  (after-init . engine-mode)
  :config
  (defengine google
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "/")

  (defengine youtube
    "https://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine wikipedia
  "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g"))

(use-package enwc
  :custom (enwc-default-backend 'nm))

(use-package eshell
  :custom
  (eshell-prefer-lisp-functions nil)
  (eshell-prefer-lisp-variables nil)
  (eshell-command-aliases-list '(("c" "clear 1")))
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(use-package evil
  :init (setq evil-disable-insert-state-bindings t))

(use-package eww
  :custom (eww-auto-rename-buffer 'title))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package exwm
  :preface
  (defun exwm-run-command-async (command)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (defun exwm-rename-buffer-title ()
    (exwm-workspace-rename-buffer exwm-title))
  :commands exwm-enable
  :custom
  (exwm-input-global-keys
   `(;; hyper as prefix
     ([?\H-Z] . insecure-lock-enter)
     ([?\H-r] . exwm-run-command-async)
     ,@(mapcar (lambda (i)
                 `(,(kbd (format "H-%d" i)) .
                   (lambda ()
                     (interactive)
                     (tab-bar-select-tab ,i))))
               (number-sequence 1 8))
     ([?\H-9] . tab-last)
     ([H-f1] . pulseaudio-control-toggle-current-sink-mute)
     ([H-f2] . pulseaudio-control-decrease-sink-volume)
     ([H-f3] . pulseaudio-control-increase-sink-volume)
     ([H-S-f1] . pulseaudio-control-toggle-current-source-mute)
     ([H-S-f2] . pulseaudio-control-decrease-source-volume)
     ([H-S-f3] . pulseaudio-control-increase-source-volume)
     ,@(mapcan (lambda (dir)
                 (list `(,(kbd (format "H-<%s>" dir)) .
                         ,(intern-soft (format "windmove-%s" dir)))
                       `(,(kbd (format "H-S-<%s>" dir)) .
                         ,(intern-soft (format "windmove-swap-states-%s" dir)))
                       `(,(kbd (format "H-x <%s>" dir)) .
                         ,(intern-soft (format "windmove-display-%s" dir)))
                       `(,(kbd (format "H-x S-<%s>" dir)) .
                         ,(intern-soft (format "windmove-delete-%s" dir)))))
               '("up" "down" "left" "right"))
     ,@(cl-mapcan (lambda (key dir)
                    (list `(,(kbd (concat "H-" key)) .
                            ,(intern-soft (concat "windmove-" dir)))
                          `(,(kbd (concat "H-" (capitalize key))) .
                            ,(intern-soft (concat "windmove-swap-states-" dir)))
                          `(,(kbd (concat "H-x " key)) .
                            ,(intern-soft (concat "windmove-display-" dir)))
                          `(,(kbd (concat "H-x " (capitalize key))) .
                            ,(intern-soft (concat "windmove-delete-" dir)))))
                  '("h" "j" "k" "l") '("left" "down" "up" "right"))     
     ([H-f5] . exwm-reset)
     ([H-print] . scrot)

     ;; super as prefix
     ([?\s-Z] . insecure-lock-enter)
     ([?\s-r] . exwm-run-command-async)
     ,@(mapcar (lambda (i)
                 `(,(kbd (format "s-%d" i)) .
                   (lambda ()
                     (interactive)
                     (tab-bar-select-tab ,i))))
               (number-sequence 1 8))
     ([?\s-9] . tab-last)
     ([s-f1] . pulseaudio-control-toggle-current-sink-mute)
     ([s-f2] . pulseaudio-control-decrease-sink-volume)
     ([s-f3] . pulseaudio-control-increase-sink-volume)
     ([s-S-f1] . pulseaudio-control-toggle-current-source-mute)
     ([s-S-f2] . pulseaudio-control-decrease-source-volume)
     ([s-S-f3] . pulseaudio-control-increase-source-volume)
     ,@(mapcan (lambda (dir)
                 (list `(,(kbd (format "s-<%s>" dir)) .
                         ,(intern-soft (format "windmove-%s" dir)))
                       `(,(kbd (format "s-S-<%s>" dir)) .
                         ,(intern-soft (format "windmove-swap-states-%s" dir)))
                       `(,(kbd (format "s-x <%s>" dir)) .
                         ,(intern-soft (format "windmove-display-%s" dir)))
                       `(,(kbd (format "s-x S-<%s>" dir)) .
                         ,(intern-soft (format "windmove-delete-%s" dir)))))
               '("up" "down" "left" "right"))
     ,@(cl-mapcan (lambda (key dir)
                    (list `(,(kbd (concat "s-" key)) .
                            ,(intern-soft (concat "windmove-" dir)))
                          `(,(kbd (concat "s-" (capitalize key))) .
                            ,(intern-soft (concat "windmove-swap-states-" dir)))
                          `(,(kbd (concat "s-x " key)) .
                            ,(intern-soft (concat "windmove-display-" dir)))
                          `(,(kbd (concat "s-x " (capitalize key))) .
                            ,(intern-soft (concat "windmove-delete-" dir)))))
                  '("h" "j" "k" "l") '("left" "down" "up" "right"))     
     ([s-f5] . exwm-reset)
     ([s-print] . scrot)))
  :hook
  (exwm-update-title . exwm-rename-buffer-title))

(use-package exwm-randr
  :preface
  (defun exwm-change-screen-hook ()
    (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
          default-output)
      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
        (re-search-forward xrandr-output-regexp nil 'noerror)
        (setq default-output (match-string 1))
        (forward-line)
        (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
            (call-process "xrandr" nil nil nil "--output" default-output "--auto")
          (call-process
           "xrandr" nil nil nil
           "--output" (match-string 1) "--primary" "--auto"
           "--output" default-output "--off")
          (setq exwm-randr-workspace-monitor-plist (list 0 (match-string 1)))))))
  :hook
  (after-init . exwm-randr-enable)
  (exwm-randr-screen-change . exwm-change-screen-hook))

(use-package files
  :custom
  (auto-save-default nil)
  (confirm-kill-emacs nil)
  (confirm-kill-processes nil)
  (make-backup-files nil))

(use-package filesets
  :hook (after-init . filesets-init))

(use-package find-dired
  :bind (:map search-map ("f" . find-name-dired)))

(use-package flymake
  :custom
  (flymake-wrap-around t)
  :bind
  (:map flymake-mode-map
        ("s-n" . flymake-goto-next-error)
        ("s-p" . flymake-goto-prev-error)))

(use-package frame
  :custom (blink-cursor-mode nil))

(use-package gcmh
  :hook (emacs-startup . gcmh-mode))

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-restore-window-configuration-after-quit t))

(use-package ggtags
  :custom
  (ggtags-enable-navigation-key nil)
  (ggtags-mode-prefix-key (kbd "C-c g"))
  :bind
  (:map ggtags-mode-map
        ("C-M-." . nil)
        ("M-." . nil)
        ("M-]" . nil)
        :map ggtags-mode-prefix-map
        ("." . ggtags-find-tag-dwim)
        ("/" . ggtags-view-search-history)
        ("?" . ggtags-find-reference)
        ("SPC" . ggtags-save-to-register)
        ("a" . ggtags-find-tag-regexp)
        ("b" . ggtags-browse-file-as-hypertext)
        ("d" . ggtags-show-definition)
        ("f" . ggtags-find-file)
        ("g" . ggtags-grep)
        ("h" . ggtags-view-tag-history)
        ("i" . ggtags-idutils-query)
        ("j" . ggtags-view-project-root)
        ("k" . ggtags-kill-file-buffers)
        ("n" . ggtags-next-mark)
        ("o" . ggtags-find-other-symbol)
        ("p" . ggtags-prev-mark))
  :hook
  (c-mode . ggtags-mode))

(use-package gnus
  :custom
  (gnus-select-method '(nnimap "imap.gmail.com"))
  (gnus-secondary-select-methods
   '((nntp "news.gmane.io")
     (nntp "nntp.lore.kernel.org"))))

(use-package google-translate
  :custom
  (google-translate-default-source-language "auto")
  (google-translate-default-target-language "zh-TW")
  :bind
  ("C-c t b" . google-translate-buffer)
  ("C-c t q" . google-translate-query-translate)
  ("C-c t s" . google-translate-smooth-translate)
  ("C-c t ." . google-translate-at-point))

(use-package grep
  :custom (grep-template "ugrep --color=always -0Iinr -e <R>"))

(use-package hackernews)

(use-package helm
  :custom
  (helm-grep-ag-command
   "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (helm-minibuffer-history-key nil)
  :bind
  (:map helm-map
   ("C-s" . nil)
   ("C-t" . nil)
   ("C-|" . helm-toggle-resplit-window)
   :map helm-command-map
   ("C-x b" . helm-mini))
  :defer 0.1)

(use-package helm-company
  :after helm
  :bind (:map helm-command-map ("C-M-i" . helm-company)))

(use-package helm-gtags
  :custom
  (helm-gtags-cache-select-result t)
  (helm-gtags-direct-helm-completing t)
  (helm-gtags-ignore-case t)
  :hook
  (c-mode . helm-gtags-mode))

(use-package helm-themes)

(use-package help
  :custom (help-window-keep-selected t))

(use-package hippie-exp
  :custom
  (hippie-expand-try-functions-list '(try-complete-file-name
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-expand-line
                                      try-expand-line-all-buffers
                                      try-expand-whole-kill
                                      try-complete-lisp-symbol
                                      try-expand-list
                                      try-expand-list-all-buffers))
  (hippie-expand-verbose nil)
  :bind ("M-/" . hippie-expand))

(use-package howdoyou
  :custom
  (howdoyou-max-history 30)
  (howdoyou-number-of-answers 5)
  (howdoyou-switch-to-answer-buffer t))

(use-package ibuffer
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-show-empty-filter-groups nil)
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-vc
  :preface
  (defun ibuffer-vc-setup ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :custom
  (ibuffer-vc-skip-if-remote nil)
  :hook
  (ibuffer . ibuffer-vc-setup))

(use-package imenu-list
  :bind ("M-g I" . imenu-list))

(use-package insecure-lock
  :custom (insecure-lock-require-password nil))

(use-package isearch
  :custom
  (lazy-highlight-initial-delay 0.1)
  (isearch-lazy-count t)
  (isearch-resume-in-command-history t)
  (isearch-wrap-pause 'no)
  :bind
  (:map isearch-mode-map
        ("DEL" . isearch-del-char)
        ("C-M-d" . isearch-delete-char)
        ("C-g" . isearch-cancel)))

(use-package kkp
  :hook (after-init . global-kkp-mode))

(use-package keycast)

(use-package llm)

(use-package lsp-mode)

(use-package lua-ts-mode
  :mode ("\\.lua\\'" . lua-ts-mode))

(use-package magit)

(use-package marginalia
  :bind (:map vertico-map ("M-A" . marginalia-cycle)))

(use-package mb-depth
  :hook (after-init . minibuffer-depth-indicate-mode))

(use-package md4rd
  :custom
  (md4rd-subs-active
   '(Common_Lisp
     emacs
     linux
     lisp
     unixporn)))

(use-package midnight
  :hook (after-init . midnight-mode))

(use-package minibuffer
  :custom
  (completion-category-overrides '((eglot (styles basic flex))))
  (completions-max-height (round (* (frame-height) 0.3)))
  (read-file-name-completion-ignore-case t))

(use-package misc
  :bind ("C-<return>" . duplicate-line))

(use-package move-dup
  :bind
  ("M-<down>" . move-dup-move-lines-down)
  ("M-<up>" . move-dup-move-lines-up))

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :custom
  (mu4e-completing-read-function 'completing-read)
  (mu4e-confirm-quit nil)
  (mu4e-get-mail-command "mbsync --all")
  (mu4e-headers-visible-columns (/ (frame-width) 2))
  (mu4e-hide-index-messages t)
  (mu4e-search-threads nil)
  (mu4e-split-view 'vertical)
  (mu4e-update-interval 100)
  (mu4e-use-fancy-chars t)
  :commands
  mu4e)

(use-package notmuch
  :custom
  (notmuch-column-control 0.25)
  (notmuch-hello-sections
   '(notmuch-hello-insert-header
     notmuch-hello-insert-saved-searches
     notmuch-hello-insert-search))
  (notmuch-saved-searches
   `((:name "draft" :query "tag:draft" :key ,(kbd "d")))
   `((:name "inbox" :query "tag:inbox" :key ,(kbd "i")))
   `((:name "sent" :query "tag:sent" :key ,(kbd "s")))
   `((:name "unread" :query "tag:unread" :key ,(kbd "u"))))
  (notmuch-search-oldest-first nil)
  (notmuch-show-empty-saved-searches t))

(use-package novice
  :init (setq disabled-command-function nil))

(use-package orderless)

(use-package org-gcal)

(use-package osm)

(use-package pdf-tools)

(use-package pdfgrep)

(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

(use-package project
  :preface
  (defun project-try-override (path)
    "Search for .project file as project root"
    (when-let ((root (locate-dominating-file path ".project")))
      (cons 'transient (expand-file-name root))))
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  :config
  (add-to-list 'project-find-functions #'project-try-override))

(use-package project-shells
  :custom
  (project-shells-eshell-keys nil)
  (project-shells-term-keys nil)
  (project-shells-vterm-keys '("9" "0" "-" "="))
  :hook
  (after-init . global-project-shells-mode))

(use-package projectile
  :custom (projectile-dirconfig-file ".project"))

(use-package pulseaudio-control
  :custom
  (pulseaudio-control-volume-step "2%")
  :hook
  (after-init . pulseaudio-control-default-sink-mode)
  (after-init . pulseaudio-control-default-source-mode))

(use-package recentf
  :custom (recentf-max-saved-items nil)
  :bind ("C-c f" . recentf-open-files)
  :hook (after-init . recentf-mode))

(use-package repeat
  :hook (after-init . repeat-mode))

(use-package replace
  :bind ("C-%" . replace-regexp))

(use-package repo)

(use-package rotate
  :bind ("C-|" . rotate-layout))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package scrot
  :vc (:url "https://github.com/dakra/scrot.el.git")
  :custom
  (scrot-local-path "~/Pictures/Screenshots/")
  (scrot-upload-func nil))

(use-package sendmail
  :custom (send-mail-function 'smtpmail-send-it))

(use-package shell
  :custom
  (explicit-shell-file-name "/bin/bash")
  (shell-has-auto-cd nil))

(use-package shell-command-x)

(use-package shr
  :custom
  (shr-discard-aria-hidden t)
  (shr-use-fonts nil)
  (shr-use-xwidgets-for-media t))

(use-package shx)

(use-package simple
  :custom
  (indent-tabs-mode nil)
  (backward-delete-char-untabify-method nil)
  (completion-show-help nil)
  (kill-region-dwim 'emacs-word)
  :hook
  (after-init . line-number-mode)
  (after-init . column-number-mode))

(use-package sly
  :custom (inferior-lisp-program "sbcl"))

(use-package smtpmail
  :custom
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package spacemacs-theme)

(use-package speed-type)

(use-package sx)

(use-package syntax
  :custom (syntax-wholeline-max 1000))

(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-show nil)
  :hook
  (after-init . tab-bar-mode)
  (after-init . tab-bar-history-mode))

(use-package telega)

(use-package tldr)

;; (use-package tramp
;;   :defer nil
;;   :config
;;   (setq tramp-default-remote-shell "/bin/bash"
;;         tramp-ssh-controlmaster-options
;;         "-o ControlPath=%%C -o ControlMaster=auto -o ControlPersist=yes"))

(use-package transmission)

(use-package uniquify
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/"))

(use-package urgrep
  :custom
  (urgrep-preferred-tools '(ripgrep ugrep git-grep grep))
  (urgrep-search-regexp t)
  :bind
  (:map search-map ("g" . urgrep)))

(use-package vertico
  :preface
  (defun vertico-plugin-setup ()
    (if vertico-mode
        (progn
          (marginalia-mode)
          (add-to-list 'completion-styles 'orderless t)
          (global-set-key (kbd "C-x c") 'consult-command-map))
      (marginalia-mode -1)
      (delq 'orderless completion-styles)
      (global-set-key (kbd "C-x c") 'helm-command-prefix)))
  :custom
  (vertico-cycle t)
  :hook
  (vertico-mode . vertico-plugin-setup))

(use-package vertico-multiform
  :hook
  (vertico-mode . vertico-multiform-mode)
  :config
  (add-to-list 'vertico-multiform-commands '(t unobtrusive))

  (dolist (cmd '(consult-bookmark consult-buffer consult-company
                 consult-complex-command consult-eglot-symbols
                 consult-find consult-git-grep consult-grep
                 consult-history consult-imenu consult-imenu-multi
                 consult-info consult-line consult-locate consult-man
                 consult-mode-command consult-project-buffer
                 consult-recentf consult-register consult-ripgrep
                 consult-theme consult-yank-from-kill-ring))
    (add-to-list 'vertico-multiform-commands `(,cmd buffer))))

(use-package vterm
  :custom
  (vterm-clear-scrollback-when-clearing t)
  (vterm-enable-manipulate-selection-data-by-osc52 t)
  (vterm-max-scrollback 100000)
  (vterm-timer-delay 0.01)
  (vterm-tramp-shells '(("ssh" "/bin/bash")))
  :bind
  (:map vterm-mode-map ("C-S-SPC" . vterm-copy-mode)))

(use-package warning
  :custom (warning-minimum-level :error))

(use-package wgrep)

(use-package which-function
  :hook (after-init . which-function-mode))

(use-package windmove
  :custom
  (windmove-default-keybindings '([ignore] super))
  (windmove-delete-default-keybindings '([?\s-x] shift))
  (windmove-display-default-keybindings '([?\s-x]))
  (windmove-swap-states-default-keybindings '([ignore] super shift))
  (windmove-wrap-around t)
  :hook
  (after-init . windmove-mode)
  :config
  (cl-loop for (key . dir) in '(("h" . "left")
                                ("j" . "down")
                                ("k" . "up")
                                ("l" . "right"))
           do
           (define-key windmove-mode-map
                       (kbd (concat "s-" key))
                       (intern-soft (concat "windmove-" dir)))
           (define-key windmove-mode-map
                       (kbd (concat "s-x " (capitalize key)))
                       (intern-soft (concat "windmove-delete-" dir)))
           (define-key windmove-mode-map
                       (kbd (concat "s-x " key))
                       (intern-soft (concat "windmove-display-" dir)))
           (define-key windmove-mode-map
                       (kbd (concat "s-" (capitalize key)))
                       (intern-soft (concat "windmove-swap-states-" dir)))))

(use-package window
  :custom
  (switch-to-buffer-obey-display-actions t)
  (split-height-threshold nil))

(use-package window-divider
  :custom
  (window-divider-default-bottom-width 4)
  (window-divider-default-right-width 4)
  :hook
  (after-init . window-divider-mode))

;; (use-package winner
;;   :hook (after-init . winner-mode))

(use-package winum
  :bind
  (:map winum-keymap
        ("C-<f1>" . winum-select-window-1)
        ("C-<f2>" . winum-select-window-2)
        ("C-<f3>" . winum-select-window-3)
        ("C-<f4>" . winum-select-window-4)
        ("C-<f5>" . winum-select-window-5)
        ("C-<f6>" . winum-select-window-6)
        ("C-<f7>" . winum-select-window-7)
        ("C-<f8>" . winum-select-window-8)
        ("C-<f9>" . winum-select-window-9))
  :hook
  (after-init . winum-mode))

(use-package xref
  :custom
  (xref-after-jump-hook '(xref-pulse-momentarily))
  (xref-search-program 'ripgrep)
  (xref-truncation-width 100))

(use-package zenburn-theme
  :demand t
  :config (load-theme 'zenburn t))

;;; aliases

(defalias 'ev  'emacs-version)
(defalias 'eit 'emacs-init-time)

(defalias 'plp 'package-list-packages)
(defalias 'pi  'package-install)
(defalias 'pd  'package-delete)
(defalias 'pa  'package-autoremove)

(defalias 'ev-b 'eval-buffer)
(defalias 'ev-r 'eval-region)
(defalias 'ev-d 'eval-defun)

(defalias 'cf 'copy-file)
(defalias 'df 'delete-file)
(defalias 'rf 'rename-file)

(defalias 'lt 'load-theme)
(defalias 'dt 'disable-theme)

(defalias 'sb 'scratch-buffer)

(provide 'init)
