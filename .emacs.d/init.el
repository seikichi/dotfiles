;; ==================================================
;; $HOME/.emacs.d/init.el
;;
;; Author: seikichi@kmc.gr.jp
;; ==================================================

;; ==================================================
;; el-get
;; ==================================================
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle avy)
(el-get-bundle color-theme)
(el-get-bundle company)
(el-get-bundle eglot)
(el-get-bundle flycheck)
(el-get-bundle git-gutter)
(el-get-bundle helm)
(el-get-bundle helm-descbinds)
(el-get-bundle helm-flycheck)
(el-get-bundle helm-git-grep)
(el-get-bundle helm-ls-git)
(el-get-bundle highlight-symbol)
(el-get-bundle howm)
(el-get-bundle multiple-cursors)
(el-get-bundle region-bindings-mode)
(el-get-bundle undo-tree)
(el-get-bundle zenburn-theme)

;; prog
(el-get-bundle dockerfile-mode)
(el-get-bundle gitconfig-mode)
(el-get-bundle gitignore-mode)
(el-get-bundle go-mode)
(el-get-bundle graphviz-dot-mode)
(el-get-bundle groovy-mode)
(el-get-bundle json-mode)
(el-get-bundle less-css-mode)
(el-get-bundle lua-mode)
(el-get-bundle markdown-mode)
(el-get-bundle ruby-mode)
(el-get-bundle rust-mode)
(el-get-bundle typescript-mode)
(el-get-bundle web-mode)
(el-get-bundle yaml-mode)

(package-initialize)

;; ==================================================
;; Server
;; ==================================================
(require 'server)
(unless (server-running-p) (server-start))

;; ==================================================
;; Language
;; ==================================================
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;; (setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; ==================================================
;; *scratch* AA
;; ==================================================
(setq initial-scratch-message
";;                          .::::::::::::::::::::::::::::::::::::.
;;                       ／::::::::::::::::::::::::::::::::::::::::＼
;;                     ／::::::::::::::::::::::::::::::::::::::::::::丶
;;                    .::::::::::::::::/:::::::::::::::::::::::::::::::.
;;                  /::/:::::::::::::／:::::::::::::::::::::::::::::::::.
;; .               /:／::::::::::/::/ |:::::::l::::::::::::::::::::::::
;;                .:/ .′:::::::／':/  .:::::::|:::|::::::::::::::::／
;;               '/  :::::::::/  .:′  八::::::|i::八:::::::::::::／
;;              {i   |:::::::/   ヘ      ､::::|ﾍ::: ＼:::::::::／
;;                   |::::::′    {: ＼    ＼::!  ＼〉  ＼:::::/
;; .                 l:l ::{          ＼    ＼     ＞ ⌒  ＼: /
;;                   j八:::l 「￣厂::乙ト     ~¨  斗────────/
;;                      l::| 人, 乂::::ｿ            ん:::(_′
;;                      l::|                        乂:::ソ
;;                      |:: .:/:/:/:/:.                   l
;;                      :::{             ────  、 .:/:/:/:.
;;                     .:::∧           Ⅴ ￣￣   }         ∧
;;                    .:::::＼         、      ′         ::ﾍ
;;                  /::::::::⌒ヽ、       ＿＿ノ        ノ:::ﾍ
;;                .′::::〉::}  }:＞...  ＿＿,.. -=ﾆ:::/:::::/:＼
;;               /:::::/⌒::ﾘ    弋__  - ' _,. --イ〉ヽ ::::/── 〉
;; .            /:::::/   ∨             ⌒)イ '  ／  / ::: /  ／ ／:＞...
;;             /:::: ′    |            ／) ﾍ  ／   /:::::く⌒ヽ／  l:::::∧
;; .          /:::::′     }              ﾉ  V   ./:::r{⌒｀ ヽ ',  |::::: ∧
;;           /:::::|     ′           イ⌒       .::: ﾉ 、  ｀   i  j ::::: ∧
;; .        /:::::/     /         ／  ///{    /:::( ﾍ         /   | :::::: ∧
;;         /:::::(     /         /   ////!   {::::／/        /    }:::::::::∧
")

;; ==================================================
;; Key Bindings
;; ==================================================
(global-set-key "\C-h" 'delete-backward-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-o" 'dabbrev-expand)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(global-set-key "\M-," 'pop-tag-mark)
(define-key minibuffer-local-completion-map  "\C-w" 'backward-kill-word)

;; ==================================================
;; Font
;; ==================================================

;; set east asian ambiguous width 1
(defun set-east-asian-ambiguous-width (width)
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil)))
    (dolist
        (range
         '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
                  (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
                  #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
                  (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0
                  (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
                  #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
                  (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
                  (#x0148 . #x014B) #x014D (#x0152 . #x0153)
                  (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
                  #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
                  (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
                  #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
                  (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401
                  (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
                  (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
                  (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
                  #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
                  #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
                  #x212B (#x2153 . #x2154) (#x215B . #x215E)
                  (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
                  (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
                  (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
                  #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
                  (#x2227 . #x222C) #x222E (#x2234 . #x2237)
                  (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
                  (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
                  (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
                  #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
                  (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
                  (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
                  (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
                  (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1)
                  (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
                  (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
                  #x2642 (#x2660 . #x2661) (#x2663 . #x2665)
                  (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
                  (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F)
                  #xFFFD
                  ))
      (set-char-table-range table range width))
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))
(set-east-asian-ambiguous-width 1)

;; ==================================================
;; Looks
;; ==================================================

(if (display-graphic-p)
    (progn (menu-bar-mode 0)
           (toggle-scroll-bar 0)
           (tool-bar-mode 0)))

(blink-cursor-mode 0)
(menu-bar-mode 0)
(setq transient-mark-mode t)
(set-face-background 'region "gray")
(set-face-foreground 'region "black")
(show-paren-mode)

;; hide *GNU Emacs* buffer
(setq inhibit-startup-screen t)

(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     ;;("[\r]*\n" 0 my-face-r-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; use whitespaces instead of TAB
(setq-default tab-width 2 indent-tabs-mode nil)
(setq c-basic-offset 2)

;; show line number
(global-display-line-numbers-mode t)
(setq display-line-numbers "%4d: ")

;; truncate
(set-default 'truncate-lines t)
(set-default 'truncate-partial-width-windows t)

;; ==================================================
;; Indent
;; ==================================================
(setq c-default-style '((java-mode . "java") (other . "linux")))
(setq-default tab-width 2 indent-tabs-mode nil)
(setq c-basic-offset 2)

;; ==================================================
;; Thema
;; ==================================================
(require 'zenburn-theme)

;; ==================================================
;; Company
;; ==================================================
(global-company-mode +1)
(custom-set-variables '(company-idle-delay nil))

(set-face-attribute 'company-tooltip nil :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common nil :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common-selection nil :foreground "white" :background "steelblue")
(set-face-attribute 'company-tooltip-selection nil :foreground "black" :background "steelblue")
(set-face-attribute 'company-preview-common nil :background nil :foreground "lightgrey" :underline t)
(set-face-attribute 'company-scrollbar-fg nil :background "orange")
(set-face-attribute 'company-scrollbar-bg nil :background "gray40")

(global-set-key (kbd "M-o") 'company-complete)

;; ==================================================
;; Helm
;; ==================================================
(require 'helm-config)
(require 'helm)

;; customize
(setq helm-boring-file-regexp-list '("~$" "\\.elc$"))
(setq helm-delete-minibuffer-contents-from-point t)
(setq helm-ff-skip-boring-files t)
(setq helm-ls-git-show-abs-or-relative 'relative)
(setq helm-truncate-lines 't)

;; http://d.hatena.ne.jp/syohex/20131016/1381935863
(defun my/helm-etags-select (arg)
  (interactive "P")
  (let ((tag  (helm-etags-get-tag-file))
        (helm-execute-action-at-once-if-one t))
    (when (or (equal arg '(4))
              (and helm-etags-mtime-alist
                   (helm-etags-file-modified-p tag)))
      (remhash tag helm-etags-cache))
    (if (and tag (file-exists-p tag))
        (helm :sources 'helm-source-etags-select :keymap helm-etags-map
              :input (concat (thing-at-point 'symbol) " ")
              :buffer "*helm etags*"
              :default (concat "\\_<" (thing-at-point 'symbol) "\\_>"))
      (message "Error: No tag file found, please create one with etags shell command."))))

;; set helm-command-prefix-key to "C-q"
(global-set-key (kbd "C-c q") 'quoted-insert)
(global-unset-key (kbd "C-q"))
(custom-set-variables '(helm-command-prefix-key "C-q"))

;; key settings
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x")   'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-command-map (kbd "d") 'helm-descbinds)
(define-key helm-command-map (kbd "f") 'helm-flycheck)
(define-key helm-command-map (kbd "g") 'helm-git-grep)
(define-key helm-command-map (kbd "i") 'helm-imenu)
(define-key helm-command-map (kbd "l") 'helm-ls-git-ls)
(define-key helm-command-map (kbd "o") 'helm-occur)
(define-key helm-command-map (kbd "r") 'helm-resume)
(define-key helm-command-map (kbd "t") 'my/helm-etags-select)
(define-key helm-command-map (kbd "y") 'helm-show-kill-ring)

;; key settings for helm
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-map (kbd "C-w") 'backward-kill-word)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
;; move to occur mode when C-o is typed in isearch
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

;; ==================================================
;; Howm
;; ==================================================
(require 'howm)
(setq howm-menu-lang 'ja)
(setq howm-directory "~/Dropbox/howm")
(setq howm-view-use-grep t)
(setq howm-process-coding-system 'utf-8-unix)

;; ==================================================
;; Undo
;; ==================================================
(require 'undo-tree)
(global-undo-tree-mode t)
(setq undo-tree-auto-save-history t)

;; ==================================================
;; multiple-cursor
;; ==================================================
;; multiple-cursors & smartrep
(require 'multiple-cursors)
(require 'region-bindings-mode)
(region-bindings-mode-enable)

(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "d" 'mc/mark-all-dwim)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "N" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map "u" 'mc/unmark-next-like-this)
(define-key region-bindings-mode-map "U" 'mc/unmark-previous-like-this)
(define-key region-bindings-mode-map "s" 'mc/skip-to-next-like-this)
(define-key region-bindings-mode-map "S" 'mc/skip-to-previous-like-this)
(define-key region-bindings-mode-map "i" 'mc/insert-numbers)
(define-key region-bindings-mode-map "h" 'mc-hide-unmatched-lines-mode)
(define-key mc/keymap (kbd "C-c h") 'mc-hide-unmatched-lines-mode)

;; ==================================================
;; Misc.
;; ==================================================

;; git-gutter
(global-git-gutter-mode t)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ignores cases in file completetion
(setq completion-ignore-case t)

;; auto revert
(global-auto-revert-mode 1)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; add +x to files that start with "#!"
(add-hook 'after-save-hook
          '(lambda ()
             (save-restriction
               (widen)
               (if (string= "#!" (buffer-substring 1 (min 3 (point-max))))
                   (let ((name (buffer-file-name)))
                     (or (char-equal ?. (string-to-char (file-name-nondirectory name)))
                         (let ((mode (file-modes name)))
                           (set-file-modes name (logior mode (logand (/ mode 4) 73)))
                           (message (concat "Wrote " name " (+x)")))))))))

;; avy
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "C-M-j") 'avy-goto-char)
;; (global-set-key (kbd "C-M-J") 'avy-goto-line)

;; local
(if (file-exists-p "~/.emacs.d/init-local.el") (load "~/.emacs.d/init-local.el"))

;; ==================================================
;; Prog Modes
;; ==================================================
;; Javascript
(setq js-indent-level 2)

;; TypeScript
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(setq typescript-indent-level 2)
(add-hook 'typescript-mode-hook 'prettier-js-mode)

;; Ruby
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile" . ruby-mode))

;; Go
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") 'godef-jump)))
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-c C-i") 'go-impl)))
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-c C-r") 'go-rename)))
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(set-face-attribute 'eldoc-highlight-function-argument
                    nil :underline t :foreground "#7F9F7F" :weight 'bold)
(setq gofmt-command "goimports")
(font-lock-add-keywords 'go-mode '(("\\b\\(err\\)\\b" 1 '((:foreground "#7F9F7F") (:weight bold)) t)))

;; Java/Groovy
(add-to-list 'auto-mode-alist '("\\.gradle" . groovy-mode))

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; Rust
(setq rust-format-on-save t)
