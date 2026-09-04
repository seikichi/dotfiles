;;; init.el -*- lexical-binding: t -*-
(when (version< emacs-version "31")
  (error "This init.el requires Emacs 31 or later (running %s)" emacs-version))

;;;; パッケージ管理
;; ELPA/MELPA は使わない (未固定の取得経路を塞ぐ)
(setq package-archives nil)

;; Emacs が自動生成する設定は custom.el に保存する
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error :no-message)

(use-package avy
  :vc (:url "https://github.com/abo-abo/avy"
       :rev "933d1f36cca0f71e4acb5fac707e9ae26c536264") ; master 2026-09-02 時点
  :bind (("M-j"   . avy-goto-word-1)))

(use-package vundo
  :vc (:url "https://github.com/casouri/vundo"
       :rev "b89f719824fe5da0f6a7590fad3ece798fd59909") ; 2.4.0 2026-09-04 時点
  :bind (("C-x u" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  ;; (vundo-compact-display t)
  )

(use-package undo-fu-session
  :vc (:url "https://github.com/emacsmirror/undo-fu-session"
            :rev "92d733a5b162a70c572fac17b9f9e872426df547") ; 0.8 2026-09-04 時点
  :custom
  (undo-fu-session-directory (expand-file-name "undo/" "~/.ehist/"))
  ;; 履歴ファイルが無制限に増え続けないようにする
  (undo-fu-session-file-limit 1000)
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode 1))

(use-package multiple-cursors
  :vc (:url "https://github.com/magnars/multiple-cursors.el"
            :rev "94b8b07a4bab87f803123723b68227565429dfa1") ; master 2026-09-02 時点
  :defer t)

(use-package consult
  :vc (:url "https://github.com/minad/consult"
            :rev "3ddec5493bce5445f099537be50b7a4f79c68321") ; 3.7 2026-09-02 時点
  :bind (("C-x b" . consult-buffer)
         ("M-s g" . consult-git-grep)
         ("M-s r" . consult-ripgrep))
  :config
  (add-to-list 'consult-preview-allowed-hooks
               'global-display-line-numbers-mode))

(use-package orderless
  :vc (:url "https://github.com/oantolin/orderless"
            :rev "cebe19e3cf0f30604d1ed1bfaa74fff21a4e89a5") ; 1.7 2026-09-02 時点
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)
  :config
  ;; Orderless の component 区切りとして SPC を入力できるようにする
  (keymap-unset minibuffer-local-completion-map "SPC")

  ;; Fido が設定する `flex' を Orderless に上書き
  (add-hook 'icomplete-minibuffer-setup-hook
            (lambda ()
              (setq-local completion-styles '(orderless basic)))))

;; リージョン選択中だけ有効な 1 キー操作
(defvar-keymap my/region-map
  "a" #'mc/mark-all-like-this
  "n" #'mc/mark-next-like-this
  "p" #'mc/mark-previous-like-this
  "u" #'mc/unmark-next-like-this
  "U" #'mc/unmark-previous-like-this
  "s" #'mc/skip-to-next-like-this
  "S" #'mc/skip-to-previous-like-this)
(add-to-list 'emulation-mode-map-alists `((mark-active . ,my/region-map)))

;;;; Tree-sitter: grammar もコミット固定 (Emacs 31 の各 ts-mode が動作確認済みの版)
;; いずれも ABI 14 で生成済み (このビルドの libtree-sitter 0.20 が受け付ける上限)
(setq treesit-language-source-alist
      '((markdown        "https://github.com/tree-sitter-grammars/tree-sitter-markdown" ; v0.4.1
                         :commit "413285231ce8fa8b11e7074bbe265b48aa7277f9"
                         :source-dir "tree-sitter-markdown/src")
        (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" ; v0.4.1
                         :commit "413285231ce8fa8b11e7074bbe265b48aa7277f9"
                         :source-dir "tree-sitter-markdown-inline/src")
        (typescript      "https://github.com/tree-sitter/tree-sitter-typescript"
                         :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
                         :source-dir "typescript/src")
        (tsx             "https://github.com/tree-sitter/tree-sitter-typescript"
                         :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
                         :source-dir "tsx/src")
        (dockerfile      "https://github.com/camdencheek/tree-sitter-dockerfile"
                         :commit "087daa20438a6cc01fa5e6fe6906d77c869d19fe")
        (json            "https://github.com/tree-sitter/tree-sitter-json"
                         :commit "4d770d31f732d50d3ec373865822fbe659e47c75")
        (yaml            "https://github.com/tree-sitter-grammars/tree-sitter-yaml" ; v0.7.2
                         :commit "7708026449bed86239b1cd5bce6e3c34dbca6415"))
      treesit-auto-install-grammar 'always) ; 未インストールなら上記から自動ビルド

;; Emacs 31 は既定で ts-mode を使わない。従来モードがある言語はここで ts 版に切り替える
(setopt treesit-enabled-modes '(json-ts-mode yaml-ts-mode))

;; .ts / .tsx / Dockerfile は Emacs 31 が自動で *-ts-mode に割り当てる。
;; markdown-ts-mode (experimental) は autoload されていないので手で登録する
(autoload 'markdown-ts-mode "markdown-ts-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))

;;;; 文字コード / 文字幅
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(use-default-char-width-table)          ; East Asian Ambiguous 文字を半角幅に

;;;; 見た目
(add-to-list 'custom-theme-load-path (locate-user-emacs-file "themes"))
(setq treesit-font-lock-level 4)
(load-theme 'vscode-light t)
(menu-bar-mode 0)
(setq inhibit-startup-screen t
      ring-bell-function #'ignore      ; C-g などでベルを鳴らさない
      visible-cursor nil)              ; 端末に「カーソル点滅 ON」(cvvis) を送らない

;; 行番号を表示 + 4桁分の幅を確保
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers-width 4)
(setq-default line-prefix "  ")

(setq-default truncate-lines t)

;; TAB・全角スペース・行末空白を可視化
(setq whitespace-style '(face tabs trailing spaces)
      whitespace-space-regexp "\\(\u3000+\\)"
      whitespace-global-modes '(prog-mode text-mode conf-mode))
(global-whitespace-mode 1)

;; JSON のキー/定数を VS Code Light+ の配色に (face を他言語と共有しているので mode 単位で差し替え)
(add-hook 'json-ts-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-property-use-face :foreground "#0451A5")
            (face-remap-add-relative 'font-lock-constant-face :foreground "#0000FF")))

;;;; インデント
(setq-default indent-tabs-mode nil tab-width 2)
(setq c-basic-offset 2
      js-indent-level 2)

;;;; 補完: ミニバッファは fido、バッファ内は completion-preview (Emacs 30+)
(fido-vertical-mode 1)
(savehist-mode 1)
(recentf-mode 1)

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(global-completion-preview-mode 1)
(which-key-mode 1)
(editorconfig-mode 1)

(setq use-short-answers t                ; yes/no を y/n に
      isearch-lazy-count t)              ; 検索中に件数 (n/m) を表示

;;;; キーバインド (ヘルプは F1 で)
(keymap-global-set "C-h" #'delete-backward-char)
(keymap-set isearch-mode-map "C-h" #'isearch-delete-char)
(keymap-set minibuffer-local-map "C-w" #'backward-kill-word)
(keymap-global-set "M-g" #'goto-line)
(keymap-global-set "C-o" #'dabbrev-expand)
(keymap-global-set "M-o" #'completion-at-point)

;;;; バックアップ / auto-save
(global-auto-revert-mode 1)

(let ((backup-dir   (expand-file-name "~/.ehist/backup/"))
      (auto-save-dir (expand-file-name "~/.ehist/auto-save/"))
      (session-dir   (expand-file-name "~/.ehist/auto-save-list/"))
      (lock-dir      (expand-file-name "~/.ehist/lock/")))
  (dolist (dir (list backup-dir auto-save-dir session-dir lock-dir)) (make-directory dir t))
  (setq backup-directory-alist `((".*" . ,backup-dir))
        auto-save-default t
        auto-save-file-name-transforms `((".*" ,auto-save-dir sha1))
        auto-save-list-file-prefix (expand-file-name ".saves-" session-dir)
        lock-file-name-transforms `((".*" ,lock-dir sha1))
        create-lockfiles t))
(setq undo-limit        (* 8 1024 1024)
      undo-strong-limit (* 12 1024 1024)
      undo-outer-limit  (* 64 1024 1024))

;;;; その他
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; WSL: kill した文字列を Windows クリップボードへ (clip.exe は CP932 で読むので変換して渡す)
(when-let* ((clip (executable-find "clip.exe")))
  (setq interprogram-cut-function
        (lambda (text)
          (let ((proc (make-process :name "clip.exe" :command (list clip)
                                    :connection-type 'pipe :noquery t
                                    :coding 'cp932-dos)))
            (process-send-string proc text)
            (process-send-eof proc)))))

;; マシン固有の設定
(load (locate-user-emacs-file "init-local.el") :no-error :no-message)
