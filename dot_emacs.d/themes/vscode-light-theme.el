;;; vscode-light-theme.el --- VS Code "Light Modern" theme -*- lexical-binding: t; -*-

;; This theme maps VS Code's built-in "Light Modern" theme to Emacs faces.
;;
;; Upstream VS Code theme inheritance:
;;   light_modern.json -> light_plus.json -> light_vs.json
;;
;; UI colors come from light_modern.json.
;; Syntax colors come from light_plus.json / light_vs.json.
;;
;; Drop this file into ~/.emacs.d/themes/ (or the equivalent chezmoi path)
;; and keep loading it with:
;;   (load-theme 'vscode-light t)

;;; Code:

(deftheme vscode-light
  "VS Code built-in Light Modern colors."
  :background-mode 'light
  :kind 'color-scheme)

(let* (;; Light Modern: workbench/editor colors
       (bg              "#FFFFFF")
       (fg              "#3B3B3B")
       (panel           "#F8F8F8")
       (border          "#E5E5E5")
       (inactive        "#868686")
       (shadow          "#767676")
       (accent          "#005FB8")
       (selection       "#ADD6FF")
       (inactive-sel    "#E5EBF1")
       (selection-hi    "#D6EAFF")
       (hover           "#F2F2F2")
       (list-sel        "#E8E8E8")
       (line-num        "#6E7681")
       (line-num-active "#171184")
       (indent-guide    "#D3D3D3")

       ;; Light+ / Visual Studio Light: syntax colors
       (keyword         "#0000FF")
       (control         "#AF00DB")
       (string          "#A31515")
       (comment         "#008000")
       (number          "#098658")
       (function        "#795E26")
       (type            "#267F99")
       (variable        "#001080")
       (constant        "#0070C1")
       (property        "#001080")
       (regexp          "#811F3F")
       (regexp-group    "#D16969")
       (escape          "#EE0000")
       (tag             "#800000")
       (attribute       "#E50000")
       (markup-blue     "#0451A5")
       (markup-heading  "#800000")
       (markup-bold     "#000080")
       (markup-italic   "#800080")

       ;; Diagnostics / diffs
       (error           "#F85149")
       (warning         "#BF8803")
       (info            "#005FB8")
       (added           "#2EA043")
       (deleted         "#F85149")
       (add-bg          "#E6FFEC")
       (del-bg          "#FFEBE9")
       (add-hi          "#ACF2BD")
       (del-hi          "#FFC0C0"))

  (custom-theme-set-faces
   'vscode-light

   ;; ------------------------------------------------------------------
   ;; Core editor / Light Modern UI
   ;; ------------------------------------------------------------------
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,accent))))
   `(region ((t (:background ,selection :foreground unspecified :extend t))))
   `(secondary-selection ((t (:background ,selection-hi :extend t))))
   `(highlight ((t (:background ,hover))))
   `(hl-line ((t (:background ,panel :extend t))))
   `(fringe ((t (:background ,bg :foreground ,line-num))))
   `(vertical-border ((t (:foreground ,border))))
   `(window-divider ((t (:foreground ,border))))
   `(window-divider-first-pixel ((t (:foreground ,border))))
   `(window-divider-last-pixel ((t (:foreground ,border))))
   `(shadow ((t (:foreground ,shadow))))
   `(escape-glyph ((t (:foreground ,escape))))
   `(nobreak-space ((t (:background ,panel))))
   `(trailing-whitespace ((t (:background "#FADADD"))))

   ;; VS Code Light Modern line numbers.
   `(line-number ((t (:foreground ,line-num :background ,bg))))
   `(line-number-current-line
     ((t (:foreground ,line-num-active :background ,bg :weight normal))))

   ;; VS Code Light Modern status bar is light, not blue.
   `(mode-line
     ((t (:background ,panel :foreground ,fg
                      :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive
     ((t (:background ,panel :foreground ,inactive
                      :box (:line-width 1 :color ,border)))))
   `(mode-line-buffer-id ((t (:foreground ,fg :weight bold))))
   `(header-line
     ((t (:background ,panel :foreground ,fg
                      :box (:line-width 1 :color ,border)))))

   ;; Selection / search
   `(match ((t (:background "#F8C9AB"))))
   `(isearch ((t (:background "#A8AC94" :foreground ,fg))))
   `(lazy-highlight ((t (:background "#F8C9AB" :foreground ,fg))))
   `(isearch-fail ((t (:background ,del-hi :foreground ,fg))))
   `(show-paren-match ((t (:background "#DCE9DC" :weight bold))))
   `(show-paren-mismatch ((t (:background ,error :foreground ,bg))))

   ;; Links and prompts
   `(link ((t (:foreground ,accent :underline t))))
   `(link-visited ((t (:foreground "#68217A" :underline t))))
   `(minibuffer-prompt ((t (:foreground ,accent :weight bold))))
   `(error ((t (:foreground ,error :weight bold))))
   `(warning ((t (:foreground ,warning :weight bold))))
   `(success ((t (:foreground ,added :weight bold))))

   ;; ------------------------------------------------------------------
   ;; Font Lock -- Light+ syntax palette
   ;; ------------------------------------------------------------------
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-doc-face ((t (:foreground ,comment))))

   ;; Light+ makes ordinary keywords blue, but control-flow/special keywords
   ;; magenta.  Emacs has no universal control-flow face, so keyword stays blue;
   ;; mode-specific faces can use `font-lock-preprocessor-face' / custom faces.
   `(font-lock-keyword-face ((t (:foreground ,keyword))))
   `(font-lock-builtin-face ((t (:foreground ,keyword))))
   `(font-lock-preprocessor-face ((t (:foreground ,control))))

   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-number-face ((t (:foreground ,number))))
   `(font-lock-constant-face ((t (:foreground ,constant))))
   `(font-lock-function-name-face ((t (:foreground ,function))))
   `(font-lock-function-call-face ((t (:foreground ,function))))
   `(font-lock-type-face ((t (:foreground ,type))))
   `(font-lock-variable-name-face ((t (:foreground ,variable))))
   `(font-lock-variable-use-face ((t (:foreground ,variable))))
   `(font-lock-property-name-face ((t (:foreground ,property))))
   `(font-lock-property-use-face ((t (:foreground ,property))))
   `(font-lock-escape-face ((t (:foreground ,escape))))
   `(font-lock-regexp-face ((t (:foreground ,regexp))))
   `(font-lock-regexp-grouping-construct
     ((t (:foreground ,regexp-group :weight normal))))
   `(font-lock-regexp-grouping-backslash
     ((t (:foreground ,regexp-group :weight normal))))
   `(font-lock-operator-face ((t (:foreground ,fg))))
   `(font-lock-punctuation-face ((t (:foreground ,fg))))
   `(font-lock-bracket-face ((t (:foreground ,fg))))
   `(font-lock-delimiter-face ((t (:foreground ,fg))))
   `(font-lock-misc-punctuation-face ((t (:foreground ,fg))))
   `(font-lock-negation-char-face ((t (:foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,error :weight bold))))

   ;; ------------------------------------------------------------------
   ;; Completion / minibuffer UI -- Light Modern list colors
   ;; ------------------------------------------------------------------
   `(icomplete-selected-match
     ((t (:background ,list-sel :foreground "#000000" :extend t))))
   `(icomplete-first-match ((t (:weight bold))))
   `(completions-common-part ((t (:foreground ,accent :weight bold))))
   `(completions-first-difference ((t (:weight bold))))
   `(completion-preview ((t (:foreground ,shadow))))
   `(completion-preview-common ((t (:foreground ,shadow :underline t))))
   `(completion-preview-exact ((t (:foreground ,shadow :underline t))))

   `(which-key-key-face ((t (:foreground ,constant))))
   `(which-key-command-description-face ((t (:foreground ,fg))))
   `(which-key-note-face ((t (:foreground ,comment))))

   ;; ------------------------------------------------------------------
   ;; Whitespace / guides
   ;; ------------------------------------------------------------------
   `(whitespace-tab ((t (:foreground ,indent-guide))))
   `(whitespace-space ((t (:foreground ,indent-guide))))
   `(whitespace-newline ((t (:foreground ,indent-guide))))
   `(whitespace-line ((t (:foreground ,warning))))
   `(whitespace-trailing ((t (:background "#FADADD"))))

   ;; ------------------------------------------------------------------
   ;; Flymake / Eglot / Eldoc
   ;; ------------------------------------------------------------------
   `(flymake-error ((t (:underline (:style wave :color ,error)))))
   `(flymake-warning ((t (:underline (:style wave :color ,warning)))))
   `(flymake-note ((t (:underline (:style wave :color ,info)))))
   `(eglot-highlight-symbol-face ((t (:background ,inactive-sel))))
   `(eldoc-highlight-function-argument
     ((t (:foreground ,function :weight bold))))

   ;; ------------------------------------------------------------------
   ;; Avy
   ;; ------------------------------------------------------------------
   `(avy-lead-face ((t (:background ,error :foreground ,bg :weight bold))))
   `(avy-lead-face-0 ((t (:background ,accent :foreground ,bg :weight bold))))
   `(avy-lead-face-1 ((t (:background ,shadow :foreground ,bg))))
   `(avy-lead-face-2 ((t (:background ,comment :foreground ,bg :weight bold))))

   ;; ------------------------------------------------------------------
   ;; Diff / VC
   ;; ------------------------------------------------------------------
   `(diff-added ((t (:foreground ,added :background ,add-bg :extend t))))
   `(diff-removed ((t (:foreground ,deleted :background ,del-bg :extend t))))
   `(diff-refine-added ((t (:background ,add-hi))))
   `(diff-refine-removed ((t (:background ,del-hi))))
   `(diff-header ((t (:background ,panel :foreground ,fg :extend t))))
   `(diff-file-header
     ((t (:background ,panel :foreground ,fg :weight bold :extend t))))
   `(diff-hunk-header
     ((t (:foreground ,markup-blue :background ,panel :extend t))))

   ;; ------------------------------------------------------------------
   ;; Markdown tree-sitter
   ;;
   ;; light_modern.json includes Light+, which includes light_vs.json.
   ;; Markdown-specific TextMate rules therefore come from light_vs.json:
   ;; heading #800000, bold #000080, italic #800080,
   ;; list/quote punctuation #0451A5, inline raw #800000.
   ;; ------------------------------------------------------------------
   `(markdown-ts-heading-1
     ((t (:foreground ,markup-heading :weight bold :height 1.0))))
   `(markdown-ts-heading-2
     ((t (:foreground ,markup-heading :weight bold))))
   `(markdown-ts-heading-3
     ((t (:foreground ,markup-heading :weight bold))))
   `(markdown-ts-heading-4
     ((t (:foreground ,markup-heading :weight bold))))
   `(markdown-ts-heading-5
     ((t (:foreground ,markup-heading :weight bold))))
   `(markdown-ts-heading-6
     ((t (:foreground ,markup-heading :weight bold))))
   `(markdown-ts-setext-heading
     ((t (:foreground ,markup-heading :weight bold))))

   `(markdown-ts-bold
     ((t (:foreground ,markup-bold :weight bold))))
   `(markdown-ts-emphasis
     ((t (:foreground ,markup-italic :slant italic))))
   `(markdown-ts-strikethrough
     ((t (:strike-through t))))

   ;; VS Code's "markup.inline.raw" sets foreground only.
   `(markdown-ts-code-span ((t (:foreground ,markup-heading))))

   ;; Workbench textCodeBlock.background is #F8F8F8 in Light Modern.
   `(markdown-ts-code-block ((t (:background ,panel :extend t))))
   `(markdown-ts-in-code-block ((t (:background ,panel :extend t))))
   `(markdown-ts-indented-code-block ((t (:background ,panel :extend t))))
   `(markdown-ts-language-keyword ((t (:foreground ,keyword))))

   ;; VS Code colors list and quote punctuation blue.  Keep quote body normal
   ;; so the result is closer to VS Code than a globally grey/italic quote.
   `(markdown-ts-block-quote ((t (:foreground ,fg))))
   `(markdown-ts-list-marker ((t (:foreground ,markup-blue))))

   `(markdown-ts-link ((t (:foreground ,accent :underline t))))
   `(markdown-ts-link-destination ((t (:foreground ,accent))))
   `(markdown-ts-delimiter ((t (:foreground ,markup-blue))))
   `(markdown-ts-thematic-break ((t (:foreground ,markup-blue))))
   `(markdown-ts-html-tag ((t (:foreground ,tag))))
   `(markdown-ts-table-header ((t (:weight bold))))
   `(markdown-ts-task-checked ((t (:foreground ,added))))
   `(markdown-ts-task-unchecked ((t (:foreground ,shadow))))

   ;; ------------------------------------------------------------------
   ;; A few common non-tree-sitter package faces
   ;; ------------------------------------------------------------------
   `(markdown-header-face ((t (:foreground ,markup-heading :weight bold))))
   `(markdown-header-face-1 ((t (:foreground ,markup-heading :weight bold))))
   `(markdown-header-face-2 ((t (:foreground ,markup-heading :weight bold))))
   `(markdown-header-face-3 ((t (:foreground ,markup-heading :weight bold))))
   `(markdown-bold-face ((t (:foreground ,markup-bold :weight bold))))
   `(markdown-italic-face ((t (:foreground ,markup-italic :slant italic))))
   `(markdown-inline-code-face ((t (:foreground ,markup-heading))))
   `(markdown-code-face ((t (:background ,panel))))
   `(markdown-list-face ((t (:foreground ,markup-blue))))

   ;; HTML / SGML-ish faces, when modes expose them.
   `(font-lock-tag-face ((t (:foreground ,tag))))
   `(font-lock-attribute-name-face ((t (:foreground ,attribute))))))

(provide-theme 'vscode-light)

;;; vscode-light-theme.el ends here
