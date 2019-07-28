;; -*- lexical-binding: t -*-

(fset 'yes-or-no-p 'y-or-n-p)

(require 'dash)
(require 's)

(setq user-full-name "Florian Schrödl")

(defcustom downloads-dir "~/Downloads/"
  "Directory containing my downloads."
  :type 'string)

(defcustom screenshot-dir "~/Pictures/Screenshots"
  "Directory containing my screenshots."
  :type 'string)

(defalias 'λ 'lambda)

(defmacro template (text)
  "Template literals"
  (let ((pattern "<<\\(.*?\\)>>"))
    ;; The regexp matches anything between delimiters, non-greedily
    (with-temp-buffer
      (save-excursion (insert text))
      (let ((matches '()))
        (while (re-search-forward pattern nil t)
          (push (match-string 1) matches)
          (replace-match "%s" t t))
        `(format ,(buffer-string) ,@(reverse (mapcar 'read matches)))))))

(defun noop (args) nil)

(defun do-jxa-script (cmd)
  "Run a osx javascript automation script via bash"
  (shell-command-to-string
   (concat "osascript -l 'JavaScript' -e '" cmd "'")))

(defun eval-and-replace-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (right-char) ;; Fix for normal mode
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun copy-message (x)
  "Executes kill-new but with a message log side effect"
  (kill-new x)
  (message "Copied to clipboard: %s" x))

(defun swap-list-items (LIST el1 el2)
  "in LIST swap indices EL1 and EL2 in place"
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))

(defun -shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (loop for i in (reverse (number-sequence 1 (1- (length LIST))))
        do (let ((j (random (+ i 1))))
             (swap-list-items LIST i j)))
  LIST)

(defun math-on-number (f &optional num)
  "Read user input and apply with function f to the number at point"
  (let* ((x (thing-at-point 'number))
         (arithmetic-symbol (pcase f
                              ('+ "+")
                              ('- "-")
                              ('/ "/")
                              ('* "*")
                              (_ (error "Unknown function %s" f))))
         (readline (concat (number-to-string x) " " arithmetic-symbol " "))
         (y (or num (read-number readline)))
         (result (funcall f x y))
         (bounds (bounds-of-thing-at-point 'evil-WORD)))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "%.02f" result))))

(defun +math|add-to-number ()
  (interactive)
  (math-on-number '+))

(defun +math|subtract-from-number ()
  (interactive)
  (math-on-number '-))

(defun +math|subtract-maran-vegan ()
  (interactive)
  (math-on-number '- 8.60))

(defun +math|divide-by-number ()
  (interactive)
  (math-on-number '/))

(defun +math|multiply-by-number ()
  (interactive)
  (math-on-number '*))

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Window locked!"
     "%s: Window unlocked!")
   (current-buffer)))

(defun +file/latest-file-in-dir (path)
  (let ((e (f-entries path)))
    (car (sort e (lambda (a b)
                   (not (time-less-p (file-timestamp a)
                                     (file-timestamp b))))))))

(defun file-timestamp (f) (let ((attrs (file-attributes f))) (nth 5 attrs)))

(defun +macos/reveal-in-finder ()
  "Reveal the current file in finder and select it"
  (interactive)
  (do-applescript (concat
                   "tell application \"Finder\"\n"
                   "    activate\n"
                   "    reveal POSIX file \"" (buffer-file-name) "\"\n"
                   "end tell")))

(defhydra hydra-expand-region ()
   "region: "
   ("f" er/mark-defun "defun")
   ("v" er/expand-region "expand")
   ("V" er/contract-region "contract"))

(evil-define-key 'visual 'global (kbd "v") #'hydra-expand-region/body)

(when (display-graphic-p)

(defcustom my-default-frame-size nil
  "A cons cell of screen dimensions (width . height)"
  :type 'cons)

(defcustom osx-screen-reserved-y-space 48
  "How much of the screen is available when subtracting the
1. app title bar
2. menu bar"
  :type 'integer)

(defcustom osx-screen-reserved-x-space 12
  "How much of the screen is available when subtracting the
1. Window Border"
  :type 'integer)

(defun my-set-initial-frame-size (size)
  "Set the initial frame"
  (let* ((width (car size))
         (height (cdr size))
         (left (- (x-display-pixel-width) width)))
    (setq initial-frame-alist
          (append (list `(left . ,left)
                        `(width . ,width)
                        '(fullscreen . fullheight))
                  initial-frame-alist)))
  (setq my-default-frame-size size))

(defun my-frame-resize-width (width)
  "Set the frame WIDTH. Maximize the frame vertically (minus the osx reserver space)"
  (set-frame-width (selected-frame) width nil t)
  (set-frame-height (selected-frame) (- (x-display-pixel-height) osx-screen-reserved-y-space) nil t)
  (set-frame-position (selected-frame) (- (x-display-pixel-width) width osx-screen-reserved-x-space) 0))

(defun my-frame-resize-work-external ()
  "External Monitor at work"
  (interactive)
  (my-frame-resize-width 1410))

(cond
 ((--first (s-contains? it system-name) '("Florians-MBP" "Florians-MacBook-Pro"))
  (my-frame-resize-work-external)
  (setq-default line-spacing 0.3))
 ((string= system-name "Florians-iMac.local")
  (setq-default line-spacing 10)
  (setq default-line-spacing 10)
  (setq doom-font (font-spec :family "Menlo" :size 14)))
 ((string= system-name "Florians-MacBook-Air.local")
  (setq-default line-spacing 0.4)
  (setq initial-frame-alist
        (append (list '(left . 272)
                      '(width . 165)
                      '(fullscreen . fullheight))
                initial-frame-alist)))
 (t (setq-default line-spacing 0.15)))

)

(defconst light-theme 'doom-one)
(defconst dark-theme  'doom-one-light)

(defun +doom|toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (cond ((eq doom-theme dark-theme)
         (message "Toggling to light-theme: %s" light-theme)
         (setq doom-theme light-theme)
         (doom/reload-theme))
        ((eq doom-theme light-theme)
         (message "Toggling to dark-theme: %s" dark-theme)
         (setq doom-theme dark-theme)
         (doom/reload-theme))
        (t (message "Toggling theme is not possible. Theme is not currently light-theme (%s) or dark-theme (%s)." light-theme dark-theme))))

(setq
 scroll-conservatively 10
 scroll-margin 10)

(add-hook 'term-mode-hook (λ! (setq-local scroll-margin 0)))
(add-hook 'ivy-mode-hook (λ! (setq-local scroll-margin 0)))

(defvar default-line-spacing 0.2)

(defun set-line-spacing (&optional spacing)
  "Set the line spacing
When no line spacing is given is the default-line-spacing"
  (if line-spacing
      (setq-default line-spacing (+ (or spacing default-line-spacing) line-spacing))
    (setq-default line-spacing (+ 0 default-line-spacing))))

(defun +ui|reset-line-spacing ()
  (interactive)
  (setq-default line-spacing nil))

(defun +ui|increase-line-spacing ()
  (interactive)
  (set-line-spacing))

(defun +ui|decrease-line-spacing ()
  (interactive)
  (set-line-spacing (- default-line-spacing)))

(evil-define-key 'normal 'global (kbd "]z") #'+line-spacing/step/body)

;;;###autoload (autoload '+common-lisp/macrostep/body "lang/common-lisp/autoload/hydras" nil nil)
(defhydra +line-spacing/step (:exit nil :hint nil :foreign-keys run)
  "
Macro Expansion
^^Definitions                           ^^Compiler Notes             ^^Stickers
^^^^^^─────────────────────────────────────────────────────────────────────────────────────
[_z_] Expand
[_Z_] Collapse
"
  ("z" +ui|increase-line-spacing)
  ("Z" +ui|decrease-line-spacing)
  ("q" noop :exit t))

(after! org
  (set-popup-rule! "^\\*Org Agenda" :side 'right :size 0.55 :select t :modeline t :ttl nil :quit nil)
  (set-popup-rule! "^\\*Org Src" :ignore t)
  (set-popup-rule! "^\\*Org QL Search" :side 'bottom :size 0.5 :select t :modeline t :ttl nil))

(set-popup-rule! "^\\*helm" :vslot -100 :size 0.32 :ttl nil)
(set-popup-rule! "^\\*doom:scratch" :ignore t)

(set-popup-rule! "^\\*nodejs" :side 'right :size 0.55 :select t :modeline t :ttl nil)

(set-popup-rule! "^\\*compilation" :side 'right :size 0.5 :select t :modeline t :ttl nil)

(setq-default fill-column 110)
(setq visual-fill-column-width fill-column)

(setq visual-fill-column-center-text t
      visual-fill-column-width
      ;; take Emacs 26 line numbers into account
      (+ (if EMACS26+ 6 0) fill-column))

(setq birthday-slack-emojis '("🍰" "🎂" "🎉" "🎈" "🎁"))

(defun birthday-msg (name &optional emojis)
  "Creates birthday string"
  (let ((emojis (or emojis (--> birthday-slack-emojis
                               (-shuffle it)
                               (-take 3 it)
                               (string-join it " ")))))
    (template "<<(reverse emojis)>> !! Happy Birthday <<name>> !! <<emojis>>")))

(defun birthday-msg|copy ()
  "Copies birthday string"
  (interactive)
  (--> (read-string "Name: ")
       (birthday-msg it)
       (copy-message it)))

(def-package! blimp
  :hook (image-mode-hook . blimp-mode))

(map! :map comint-mode-map
      :n "RET" (λ! (comint-send-input nil t)))

(setq company-transformers '(company-sort-by-occurrence)
      company-idle-delay 0.5)

(defun floscr:buffer-list-with-modes (modes)
  "Get all buffers that match MODES"
  (--filter
   (with-current-buffer it (-contains? (doom-enlist modes) major-mode))
   (buffer-list)))

(defun floscr:buffer-list-with-major-mode ()
  "Get all buffers matching the current major-mode
Has built in aliases"
  (let ((javascript-modes (list 'rjsx-mode 'js2-mode)))
    (pcase major-mode
      ('rjsx-mode
       (floscr:buffer-list-with-modes javascript-modes))
      ('js2-mode
       (floscr:buffer-list-with-modes javascript-modes))
      (_
       (floscr:buffer-list-with-modes major-mode)))))

(defun +company/whole-lines-all-buffers (command &optional arg &rest ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (pcase command
    (`interactive (company-begin-backend '+company/whole-lines-all-buffers))
    (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
    (`candidates
     (all-completions
      arg
      (funcall (-compose
                #'-uniq
                #'-flatten
                (lambda (xs)
                  (--map (with-current-buffer it
                           (split-string
                            (replace-regexp-in-string
                             "^[\t\s]+" ""
                             (buffer-substring-no-properties (point-min) (point-max)))
                            "\\(\r\n\\|[\n\r]\\)" t)) xs)))
               (floscr:buffer-list-with-major-mode))))))

(map!
 (:prefix "C-x"
   :i "C-l" #'+company/whole-lines-all-buffers
   :i "C-." #'+company/whole-lines))

(after!
  dired
  :config
  (when (and IS-MAC (locate-file "gls" exec-path))
    (setq dired-listing-switches "-la -h --group-directories-first"
          dired-k-human-readable t
          insert-directory-program "gls" dired-use-ls-dired t)))

(setq dired-dwim-target t)

(def-package! dired-x
  :after dired
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                ;; Reason Compiled Files
                "\\|\\.bs.js$")))

(put 'dired-find-alternate-file 'disabled nil)

(defun +dired|kill-dired-buffers ()
  "Kills all dired buffers
Dired creates a buffer for every directory which it visits
Which is fine since you can easily switch between visited buffers
But at some time I want to purge those buffers"
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(map! :when (featurep! :editor evil +everywhere)
      :after dired
      :map dired-mode-map
      :n "Q" #'+dired|kill-dired-buffers)

(defun +dired|change-to-wdired-mode ()
  "Simple forward to wdired-change-to-wdired-mode, but staying in normal mode."
  (interactive)
  (wdired-change-to-wdired-mode)
  (evil-normal-state)
  (forward-char))

(map! :when (featurep! :editor evil +everywhere)
      :after dired
      :map dired-mode-map
      :n "\\" #'+dired|change-to-wdired-mode)

(def-package! dired-recent
  :after dired
  :init
  :config
  (setq dired-recent-directories-file (concat doom-cache-dir "recentdir")
        dired-recent-max-directories 300)
  (dired-recent-mode 1))

;; Always truncate ElDoc messages to one line. This prevents the echo
;; area from resizing itself unexpectedly when point is on a variable
;; with a multiline docstring.
(setq eldoc-echo-area-use-multiline-p nil)

;; Show ElDoc messages in the echo area immediately, instead of after
;; 1/2 a second.
(setq eldoc-idle-delay 0)

;; Disable eldoc mode
(global-eldoc-mode -1)

(setq +eshell-aliases
  '(("q"      "exit")
    ("f"      "find-file $1")
    ("bd"     "eshell-up $1")
    ("rg"     "rg --color=always $*")
    ("ag"     "ag --color=always $*")
    ("l"      "ls -lh")
    ("ll"     "ls -lah")
    ("gs"     "git status")
    ("groot"  "cd (projectile-project-root)")
    ("gc"     "git commit")
    ("grha"   "git reset --hard; git clean -f -d")
    ("clear"  "clear-scrollback")))

(defun +eshell/cat (file)
  "Like `cat' but output with Emacs syntax highlighting."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (delay-mode-hooks
        (set-auto-mode)
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))))
    (buffer-string)))

(add-to-list '+eshell-aliases '("cat" "+eshell/cat $1"))

(after! evil-snipe
  (setq evil-snipe-repeat-keys t))

(setq shr-width 120)

(after! flycheck
  :config
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
        (id (one-or-more (not (any " "))))
        (message) line-end))
    :modes (text-mode markdown-mode gfm-mode))

  (add-to-list 'flycheck-checkers 'proselint))

(def-package! git-lens
  :commands (git-lens))

(def-package! indium
  :commands indium-connect
  :config
  (setq indium-chrome-executable "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary"))

(setq
 flycheck-javascript-eslint-executable (executable-find "eslint_d")
 flycheck-disabled-checkers '(javascript-jshint javascript))

(after! rjsx-mode
  (add-hook 'js2-mode-hook #'eslintd-fix-mode)
  (map! :map rjsx-mode-map
        :localleader
        (:desc "Open Self-Closing Tag" :n ">" #'+rjsx|expand-insert-self-closing-tag)
        (:desc "Open Self-Closing Tag" :n "<" #'rjsx-rename-tag-at-point)))

(after! js2-mode
  (add-hook 'js2-mode-hook #'eslintd-fix-mode)
  :config
  (map! :map js2-mode-map
        :localleader
        (:desc "import" :n "i" #'+js|ivy-import-file)
        (:desc "Indium" :prefix "I"
          :desc "Reload" :n  "r" #'indium-reload
          :desc "Start" :n  "s" #'indium-connect)))

(defun +js/remove-js-ext (f)
  "Remove js extension from string"
  (replace-regexp-in-string "\.js$" "" f))

(defun +js/match-const-function-name (line)
  "Matches a line to the word after the declaration"
  (nth 2 (s-match
          "\\(const\\|let\\|class\\)\s\\(.+?\\)\s"
          line)))

(defun +js/const-function-at-point ()
  "Returns the current function name at the current line"
  (+js/match-const-function-name (thing-at-point 'line t)))

(defun js2r-export-default ()
  "Exports the current declaration at the end of the file"
  (interactive)
  (save-excursion
    (let* ((name (+js/const-function-at-point)))
      (goto-char (point-max))
      (insert "\n")
      (insert (template "export default <<name>>;")))))

(defun js2r-extract-const-to-file ()
  "Extracts function to external file"
  (interactive)
  (let* ((name (+js/const-function-at-point))
         (path (concat "./" name ".js")))
    (evil-digit-argument-or-evil-beginning-of-line)
    (js2r-kill)
    (f-write-text "" 'utf-8 path)
    (find-file path)
    (yank)))

(defun +js/index-file-names (&optional actions-dir)
  "Get filenames from current buffers directory"
  (let ((fs (directory-files (or actions-dir default-directory) nil ".*\\.js")))
    (mapcar '+js/remove-js-ext
            (remove "index.js" fs))))

(defun +js|generate-index (&optional actions-dir)
  "Generate an index import file for files in directory"
  (interactive)
  (erase-buffer)
  (let* ((fs (+js/index-file-names actions-dir)))
    (mapc (lambda (f) (insert "import " f " from './" f "';\n")) fs)
    (insert "\n")
    (insert "export default {\n")
    (mapc (lambda (f) (insert "    " f ",\n")) fs)
    (insert "};")))

(defun +js|convert-sexp-to-template-string ()
  "Wrap sexp into a template string"
  (interactive)
  (kill-sexp)
  (insert (concat "`${" (substring-no-properties (car kill-ring)) "}`"))
  (pop kill-ring))

(add-hook! js-mode
  (require 'evil-text-objects-javascript)
  (evil-text-objects-javascript/install))

(defun +rjsx|expand-insert-self-closing-tag ()
  "Opens the current tag at any position of the cursor and starts insert mode"
  (interactive)
  (search-forward "/>")
  (evil-backward-char)
  (call-interactively #'delete-backward-char)
  (call-interactively #'rjsx-electric-gt)
  (newline)
  (call-interactively #'evil-indent-line)
  (call-interactively #'evil-open-above))

(defun company-js-files (command &optional arg &rest ignored)
  "Company complete path. Remove extension after completion"
  (interactive (list 'interactive))
  (require 'company)
  (cl-case command
    (interactive (company-begin-backend 'company-js-files))
    (prefix (company-files--grab-existing-name))
    (candidates (company-files--complete arg))
    (location (cons (dired-noselect
                     (file-name-directory (directory-file-name arg))) 1))
    (post-completion (when (s-matches? "\.js$" arg) (delete-backward-char 3)))
    (sorted t)
    (no-cache t)))

(map! :map js2-mode-map
      :i "C-x C-f" #'company-js-files)

;; TODO Make template accepts a cursor placeholder
;; TODO Maybe even look at exports
(defun +js/import-file (file)
  (let ((cursor-postion (point))
        (filename (+js/remove-js-ext file)))
    (insert (template "import  from '<<filename>>';"))
    (goto-char cursor-postion)
    (forward-char 7)
    (evil-insert-state)))

(defun +js|ivy-import-file (&optional action)
  (interactive)
  (ivy-read "Import file "
            (append
             (--map (concat "./" it)
                    (split-string (shell-command-to-string (concat find-program " " counsel-file-jump-args)) "\n" t))
             (split-string (shell-command-to-string
                            (concat "jq -r '.dependencies | keys | .[]' " (concat (projectile-project-root) "package.json"))) "\n" t))
            :action (or action 'my-js-import-file)))

(defun json-fix ()
  "Autofix json buffer"
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     (template "json-fix --no-sort --spaces <<tab-width>>") (current-buffer) t)))

(def-package! lsp-mode
  :config
  (setq lsp-eldoc-render-all nil
        lsp-eldoc-enable-hover nil
        lsp-eldoc-enable-signature-help nil
        lsp-eldoc-prefer-signature-help nil
        lsp-inhibit-message t
        lsp-highlight-symbol-at-point nil
        ;; Disable make error highlighting
        lsp-prefer-flymake nil))

(add-hook! 'doom-load-theme-hook
  (after! lsp
    (dolist (face '(lsp-face-highlight-read
                    lsp-face-highlight-write
                    lsp-face-highlight-textual))
      (set-face-attribute
       face nil
       :foreground nil :distant-foreground nil :background nil))))

(defun magit-worktree-branch-project-worktree (branch start-point &optional force)
  "Create a new BRANCH and check it out in a new worktree at PATH in a new workspace."
  (interactive
   `(,@(butlast (magit-branch-read-args "Create and checkout branch"))
     ,current-prefix-arg))
  (let* ((worktree-path (f-join (projectile-project-root) ".worktrees"))
         (path (f-join (projectile-project-root) ".worktrees" branch)))
    (when (not (f-exists-p worktree-path))
      (mkdir worktree-path t))
    (magit-run-git "worktree" "add" (if force "-B" "-b")
                   branch (expand-file-name path) start-point)
    (f-touch (f-join path ".projectile"))
    (+workspace-new branch)
    (+workspace-switch branch)
    (magit-diff-visit-directory path)
    (projectile-add-known-project path)
    path))

(defun magit-revision-show-original-file ()
  "Show the orginal file from a revision buffer
If possible also go to the pointing line"
  (interactive)
  (when magit-buffer-file-name
    (let ((file-name magit-buffer-file-name)
          (line-number (line-number-at-pos)))
      (delete-other-windows)
      (find-file file-name)
      (goto-line line-number))))

(defun shell-command-to-list (cmd)
  "Split output from shell-command to list"
  (split-string (shell-command-to-string cmd) "\n" t))

(defun git-new-files ()
  (shell-command-to-list "git ls-files -om --exclude-standard"))

(defun git-modified-files (&optional branch)
  (shell-command-to-list
   (template "git --no-pager diff --no-renames --name-only --no-merges <<(magit-rev-parse \"HEAD\")>> <<branch>>;")))

(defun git-get-changed-files (b)
    (delete-dups (append (git-modified-files b) (git-new-files))))

(defun +git|ivy-changed-files (&optional branch)
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read (template "Changed files for <<branch>>:")
              (git-get-changed-files (or branch "master"))
              :require-match t
              :history 'file-name-history
              :action counsel-projectile-find-file-action
              :caller '+git|ivy-changed-files)))

(defun +git|undo ()
  "Soft reset current git repo to HEAD~1."
  (interactive)
  (magit-reset-soft "HEAD~1"))

(defun +git|push-dated (&optional branch)
  "Pushes the given the current BRANCH with a dated prefix
my-branch-name -> 19-01-my-branch-name
When no BRANCH is given, take the current one."
  (interactive)
  (let* ((branch (or branch (magit-get-current-branch)))
         (date (format-time-string "%y-%m"))
         (remote (template "origin/<<date>>-<<branch>>")))
    (magit-git-push branch remote nil)))

(after! magit
  (transient-append-suffix 'magit-push "p" '("d" "dated" +git|push-dated)))

(defun browse-git-link ()
  "Browse the git link at the current point"
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link)))

(setq-default magit-save-repository-buffers 'dontask)

(after! magit
  :config
  (setq
   magithub-clone-default-directory "~/Code/Repositories"
   git-commit-summary-max-length 120))

(defun floscr:magit-jumpunfold-section (&optional forward)
  "Fold all section. Go to next section when FORWARD. Show all children"
  (interactive)
  (magit-section-show-level-1-all)
  (call-interactively (if forward #'magit-section-forward-sibling #'magit-section-backward-sibling))
  (call-interactively #'magit-section-show-children))

(map!
 (:after evil-magit
   :map (magit-diff-mode-map)
   :n "}" (λ! (floscr:magit-jumpunfold-section 't))
   :n "{" (λ! (floscr:magit-jumpunfold-section))))

(map!
 (:after evil-magit
   :map (magit-status-mode-map magit-revision-mode-map magit-diff-mode-map)
   :n "C-j" #'evil-window-down
   :n "C-k" #'evil-window-up
   :n "C-h" #'evil-window-left
   :n "C-l" #'evil-window-right))

(def-package! markdown-mode
  :init
  (setq markdown-fontify-code-blocks-natively t)
  :config
  (add-hook! markdown-mode
    (visual-line-mode)
    (visual-fill-column-mode)
    (outline-minor-mode)
    (setq visual-fill-column-width 90
          display-line-numbers nil)
    (setq line-spacing 2
          fill-column 80))

  (map! (:map markdown-mode-map
          :n "<"    #'markdown-promote
          :n ">"    #'markdown-demote)))

(defun my-nov-config ()
  (setq line-spacing 5)
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif" :height 1.4)
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width (+ nov-text-width 25))
  (visual-fill-column-mode t))

(def-package! nov
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 75)
  :config
  (progn
    (add-hook 'nov-mode-hook 'my-nov-config)))

(defun npm-mode-npm-ci ()
  "Run the 'npm install' command."
  (interactive)
  (npm-mode--exec-process "npm ci"))

(defun +org|source-properties-key-from-browser ()
  "Add the link from the frontmost chrome tab as a source property."
  (interactive)
  (org-set-property "SOURCE" (org-mac-chrome-get-frontmost-url)))

(defun +org|archive-and-done ()
  "Mark task as done and archive"
  (interactive)
  (org-todo "DONE")
  (org-archive-subtree))

(defun +org|agenda-archive-and-done ()
  "Mark agenda task as done and archive"
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-archive))

(defun +org|copy-block ()
  "Copies the current block to clipboard"
  (interactive)
  (org-edit-src-code)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun +org/copy-as-markdown (&optional subtree-p)
  "Copy the current subtree as markdown to clipboard"
    (let* ((org-export-with-toc nil)
           (md (org-md-export-as-markdown nil subtree-p)))
      (kill-ring-save (point-min) (point-max))
      (kill-buffer md)))

(defun +org|copy-buffer-as-markdown ()
  "Copy the entire buffer as markdown to clipboard."
  (interactive)
  (+org/copy-as-markdown))

(defun +org|copy-subtree-as-markdown ()
  "Copy the subtree as markdown to clipboard."
  (interactive)
  (+org/copy-as-markdown t))

(defun +org|grab-chrome-tabs ()
  "Grab all the chrome tabs as an org list to save for later inspection"
  (interactive)
  (let ((tabs
         (do-jxa-script
          (concat
           "Application(\"Chrome\").windows[0].tabs()"
           ".map(tab => `"
           "- [[${tab.url()}][${tab.title()}]]"
           "`)"
           ".join(\"\\n\")"))))
    (insert tabs)))

(defun +org-attach/downloads-file (file)
  "Attach a file in your downloads-directory"
  (interactive (list (read-file-name "Attach File: " downloads-dir)))
  (+org-attach/uri file))

(defun +org-attach/last-screenshot ()
  "Attaches the last screenshot"
  (interactive)
  (+org-attach/file (+file/latest-file-in-dir screenshot-dir)))

(defun +org|paste-chrome-link ()
  "Paste the frontmost chrome link"
  (interactive)
  (unless (looking-at-p "[\s\t\n\r]") (forward-char))
  (insert " ")
  (insert (org-mac-chrome-get-frontmost-url)))

(defun +org|paste-markdown-as-org ()
  "Convert the current clipboard to markdown"
  (interactive)
  (insert (shell-command-to-string "pbpaste | pandoc -f markdown -t org")))

(defun +org|schedule-tomorrow ()
  "Return scheduled string on tomorrow."
  (format-time-string "SCHEDULED: <%F %a>"
                      (time-add (current-time) (* 24 3600))))

(defun +org|sort-entries ()
  "Go to header and sort entries"
  (interactive)
  (org-up-element)
  (org-sort)
  (org-shifttab)
  (org-cycle))

(defun +org|visit-archive-file ()
  (interactive)
  (let ((archive-filename (car (org-archive--compute-location org-archive-location))))
    (find-file archive-filename)
    (end-of-buffer)))

(defun +org|align-all-tags ()
  "Interactive version of org-align-all-tags"
  (interactive)
  (org-align-all-tags))

(defun +github/just-pr-title (title)
  (car (s-split " · " title)))

(defun +org/mac-chrome-get-frontmost-url-custom-format ()
  "Adaption for org-as-mac-chrome-get-frontmost-url"
    (--> (org-as-mac-chrome-get-frontmost-url)

         (s-split "::split::" it)

         (pcase it
           ((pred (s-contains? "github.com" (-first-item it)))
            (-update-at (- (length it) 1)
                        #'+github/just-pr-title
                        it))
           (_ it))

         (s-join "::split::" it)

         (org-mac-paste-applescript-links it)))

(setq-default +org-created-property "DATE_CREATED")

(defun +org|compare-created-date-property (a b)
  "Compare two `org-mode' agenda entries, `A' and `B', by the \"CREATED\" property."
  (let* ((a-pos (get-text-property 0 'org-marker a))
         (b-pos (get-text-property 0 'org-marker b))
         (a-date (or (org-entry-get a-pos +org-created-property)
                     (format "<%s>" (org-read-date t nil "now"))))
         (b-date (or (org-entry-get b-pos +org-created-property)
                     (format "<%s>" (org-read-date t nil "now"))))
         (cmp (compare-strings a-date nil nil b-date nil nil)))
    (if (eq cmp t) nil (signum cmp))))

(after! org
  :config
  (setq org-todo-keywords
        '((sequence "ACTIVE(a)" "NEXT(n)" "|" "DONE")
          (sequence "TODO(t)" "|" "DONE(x)")
          (sequence "[ ]([)" "|" "[X](])")
          (sequence "PROJECT(p)" "|" "DONE")
          (sequence "NOTE(N)")
          (sequence "WAITING(w)" "LATER(l)" "SOMEDAY(s)" "|" "DONE" "CANCELLED(c)"))
   org-todo-keyword-faces
   '(("ACTIVE" :inherit warning :weight bold)
     ("NEXT" :inherit warning :weight bold)
     ("NOTE" :inherit warning :weight bold)
     ("WAITING" :inherit default :weight bold)
     ("SOMEDAY" :inherit default :weight bold)
     ("LATER" :inherit default :weight bold)
     ("PROJECT" :inherit 'org-todo :weight bold))))

(after! projectile
  (add-to-list 'projectile-globally-ignored-file-suffixes ".org_archive")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".DS_Store"))

(setq
 org-directory (expand-file-name "~/Dropbox/org")
 org-pinboard-dir org-directory
 org-pinboard-file (concat org-pinboard-dir "/Bookmarks/bookmarks.org")
 org-pinboard-archive-file (concat org-pinboard-dir "/Bookmarks/.archive/pinboard.org")
 org-default-notes-file (concat org-directory "/inbox.org")
 org-shopping-list-file (concat org-directory "/shoppinglist.org")
 +org-reading-list-file (concat org-directory "/reading-list.org"))

(after! org
  :config
  (setq
   org-tags-column (- fill-column)
   org-image-actual-width 600
   org-default-notes-file (concat org-directory "/inbox.org")))

(setq org-latex-create-formula-image-program 'dvisvgm)

(defun expand-org-file-names (xs)
  (mapcar (λ (x) (expand-file-name x org-directory)) xs))

(setq level-1-refile-targets (expand-org-file-names '("reading-list.org"
                                                      "cooking.org"
                                                      ;; "books.org"
                                                      "programming.org"
                                                      "shoppinglist.org")))

(setq max-level-2-refile-targets (expand-org-file-names '("Emacs.org"
                                                          "art.org"
                                                          "diary"
                                                          "games.org"
                                                          "hardware.org"
                                                          "home.org"
                                                          "inbox.org"
                                                          "mealplan.org"
                                                          "misc.org"
                                                          "movies.org"
                                                          "music.org"
                                                          "osx.org"
                                                          "personal.org"
                                                          "podcasts.org"
                                                          "projects.org"
                                                          "sleep.org"
                                                          "sports.org"
                                                          "travel.org"
                                                          "Work/work.org")))

(defun level-1-refile-targets () level-1-refile-targets)

(defun max-level-2-refile-targets () max-level-2-refile-targets)

(after! org
  :config
  (setq org-refile-targets (quote ((nil :maxlevel . 5)
                                   (max-level-2-refile-targets :maxlevel . 2)
                                   (level-1-refile-targets :level . 1)))
        org-agenda-refile org-agenda-files))

(setq +org-capture-frame-parameters
  `((name . "org-capture")
    (width . 120)
    (height . 35)
    (transient . t)))

(defun +org|add-created-date-property ()
  "Add DATE_CAPTURED property to the current item."
  (interactive)
  (org-set-property +org-created-property (format-time-string  "[%Y-%m-%d %a %H:%M]")))

(add-hook 'org-capture-before-finalize-hook '+org|add-created-date-property)

(defun org-capture-bookmark-pair ()
  (split-string (org-as-mac-chrome-get-frontmost-url) "::split::"))

(defun org-capture-bookmark-string-url ()
  (car (org-capture-bookmark-pair)))

(defun org-capture-bookmark-string-description ()
  (cadr (org-capture-bookmark-pair)))

(after! org
  :config
  (setq org-capture-templates
        `(("t" "todo" entry
           (file org-default-notes-file)
           "* TODO %?")

          ("c" "Calendar Event" entry
           (file+headline ,(concat org-directory "/home.org") "Calendar")
           "* %?")

          ("e" "Emacs Todo" entry
           (file+headline ,(concat org-directory "/emacs.org") "Emacs Todos")
           "* TODO %?")

          ("p" "Pin Bookmark" entry (file+headline org-pinboard-file "Pinboard")
           "* %(org-capture-bookmark-string-description)%?\n:PROPERTIES:\n:URL:  %(org-capture-bookmark-string-url)\n:TIME: %U\n:END:")

          ("rr" "Add to reading list" entry (file+headline +org-reading-list-file "Reading List")
           "* TODO %(org-mac-chrome-get-frontmost-url)%?")

          ("rw" "Add to watching list" entry (file+headline +org-reading-list-file "Watching List")
           "* TODO %(org-mac-chrome-get-frontmost-url)%?")

          ("C" "Browser" entry
           (file org-default-notes-file)
           "* TODO %(org-mac-chrome-get-frontmost-url)%?")

          ("ww" "Work Task" entry
           (file+headline ,(concat org-directory "/Work/work.org") "Work Todos")
           "* TODO %?")

          ("wr" "Work Review" entry
           (file+headline ,(concat org-directory "/Work/work.org") "Work Todos")
           "* TODO %(+org/mac-chrome-get-frontmost-url-custom-format)%? :REVIEW: "))))

(map! :leader (:desc "Notes" :prefix "n"
                :desc "Pinboard File"             "B"  (λ! (find-file org-pinboard-file))
                :desc "Save All Org Buffers"      "S"  #'org-save-all-org-buffers
                :desc "Agenda"                    "a"  #'org-agenda
                :desc "Search Pinboard"           "b"  #'helm-org-pinboard
                (:prefix-map ("c" . "clock")
                  :desc "Clock In"                "c"  #'org-clock-in
                  :desc "Clock Out"               "C"  #'org-clock-out
                  :desc "Mark Default Task"       "d"  #'org-clock-mark-default-task
                  :desc "Modify Effort Estimate"  "e"  #'org-clock-modify-effort-estimate
                  :desc "Clock In Last"           "l"  #'org-clock-in-last
                  :desc "Goto Current"            "g"  #'org-clock-goto
                  :desc "Goto Select"             "G"  (λ! (org-clock-goto 'select))
                  :desc "Cancel"                  "x"  #'org-clock-cancel
                  :desc "Timestamp Up"            "="  #'org-clock-timestamps-up
                  :desc "Timestamp Down"          "-"  #'org-clock-timestamps-down)
                :desc "Emacs"                     "e"  (λ! (find-file (concat org-directory "/Emacs.org")))
                :desc "Home"                      "h"  (λ! (find-file (concat org-directory "/home.org")))
                :desc "Inbox"                     "i"  (λ! (find-file (concat org-directory "/inbox.org")))
                :desc "Reading List"              "r"  #'+org-reading-list/org-open-reading-list-file
                :desc "Work"                      "w"  (λ! (find-file (concat org-directory "/Work/work.org")))
                :desc "Store Link"                "y"  #'org-store-link))

(after! org
  (map! :map evil-org-mode-map
        :n "s-j" #'org-move-subtree-down
        :n "s-k" #'org-move-subtree-up

        :localleader
        :desc "Archive Subtree"          :m "a" #'org-archive-subtree
        :desc "Archive Subtree and Done" :m "A" #'+org|archive-and-done
        :desc "Paste Chrome Link"        :m "p" #'+org|paste-chrome-link
        :desc "Grab tabs"                :m "P" #'+org|grab-chrome-tabs
        :desc "Cut Subtree"              :m "C" #'org-cut-subtree
        :desc "Paste Subtree"            :m "P" #'org-paste-subtree
        :desc "Sort Entries"             :m "S" #'+org|sort-entries

        :desc "Create/Edit Todo"  :nve "o" #'org-todo
        :desc "Schedule"          :nve "s" #'org-schedule
        :desc "Deadline"          :nve "d" #'org-deadline
        :desc "Refile"            :nve "r" #'org-refile
        :desc "Filter"            :nve "f" #'org-match-sparse-tree
        :desc "Tag heading"       :nve "t" #'org-set-tags-command

        (:desc "Attach" :prefix "F"
          :desc "Downloads File" :m "d" '+org-attach/downloads-file
          :desc "Screenshot" :m "s" '+org-attach/last-screenshot
          :desc "URI" :m "u" '+org-attach/uri
          :desc "File" :m "f" '+org-attach/file)

        (:desc "Insert" :prefix "i"
          :desc "Subheadeing" :m "s" (λ!
                                      (call-interactively 'org-insert-subheading)
                                      (evil-insert-state))
          :desc "Inavtive Timestamp" :m "i" 'org-time-stamp-inactive)
        (:desc "Narrow" :prefix "n"
          :desc "Indirect Buffer Tree" :m "i" #'org-tree-to-indirect-buffer
          :desc "Subtree"              :m "s" #'org-narrow-to-subtree
          :desc "Block"                :m "b" #'org-narrow-to-block
          :desc "Element"              :m "e" #'org-narrow-to-element
          :desc "widen"                :m "w" #'widen)))

(after! org-agenda
  (setq-default
   org-agenda-cmp-user-defined #'+org|compare-created-date-property
   org-agenda-sorting-strategy '((agenda habit-down user-defined-up time-up priority-down category-keep)
                                 (todo priority-down category-keep user-defined-up time-up)
                                 (tags priority-down category-keep user-defined-up time-up)
                                 (search category-keep))))

(evil-define-key 'motion org-agenda-mode-map
  "vd" 'org-agenda-day-view
  "ds" 'org-agenda-schedule
  "vw" 'org-agenda-week-view
  "vm" 'org-agenda-month-view
  "vy" 'org-agenda-year-view)

(after! org-agenda

(add-to-list 'org-agenda-custom-commands
             '("d" "Today" ((agenda "a"
                                    ((org-agenda-prefix-format "  %?-12t% s")
                                     (org-agenda-start-on-weekday nil)
                                     (org-agenda-span 1)
                                     (org-agenda-files (--map (concat org-directory "/" it) '("inbox.org" "home.org" "Work/work.org")))
                                     (org-agenda-start-day ".")
                                     (org-agenda-skip-scheduled-if-done t)
                                     (org-agenda-sorting-strategy '(timestamp-up time-up))
                                     (org-agenda-day-view)
                                     (org-super-agenda-groups '((:name "Today" :date today :time-grid t)
                                                                (:name "Overdue" :deadline past :scheduled past)
                                                                (:name "Future" :anything (:scheduled future)))))))))

(add-to-list 'org-agenda-custom-commands
             '("x" "Todo Items" alltodo ""
               ((org-agenda-prefix-format "  %?-12t% s")
                (org-agenda-sorting-strategy '(timestamp-down todo-state-down))
                (org-agenda-files (--map (concat org-directory "/" it) '("inbox.org" "home.org")))
                (org-super-agenda-groups '((:name "Next" :todo ("ACTIVE"))
                                           (:name "Scheduled" :scheduled t)
                                           (:name "Inbox" :file-path ".*inbox.org$" :order 2)
                                           (:name "Unscheduled" :and (:todo "TODO" :scheduled nil :not (:tag "BACKLOG")) :order 1)
                                           (:name "Backlog" :tag "BACKLOG" :order 3))))))

(add-to-list 'org-agenda-custom-commands
             '("e" "Emacs Items" alltodo ""
               ((org-agenda-prefix-format "  %?-12t% s")
                (org-agenda-sorting-strategy '(timestamp-down todo-state-down))
                (org-agenda-files (--map (concat org-directory "/" it) '("Emacs.org")))
                (org-super-agenda-groups '((:name "Active" :todo "ACTIVE")
                                           (:name "Next" :todo "NEXT")
                                           (:name "Low Effort" :effort< "0:30")
                                           (:name "Todo" :todo "TODO"))))))

(add-to-list 'org-agenda-custom-commands
             '("w" "Work Agenda"
               ((agenda "a" ((org-agenda-span 1)
                             (org-agenda-use-time-grid 'require-timed)
                             (org-agenda-start-day ".")))
                (tags-todo "+WORK-EVENT"
                   ((org-agenda-sorting-strategy '(timestamp-down time-down))
                    (org-super-agenda-groups '((:name "In Progress" :todo "ACTIVE")
                                               (:name "Coming Up Today" :scheduled today)
                                               (:name "Meta Work" :tag "META_WORK" :order 3)
                                               (:name "Reviews" :tag "REVIEW" :order 5)
                                               (:name "Tasks" :not (:todo "SOMEDAY" :todo "WAITING" :tag ("BACKLOG" "TEXT" "EMACS")) :order 4)
                                               (:name "Waiting" :todo "WAITING" :order 6)
                                               (:name "Backlog" :tag "BACKLOG" :order 11)
                                               (:name "Reading List" :tag "TEXT" :order 10))))))
               ((org-agenda-hide-tags-regexp "WORK\\|BACKLOG")
                (org-agenda-tag-filter-preset '("+WORK"))
                (org-agenda-files (--map (concat org-directory "/" it) '("Work/work.org" "inbox.org"))))))

(add-to-list 'org-agenda-custom-commands
             '("c" "Calendar" agenda ""
               ((org-agenda-span 7)
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day "-1d")
                (org-agenda-tag-filter-preset '("+CALENDAR")))))

:config
(setq org-agenda-files (list org-directory (concat org-directory "/Work"))))

(after! org

(defun +org/org-clock-in-if-starting ()
  "Clock in when the task is marked ACTIVE."
  (when (and (string= org-state "ACTIVE")
             (not (string= org-last-state org-state)))
    (org-clock-in)))

(add-hook 'org-after-todo-state-change-hook '+org/org-clock-in-if-starting)

(defun +org/org-clock-out-if-waiting ()
  "Clock out when the task is marked WAITING."
  (when (and (-contains? '("WAITING" "SOMEDAY" "CANCELLED") org-state)
             (equal (marker-buffer org-clock-marker) (current-buffer))
             (< (point) org-clock-marker)
             (> (save-excursion (outline-next-heading) (point))
               org-clock-marker)
             (not (string= org-last-state org-state)))
    (org-clock-out)))

(add-hook 'org-after-todo-state-change-hook '+org/org-clock-out-if-waiting)

(defun +org/org-set-active-state (&optional args)
  "Set the active state for the current item."
  (org-todo "ACTIVE"))

(advice-add #'org-clock-in :after #'+org/org-set-active-state)

)

(def-package! org-ql
  :commands (org-ql-search))

(def-package! org-super-agenda
  :after org
  :config
  (org-super-agenda-mode 1)
  ;; Disable org-super-agenda keymap which breaks evil mappings
  (setq org-super-agenda-header-map (make-sparse-keymap)))

;; Enable Retina pdfs
(setq pdf-view-use-scaling t)

;; Fix midnight colors for doom-one theme
(setq pdf-view-midnight-colors '("#BBC2CD" . "#282C34"))

(def-package! rainbow-mode
  :commands (rainbow-mode))

(after! smerge-mode
  :config
  ;; TODO This is broken after switching the theme but works for now
  ;; This fixes the smerge diff color is really bright an ugly
  (set-face-attribute 'smerge-refined-added nil :foreground nil :background nil))

(use-package smerge-mode
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (flycheck-mode -1)
                                     (unpackaged/smerge-hydra/body)))))

(map!
 (:after treemacs-evil
   (:map evil-treemacs-state-map
     "C-h" #'evil-window-left
     "C-l" #'evil-window-right)))

(defun treemacs-is-file-happypack? (f _)
  (string= f ".happypack"))

(after! treemacs
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-is-file-happypack?))

(setq +workspaces-on-switch-project-behavior t)

(after! persp-mode
  (setq persp-add-buffer-on-find-file nil
        persp-add-buffer-on-after-change-major-mode nil)
  (defun +workspaces|add-current-buffer ()
    (persp-add-buffer (current-buffer) (get-current-persp)))
  (add-hook 'doom-switch-buffer-hook #'+workspaces|add-current-buffer))

(defun +workspace/my-switch-to ()
  (interactive)
  (ivy-read "Switch to workspace: "
            (+workspace-list-names)
            :action '(1
                      ("RET" +workspace/switch-to "Switch to workspace")
                      ("C-<backspace>" (lambda (x)
                                         (let* ((current-workspace-name (+workspace-current-name))
                                                (new-workspace-name
                                                 (or (--first (string= current-workspace-name it) (+workspace-list-names)) "main")))
                                           (+workspace/delete x)
                                           (+workspace-switch new-workspace-name)
                                           (+workspace/my-switch-to)))
                       "Continous delete workspace"))))

(defun +workspace/switch-to-last-visited ()
  "Switch to the last visited workspace."
  (interactive)
  (+workspace/switch-to +workspace--last))

(defun +workspace/workspace-project-root (&optional arg)
  "Gets the root dir for the current workspace"
  (--find (s-match (concat (+workspace-current-name) "/$") it) projectile-known-projects))

(defun +workspace|find-workspace-project-file ()
"Projectile find file for the project named after the current workspace."
  (interactive)
  (cl-letf (((symbol-function 'projectile-project-root) #'+workspace/workspace-project-root))
      (projectile-find-file)))

(defun +workspace/new-named ()
  "Create a new named workspace."
  (interactive)
  (let ((name (read-string "New workspace name: ")))
    (if name (+workspace/new name))))

(defun yasnippet/expand-first-item ()
  (interactive)
  (call-interactively #'company-yasnippet)
  (company-complete-selection))

(setq +lookup-provider-url-alist
  '(("DuckDuckGo"        . "https://duckduckgo.com/?q=%s")
    ("DuckDuckGo Lucky"  . "https://duckduckgo.com/?q=\\%s")
    ("Github Code"       . "https://github.com/search?search&q=%s&type=Code")
    ("Google"            . "https://google.com/search?q=%s")
    ("Google images"     . "https://google.com/images?q=%s")
    ("Google maps"       . "https://maps.google.com/maps?q=%s")
    ("NPM"               . "https://npmjs.com/search?q=%s")
    ("Hoogle"            . "https://www.haskell.org/hoogle/?hoogle=%s")
    ("Project Gutenberg" . "http://www.gutenberg.org/ebooks/search/?query=%s")
    ("DevDocs.io"        . "https://devdocs.io/#q=%s")
    ("Explain Shell"     . "https://explainshell.com/explain?cmd=%s")
    ("StackOverflow"     . "https://stackoverflow.com/search?q=%s")
    ("Github"            . "https://github.com/search?ref=simplesearch&q=%s")
    ("Youtube"           . "https://youtube.com/results?aq=f&oq=&search_query=%s")
    ("Wolfram alpha"     . "https://wolframalpha.com/input/?i=%s")
    ("Wikipedia"         . "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")))

(setq bookmark-default-file "~/Dropbox/Temp/bookmarks")

(add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))

(setq evil-want-fine-undo t)

(setq
 trash-directory "~/.Trash/"
 delete-by-moving-to-trash t)

;; auto-mode-alist
(add-to-list 'auto-mode-alist '("Brewfile" . shell-script-mode))

;; Set the default multi-term to zsh
(setq multi-term-program "/bin/zsh")

(savehist-mode 1)

(setq tags-revert-without-query 1)

(setq-hook! 'prog-mode-hook show-trailing-whitespace nil)

(map! :en "C-±" #'+popup/raise)

(map!
 (:map override
   :gniv "s-w" #'+workspace/close-window-or-workspace
   :gniv "s-;" #'eval-expression
   :gniv "s-S" #'write-file
   :gniv "s-x" #'execute-extended-command

   ;; Workspace Switching
   :gniv "s-1" (λ! (+workspace/switch-to 0))
   :gniv "s-2" (λ! (+workspace/switch-to 1))
   :gniv "s-3" (λ! (+workspace/switch-to 2))
   :gniv "s-4" (λ! (+workspace/switch-to 3))
   :gniv "s-5" (λ! (+workspace/switch-to 4))
   :gniv "s-6" (λ! (+workspace/switch-to 5))
   :gniv "s-7" (λ! (+workspace/switch-to 6))
   :gniv "s-8" (λ! (+workspace/switch-to 7))
   :gniv "s-9" (λ! (+workspace/switch-to 9))

   ;; Text scale
   :gniv "s-="   #'doom/increase-font-size
   :gniv "s--"   #'doom/decrease-font-size
   :gniv "s-0"   #'doom/reset-font-size))

(map!
 :en "C-h"   #'evil-window-left
 :en "C-j"   #'evil-window-down
 :en "C-k"   #'evil-window-up
 :en "C-l"   #'evil-window-right)

(def-package! evil-replace-with-register
  :config
  (setq evil-replace-with-register-key (kbd "gr"))
  (define-key evil-normal-state-map
    evil-replace-with-register-key 'evil-replace-with-register)
  (define-key evil-visual-state-map
    evil-replace-with-register-key 'evil-replace-with-register))

(after! evil
  (require 'evil-textobj-anyblock)
  (evil-define-text-object my-evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my-evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "q" 'my-evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q" 'my-evil-textobj-anyblock-a-quote)
  (define-key evil-inner-text-objects-map "r" 'evil-inner-bracket))

(after! evil
  (map! :m  "-"  #'dired-jump))

(define-key minibuffer-local-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-map "\C-n" 'next-history-element)

(map! :n "gb" #'evil-switch-to-windows-last-buffer)

(map!
 :n "]F" #'dumb-jump-go
 :n "[F" #'dumb-jump-back)

(map!
 :n "]e" #'flycheck-next-error
 :n "[e" #'flycheck-previous-error)

(map! :i "A-y" #'helm-show-kill-ring)

(map!
 :i "M-;"   (λ! (insert "ö"))
 :i "M-:"   (λ! (insert "Ö"))
 :i "M-'"   (λ! (insert "ä"))
 :i "M-\""  (λ! (insert "Ä"))
 :i "M-["   (λ! (insert "ü"))
 :i "M-{"   (λ! (insert "Ü"))
 :i "M-s"   (λ! (insert "ß"))
 :i "M-e"   (λ! (insert "€"))
 :i "M-`"   (λ! (insert "°"))
 :i "M-."   (λ! (insert "…"))
 :i "M-^"   (λ! (insert "°"))
 :i "M-l"   (λ! (insert "λ"))
 :i "M-w"   (λ! (insert "⚠"))
 :i "M-i"   (λ! (insert "ℹ")))

(map!
 :leader
 :n "'"  #'+popup/toggle
 :n "au" #'undo-tree-visualize
 :n "//" #'+default/search-project
 :n "-"  #'quick-calc

 (:desc "Toggle last iBuffer" :n "=" #'+popup/toggle))

(map!
 :leader
 (:desc "buffer" :prefix "b"
   :desc "Rename Buffer" :n "r" #'rename-buffer))

(map!
 :leader
 (:desc "file" :prefix "f"
   :desc "find in literate config file" :n  "D" (λ! (counsel-find-file (projectile-project-root)))
   :desc "find in literate config file" :n  "p" #'counsel-org-doom-config))

(map!
 :leader
 (:desc "dir" :prefix "d"
   :desc "find in literate config file" :n  "r" #'dired-recent-open
   :desc "Project Root"                 :n  "p" #'projectile-dired))

(map!
 :leader
 (:desc "git" :prefix "g"
   :desc "Worktree Popup"              :n "%" #'magit-worktree
   :desc "Blame"                       :n "B" #'magit-blame
   :desc "Changed Files"               :n "F" #'+git|ivy-changed-files
   :desc "New Branch"                  :n "N" #'magit-branch-spinoff
   :desc "Show revision original File" :n "O" #'magit-revision-show-original-file
   :desc "Map-editor Changed Files"    :n "T" (λ! (+git|ivy-changed-files "map-editor"))
   :desc "Amend Commit"                :n "a" #'magit-commit-amend
   :desc "Checkout"                    :n "b" #'magit-checkout
   :desc "Diff"                        :n "d" #'magit-diff
   :desc "Push"                        :n "p" #'magit-push
   :desc "Undo"                        :n "u" #'+git|undo))

(map!
 :leader
 (:desc "insert" :prefix "i"
   :desc "Killring"   :n  "y" #'counsel-yank-pop))

(map!
 :leader
 (:desc "project" :prefix "p"
   :desc "services" :n  "s" #'prodigy
   :desc "Workspace Project Files" :n  "P" #'+workspace|find-workspace-project-file))

(map!
 :leader
 (:desc "toggle" :prefix "t"
   :desc "Theme Dark/Light" :n  "t" #'+doom|toggle-theme))

(map!
 :leader
 (:desc "code" :prefix "c"
   :desc "Compile" "c" #'compile
   :desc "Compile" "Compile last command" (λ! (compile (car compile-history)))))

(map!
 :leader
 (:desc "open" :prefix "o"
   :desc "Eshell in Current Dir" :n  "." (λ! (+eshell/open t))
   :desc "Eshell Popup in Current Dir" :n  ">" (λ! (+eshell/open t))))

(map!
 :leader
 (:desc "window" :prefix "w"
  :desc  "Split Vertical"   :n  "|"    #'evil-window-vsplit
  :desc  "Split Horizontal" :n  "_"    #'evil-window-split
  :desc  "Split Horizontal" :n  "_"    #'evil-window-split
  :desc  "Set Height"       :n  "C-_"    #'evil-window-set-height
  :desc  "Set Height"       :n  "C-|"    #'evil-window-set-width
  :desc  "Swap"             :n  "SPC"  #'ace-swap-window
  :desc "Toggle Locked" :n "#" #'toggle-window-dedicated))

(map!
 :leader
 (:desc "workspace" :prefix "<tab>"
   :desc "Switch to"    :n "." #'+workspace/my-switch-to
   :desc "Create"       :n "c" #'+workspace/new-named
   :desc "Rename"       :n "," #'+workspace/rename
   :desc "Last visited" :n "0" #'+workspace/switch-to-last-visited
   :desc "Clone"        :n "C" (λ!
                                (+workspace/new (format "Clone: %s" (+workspace-current-name)) t)
                                (message "Cloned current workspace %s" (+workspace-current-name)))

   :desc "Display tab bar"          "TAB" #'+workspace/display
   :desc "New workspace"            "n"   #'+workspace/new
   :desc "Load workspace from file" "l"   #'+workspace/load
   :desc "Save workspace to file"   "s"   #'+workspace/save
   :desc "Switch workspace"         "."   #'+workspace/switch-to
   :desc "Delete session"           "x"   #'+workspace/kill-session
   :desc "Delete this workspace"    "d"   #'+workspace/delete
   :desc "Rename workspace"         "r"   #'+workspace/rename
   :desc "Restore last session"     "R"   #'+workspace/restore-last-session
   :desc "Next workspace"           "]"   #'+workspace/switch-right
   :desc "Previous workspace"       "["   #'+workspace/switch-left
   :desc "Switch to 1st workspace"  "1"   (λ! (+workspace/switch-to 0))
   :desc "Switch to 2nd workspace"  "2"   (λ! (+workspace/switch-to 1))
   :desc "Switch to 3rd workspace"  "3"   (λ! (+workspace/switch-to 2))
   :desc "Switch to 4th workspace"  "4"   (λ! (+workspace/switch-to 3))
   :desc "Switch to 5th workspace"  "5"   (λ! (+workspace/switch-to 4))
   :desc "Switch to 6th workspace"  "6"   (λ! (+workspace/switch-to 5))
   :desc "Switch to 7th workspace"  "7"   (λ! (+workspace/switch-to 6))
   :desc "Switch to 8th workspace"  "8"   (λ! (+workspace/switch-to 7))
   :desc "Switch to 9th workspace"  "9"   (λ! (+workspace/switch-to 8))
   :desc "Switch to last workspace" "0"   #'+workspace/switch-to-last))

(map!
  :leader
  (:desc "Yank" :prefix "y"
    :desc "filename"  :n  "f" (λ! (copy-message (file-name-nondirectory buffer-file-name)))
    :desc "base"      :n  "b" (λ! (copy-message (file-name-base (buffer-file-name))))
    :desc "directory" :n  "d" (λ! (copy-message (file-name-directory (buffer-file-name))))
    :desc "path"      :n  "p" (λ! (copy-message (file-name-directory (buffer-file-name))))
    :desc "project"   :n  "r" (λ! (copy-message (s-replace (projectile-project-root) "" (buffer-file-name))))))

(defun load-evil-camel-case-motion ()
  (require 'evil-little-word)
  (define-key evil-normal-state-map (kbd "M-w") 'evil-forward-little-word-begin)
  (define-key evil-normal-state-map (kbd "M-b") 'evil-backward-little-word-begin)
  (define-key evil-operator-state-map (kbd "M-w") 'evil-forward-little-word-begin)
  (define-key evil-operator-state-map (kbd "M-b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "M-w") 'evil-forward-little-word-begin)
  (define-key evil-visual-state-map (kbd "M-b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "i M-w") 'evil-inner-little-word))

(after! rjsx-mode
  (load-evil-camel-case-motion))

(after! reason-mode
  (load-evil-camel-case-motion))

(after! js2-mode
  (load-evil-camel-case-motion))

(map! :map org-mode-map
      :localleader
      :desc  "Set source key to tab"    "k"  #'+org|source-properties-key-from-browser
      :desc  "Copy Buffer To Markdown"  "y"  #'+org|copy-buffer-as-markdown
      :desc  "Align Tags"               "%"  #'+org|align-all-tags
      :desc  "Goto Archive"             "$"  #'+org|visit-archive-file
      :desc  "Schedule Tomorrow"        "+"  #'+org|schedule-tomorrow
      (:prefix ("g" . "goto")
        :desc "Org Web Link" "l" #'+org-web-tools/read-url-at-point))

(map! :niv "s-X" #'+org-capture/open-frame)

(map! :map org-mode-map
        :gni [s-return]   #'+org/insert-item-below
        :gni [s-S-return] #'+org/insert-item-above)

(map! :map outline-mode-map
      :n "s-k" #'org-metaup
      :n "s-j" #'org-metadown)

(defun my-evil-org-agenda-set-keys ()
  (evil-define-key 'motion org-agenda-mode-map
    "da" 'org-agenda-archive
    "dA" '+org|agenda-archive-and-done))

(advice-add #'evil-org-agenda-set-keys :after #'my-evil-org-agenda-set-keys)

(map! :map org-agenda-mode-map
      :desc "Org Window Left" "C-h" #'evil-window-left
      :desc "Org Window Left" "C-l" #'evil-window-right)

(defun floscr|+eshell|init-keymap ()
  "Setup additional custom eshell keybindings to already existing doom bindings. This must be done in a hook because eshell-mode
redefines its keys every time `eshell-mode' is enabled."
  (map! :map eshell-mode-map
        :localleader "l" #'eshell/clear))
(add-hook 'eshell-first-time-mode-hook #'floscr|+eshell|init-keymap)

(map! :map emacs-lisp-mode-map
      ;; Rearrange Sexps
      :n "s-k"   (λ! (sp-transpose-sexp)
                     (evil-previous-line))
      :n "s-j"   (λ! (sp-push-hybrid-sexp)
                     (evil-next-line))

      ;; Eval Buffer
      :n "s-r" #'eval-buffer

      ;; Slurp and barf
      :n "g]"   #'sp-slurp-hybrid-sexp
      :n "g["   #'sp-forward-barf-sexp
      :localleader
      :desc  "Raise sexp" "<" #'raise-sexp
      :desc  "Barf Sexp" ">" #'barf-sexp)

(defun evil-get-register-string (REGISTER)
  "Get evil-register pure text content
Registers can be selected with ?letter
E.g.: ?* -> Clipboard Contents"
  (evil-vector-to-string (evil-get-register REGISTER)))

(defun paste-evil-register-clipboard-pruned ()
  "Paste the current clipboard pruned from newlines"
  (interactive)
  (insert (s-trim (shell-command-to-string "pbpaste")))
  (doom/forward-to-last-non-comment-or-eol))

(defun copy-minibuffer-line ()
  "Copies the minibuffer content to the clipboard"
  (interactive)
  (save-excursion
    (doom/forward-to-last-non-comment-or-eol)
    (set-mark-command nil)
    (doom/backward-to-bol-or-indent)
    (kill-ring-save (mark) (point))))

(defun setup-minibuffer ()
  "Set up keybindings for the minibuffer"
  (local-set-key (kbd "s-v") 'paste-evil-register-clipboard-pruned)
  (local-set-key (kbd "s-c") 'copy-minibuffer-line))

(add-hook 'minibuffer-setup-hook 'setup-minibuffer)
