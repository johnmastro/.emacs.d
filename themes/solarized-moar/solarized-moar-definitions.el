;; solarized-moar-definitions.el  -*- lexical-binding:t -*-

(require 'solarized-definitions)

(defun solarized-moar-find-color (name)
  (let ((row (cdr (assoc name solarized-colors)))
        (idx (if (or solarized-degrade (not (display-graphic-p)))
                 (let ((cells (display-color-cells)))
                   (cond ((>= cells 256) 2)
                         ((>= cells 16) 3)
                         (t 4)))
               (if solarized-broken-srgb
                   1
                 0))))
    (nth idx row)))

(defun solarized-moar-color-definitions ()
  (let ((bold        (if solarized-bold 'bold 'normal))
        (bright-bold (if solarized-bold 'normal 'bold))
        (underline   (if solarized-underline t nil))
        (opt-under   nil)
        (italic      (if solarized-italic 'italic 'normal)))
    (let ((bg-back   '(:background back))
          (bg-base03 '(:background base03))
          (bg-base02 '(:background base02))
          (bg-base01 '(:background base01))
          (bg-base00 '(:background base00))
          (bg-base0 '(:background base0))
          (bg-base1 '(:background base1))
          (bg-base2 '(:background base2))
          (bg-base3 '(:background base3))
          (bg-green '(:background green))
          (bg-yellow '(:background yellow))
          (bg-orange '(:background orange))
          (bg-red '(:background red))
          (bg-magenta '(:background magenta))
          (bg-violet '(:background violet))
          (bg-blue '(:background blue))
          (bg-cyan '(:background cyan))

          (fg-base03 '(:foreground base03))
          (fg-base02 '(:foreground base02))
          (fg-base01 '(:foreground base01))
          (fg-base00 '(:foreground base00))
          (fg-base0 '(:foreground base0))
          (fg-base1 '(:foreground base1))
          (fg-base2 '(:foreground base2))
          (fg-base3 '(:foreground base3))
          (fg-green '(:foreground green))
          (fg-yellow '(:foreground yellow))
          (fg-orange '(:foreground orange))
          (fg-red '(:foreground red))
          (fg-magenta '(:foreground magenta))
          (fg-violet '(:foreground violet))
          (fg-blue '(:foreground blue))
          (fg-cyan '(:foreground cyan))

          (fmt-none `(:weight normal :slant normal  :underline nil        :inverse-video nil))
          (fmt-bold `(:weight ,bold  :slant normal  :underline nil        :inverse-video nil))
          (fmt-bldi `(:weight ,bold                 :underline nil        :inverse-video nil))
          (fmt-undr `(:weight normal :slant normal  :underline ,underline :inverse-video nil))
          (fmt-undb `(:weight ,bold  :slant normal  :underline ,underline :inverse-video nil))
          (fmt-undi `(:weight normal                :underline ,underline :inverse-video nil))
          (fmt-uopt `(:weight normal :slant normal  :underline ,opt-under :inverse-video nil))
          ;; FIXME: don’t hardcode the SRGB color names
          (fmt-curl-red    `(:weight normal :slant normal :underline (:color "#dc322f" :style wave) :inverse-video nil))
          (fmt-curl-yellow `(:weight normal :slant normal :underline (:color "#b58900" :style wave) :inverse-video nil))
          (fmt-ital `(:weight normal :slant ,italic :underline nil        :inverse-video nil))
          ;; FIXME: not quite the same
          (fmt-stnd `(:weight normal :slant normal  :underline nil        :inverse-video t))
          (fmt-revr `(:weight normal :slant normal  :underline nil        :inverse-video t))
          (fmt-revb `(:weight ,bold  :slant normal  :underline nil        :inverse-video t))
          (fmt-revbb `(:weight ,bright-bold :slant normal :underline nil  :inverse-video t))
          (fmt-revbbu `(:weight ,bright-bold :slant normal :underline ,underline :inverse-video t)))
      (eval-after-load 'ansi-color
        '(setf ansi-color-names-vector [,base02 ,red ,green ,yellow ,blue ,magenta ,cyan ,base00]))
      (mapcar (lambda (face) (apply 'create-face-spec face))
              `(
                ;; guide-key
                (guide-key/key-face (,@fg-blue))
                ;; helm
                (helm-source-header (,@bg-blue ,@fg-base03 ,@fmt-none))
                (helm-visible-mark (,@bg-green ,@fg-base3))
                (helm-selection (:inherit highlight))
                (helm-selection-line (,@fg-base02 ,@bg-yellow))
                (helm-candidate-number (:inherit mode-line))
                (helm-match (:inherit match))
                ;; helm-M-x
                (helm-M-x-key (,@fg-yellow ,@fmt-undr))
                ;; helm-files
                (helm-ff-prefix (,@bg-yellow ,@fg-base03))
                (helm-ff-file (,@fg-base0))
                (helm-ff-executable (,@fg-green))
                (helm-ff-directory (,@fg-blue))
                (helm-ff-dotted-directory (,@fg-blue))
                (helm-ff-symlink (,@fg-cyan))
                (helm-ff-invalid-symlink (,@bg-base03 ,@fg-orange))
                (helm-history-deleted (,@fg-magenta))
                (helm-history-remote (,@fg-violet))
                ;; helm-locate
                (helm-locate-finish (:inherit mode-line))
                ;; helm-buffers
                (helm-buffer-not-saved (,@fg-orange))
                (helm-buffer-saved-out (,@bg-base03 ,@fg-red))
                (helm-buffer-file (:inherit helm-ff-file))
                (helm-buffer-directory (:inherit helm-ff-directory))
                (helm-buffer-size (:inherit font-lock-comment-face))
                (helm-buffer-process (,@fg-orange))
                ;; helm-bookmark
                (helm-bookmark-directory (:inherit helm-ff-directory))
                (helm-bookmark-file (:inherit helm-ff-file))
                (helm-bookmark-gnus (,@fg-cyan))
                (helm-bookmark-info (,@fg-green))
                (helm-bookmark-man (,@fg-violet))
                (helm-bookmark-w3m (,@fg-yellow))
                (helm-bookmark-su (,@fg-orange))
                ;; helm-grep
                (helm-grep-match (:inherit match))
                (helm-grep-file (:inherit compilation-info))
                (helm-grep-lineno (:inherit compilation-info))
                (helm-grep-running (,@fg-red))
                (helm-grep-finish (:inherit mode-line))
                (helm-moccur-buffer (,@fg-cyan ,@fmt-undr))
                ;; helm-swoop
                (helm-swoop-target-line-face (:inherit match))
                (helm-swoop-target-line-block-face (:inherit match))
                (helm-swoop-target-word-face (:inherit isearch))
                (helm-swoop-line-number-face (:inherit shadow))
                ;; helm-time-zone
                (helm-time-zone-current (,@fg-green))
                (helm-time-zone-home (,@fg-red))
                ;; helm-apt
                (helm-apt-installed (,@fg-green))
                (helm-apt-deinstalled (,@fg-base01))
                ;; company-mode
                (company-tooltip (,@fg-base00 ,@bg-base02))
                (company-tooltip-selection (,@fg-base1 ,@bg-base02))
                (company-tooltip-mouse (,@fg-base1 ,@bg-base02))
                (company-tooltip-common (,@fg-base1 ,@bg-base02))
                (company-tooltip-common-selection (,@fg-base1 ,@bg-base02))
                (company-tooltip-annotation (,@fg-yellow ,@bg-base02))
                (company-scrollbar-fg (,@bg-base0))
                (company-scrollbar-bg (,@bg-base02))
                (company-preview (,@bg-green))
                (company-preview-common (,@bg-base02 :underline t))
                (company-template-field (,@fg-base03 ,@bg-yellow))
                ;; magit (for the 'next' branch)
                (magit-dimmed (:inherit shadow))
                (magit-hash (,@fg-yellow))
                (magit-tag (,@fg-yellow))
                (magit-filename (:inherit magit-hash))
                (magit-branch-remote (,@fg-green))
                (magit-branch-local (,@fg-blue))
                (magit-branch-current (:inherit magit-branch-local))
                (magit-head (:inherit magit-branch-local))
                (magit-refname (:inherit magit-hash))
                (magit-refname-stash (:inherit magit-hash))
                (magit-refname-wip (:inherit magit-hash))
                (magit-signature-good (,@fg-green))
                (magit-signature-bad (,@fg-red))
                (magit-signature-untrusted (,@fg-cyan))
                (magit-cherry-unmatched (,@fg-cyan))
                (magit-cherry-equivalent (,@fg-magenta))
                ;; magit-section
                (magit-section-heading (,@fg-base03 ,@bg-base0))
                (magit-section-highlight (:inherit highlight))
                ;; magit-diff
                (magit-diff-file-heading (:inherit default ,@fmt-bold))
                (magit-diff-file-heading-highlight (:inherit (highlight magit-diff-file-heading) ,@fmt-bold))
                (magit-diff-file-heading-selection (:inherit magit-diff-file-heading-highlight ,@fg-orange))
                (magit-diff-hunk-heading (:inherit diff-hunk-header ,@fmt-bold))
                (magit-diff-hunk-heading-highlight (:inherit (highlight magit-diff-hunk-heading) ,@fmt-bold))
                (magit-diff-hunk-heading-selection (:inherit magit-diff-hunk-heading-highlight ,@fg-orange))
                (magit-diff-lines-heading (,@bg-orange ,@fg-base3))
                (magit-diff-context (:inherit default))
                (magit-diff-context-highlight (:inherit highlight))
                (magit-diff-added (,@fg-green))
                (magit-diff-added-highlight (:inherit (highlight magit-diff-added)))
                (magit-diff-removed (,@fg-red))
                (magit-diff-removed-highlight (:inherit (highlight magit-diff-removed)))
                (magit-diffstat-added (,@fg-green))
                (magit-diffstat-removed (,@fg-red))
                ;; magit-log
                (magit-log-author (,@fg-orange))
                (magit-log-date (:inherit magit-diff-hunk-heading))
                (magit-log-graph (:inherit default))
                (magit-reflog-commit (,@fg-yellow))
                (magit-reflog-amment (,@fg-orange))
                (magit-reflog-checkout (,@fg-cyan))
                (magit-reflog-reset (,@fg-red))
                (magit-reflog-rebase (,@fg-green))
                (magit-reflog-cherry-pick (,@fg-violet))
                (magit-reflog-remote (,@fg-blue))
                (magit-reflog-other (,@fg-magenta))
                ;; magit-blame
                (magit-blame-heading (,@bg-base3 ,@fg-base02))
                ;; magit-process
                (magit-process-ok (,@fg-green))
                (magit-process-ng (,@fg-red))
                ;; magit-sequence
                (magit-sequence-stop (,@fg-green))
                (magit-sequence-part (,@fg-yellow))
                (magit-sequence-head (,@fg-blue))
                (magit-sequence-drop (,@fg-red))
                ;; magit-bisect
                (magit-bisect-good (,@fg-green))
                (magit-bisect-skip (,@fg-yellow))
                (magit-bisect-bad (,@fg-red))
                ;; git-rebase
                (git-rebase-hash (:inherit magit-hash))
                ;; macrostep
                (macrostep-gensym-1 (,@fg-violet :box (,@fg-violet)))
                (macrostep-gensym-2 (,@fg-green :box (,@fg-green)))
                (macrostep-gensym-3 (,@fg-yellow :box (,@fg-yellow)))
                (macrostep-gensym-4 (,@fg-red :box (,@fg-red)))
                (macrostep-gensym-5 (,@fg-magenta :box (,@fg-magenta)))
                (macrostep-expansion-highlight-face (:inherit highlight))
                (macrostep-macro-face (:inherit font-lock-keyword-face ,@fmt-undr))
                ;; which-func
                (which-func (,@fg-base1))
                )))))

(provide 'solarized-moar-definitions)
