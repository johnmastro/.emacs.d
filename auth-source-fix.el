
;; Temporary fix for Emacs bug #19074. The version of auth-source.el that ships
;; with Emacs 24.5 and later will be patched.

(require 'auth-source)
(eval-when-compile (require 'cl))

(unless (string= emacs-version "24.4.1")
  (error "The auth-source-fix.el kludge should only be loaded for Emacs 24.4"))

(defun* auth-source-macos-keychain-search-items (coll type max
                                                      &rest spec
                                                      &key label type
                                                      host user port
                                                      &allow-other-keys)

  (let* ((keychain-generic (eq type 'macos-keychain-generic))
         (args `(,(if keychain-generic
                      "find-generic-password"
                    "find-internet-password")
                 "-g"))
         (ret (list :type type)))
    (when label
      (setq args (append args (list "-l" label))))
    (when host
      (setq args (append args (list (if keychain-generic "-c" "-s") host))))
    (when user
      (setq args (append args (list "-a" user))))

    (when port
      (if keychain-generic
          (setq args (append args (list "-s" port)))
        (setq args (append args (list
                                 (if (string-match "[0-9]+" port) "-P" "-r")
                                 port)))))

    (unless (equal coll "default")
      (setq args (append args (list coll))))

    (with-temp-buffer
      (apply 'call-process "/usr/bin/security" nil t nil args)
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at "^password: \"\\(.+\\)\"$")
          (setq ret (auth-source-macos-keychain-result-append
                     ret
                     keychain-generic
                     "secret"
                     (lexical-let ((v (match-string 1)))
                       (lambda () v)))))
         ;; TODO: check if this is really the label
         ;; match 0x00000007 <blob>="AppleID"
         ((looking-at "^[ ]+0x00000007 <blob>=\"\\(.+\\)\"")
          (setq ret (auth-source-macos-keychain-result-append
                     ret
                     keychain-generic
                     "label"
                     (match-string 1))))
         ;; match "crtr"<uint32>="aapl"
         ;; match "svce"<blob>="AppleID"
         ((looking-at "^[ ]+\"\\([a-z]+\\)\"[^=]+=\"\\(.+\\)\"")
          (setq ret (auth-source-macos-keychain-result-append
                     ret
                     keychain-generic
                     (match-string 1)
                     (match-string 2)))))
        (forward-line)))
    ;; return `ret' iff it has the :secret key
    (and (plist-get ret :secret) (list ret))))
