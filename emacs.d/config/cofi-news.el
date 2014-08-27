(require 'elfeed)
(defvar cofi/elfeed-url-tags (make-hash-table :size 200 :test #'equal))

(setq-default elfeed-search-filter "-junk +unread ")
(setq elfeed-max-connections 8)

(load-and-exec "private"
  (cl-loop for (gtag . feeds) in cofi/feeds
           do (cl-loop for (url . tags) in feeds
                       do (progn
                            (puthash url (cons gtag tags) cofi/elfeed-url-tags)
                            (pushnew url elfeed-feeds :test #'string=)))))

(defun cofi/elfeed-tag-by-url (entry)
  (apply #'elfeed-tag entry (gethash (elfeed-feed-url (elfeed-entry-feed entry))
                                     cofi/elfeed-url-tags)))

(add-hook 'elfeed-new-entry-hook #'cofi/elfeed-tag-by-url)

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "14 days ago"
                              :add 'expiring))

(add-hook 'elfeed-new-entry-hook
          (lambda (entry)
            (setf (elfeed-entry-content entry) nil)))

(defun cofi/elfeed ()
  (interactive)
  (condition-case nil
      (select-frame-by-name "Elfeed")
    (error (select-frame
            (make-frame '((name . "Elfeed"))))))
  (add-hook 'kill-emacs-hook #'elfeed-db-compact)
  (elfeed)
  (elfeed-update))

(fill-keymap elfeed-search-mode-map
  "R" (cmd elfeed-mark-all-as-read
           (mark-whole-buffer)
           (elfeed-search-untag-all-unread))
  "B" (cmd elfeed-browse-all
           (mark-whole-buffer)
           (elfeed-search-browse-url))

  "SPC" (cmd elfeed-default-filter
             (setq elfeed-search-filter (default-value 'elfeed-search-filter))
             (elfeed-search-update--force))
  )

(defun cofi--elfeed-feed-hosts ()
  (mapcar #'url-host (mapcar #'url-generic-parse-url elfeed-feeds)))

(defun cofi/elfeed-reset-processes ()
  (interactive)
  (setq elfeed-connections nil
        elfeed-waiting nil)
  (let ((feed-hosts (cofi--elfeed-feed-hosts)))
    (cl-loop for p in (process-list)
             for n = (process-name p)
             when (member n feed-hosts)
             do (delete-process p))))

(provide 'cofi-news)
