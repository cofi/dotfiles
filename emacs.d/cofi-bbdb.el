(require-and-exec 'bbdb
    (bbdb-initialize)
    (setq bbdb-file-coding-system 'utf-8)
    (add-hook 'bbdb-load-hook
              (lambda () (setq bbdb-file-coding-system 'utf-8)))

    (setq bbdb-offer-save 1
          bbdb-use-pop-up t
          bbdb-electric-p t
          bbdb-popup-target-lines 2
          bbdb-dwim-net-address-allow-redundancy t
          bbdb-quiet-about-name-mismatches 2
          bbdb-always-add-address t
          bbdb-canonicalize-redundant-nets-p t
          bbbd-completion-type nil
          bbdb-complete-name-allow-cycling t
          bbdb-message-caching-enabled t
          bbdb-use-alternate-names t
          bbdb-elided-display t
          bbdb-north-american-phone-numbers-p nil
          bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
          bbdb-ignore-some-messages-alist
          '(
            ( "From" . "info\\|no.?reply\\|no.?mail\\|DAEMON\\|daemon\\|promotion\\|service\\|newsletter\\|mailman")
            )
          bbdb-auto-notes-alist
          '(
            ("Organization" (".*" Organization 0))
            ("X-ML-Name" (".*$" ML 0))
            ("X-Mailinglist" (".*$" ML 0))
            ("X-Ml-Name" (".*$" ML 0)))
          )

    (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

    (setq bbdb-display-layout 'multi-line)
    (setq bbdb-pop-up-display-layout 'one-line)
    (setq bbdb-display-layout-alist
          '((one-line          (order     . (net AKA notes phones))
                               (name-end  . 24)
                               (toggle    . t)
                               (omit      . (mail-alias gnus-private
                                             creation-date timestamp)))
            (multi-line        (indention . 14)
                               (toggle    . t)
                               (omit      . (AKA creation-date timestamp)))
            (pop-up-multi-line (indention . 14))))

    (require-and-exec 'bbdb-wl
        (require 'mime-bbdb)
        (bbdb-wl-setup)
        (setq bbdb-wl-ignore-folder-regexp "^@"
              bbdb-wl-folder-regexp "^.inbox$\\|^Gmail/gesendet$"
              wl-summary-get-petname-function 'bbdb-wl-get-petname
              )
        (defadvice wl-folder-suspend (after wl-bbdb-suspend activate compile)
          (interactive)
          (bbdb-wl-exit))
        (defadvice wl-exit (after wl-bbdb-suspend activate compile)
          (interactive)
          (bbdb-wl-exit))
        )
    )

(provide 'cofi-bbdb)
