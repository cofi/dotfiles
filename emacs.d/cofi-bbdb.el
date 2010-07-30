(require-and-exec 'bbdb
    (bbdb-initialize)

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
            ( "From" . "info\\|no.?reply\\|DAEMON\\|daemon\\|promotion\\|service\\|newsletter\\|mailman")
            )
          bbdb-auto-notes-alist
          '(
            ("X-ML-Name" (".*$" ML 0))
            ("X-Mailinglist" (".*$" ML 0))
            ("X-Ml-Name" (".*$" ML 0)))
          )

    (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

    (require-and-exec 'bbdb-wl
        (require 'mime-bbdb)
        (bbdb-wl-setup)
        (setq bbdb-wl-ignore-folder-regexp "^@"
              bbdb-wl-folder-regexp "^.inbox$\\|^Gmail/gesendet$"
              )
        (defadvice wl-folder-suspend (after wl-bbdb-suspend activate compile)
          (interactive)
          (bbdb-wl-exit))
        (defadvice wl-exit (after wl-bbdb-suspend activate compile)
          (interactive)
          (bbdb-wl-exit))
        (setq wl-summary-from-function 'bbdb-wl-from-func)
        )
    )

(provide 'cofi-bbdb)
