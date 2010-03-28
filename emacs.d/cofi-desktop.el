(require-and-exec 'desktop
  (lambda ()
    (setq desktop-save t)
    (setq desktop-clear-preserve-buffers (list
                                          "\\*scratch\\*"
                                          "\\*Messages\\*"
                                          "Org Agenda"
                                          ))
    (desktop-save-mode 1)
    ))
(provide 'cofi-desktop)
