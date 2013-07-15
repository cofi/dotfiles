(require-and-exec 'diminish
  (loop for (file mode lighter) in '(("yasnippet" 'yas-minor-mode " Y")
                                     ("smartparens" 'smartparens-mode " p")
                                     ("eldoc" 'eldoc-mode " elD")
                                     ("highlight-parentheses" 'highlight-parentheses-mode)
                                     ("undo-tree" 'undo-tree-mode)
                                     ("paredit" 'paredit-mode " pE"))
        do
        (eval-after-load file
             `(diminish ,mode ,lighter))))

(require-and-exec 'sml-modeline
                  (setq sml-modeline-len 8)
                  (sml-modeline-mode 1))

(setq display-time-24hr-format t
      display-time-format "%H:%M %d.%m"
      display-time-mail-file 'none
      display-time-default-load-average nil)

(setq eol-mnemonic-dos "W"
      eol-mnemonic-mac "M"
      eol-mnemonic-unix "U"
      eol-mnemonic-undecided "-")

(setq battery-mode-line-format " [%L %p%%]")
(setq-default mode-line-format
              `(
                (evil-mode ("" evil-mode-line-tag))
                " "
                ;; buffer name and file name as help
                (:propertize "%b " face mode-line-buffer
                             help-echo (buffer-file-name))
                ;; coding and line ends
                mode-line-mule-info
                ;; buffer modified
                "%*"
                ;; appt
                appt-mode-string
                " "
                ;; sml modeline
                (sml-modeline-mode (:eval (list (sml-modeline-create))))
                ;; workgroup
                (cofi/full-emacs
                 (:eval (if (and (bound-and-true-p wg-mode-line-on)
                               (bound-and-true-p workgroups-mode))
                            (wg-mode-line-string)
                          "")))
                ;; line and column
                "<%l,%c>"
                (cofi/comm-instance (:eval (offlineimap-mode-line)))
                " "
                ;; recursive edit
                "%[("
                ;; major mode
                (:propertize ("" mode-name)
                             face mode-line-major-mode
                             mouse-face mode-line-highlight
                             help-echo "Major mode\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode, mouse-3: Toggle minor modes"
                             local-map ,mode-line-major-mode-keymap)

                ;; minor modes
                (:propertize ("" minor-mode-alist)
                             face mode-line-minor-mode
                             mouse-face mode-line-highlight
                             help-echo "Minor mode\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode, mouse-3: Toggle minor modes"
                             local-map ,mode-line-minor-mode-keymap)
                ")%]"
                (t erc-modified-channels-object)
                " %-"
                ))

(provide 'cofi-modeline)
