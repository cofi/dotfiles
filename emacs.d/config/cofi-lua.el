(let ((local (expand-file-name "~/doc/lua/manual.html")))
  (setq lua-documentation-url
        (or (and (file-readable-p local)
                 (concat "file://"  local))
            "http://www.lua.org/manual/5.3/manual.html")))

(defun cofi/lua-send-dwim (pos)
  "Send region if region is active and current defun else."
  (interactive "d")
  (if (region-active-p)
      (lua-send-region (region-beginning) (region-end))
    (lua-send-defun pos)))

(add-all-to-hook 'lua-mode-hook
                 (gen-local-fill-keymap-hook
                     "C-c C-c"   #'cofi/lua-send-dwim
                     "C-c C-l"   #'lua-send-current-line
                     "C-c C-f"   #'lua-send-buffer
                     "C-c C-b"   #'lua-send-buffer
                     "C-c C-d"   #'lua-search-documentation
                   ))


(provide 'cofi-lua)
