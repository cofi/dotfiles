(require 'column-marker)

(defun highlight-80 ()
  (interactive)
  (column-marker-1 80))

(add-hook 'python-mode-hook 'highlight-80)

(setq-default fill-column 80)
