(add-to-loadpath "~/.elisp/vendor/scala-mode2/")
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(autoload 'scala-mode  "scala-mode2" nil t)

(provide 'cofi-scala)
