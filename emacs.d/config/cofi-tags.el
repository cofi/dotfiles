(add-hook 'c-mode-common-hook #'ggtags-mode)

(eval-after-load "ggtags"
  '(fill-keymap ggtags-mode-map
     "C-c ." 'ggtags-find-tag
     "C-c ," 'ggtags-find-tag-resume
     "C-c /" 'ggtags-navigation-mode-done))

(provide 'cofi-tags)
