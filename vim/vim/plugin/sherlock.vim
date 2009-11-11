"=============================================================================
" File:						sherlock.vim
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Thu Aug  6 10:57:14 CEST 2009
" Licence:					GPL version 2.0 license
" GetLatestVimScripts:	2731 11146 :AutoInstall: sherlock.vim
"=============================================================================
if (!exists('sherlock#disable') || sherlock#disable == 0) && !exists('sherlock#loaded')
	let sherlock#name = 'sherlock'
	let sherlock#loaded = 1

	if v:version < 700
		echo "Vim version >= 7 is required for sherlock.vim."
	else
		let s:cpo = &cpo

		setlocal cpo&vim

		if !hasmapto('<Plug>sherlockCompleteBackward()')
			cnoremap <silent> <C-S-Tab> <C-\>esherlock#completeBackward()<CR>
		endif

		if !hasmapto('sherlock#completeForward()')
			cnoremap <silent> <C-Tab> <C-\>esherlock#completeForward()<CR>
		endif

		command -nargs=0 SherlockVimball call sherlock#makeVimball()

		let &cpo= s:cpo
		unlet s:cpo
	endif
endif

finish

" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
