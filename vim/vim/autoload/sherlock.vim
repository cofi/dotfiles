"=============================================================================
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Fri Aug  7 09:43:24 CEST 2009
" Licence:					GPL version 2.0 license
"=============================================================================
let s:incsearch = &incsearch
let s:folding = has('folding')
"s:reset {{{1
function s:reset()
	let commandLine = getcmdline()

	if exists('s:pattern') && s:pattern['value'] != ''
		call s:resetMatch()

		let commandType = getcmdtype()

		if commandType == ':' || commandType == '/'
			cunmap /
		elseif commandType == '?'
			cunmap ?
		endif

		cunmap <Esc>
		cunmap <CR>

		let &incsearch = s:incsearch

		if s:pattern['folding']
			normal zx
			let s:pattern['folding'] = 0
		endif
	endif

	let s:pattern = { 'value': '', 'commandLine': '', 'position': [], 'matchID': 0, 'match': '', 'folding': 0 }

	return commandLine
endfunction
"s:escape {{{1
function s:escape()
	let commandLine = s:pattern['commandLine']

	call s:reset()

	return commandLine
endfunction
"s:setMatch {{{1
function s:setMatch(match)
	call s:resetMatch()
	let s:pattern['matchID'] = matchadd('IncSearch', s:pattern['value'] . a:match)
endfunction
"s:resetMatch {{{1
function s:resetMatch()
	if s:pattern['matchID'] != 0
		call matchdelete(s:pattern['matchID'])
		let s:pattern['matchID'] = 0
	endif
endfunction
"s:complete {{{1
function s:complete(direction)
	if !exists('s:pattern')
		call s:reset()
	endif

	let commandLine = getcmdline()
	let commandType = getcmdtype()
	let cursorPosition = getcmdpos() - 1

	if commandType == ':' || commandType == '/' || commandType == '?'
		let value = ''
		let separator = ''

		if commandType == '/' || commandType == '?'
			let value = strpart(commandLine, 0, cursorPosition)
			let separator = commandType
		elseif commandType == ':' && commandLine =~ '\/.\{-}$'
			let value = substitute(strpart(commandLine, 0, cursorPosition), '^.*\/\(.\{-}\)$', '\1', '')
			let separator = '/'
		endif

		if value == ''
			call s:reset()
		elseif value != s:pattern['match']
			call s:reset()

			let s:pattern['value'] = value
			let s:pattern['commandLine'] = commandLine

			if commandType == '/' || commandType == ':'
				cnoremap / <C-\>e<SID>reset()<CR>/
			else
				cnoremap ? <C-\>e<SID>reset()<CR>?
			endif

			cnoremap <Esc> <C-\>e<SID>escape()<CR>
			cnoremap <CR> <C-\>e<SID>reset()<CR><CR>
		endif

		if s:pattern['value'] != ''
			set noincsearch

			let commandLine = s:pattern['commandLine']

			let position = searchpos(s:pattern['value'] . '\k*', 'w' . (a:direction < 0 ? 'b' : ''))

			if (position[0] == 0 && position[1] == 0) || (len(s:pattern['position']) > 0 && position[0] == s:pattern['position'][0] && position[1] == s:pattern['position'][1])
				call s:reset()
			else
				if s:folding
					if s:pattern['folding'] && position[0] != s:pattern['folding']
						normal zx
						let s:pattern['folding'] = 0
					endif

					if foldclosed(position[0]) || position[0] == s:pattern['folding']
						normal zv
						let s:pattern['folding'] = position[0]
					endif
				endif

				let match = escape(substitute(strpart(getline(position[0]), position[1] - 1), '^' . s:pattern['value'] . '\(\k*\).*$', '\1', ''), '/[]')
				let s:pattern['match'] = s:pattern['value'] . match

				if len(s:pattern['position']) <= 0
					let s:pattern['position'] = position
				endif

				let separatorIndex = stridx(commandLine, separator, cursorPosition)
				let commandLine = strpart(commandLine, 0, cursorPosition) . match . (separatorIndex < 0 ? '' : strpart(commandLine, separatorIndex))

				call s:setMatch(match)
			endif

			nohlsearch
		endif
	endif

	return commandLine
endfunction
"makeVimball {{{1
function sherlock#makeVimball()
	split sherlockVimball

	setlocal bufhidden=delete
	setlocal nobuflisted
	setlocal noswapfile

	let files = 0

	for file in split(globpath(&runtimepath, '**/sherlock*'), "\n")
		for runtimepath in split(&runtimepath, ',')
			if file =~ '^' . runtimepath
				if getftype(file) != 'dir'
					let files += 1
					call setline(files, substitute(file, '^' . runtimepath . '/', '', ''))
				else
					for subFile in split(glob(file . '/**'), "\n")
						if getftype(subFile) != 'dir'
							let files += 1
							call setline(files, substitute(subFile, '^' . runtimepath . '/', '', ''))
						endif
					endfor
				endif
			endif
		endfor
	endfor

	try
		execute '%MkVimball! sherlock'

		setlocal nomodified
		bwipeout

		echomsg 'Vimball is in ''' . getcwd() . ''''
	catch /.*/
		echohl ErrorMsg
		echomsg v:exception
		echohl None
	endtry
endfunction
" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
"sherlock#completeForward {{{1
function sherlock#completeForward()
	return s:complete(1)
endfunction
"sherlock#completeBackward {{{1
function sherlock#completeBackward()
	return s:complete(-1)
endfunction
" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
