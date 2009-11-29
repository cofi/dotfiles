"Vim color file
"Name: cocol.vim
"Maintainer: Michael Markert <markert.michael@googlemail.com>
"Homepage: http://github.com/cofi/cocol/

if &t_Co != 256 && !has("gui_running")
  echomsg ""
  echomsg "Error: 256 colors required. Please use GUI or a 256-color terminal"
  echomsg ""
  finish
endif

set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "cocol"

" General colors
hi Normal ctermbg=232 ctermfg=15 guibg=#202020 guifg=#FFFFFF gui=none
hi LineNr ctermfg=226 guifg=#D0D0A0 guibg=#2E2E37 gui=none

hi CursorColumn ctermbg=none ctermfg=none guibg=#2E2E37 gui=none
hi CursorLine ctermbg=none ctermfg=none guibg=#2E2E37 gui=none

hi FoldColumn ctermbg=bg guibg=bg guifg=#8FA5D1 gui=none
hi Folded guibg=#4E4E4E guifg=#C0C0C0 gui=none

hi TabLine ctermfg=fg guifg=fg ctermbg=242 guibg=#666666 cterm=underline gui=underline
hi TabLineSel cterm=bold gui=bold
hi TabLineFill  ctermfg=fg  guifg=fg      ctermbg=242 guibg=#666666 cterm=underline gui=underline

hi Visual ctermbg=239  guibg=#4E4E4E gui=none
hi VisualNOS ctermbg=237 ctermfg=0 guibg=#3A3A3A guifg=#A0A0A0 gui=bold,underline cterm=bold
hi WarningMsg guibg=bg guifg=#EE2C2C gui=bold
hi WildMenu ctermbg=202 ctermfg=0 guibg=#FF5F00 guifg=#000000 cterm=bold gui=bold

if has("spell")
    hi SpellBad guisp=#CC6666 gui=undercurl cterm=undercurl
    hi SpellCap guisp=#2C2CEE gui=undercurl cterm=undercurl
    hi SpellLocal guisp=#CCCC66 gui=undercurl cterm=undercurl
    hi SpellRare guisp=#66CCCC gui=undercurl cterm=undercurl
endif

if v:version >= 700
    hi Pmenu ctermfg=0   guifg=#000000 ctermbg=246 guibg=#949494
    hi PmenuSbar ctermbg=243 guibg=#767676
    hi PmenuSel ctermfg=0   guifg=#000000 ctermbg=243 guibg=#767676
    hi PmenuThumb ctermbg=252 guibg=#d0d0d0

    hi MatchParen ctermbg=34 guibg=#00AF00 gui=none
endif

hi NonText ctermfg=58 guibg=bg guifg=#5F5F00 gui=none
hi SignColumn ctermbg=bg ctermfg=68 guibg=bg guifg=#8FA5D1 gui=none
hi StatusLine ctermbg=63 ctermfg=15 guibg=#334B7D guifg=fg gui=bold cterm=bold
hi StatusLineNC ctermbg=252 ctermfg=8 guibg=#25365A guifg=fg gui=none cterm=none
hi VertSplit ctermbg=252 ctermfg=17 guibg=#25365A guifg=fg gui=none

hi Cursor ctermbg=160 guibg=#E20700 guifg=bg gui=none
hi lCursor ctermbg=40 ctermfg=0 guibg=#00D700 guifg=#000000 gui=none

hi Directory ctermfg=126 guibg=bg guifg=#AF0087 gui=none

hi ErrorMsg ctermfg=9 guibg=#EE2C2C guifg=#FFFFFF gui=bold cterm=bold
hi ModeMsg ctermfg=214 guibg=bg guifg=#FFAF00 gui=bold cterm=bold
hi MoreMsg ctermfg=214 guibg=bg guifg=#FFAF00 gui=bold cterm=bold
hi Question guibg=bg guifg=#E8B87E gui=bold cterm=bold

hi Search ctermbg=154 ctermfg=0 guibg=#AFFF00 guifg=#000000 gui=none
hi IncSearch ctermbg=227 ctermfg=0 guibg=#E0CD78 guifg=#000000 gui=none

hi SpecialKey ctermfg=185 guibg=bg guifg=#DFDF5F gui=none
hi Title gui=bold cterm=bold

" Diff colors
hi diffAdded ctermfg=82 guibg=#5FFF00 guifg=fg gui=none
hi diffRemoved ctermfg=160 guibg=#DF0000 guifg=fg gui=none
hi DiffDelete ctermfg=160 guibg=#DF0000 guifg=fg gui=none
hi DiffAdd ctermfg=82 guibg=#5FFF00 guifg=fg gui=none
hi Diff ctermfg=202 guibg=#DF5F00 guifg=fg gui=none
hi DiffChange ctermfg=19 guibg=#0000AF guifg=fg gui=none
hi DiffText ctermfg=94 guibg=#875F00 guifg=fg gui=bold

" Syntax colors
hi Comment ctermfg=228 guibg=bg guifg=#FFFF87 gui=none
hi Constant ctermfg=229 guibg=bg guifg=#FFFFAF gui=none
hi Error ctermfg=196 guibg=bg guifg=#DF0000 gui=none
hi Identifier ctermfg=62 guibg=bg guifg=#5F5FDF gui=none
hi PreProc guibg=bg guifg=#D7A0D7 gui=none
hi Special ctermfg=179 guibg=bg guifg=#DFAF5F gui=none
hi Todo ctermbg=4 ctermfg=253 guibg=#000080 guifg=#DADADA gui=underline,bold cterm=underline,bold
hi Underlined ctermfg=39 guifg=#00AFFF gui=underline cterm=underline
hi Statement ctermfg=104 guibg=bg guifg=#8787DF gui=none
hi Type ctermfg=173 guibg=bg guifg=#DF875F gui=none
hi Ignore ctermfg=238 guifg=#444444
hi Number ctermfg=180 guifg=#DFAF87
hi PreProc ctermfg=150 guifg=#AFDF87
hi Special ctermfg=174 guifg=#DF8787
hi TaglistTagName ctermfg=105 guifg=#8787FF gui=bold cterm=bold
