
set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "twilight"

if version >= 700
  hi CursorLine guibg=#262626
  hi CursorColumn guibg=#262626
  hi MatchParen guifg=white guibg=#80a090 gui=bold

  "Tabpages
  hi TabLine guifg=black guibg=#b0b8c0 gui=italic
  hi TabLineFill guifg=#9098a0
  hi TabLineSel guifg=black guibg=#f0f0f0 gui=italic,bold

  "P-Menu (auto-completion)
  hi Pmenu guifg=white guibg=#808080
  "PmenuSel
  "PmenuSbar
  "PmenuThumb
endif

hi Visual guibg=#404040

"hi Cursor guifg=NONE guibg=#586068
hi Cursor guibg=#b0d0f0

hi Normal guifg=#f8f8e3 guibg=#202020
"hi LineNr guifg=#808080 guibg=#e0e0e0
hi LineNr guifg=#605958 guibg=#303030 gui=italic
"hi Comment guifg=#5f5a60 gui=italic
hi Comment guifg=#605958 gui=italic
hi Todo guifg=#808080 guibg=NONE gui=bold,italic

hi StatusLine guifg=#f0f0f0 guibg=#101010 gui=italic
hi StatusLineNC guifg=#a0a0a0 guibg=#181818 gui=italic
hi VertSplit guifg=#181818 guibg=#181818 gui=italic

hi Folded guibg=#384048 guifg=#a0a8b0 gui=italic
hi FoldColumn guibg=#384048 guifg=#a0a8b0
hi SignColumn guibg=#384048 guifg=#a0a8b0

hi Title guifg=#c05000 gui=bold,underline

hi Constant guifg=#cf6a4c
hi String guifg=#799d6a
hi Special guifg=#99fd8a
"hi Number
"hi Float
hi Identifier guifg=#7587a6
" Type d: 'class'
hi Structure guifg=#9B859D gui=underline
hi Function guifg=#dad085
" dylan: method, library, ... d: if, return, ...
hi Statement guifg=#7187a1 gui=NONE
" Keywords  d: import, module...
hi PreProc guifg=#8fbfdc
"gui=underline
"hi Operator guifg=#ff00ff
"hi Type guifg=#708090

hi Type guifg=#f9ee98 gui=NONE

hi NonText guifg=#808080 guibg=#303030

"hi Macro guifg=#a0b0c0 gui=underline

"Tabs, trailing spaces, etc (lcs)
hi SpecialKey guifg=#808080 guibg=#343434

"hi TooLong guibg=#ff0000 guifg=#f8f8f8

hi Search guifg=#a08090 guibg=#302028 gui=underline

hi Directory guifg=#dad085 gui=NONE
hi Error guibg=#602020

