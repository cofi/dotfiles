XPTemplate priority=personal

let s:f = g:XPTfuncs() 
 
XPTemplateDef

XPT contract hint=gdi\ contract
XSET typeargs*|post=ExpandIfNotEmpty( ' ', 'typeargs*' )
;; Contract: `fname^ : `typeargs*^ -> `returntype^
;; Purpose: `description^
;; Example: `example^

XPT tests hint=htdp\ tests
XSET args1*|post=ExpandIfNotEmpty( ' ', 'args1*' )
XSET args2*|post=ExpandIfNotEmpty( ' ', 'args2*' )
(check-expect (`fname^ `args1*^) `expect1^)
(check-expect (`fname^ `args2*^) `expect2^)

XPT func hint=func\ definition
XSET typeargs*|post=ExpandIfNotEmpty( ' ', 'typeargs*' )
XSET args*|post=ExpandIfNotEmpty( ' ', 'args*' )
XSET args1*|post=ExpandIfNotEmpty( ' ', 'args1*' )
XSET args2*|post=ExpandIfNotEmpty( ' ', 'args2*' )
;; Contract: `fname^ : `typeargs*^ -> `returntype^
;; Purpose: `description^
;; Example: `example^
(define (`fname^ `args*^)
    `body^)

(check-expect (`fname^ `args1*^) `expect1^)
(check-expect (`fname^ `args2*^) `expect2^)
