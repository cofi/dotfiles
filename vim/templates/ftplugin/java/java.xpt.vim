XPTemplate priority=personal

let s:f = g:XPTfuncs() 

XPTvar $TRUE          true
XPTvar $FALSE         false
XPTvar $NULL          null
XPTvar $UNDEFINED     null

XPTvar $VOID_LINE  /* void */;
XPTvar $CURSOR_PH      /* cursor */

XPTvar $IF_BRACKET_STL     \ 
XPTvar $FOR_BRACKET_STL    \ 
XPTvar $WHILE_BRACKET_STL  \ 
XPTvar $STRUCT_BRACKET_STL \ 
XPTvar $FUNC_BRACKET_STL   \ 

XPTvar $CL    /*
XPTvar $CM    *
XPTvar $CR    */


XPTinclude
      \ _common/common
      \ _comment/doubleSign
      \ _condition/c.like
      \ _loops/java.for.like
      \ _loops/c.while.like


" ========================= Function and Variables =============================

" ================================= Snippets ===================================
XPTemplateDef

XPT class hint=class\ ..
XSET className=fileRoot()
XSET args*|post=ExpandIfNotEmpty(', ', 'args*')
public class `className^ {
    public `className^(`args*^)`$FUNC_BRACKET_STL^{
    }
}

XPT mainclass hint=class\ ..\ main
XSET className=fileRoot()
XSET main=Trigger('main')
public class `className^ {
    `main^
}
