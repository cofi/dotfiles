/* MarkTree JavaScript code
 *
 * Distributed under the terms of the MIT License.
 * See "LICENCE.MIT" or http://www.opensource.org/licenses/mit-license.php for details.
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Miika Nurminen, 12.7.2004.
 *
 * Lennart Borgman, 2010-09-25
 * - Modified to take care of scrolling.
 */

function log_to_console(msg) {
    if ((undefined===console) || (null==console))
        alert(msg);
    else
        console.log(msg);
}

/* cross-browser (tested with ie5, mozilla 1 and opera 5) keypress detection */
function get_keycode(evt) {
    // IE
    code = document.layers ? evt.which
        : document.all ? event.keyCode // event.keyCode!=evt.keyCode!
        : evt.keyCode;

    if (code==0)
        code=evt.which; // for NS
    return code;
}

var lastnode=null;
var listnodes = null;
var list_index=1;
var lastnodetype=''; // determines if node is a link, input or text;

// up, left, down, right, keypress codes
//ijkl
//var keys = new Array(105,106,107,108);
//num arrows
//var keys = new Array(56,52,50,54);
//wasd
// var press2 = new Array(119,97,115,100);
var press = new Array(47,45,42,43);

// keydown codes
//  var keys2=new Array(87,65,83,68);
var keys= new Array(38,37,40,39);

// keyset 1 = keydown, otherwise press
function checkup(keyset,n) {
    if (keyset==1) return (n==keys[0]);
    return ((n==press[0]) /*|| (n==press2[0])*/)
}

function checkdn(keyset,n) {
    if (keyset==1) return (n==keys[2]);
    return ((n==press[2]) /*|| (n==press2[2])*/)
}

function checkl(keyset,n) {
    if (keyset==1) return (n==keys[1]);
    return ((n==press[1]) /*|| (n==press2[1])*/)
}

function checkr(keyset,n) {
    if (keyset==1) return (n==keys[3]);
    return ((n==press[3]) /*|| (n==press2[3])*/)
}





function is_exp(n) {
    if (n==null) return false;
    return ((n.className=='exp') || (n.className=='exp_active'));
}

function is_col(n) {
    if (n==null) return false;
    return ((n.className=='col') || (n.className=='col_active'));
}

function is_basic(n) {
    if (n==null) return false;
    return ((n.className=='basic') || (n.className=='basic_active'));
}



/* returns i>=0 if true */
function is_active(node) {
    if (node.className==null) return false
    return node.className.indexOf('_active');
}

function toggle_class(node) {
    if ((node==null) || (node.className==null)) return;
    str=node.className;
    result="";
    i = str.indexOf('_active');
    if (i>0)
        result= str.substr(0,i);
    else
        result= str+"_active";
    node.className=result;
    return node;
}

function activate(node) {
    node.style.backgroundColor='#eeeeff';
}

function deactivate(node) {
    node.style.backgroundColor='#ffffff';
}

function is_list_node(n) {
    if (n==null) return false;
    if (n.className==null) return false;
    if ( (is_exp(n)) ||
         (is_col(n)) ||
         (is_basic(n)) )
        return true; else return false;
}


function get_href(n) {
    alist=n.attributes;
    if (alist!=null) {
        hr = alist.getNamedItem('href');
        if (hr!=null) return hr.nodeValue;
    }
    if (n.childNodes.length==0) return '';
    for (var i=0; i<n.childNodes.length; i++) {
        s = get_href(n.childNodes[i]);
        if (s!='') return s;
    }
    return '';
}

function get_link(n) {
    if (n==null) return null;
    if (n.style==null) return null;

    // disabling uncontrolled recursion to prevent error messages on IE
    // when trying to focus to invisible links (readonly mode)
    //    alert(n.nodeName+' '+n.className);
    if ((n.nodeName=='UL') && (n.className=='sub')) return null;

    if (n.nodeName=='A') return n;
    if (n.childNodes.length==0) return null;
    for (var i=0; i<n.childNodes.length; i++) {
        s = get_link(n.childNodes[i]);
        if (s!=null) return s;
    }
    return null;
}

function scrolled_into_view(n) {
    var body_top = body_scrollTop();
    var top = n.offsetTop - body_top;
    var bottom = n.offsetTop - body_top + n.offsetHeight;
    var inside = ((top < window.innerHeight) && (bottom > 0));
    // log_to_console("n.offsetTop="+n.offsetTop);
    // log_to_console("=scrolled_into_view= inside="+inside+", body_top="+body_top+", n.offsetTop="+n.offsetTop+", top="+top+", bottom="+bottom);
    return inside;
}

var lastnode_top = null;
var lastnode_bottom = null;
function set_lastnode(n) {
    /*var d = new Date();
      var t_mil = d.getMilliseconds();*/
    // testattu nopeuksia explorerilla, ei merkittäviä eroja
    if (lastnode==n) return;
    if (null==n) return;
    // log_to_console("set_lastnode("+n.offsetTop+"), lastnode="+lastnode.offsetTop);
    /*  deactivate(lastnode)
        lastnode=n;
        activate(lastnode);*/

    if (is_active(lastnode)>=0)
        toggle_class(lastnode);
    lastnode=n;
    var body_top = body_scrollTop();
    lastnode_top = lastnode.offsetTop - body_top;
    lastnode_bottom = lastnode.offsetTop - body_top + lastnode.offsetHeight;
    if (!scrolled_into_view(n))
        n.scrollIntoView();
    if (!(is_active(lastnode)>=0))
        toggle_class(lastnode);


    /*var d2 = new Date();
      var t_mil2 = d2.getMilliseconds();
      window.alert(t_mil2-t_mil);*/
}

function next_list_node() {
    tempIndex = list_index;
    while (tempIndex<listnodes.length-1) {
        tempIndex++;
        var x = listnodes[tempIndex];
        if (is_list_node(x)) {
            list_index=tempIndex;
            return;
        }
    }
}

function prev_list_node() {
    tempIndex = list_index;
    while (tempIndex>0) {
        tempIndex--;
        var x = listnodes[tempIndex];
        if (is_list_node(x)) {
            list_index=tempIndex;
            return;
        }
    }
}



function getsub (li) {
    if (li.childNodes.length==0) return null;
    for (var c = 0; c < li.childNodes.length; c++)
        if ( (li.childNodes[c].className == 'sub') || (li.childNodes[c].className == 'subexp') )
            return li.childNodes[c];
}

function find_listnode_recursive (li) {
    if (is_list_node(li)) return li;
    if (li.childNodes.length==0) return null;
    result=null;
    for (var c = 0; c < li.childNodes.length; c++) {
        result=find_listnode_recursive(li.childNodes[c]);
        if (result!=null) return result;
    }
    return null;
}

function next_child_listnode(li) {
    var result=null;
    for (var i=0; i<li.childNodes.length; i++) {
        result=find_listnode_recursive(li.childNodes[i]);
        if (result!=null) return result;
    }
    return null;
}

function next_actual_sibling_listnode(li) {
    if (li==null) return null;
    var temp=li;
    while (1) {
        var n = temp.nextSibling;
        if (n==null) {
            n=parent_listnode(temp);
            return next_actual_sibling_listnode(n);
        }
        if (is_list_node(n)) return n;
        temp=n;
    }
}

function next_sibling_listnode(li) {
    if (li==null) return null;
    var result=null;
    var temp=li;
    if (is_col(temp)) return next_child_listnode(temp);
    while (1) {
        var n = temp.nextSibling;
        if (n==null) {
            n=parent_listnode(temp);
            return next_actual_sibling_listnode(n);
        }
        if (is_list_node(n)) return n;
        temp=n;
    }
}

function last_sibling_listnode(li) {
    if (li==null) return null;
    var temp=li;
    var last=null;
    while(1) {
        var n = temp.nextSibling;
        if (is_list_node(temp))
            last = temp;
        if (n==null) {
            if (is_col(last)) return last_sibling_listnode(next_child_listnode(last));
            else return last;
        }
        temp = n;
    }
}

function prev_sibling_listnode(li) {
    if (li==null) return null;
    var temp=li;
    var n = null;
    while (1) {
        n = temp.previousSibling;
        if (n==null) {
            return parent_listnode(li);
        }
        if (is_list_node(n)) {
            if (is_col(n)) {
                return last_sibling_listnode(next_child_listnode(n));
            }
            else {
                return n;
            }
        }
        temp=n;
    }
}


function parent_listnode(li) {
    // added 12.7.2004 to prevent IE error when readonly mode==true
    if (li==null) return null;
    n=li;
    while (1) {
        n=n.parentNode;
        if (n==null) return null;
        if (is_list_node(n)) return n;
    }
}

function getVisibleParents(id) {
    var n = document.getElementById(id);
    while(1) {
        expand(n);
        n = parent_listnode(n);
        if (n==null) return;
    }
}

function onClickHandler (evt) {
    if (lastnode==null)
    {
        listnodes = document.getElementsByTagName('li');
        lastnode=listnodes[1];
        temp=listnodes[1];
    }


    var target = evt ? evt.target : event.srcElement;
    if (!is_list_node(target)) {
        set_lastnode( parent_listnode(target));
        // log_to_console("=onClickHandler= => lastnode="+lastnode.offsetTop);
        return;
    }
    toggle(target);
    set_lastnode(target);
}


function expand(node) {
    if (!is_exp(node)) return;
    if (node.className=='exp_active')
        node.className='col_active';
    else
        node.className='col';
    setSubClass(node,'subexp');
    //    getsub(node).className='subexp';
}

function collapse(node) {
    if (!is_col(node)) return;

    if (node.className=='col_active')
        node.className='exp_active'
    else
        node.className='exp';

    setSubClass(node,'sub');
    //  getsub(node).className='sub';

}

function setSubClass(node,name) {
    sub = getsub(node);
    if (sub==null) return;
    sub.className=name;
}

function toggle(target) {
    if (!is_list_node(target)) return;
    if (is_col(target)) {
        target.className='exp';
        setSubClass(target,'sub');
        //      getsub(target).className='sub';
    }
    else if (is_exp(target)) {
        target.className='col';
        setSubClass(target,'subexp');
        //      getsub(target).className='subexp';
    }

}

function expandAll(node) {
    if (node.className=='exp') {
        node.className='col';
        setSubClass(node,'subexp');
        //        getsub(node).className='subexp';
    }
    var i;
    if (node.childNodes!=null)
        //    if (node.hasChildNodes())
        for ( i = 0; i<node.childNodes.length; i++)
            expandAll(node.childNodes[i]);
}

function collapseAll(node) {
    if  (node.className=='col') {
        node.className='exp';
        setSubClass(node,'sub');
        //        getsub(node).className='sub';
    }
    var i;
    if (node.childNodes!=null)
        // for opera   if (node.hasChildNodes())
        for ( i = 0; i<node.childNodes.length; i++)
            collapseAll(node.childNodes[i]);
}



function unFocus(node) {
    // unfocuses potential link that is to be hidden (if a==null there is no link so it should not be blurred).
    // tested with mozilla 1.7, 12.7.2004. /mn (
    intemp=parent_listnode(node);
    a = get_link(intemp);     // added 6.4. to get keyboard working with
    // moved before collapse to prevent an error message with IE when readonly==true
    if (a!=null) a.blur(); // netscape after collapsing a focused node
    return intemp;
}

// mode: 0==keypress, 1==keyup
function keyfunc(evt,mode) {
    var c = get_keycode(evt);
    var temp = null;
    var a = null;

    if (lastnode==null) {
        listnodes = document.getElementsByTagName('li');
        lastnode=listnodes[1];
        temp=listnodes[1];
    }

    // Fix-me: Don't we have to assert that the last node is actually
    // sensible here first? I.e. check scrolling?
    real_scrolltest ();

    if (checkup(mode,c)) { // i
        // log_to_console("evt=up, lastnode_top="+lastnode_top);
        temp=prev_sibling_listnode(lastnode);
        if (null!=temp) {
            var body_top = body_scrollTop();
            var temp_top = temp.offsetTop - body_top;
            var lastnode_top = lastnode.offsetTop - body_top;
            // If lastnode_top is visible then go to prev node.
            if ((temp_top < 0) && (lastnode_top < window.innerHeight))
                temp = null;
        }
    }
    else if (checkdn(mode,c)) { // k
        // log_to_console("evt=down");
        // log_to_console("evt=down lastnode.offsetTop="+lastnode.offsetTop);
        temp=next_sibling_listnode(lastnode);
        if (null!=temp) {
            // log_to_console("dn temp.offsetTop="+temp.offsetTop);
            //if (lastnode_bottom < (window.innerHeight / 4.0))
            var body_top = body_scrollTop();
            var temp_top = temp.offsetTop - body_top;
            //if (temp_top > window.innerHeight) {
            if (temp_top < 0) {
                // In this case the child node is above the visible
                // part. It seems best to move on the actual sibling
                // instead.
                temp=next_actual_sibling_listnode(lastnode);
                // log_to_console("dn sibling temp.offsetTop="+temp.offsetTop);
            }
            // log_to_console("dn final temp.offsetTop="+temp.offsetTop);
        }
    }
    else if (checkr(mode,c)) { // l
        expand(lastnode);
        //  temp=next_child_listnode(lastnode);
        // if (temp==null) {
        a = get_link(lastnode);
        if (a!=null) a.focus(); else self.focus();
        //}
    }
    else if (checkl(mode,c)) { // j
        if (is_col(lastnode)) {
            unFocus(lastnode);
            collapse(lastnode);
            lastnode.scrollIntoView();
        }
        else {
            temp=unFocus(lastnode);
            collapse(temp);
            temp.scrollIntoView();
        }
        //    if (temp==null) lastnode.focus(); // forces focus to correct div (try mozilla typesearch) (doesn't seem to work -mn/6.4.2004)
    }
    else return;
    if ((temp!=null) && scrolled_into_view(temp))
        set_lastnode(temp);

    // alert('pressed ' + String.fromCharCode(c) + '(' + c + ')');
    return true;
}


function keytest (evt) {
    return keyfunc(evt,1);
};


function presstest (evt) {
    return keyfunc(evt,0);
};

function body_scrollTop () {
    var temp = document.body.scrollTop;
    if (0==temp)
        temp = document.body.parentNode.scrollTop;
    return temp;
}

// Run this later to not interfer with last keypress and also to avoid
// unnecessary computing.
function scrolltest (evt) {
    // log_to_console("=scrolltest= evt="+evt);
    clear_scrolltest_timer();
    scroll_test_timer = setTimeout("real_scrolltest()", 500);
}

var scroll_test_timer = null;

function clear_scrolltest_timer () {
    if (null!=scroll_test_timer)
        clearTimeout (scroll_test_timer);
    scroll_test_timer = null;
}

function real_scrolltest () {
    // log_to_console("=real_scrolltest= window.height="+window.innerHeight);
    if (null!=lastnode) {
        // log_to_console("scroll:body_scrollTop()="+body_scrollTop()+", offsetHeight="+document.body.offsetHeight);
        // log_to_console("scroll:lastnode.offsetTop="+lastnode.offsetTop+", offsetHeight="+lastnode.offsetHeight);
        var temp = lastnode;
        var body_top = body_scrollTop();
        if ((temp.offsetTop - body_top + temp.offsetHeight) < 0)
        {
            while ((null!=temp)
                   && ((temp.offsetTop - body_top + temp.offsetHeight) < 0)) {
                // log_to_console("next temp.offsetTop="+temp.offsetTop+", height="+temp.offsetHeight);
                temp=next_sibling_listnode(temp);
            }
        } else {
            while ((null!=temp)
                   && ((temp.offsetTop - body_top) > window.innerHeight)) {
                // log_to_console("prev temp.offsetTop="+temp.offsetTop+", height="+temp.offsetHeight);
                temp=prev_sibling_listnode(temp);
            }
        }
        // if (null==temp)
            // log_to_console("=> null");
        // else
            // log_to_console("=> temp="+temp.offsetTop);
        if (null!=temp)
            set_lastnode(temp);
    }
}

document.onclick = onClickHandler;
document.onkeypress = presstest;
document.onkeyup = keytest;
//document.onscroll = scrolltest;
window.onscroll = scrolltest;
