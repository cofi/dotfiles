XPTemplate priority=personal

let s:f = g:XPTfuncs() 

XPTemplateDef

XPT class hint=class\ ..\ ctor
XSET className=fileRoot()
XSET args*|post=ExpandIfNotEmpty(', ', 'args*')
public class `className^ {
    public `className^(`args*^)`$BRfun^{
        `cursor^
    }
}

XPT main hint=main\ (\ String\ )
public static void main(String[] args)`$BRfun^{
    `cursor^
}

XPT exception hint=class\ ..\ extends Exception
XSET className=fileRoot()
public class `className^ extends Exception {}

XPT suite hint=junit\ testsuite
XSET className=fileRoot()
import junit.framework.Assert;
import org.junit.*;

public class `className^ {
    `cursor^
}

XPT suitesetup hint=junit\ suite\ setup
@BeforeClass
public static void setUpSuite() {
    `cursor^
}

XPT setup hint=junit\ suite\ setup
@Before
public void setUp() {
    `cursor^
}

XPT test hint=junit\ testcase
@Test
public void test`Name^() {
    `cursor^
}
