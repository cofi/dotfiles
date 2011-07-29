### DesktopSearch.ps1 -- Search WDS from command line
#
# Author: Lennart Borgman
# Last-Updated: 2010-12-21 Tue

# .SYNOPSIS
# Use Windows Search from command line.
#
# .DESCRIPTION
# Use Windows Search from command line.
#
# .INPUTS
# None. You cannot pipe objects to this script.
#
# .LINK
# Available from http://EmacsWiki.org/NxhtmlMode/

param (
    # Parameter. (Ignored.)
    $params,

    # Where to search.
    $roots,

    [string[]]
    # What to search for.
    $strings
)


$ErrorActionPreference = "Continue"
$bufferSize = new-object System.Management.Automation.Host.Size 160,72
$host.UI.RawUI.BufferSize = $bufferSize
$host.UI.RawUI.WindowSize = $bufferSize
$ErrorActionPreference = "Stop"

function myout ($obj) {
    out-string -inputObject $obj -stream -width 200
}
# "(get-host).ui"
# (get-host).ui
# ""
# "(get-host).ui.rawui"
# (get-host).ui.rawui
# ""

function Search-WindowsDesktop-2 ($fields, $filter, $strings)
{
    $connection = New-Object -comObject "ADODB.Connection"
    $recordSet = New-Object -comObject "ADODB.RecordSet"

    $null = $connection.Open("Provider=Search.CollatorDSO;Extended Properties='Application=Windows';") > $null
    #out-string -inputobject $connection
    #out-string -inputobject $recordset.Open

    $ofs = ","
    $query = "SELECT $fields"
    $query += " FROM SYSTEMINDEX $filter"
    myout "query=$query\n"
    $null = $recordSet.Open($query, $connection)

    if ($recordSet.EOF) { return }
    $recordSet.MoveFirst()

    $textfiles = ".org",".txt"

    $numitems = 0
    while (-not $recordSet.EOF) {
        $numitems += 1
        #$result = New-Object Object
        $file = ""
        # foreach ($field in $fields) {
        #     #$result | Add-Member NoteProperty $field $recordSet.Fields.Item($field).Value
        #     $field
        #     $val = $recordSet.Fields.Item($field).Value
        #     #$v = "-----"+$val
        #     #$v
        #     if ($file.Equals("")) { $val = "\" + $val }
        #     $file = $val + $file
        # }
        $filefolder = $recordSet.Fields.Item("System.ItemFolderPathDisplay").Value
        $filename   = $recordSet.Fields.Item("System.FileName").Value
        $file = $filefolder + "\" + $filename
        $itemname = $recordSet.Fields.Item("System.ItemName").Value
        $title = $recordSet.Fields.Item("System.Title").Value
        myout ""
        if ($False) {
            foreach ($field in $fields) {
                $val = $recordSet.Fields.Item($field).Value
                if ($val -and ($val.Length -gt 0)) {
                    myout ("  * " + $field + "=" + $recordSet.Fields.Item($field).Value)
                }
            }
        }
        $v = "-----"+$file
        #$v
        $istext = $False
        foreach ($ext in $textfiles) {
            if ($file.EndsWith($ext)) { $istext = $True }
        }
        if ($istext) {
            myout ("Text file " + $file + " matches:")
            #$res = "text file "+ $file
            #out-host -inputobject $file
            $res = ""
            $re = $strings -join "|"
            $re = "\b(" + $re + ")\b"
            # myout ("re=" + $re)
            $r = Select-String -Path $file -Pattern $re
            if ($r) {
                $max = 100
                foreach ($row in $r) {
                    $row = $row.ToString()
                    $row = "L" + $row.Substring($file.Length)
                    if ($row.Length -gt $max) {
                        $row = $row.Substring(0, $max)
                    }
                    myout $row
                }
            }
        } else {
            myout ("Binary file " + $file + " matches")
        }
        $title = $recordset.Fields.Item("System.Title").Value
        if ($title -and ($title.Length -gt 0)) {
            myout ("  Title: "+$title)
        }
        $author = $recordset.Fields.Item("System.Author").Value
        if (-not ($author -and ($author.Length -gt 0))) {
            $author = $recordset.Fields.Item("System.Itemauthors").Value
        }
        if ($author -and ($author.Length -gt 0)) {
            myout ("  Authors: "+$author)
        }
        #$result
        $recordSet.MoveNext()
    }
    
    myout "----"
    myout "Found $numitems items"

    $null = $recordSet.Close()
    $null = $connection.Close()

    $connection = $null
    $recordSet = $null
}
function Search-WindowsDesktop-1 ($filter, $strings)
{
    # See for example this:
    # - System (Windows)
    #   http://msdn.microsoft.com/en-us/library/ff521735(VS.85).aspx
    # - Desktop Search
    #   http://technet.microsoft.com/en-us/library/ff402341.aspx
    # - Scripting Windows Desktop Search 3.0
    #   http://technet.microsoft.com/en-us/library/ff404224.aspx
    Search-WindowsDesktop-2 `
      "System.Author",
    "System.ContentType",
    "System.CopyRight",
    "System.FileName",
    "System.FileDescription",
    "System.FileExtension",
    "System.ItemAuthors",
    "System.ItemFolderPathDisplay",
    "System.ItemName",
    "System.ItemUrl",
    "System.Keywords",
    "System.Title" `
      $filter $strings
}
function Search-WindowsDesktop ($roots, $strings)
{
    $sfs =
    foreach ($str in $strings) {
        "Contains('""" + $str + """')"
    }
    #$fs
    $filter = "WHERE " + ( $sfs -join " AND " )
    if ($roots) {
        $ffs =
        foreach ($root in $roots) {
            "System.ItemFolderPathDisplay LIKE '" + $root + "%'"
        }
        $filter += " AND (" + ( $ffs -join " OR ") + ")"
    }
    myout ""
    myout $filter
    myout ""
    Search-WindowsDesktop-1 $filter $strings
}

# It looks liek $Args can not be used if a param (...) section gives
# the parameters names.
#
# $Args.Length
# $Args
# $roots
# $strings
#if ($Args.Length -gt 0) {

# myout $roots
# myout $strings
if ($strings) {
    #Search-WindowsDesktop $Args[0] $Args[1]
    if ($Args[2]) {
        help DesktopSearch
    } else {
        # There is a problem with parameter coming from a cmd
        # shell. They will show up as single string in the first array
        # slot.  Test for this and split if it looks like this is the
        # best.  This procedure means that a "," itself can not be
        # included in the search.
        #myout ("strings=" + $strings)
        if ($strings.Length -eq 1) {
            #myout "l=1!"
            $strings = $strings[0].Split(",")
            # $strings.Trim()
            #[string[]]$newstr = []
            foreach ($s in $strings) {
                [string[]]$newstr += $s.Trim()
            }
            #myout $newstr
            # myout $strings
        }
        # Sending \ in filenames fails sometimes so let us assume that
        # we always recieves /.  We must then change / => \ for
        # matching.
        foreach ($r in $roots) {
            [string[]]$newrts += $r.Replace("/", "\")
        }
        Search-WindowsDesktop $newrts $newstr
    }
} else {
    if ($roots) {
        help DesktopSearch
    }}

# process {"Checking pipe, got: $_" }

# "at the end of desktopsearch.ps1"
# exit
