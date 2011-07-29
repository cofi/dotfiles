require 'win32ole'

used_fields = []
used_fields.push("System.Author")
# If FullText below is commented out there is no error
used_fields.push("System.FullText")
fields = used_fields.join(", ")

filter = "WHERE Contains('aaa')"

query = "SELECT " + fields + " FROM SYSTEMINDEX " + filter
print "query=", query, "\n"

# Connect to Windows Desktop Search:
connection_string =  "Provider=Search.CollatorDSO;"
connection_string << "Extended Properties='Application=Windows';"

connection = WIN32OLE.new('ADODB.Connection')
connection.Open(connection_string)

recordset = WIN32OLE.new('ADODB.Recordset')
  print "Before recordset.Open\n"
recordset.Open(query, connection)
  print "After recordset.Open\n"
recordset.Close

connection.Close
