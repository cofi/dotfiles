# -*- coding: iso-8859-1 -*-
require 'win32ole'
require 'pathname'


class WdsServer
  # This class manages database connection and queries
  attr_accessor :connection, :data, :fields

  def initialize
    @connection = nil
    @data = nil
  end

  def open
    # Open ADO connection to the WDS Server database
    connection_string =  "Provider=Search.CollatorDSO;"
    connection_string << "Extended Properties='Application=Windows';"
    @connection = WIN32OLE.new('ADODB.Connection')
    @connection.Open(connection_string)
  end

  # fix-me: Split:
  def query(sql)
    # Create an instance of an ADO Recordset
    recordset = WIN32OLE.new('ADODB.Recordset')
    # Open the recordset, using an SQL statement and the existing ADO
    # connection
    recordset.Open(sql, @connection)
    # Create and populate an array of field names
    @fields = []
    recordset.Fields.each do |field|
      @fields << field.Name
    end
    begin
      # Move to the first record/row, if any exist
      recordset.MoveFirst
      # Grab all records
      @data = recordset.GetRows
    rescue
      @data = []
    end
    recordset.Close
    # An ADO Recordset's GetRows method returns an array of columns,
    # so we'll use the transpose method to convert it to an array of
    # rows
    @data = @data.transpose
  end

  def close
    @connection.Close
  end

end

class WdsResult
  # This class holds result data
  def initialize(rootpath, maxw=0)
    @roots = rootpath.split(",")
    @current_dir = Pathname.new(Dir.pwd)
    if maxw == 0
      emacs_w = ENV["EMACS-COMPILE-WINDOW-WIDTH"]
      if emacs_w; maxw = emacs_w.to_i(); else; maxw = 100; end
    end
    @maxw = maxw
    fields = @used_fields.join(", ")
    filters = []
    filters.push scope_filter if scope_filter
    filters.push filter       if filter
    filters.push file_filter  if file_filter
    used_filters = filters.join(" AND ")
    sql = "Select #{fields} From SYSTEMINDEX Where #{used_filters}"
    print used_filters, "\n"
    db = WdsServer.new
    db.open
    db.query(sql)
    @fields = db.fields
    @hits = db.data
    db.close
  end
  def scope_filter
    scope = []
    @roots.each { |i| scope.push "Scope='file:#{i}'" }
    "("+scope.join(" Or ")+")"
  end
end

# Fix-me:
# http://www.i-programmer.info/projects/38-windows/609-windows-search-wds-4.html?start=6
#
# If you use the SCOPE or the DIRECTORY predicates you can control
# where the search is performed. The difference between the two is
# that SCOPE does a deep search, i.e. including all subfolders, but
# DIRECTORY does a shallow search, i.e. just the folder you
# specify. For example:

#  @"WHERE SCOPE='file:C:\Files\Reports'"

# searches C:/Files/Reports and all the subfolders it contains
# whereas:

#  @"WHERE DIRECTORY=
#         'file:C:\Files\Reports'"

# searches just C:\Files\Reports.

# Notice that SCOPE and DIRECTORY don't work as described in the
# documentation. There is no need to put "//" after "file:" and you
# can use either "/" or "\" in the path specification.

class WdsLocateResult < WdsResult
  # This class holds and handles locate results
  def initialize(rootpath, filename, options)
    @for_locate = options[:for_locate]
    @filename = filename
    @used_fields ||= []
    @used_fields.push("SYSTEM.ITEMURL")
    @used_fields.push("SYSTEM.FILENAME")
    super(rootpath)
    @itemurl_num = @fields.index("SYSTEM.ITEMURL")
    @filename_num = @fields.index("SYSTEM.FILENAME")
  end
  def file_filter
    return $null if -1 == @filename
    fns = @filename.split(",")
    fns_like = []
    fns.each { |i|
      fns_like.push("SYSTEM.FILENAME LIKE '#{i}'")
    }
    "("+fns_like.join(" OR ")+")"
  end
  def fullurl (hit)
    url = hit[@itemurl_num]
    return url[5..-1]
  end
  def relurl (hit)
    url = hit[@itemurl_num]
    url = url[5..-1]
    this_path = Pathname.new(url)
    relurl = this_path.relative_path_from(@current_dir)
    return relurl
  end
  def output
    print "\n"
    @hits.each { |hit|
      if @for_locate
        print "#{fullurl(hit)}\n"
      else
        print "* File #{fullurl(hit)} matches\n"
      end
    }
    print "\n"
    print "----\n"
    print "Found ", @hits.length.to_s(), " matching files\n"
  end
end

class WdsSearchResult < WdsLocateResult
  # This class holds and handles search results
  def initialize(rootpath, file_patt, comma_sep_query, show_details, grep_text, options)
    # print "WdsSearchResult.file_patt=", file_patt, "\n"
    txt_ext = options[:txtexts] || [".txt", ".org", ".el"]
    maxw    = options[:maxw]    || 0
    @show_details = options[:details]
    @grep_text    = options[:greptext]
    @query_strings = comma_sep_query.split(",")
    @used_fields ||= []
    @used_fields.push("SYSTEM.AUTHOR")
    @used_fields.push("SYSTEM.ITEMAUTHORS")
    @used_fields.push("SYSTEM.FILEEXTENSION")
    @used_fields.push("SYSTEM.TITLE")
    super(rootpath, file_patt, options)
    @auth_num = @fields.index("SYSTEM.AUTHOR")
    @itemauth_num = @fields.index("SYSTEM.ITEMAUTHORS")
    @filext_num = @fields.index("SYSTEM.FILEEXTENSION")
    @tit_num = @fields.index("SYSTEM.TITLE")
    @txt_ext = txt_ext
  end
  def filter
    query_contains = []
    @query_strings.each { |i| query_contains.push("Contains('\""+i+"\"')") }
    # filter = "WHERE "
    filter = ""
    filter << query_contains.join(" AND ")
    "(#{filter})"
  end
  def title (hit)
    return hit[@tit_num]
  end
  def authors (hit)
    return hit[@auth_num] || hit[@itemauth_num]
  end
  def istxt (hit)
    ext = hit[@filext_num]
    return ext && @txt_ext.index(ext)
  end
  def output
    print "Using Windows Desktop Search\n"
    print "    Search: ", @query_strings, "\n"
    print "  in files: ", @filename, "\n\n"
    # Fix-me: sort!
    # print "hits=", @hits, "\n\n"
    sorted = @hits.sort{|a,b| a[4].downcase <=> b[4].downcase}
    # @hits.each {
    sorted.each {
      |hit|
      if @show_details
        print "\n"
      end
      print "* File ", relurl(hit), " matches\n"
      if @grep_text && istxt(hit)
        re_str = "("+@query_strings.join("|")+")"
        # Translate to regexp
        wild = re_str.split("%")
        # Fix-me: avoiding ruby-mode string fontification bug when ?
        # comes before ending ":
        re_str = wild.join(".*? "[0,-1])
        re = Regexp.new(re_str, 1)
        search_textfile(fullurl(hit), re, @maxw)
      end
      if nil
        for fn in 0..field_names.length-1
          val = hit[fn]
          if val
            print "  ", field_names[fn], "=", val, "\n"
          end
        end
      end
      if @show_details
        title = title(hit)
        if title
          print "  Title:   ", title, "\n"
        end
        authors = authors(hit)
        if authors
          print "  Authors: ", authors.join(", "), "\n"
        end
      end
    }
    print "----\n"
    print "Found ", @hits.length.to_s(), " matching files"
  end
end


### For field names see for example these:
# - System (Windows)
#   http://msdn.microsoft.com/en-us/library/ff521735(VS.85).aspx
# - Desktop Search
#   http://technet.microsoft.com/en-us/library/ff402341.aspx
# - Scripting Windows Desktop Search 3.0
#   http://technet.microsoft.com/en-us/library/ff404224.aspx

### Optional nearly named method params:
# http://devlicio.us/blogs/sergio_pereira/archive/2008/12/31/playing-with-ruby-1-9-name-parameters-sort-of.aspx

def search_textfile (filename, re, maxw)
  # See this blog posts about char encoding:
  #   http://blog.grayproductions.net/articles/ruby_19s_three_default_encodings
  #
  # Let ruby decide input, let output be UTF-8
  search_textfile_1(filename, re, maxw, "r:UTF-8")
end
def search_textfile_1 (filename, re, maxw, openflags)
  # p filename
  file = File.open(filename, openflags)
  row = 0
  maxw = maxw - 12
  # print "maxw=", maxw.to_s(), "\n"
  #file.each_line do |line|; end
  file.each_line do |line|
    p [line.encoding.name, line] unless "UTF-8" == line.encoding.name
    row += 1
    line.chomp!
    begin
      matchdata = re.match(line)
    rescue
      matchdata = $null
      print "Problem matching line below, #{$!}:\n"
      p line
    end
    if matchdata
      col = matchdata.begin(0)
      cnd = matchdata.end(0)
      if (maxw > 0) && (line.length > maxw)
        len = cnd - col
        if len > maxw
          show = "_"+line[col, maxw-3]+"_"
          part = "a"
        else
          pad = (maxw - len) / 2
          # print "col="+col.to_s()+", cnd="+cnd.to_s()+", pad="+pad.to_s()+"\n"
          start = col-pad
          start = 0 if start < 0
          show = "_"+line[start, maxw-3]+"_"
          part = "b"
        end
      else
        show = line
        part = "c"
      end
      mshow = ""
      while md = re.match(show)
        b0 = md.begin(0)
        e0 = md.end(0)
        mshow += show[0..b0-1]+
          "{{{"+
          show[b0..e0-1]+
          "}}}"
        show = show[e0..-1]
      end
      mshow += show
      #row_col = "L:"+row.to_s()+":"+col.to_s()+":"
      col += 1
      row_col = part+""+row.to_s()+":"+col.to_s()+":"
      space = "     "[row_col.length-6..-1]
      print "", row_col, space, " ", mshow, "\n"
      raise "Too wide!: "+part+", "+ mshow.length.to_s() if (show.length > maxw && maxw > 0)
    end
  end
end
if __FILE__ == $0
  $LOAD_PATH.push File.expand_path(File.dirname(__FILE__))
  require 'trollop'
  opts = Trollop::options do
    opt :root,   "The root dir", :type => :string
    opt :query,  "The query", :type => :string
    opt :locate, "Locate files", :type => :string
    opt :filepatt, "Match file names", :type => :string
    opt :details, "Show details"
    opt :greptext, "Grep in text files"
  end
  Trollop::die :root, "must be specified" unless opts[:root]
  # Trollop::die :query, "must be specified" unless opts[:query]
  if opts[:query]
    WdsSearchResult.new(opts[:root], opts[:filepatt], opts[:query], opts[:details], opts[:greptext], maxw: -1).output
  else
    WdsLocateResult.new(opts[:root], opts[:filepatt], 
                        for_locate: opts[:locate] == "locate"
                        ).output
  end
end

### Sweat tutorial:
# http://www.fincher.org/tips/Languages/Ruby/

# Local variables:
# coding: utf-8
# End:
