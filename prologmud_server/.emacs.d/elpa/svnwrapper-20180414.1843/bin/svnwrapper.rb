###############################################################################
# Wrapper for "svn", adds paging and syntax highlighting.
#
# This file assumes that the page "less" is installed and configured to
# use the package "e2ansi".
#
# Usage:
#
# svnwrapper [args-to-svnwrapper] [args-to-less ... ----] args-to-svn ...
#
# Arguments:
#
#  --forward-slashes  Make 'svn status' and 'svn update' output forward slashes
#  --svn-command CMD  Use CMD when running svn, defaults to "svn".
#
# Example:
#
#    alias svn="ruby PATH-TO/svnwrapper.rb -E -X ----"
#

# ------------------------------
# svnwrapper:s own options.
#

opt_forward_slashes = false
opt_svn_cmd = "svn"

while !ARGV.empty?
  case ARGV[0]
  when "--forward-slashes"
    ARGV.shift
    opt_forward_slashes = true
  when "--svn-command"
    ARGV.shift
    if ARGV.empty?
      puts "Expect argument after --svn-command"
      exit(1)
    end
    opt_svn_cmd = ARGV.shift
  else
    break
  end
end


# ------------------------------
# Split arguments to 'less' and 'svn'.
#
# The option "----" is used as a barrier, as it is unlikey that it
# will be used as an argument to either package.

if (idx = ARGV.index("----")) != nil
  args_to_less = ARGV[0...idx]
  args_to_svn = ARGV[(idx + 1)..-1]
else
  args_to_less = []
  args_to_svn = ARGV
end


# ------------------------------
# Help class, find subcommand and file names in option list.
class ArgumentParser
  def initialize(args)
    @remaining = args.clone
  end

  # Remove the next option and return it. Return nil when there are no
  # more options.
  def next_argument
    if @remaining.empty?
      return nil
    else
      return @remaining.shift
    end
  end

  def next_non_option
    while (arg = next_argument)
      case arg
      when "-r", "--username", "--password", "--config-dir", "--config-option"
        next_argument
      when /^-/
      else
        return arg
      end
    end
    return nil
  end
end

arg_parse = ArgumentParser.new(args_to_svn)


# ------------------------------
# Select action depending on the svn subcommand used.
#

pipe_to_less = false
convert_slashes = false

case arg_parse.next_non_option
when "blame", "di", "diff", "log", "praise"
  pipe_to_less = true
when "cat"
  pipe_to_less = true
  # Help Emacs pick a suitable major mode.
  if (filename = arg_parse.next_non_option)
    if (md = /^([^@]+)@/.match(filename))
      filename = md[1]
    end
    ENV["E2ANSI_FILE_NAME"] = File.basename(filename)
  end
when "st", "status"
  pipe_to_less = true
  convert_slashes = opt_forward_slashes
  # Help Emacs pick a suitable major mode.
  #
  # svnwrapper.el' maps the extension .svnstatus to the major mode
  # `svnwrapper-svn-status-mode'.
  ENV["E2ANSI_FILE_NAME"] = "dummy.svnstatus"
when "up", "update"
  # Note: This is not piped to "less" since it is more important that
  # an update proceeds than that the output is paged.
  convert_slashes = opt_forward_slashes
end


# ------------------------------
# And ... action!
#
# Run "svn"

# puts args_to_svn.inspect
# puts args_to_less.inspect

def quote(*args)
  arr = args.map {|x| '"' + x + '"'}
  return arr.join(" ")
end

cmd = "#{opt_svn_cmd} #{quote(*args_to_svn)}"

if convert_slashes
  cmd += " | ruby -e \"STDIN.each { |line| puts line.gsub('\\\\','/') }\""
end

if pipe_to_less && STDOUT.isatty
  cmd += " | less #{quote(*args_to_less)}"
end

system(cmd)
