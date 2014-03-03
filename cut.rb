#!/usr/bin/env ruby

# Replacement for unix "cut".  Mainly to properly parse CSV files
# Allows different input/output delimiters
# Allows column specification by name
# Allows column reordering
# Doesn't support column ranges

raise "Requires ruby 1.9.x" if RUBY_VERSION !~ /^1.9/

require 'optparse'
require 'csv'

trap("PIPE") { exit }    # Exit on SIGPIPE

options = {}
OptionParser.new do |opts|
    opts.banner = "Usage: #{$0} [options]"

    options[:in_sep] = "\t"
    options[:out_sep] = "\t"

    opts.on("-d", "--delimiter CHAR", "Delimiter character") do |c|
        options[:in_sep] = c if !options[:in_sep_forced]
        options[:out_sep] = c if !options[:out_sep_forced]
    end

    opts.on("--in-delimiter CHAR", "Force only input delimiter character") do |c|
        options[:in_sep] = c
        options[:in_sep_forced] = true
    end

    opts.on("--out-delimiter CHAR", "Force only output delimiter character") do |c|
        options[:out_sep] = c
        options[:out_sep_forced] = true
    end

    opts.on("--fields 1,2,3", Array, "Fields to select (1-based column numbering)") do |list|
        options[:fields] = list
    end

    opts.on("--headers x,y,z", Array, "Headers to select by name") do |list|
        options[:headers] = list
    end

    opts.on_head("-h", "--help", "Show this message") do
        puts opts
        exit
    end
end.parse!

#print options

CSV($stdout, :col_sep => options[:out_sep]) do |out|
    CSV($stdin, :headers => true, :return_headers=>true, :col_sep => options[:in_sep]) do |in_csv|
        in_csv.each do |line|
            l = []
            set=false
            if options.has_key?(:fields)
                options[:fields].each {|f| l.push(line[f.to_i - 1])}
                set=true
            end
            if options.has_key?(:headers)
                options[:headers].each {|f| l.push(line[f])}
                set=true
            end
            l = line if !set

            out << l
        end
    end
end

