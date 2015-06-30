#!/usr/bin/env ruby

require 'find'

quiet = 1
min_size = 1 * 1024 * 1024 * 1024  #1 GB
min_saving = 500 * 1024 * 1024     #500MB

class Integer
   def to_filesize
     conv={
       1024=>'B',
       1024*1024=>'KB',
       1024*1024*1024=>'MB',
       1024*1024*1024*1024=>'GB',
       1024*1024*1024*1024*1024=>'TB',
       1024*1024*1024*1024*1024*1024=>'PB',
       1024*1024*1024*1024*1024*1024*1024=>'EB'
     }
     conv.keys.sort.each { |mult|
        next if self >= mult
        suffix=conv[mult]
        return "%.2f %s" % [ self / (mult / 1024), suffix ]
     }
   end
end


Find.find('.') do |file|

  if FileTest.file?(file)
    if file !~ /\.(gz|zip|tgz)$/
      sz = File.size(file)
      if sz > min_size
        STDERR.write "Considering #{file}... "
        sz2 = %x(pigz -c '#{file}' | wc -c).to_i
        STDERR.write "before=#{sz.to_filesize} after=#{sz2.to_filesize} saving=#{(sz-sz2).to_filesize}... "
        if (sz - sz2 >= min_saving)
          STDERR.puts "Compress it!"
        else
          STDERR.puts "Not worth it."
        end
      end
    end
  elsif FileTest.directory?(file)
    STDERR.puts "Doing #{file}" unless quiet
  end

end

