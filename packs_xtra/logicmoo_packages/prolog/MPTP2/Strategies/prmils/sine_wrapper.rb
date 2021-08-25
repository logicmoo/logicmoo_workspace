#=== Deal with inputs.
if ARGV.length < 5
        puts "sine_wrapper.rb is a wrapper for the SAPS algorithm."
        puts "Usage: ruby sine_wrapper.rb <instance_relname> <instance_specifics> <cutoff_time> <cutoff_length> <seed> <params to be passed on>."
        exit -1
end
infilename = ARGV[0]
instance_specifics = ARGV[1]
cutoff_time = ARGV[2].to_i
cutoff_length = ARGV[3].to_i
seed = ARGV[4].to_i

#=== Here I assume instance_specifics only contains the desired target quality or nothing at all for the instance, but it could contain more (to be specified in the instance_file or instance_seed_file)
if instance_specifics == ""
        qual = 0
else
        qual = instance_specifics.split[0]
end

a = 5
while a < ARGV.length
  if ARGV[a] == "-depth"
    depth = ARGV[a+1]
  end
  if ARGV[a] == "-benevolence"
    ben = ARGV[a+1]
  end
  if ARGV[a] == "-hyp"
    hyp = ARGV[a+1]
  end
  if ARGV[a] == "-size"
    size = ARGV[a+1]
  end
  if ARGV[a] == "-fraction"
    frac = ARGV[a+1]
  end
  a = a+2
end


#paramstring = ARGV[6] + "," + ARGV[8] + ",9223372036854775807, " + ARGV[10] + "," + ARGV[12] + "," + ARGV[14]

#ARGV[5...ARGV.length].join(" ")

#=== Build algorithm command and execute it.
# cmd = "./ubcsat -alg saps #{paramstring} -inst #{cnf_filename} -cutoff #{cutoff_length} -timeout #{cutoff_time} -target #{qual} -seed #{seed} -r stats stdout default,best"
#cmd = "eprover1.6tst2  -s -R --cpu-limit=#{cutoff_time} --print-statistics --tstp-in --sine='GSinE(CountFormulas, #{paramstring} )' -tAuto -xAuto #{infilename}"

cmd = "eprover1.6tst2  -s -R --cpu-limit=#{cutoff_time} --print-statistics --tstp-in --sine='GSinE(CountFormulas, #{hyp}, #{ben}, 9223372036854775807, #{depth}, #{size}, #{frac}  )' -tAuto -xAuto #{infilename}"

filename = "sine_output#{rand}.txt"
exec_cmd = "#{cmd} > #{filename}"

puts "Calling: #{exec_cmd}"
system exec_cmd

#=== Parse algorithm output to extract relevant information for ParamILS.
solved = "ResourceOut"
runtime = 100
runlength = 1000000
best_sol = 1000000


File.open(filename){|file|
        while line = file.gets
                if line =~/ResourceOut/
                        numsolved = 0
                        runtime = 100
                        runlength = 1000000
                        best_sol = 1000000
                end
                if line =~ /CounterSatisfiable/
                        solved = "CounterSatisfiable"
                end
                if line =~ /Theorem/
                        solved = "Theorem"
                        numsolved = 1
                end
                if line =~ /Processed clauses.*: *(\d+)/
                        runlength = $1.to_i
                end
                if line =~ /User time                : *([0-9.]+) s/
                        runtime = $1.to_i
                end
        end
        if solved == "CounterSatisfiable"
                numsolved = 0
                runtime = 100
                runlength = 1000000
                best_sol = 1000000
        end
}
File.delete(filename)
puts "Result for ParamILS: #{solved}, #{runtime}, #{runlength}, #{best_sol}, #{seed}"
