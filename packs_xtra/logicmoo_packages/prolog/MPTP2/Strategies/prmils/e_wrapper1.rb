#=== Deal with inputs.
if ARGV.length < 5
        puts "e_wrapper1.rb is a wrapper for the SAPS algorithm."
        puts "Usage: ruby e_wrapper1.rb <instance_relname> <instance_specifics> <cutoff_time> <cutoff_length> <seed> <params to be passed on>."
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

# splaggr { 0, 1 } [1]
# splcl { 4, 7 } [4]
# prord { arity, invfreq, invfreqconstmin } [invfreqconstmin]
# tord { LPO4, KBO} [LPO4]
# sel {SelectMaxLComplexAvoidPosPred, SelectNewComplexAHP, SelectComplexG} [SelectMaxLComplexAvoidPosPred]
# crswcp  {0, 1, 2, 4, 6, 8, 10} [0]
# crswsos {0,1,2,3,4,6,8} [0]
# crswng  {0,1,2,3,4,6,8} [0]
# rwsos { 0,1,2,4,6,8,10 } [4]
# rwng { 0,1,2,3,4,6,8,10 } [3]
# cwproc { 0,1,2 } [1]
# fwcp { 0, 1, 2} [0]
# fwproc { 1,2 } [1]

splaggr_s = ""
splcl_s = ""
crswcp_s = ""
crswsos_s = ""
crswng_s = ""
rwsos_s = ""
rwng_s = ""
cwproc_s  = ""
fwcp_s = ""
fwproc_s = ""
forwardcntxtsr_s = ""
simparamod_s =""
srd_s = ""
cwcp_s = ""
rwpgg_s = ""
cwbcd_s = ""
cswcp_s = ""


heurparms = []

a = 5
while a < ARGV.length
  if ARGV[a] == "-prord"
    prord = ARGV[a+1]
    if prord == "invfreq"
      prord = "-winvfreqrank -c1 -Ginvfreq"
    else
      prord = "-G" + prord
    end
  end
  if ARGV[a] == "-tord"
    tord = ARGV[a+1]
  end
  if ARGV[a] == "-sel"
    sel = ARGV[a+1]
  end
  if ARGV[a] == "-splaggr"
    splaggr = ARGV[a+1]
    if splaggr.to_i > 0
      splaggr_s = "--split-aggressive"
    end
  end
  if ARGV[a] == "-srd"
    srd = ARGV[a+1]
    if srd.to_i > 0
      srd_s = "--split-reuse-defs"
    end
  end
  if ARGV[a] == "-simparamod"
    simparamod =  ARGV[a+1]
    if simparamod != "none"
      if simparamod == "oriented"
        simparamod_s = "--oriented-simul-paramod"
      else
        simparamod_s = "--simul-paramod"
      end
    end
  end
  if ARGV[a] == "-splcl"
    splcl =  ARGV[a+1]
    if splcl.to_i > 0
      splcl_s = "--split-clauses=#{splcl}"
    end
  end
  if ARGV[a] == "-forwardcntxtsr"
    forwardcntxtsr =  ARGV[a+1]
    if forwardcntxtsr.to_i > 0
      forwardcntxtsr_s = "--forward-context-sr"
    end
  end
  if ARGV[a] == "-crswcp"
    crswcp = ARGV[a+1]
    if crswcp.to_i > 0
      crswscp_s = "#{crswcp}*ConjectureRelativeSymbolWeight(ConstPrio,0.1, 100, 100, 100, 100, 1.5, 1.5, 1.5)"
      heurparms << crswscp_s
    end
  end
  if ARGV[a] == "-crswsos"
    crswsos = ARGV[a+1]
    if crswsos.to_i > 0
      crswsos_s = "#{crswsos}*ConjectureRelativeSymbolWeight(SimulateSOS,0.5, 100, 100, 100, 100, 1.5, 1.5, 1)"
      heurparms << crswsos_s
    end
  end
  if ARGV[a] == "-crswng"
    crswng = ARGV[a+1]
    if crswng.to_i > 0
      crswng_s = "#{crswng}*ConjectureRelativeSymbolWeight(PreferNonGoals,0.5, 100, 100, 100, 100, 1.5, 1.5, 1)"
      heurparms << crswng_s
    end
  end
  if ARGV[a] == "-cswcp"
    cswcp = ARGV[a+1]
    if cswcp.to_i > 0
      cswcp_s = "#{cswcp}*ConjectureSymbolWeight(ConstPrio,10,10,5,5,5,1.5,1.5,1.5)"
      heurparms << cswcp_s
    end
  end
  if ARGV[a] == "-rwsos"
    rwsos = ARGV[a+1]
    if rwsos.to_i > 0
      rwsos_s = "#{rwsos}*Refinedweight(SimulateSOS,1,1,2,1.5,2)"
      heurparms << rwsos_s
    end
  end
  if ARGV[a] == "-cwbcd"
    cwbcd = ARGV[a+1]
    if cwbcd.to_i > 0
      cwbcd_s = "#{cwbcd}*Clauseweight(ByCreationDate,2,1,0.8)"
      heurparms << cwbcd_s
    end
  end
  if ARGV[a] == "-rwng"
    rwng = ARGV[a+1]
    if rwng.to_i > 0
      rwng_s = "#{rwng}*Refinedweight(PreferNonGoals,1,1,2,1.5,1.5)"
      heurparms << rwng_s
    end
  end
  if ARGV[a] == "-cwproc"
    cwproc = ARGV[a+1]
    if cwproc.to_i > 0
      cwproc_s = "#{cwproc}*Clauseweight(PreferProcessed,1,1,1)"
      heurparms << cwproc_s
    end
  end
  if ARGV[a] == "-cwcp"
    cwcp = ARGV[a+1]
    if cwcp.to_i > 0
      cwcp_s = "#{cwcp}*Clauseweight(ConstPrio,3,1,1)"
      heurparms << cwcp_s
    end
  end
  if ARGV[a] == "-rwpgg"
    rwpgg = ARGV[a+1]
    if rwpgg.to_i > 0
      rwpgg_s = "#{rwpgg}*Refinedweight(PreferGroundGoals,2,1,2,1.0,1)"
      heurparms << rwpgg_s
    end
  end
  if ARGV[a] == "-fwcp"
    fwcp = ARGV[a+1]
    if fwcp.to_i > 0
      fwcp_s = "#{fwcp}*FIFOWeight(ConstPrio)"
      heurparms << fwcp_s
    end
  end
  if ARGV[a] == "-fwproc"
    fwproc = ARGV[a+1]
    if fwproc.to_i > 0
      fwproc_s = "#{fwproc}*FIFOWeight(PreferProcessed)"
      heurparms << fwproc_s
    end
  end
  a = a+2
end


#paramstring = ARGV[6] + "," + ARGV[8] + ",9223372036854775807, " + ARGV[10] + "," + ARGV[12] + "," + ARGV[14]

#ARGV[5...ARGV.length].join(" ")

#=== Build algorithm command and execute it.
# cmd = "./ubcsat -alg saps #{paramstring} -inst #{cnf_filename} -cutoff #{cutoff_length} -timeout #{cutoff_time} -target #{qual} -seed #{seed} -r stats stdout default,best"
#cmd = "eprover1.6tst2  -s -R --cpu-limit=#{cutoff_time} --print-statistics --tstp-in --sine='GSinE(CountFormulas, #{paramstring} )' -tAuto -xAuto #{infilename}"



heur = heurparms.join(",")

# crswsos_s + crswcp_s + crswng_s + rwsos_s + rwng_s + cwproc_s + fwcp_s + fwproc_s 

# heur = "#{crswsos}*ConjectureRelativeSymbolWeight(SimulateSOS,0.5, 100, 100, 100, 100, 1.5, 1.5, 1),#{crswng}*ConjectureRelativeSymbolWeight(PreferNonGoals,0.5, 100, 100, 100, 100, 1.5, 1.5, 1),#{rwsos}*Refinedweight(SimulateSOS,1,1,2,1.5,2),#{rwng}*Refinedweight(PreferNonGoals,1,1,2,1.5,1.5),#{cwproc}*Clauseweight(PreferProcessed,1,1,1),#{fwproc}*FIFOWeight(PreferProcessed)"

params1 = " -s -R --memory-limit=Auto --print-statistics --definitional-cnf=24 --tstp-format #{splaggr_s} #{splcl_s} #{srd_s} #{simparamod_s} #{forwardcntxtsr_s} --destructive-er-aggressive --destructive-er --prefer-initial-clauses -t#{tord} #{prord} -F1 --delete-bad-limit=150000000 -W#{sel} -H'(" + heur + ")' --cpu-limit=#{cutoff_time} #{infilename}"

# 4*Refinedweight(SimulateSOS,1,1,2,1.5,2),3*Refinedweight(PreferNonGoals,1,1,2,1.5,1.5),1*Clauseweight(PreferProcessed,1,1,1),1*FIFOWeight(PreferProcessed))' -s --print-statistics --print-pid --resources-info --memory-limit=192 -s -R --cpu-limit=5 --memory-limit=Auto --tstp-format --print-statistics

# 4*ConjectureRelativeSymbolWeight(SimulateSOS,0.5, 100, 100, 100, 100, 1.5, 1.5, 1),3*ConjectureRelativeSymbolWeight(PreferNonGoals,0.5, 100, 100, 100, 100, 1.5, 1.5, 1)

cmd = "eprover1.6tst2  #{params1} "

# cmd = "eprover1.6tst2  -s -R --cpu-limit=#{cutoff_time} --print-statistics --tstp-in --sine='GSinE(CountFormulas, #{hyp}, #{ben}, 9223372036854775807, #{depth}, #{size}, #{frac}  )' -tAuto -xAuto #{infilename}"

filename = "e_output#{rand}.txt"
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
