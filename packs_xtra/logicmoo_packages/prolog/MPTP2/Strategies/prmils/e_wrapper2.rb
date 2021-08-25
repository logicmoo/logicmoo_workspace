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


cwa_cp_1 => "ClauseWeightAge(ConstPrio,1,1,0.7,3"
cwa_cp_2 => "ClauseWeightAge(ConstPrio,1,1,1,3"
cw_bcd => "Clauseweight(ByCreationDate,2,1,0.8"
cw_cp_1 => "Clauseweight(ConstPrio,1,1,0.7"
cw_cp_2 => "Clauseweight(ConstPrio,1,1,1",
cw_cp_3 => "Clauseweight(ConstPrio,3,1,1",
cw_pp => "Clauseweight(PreferProcessed,1,1,1",
cw_pugg => "Clauseweight(PreferUnitGroundGoals,1,1,1",
cgsw_pg => "ConjectureGeneralSymbolWeight(PreferGoals,100,100,100,50,50,10,50,1.5,1.5,1",
cgsw_pgg_1 => "ConjectureGeneralSymbolWeight(PreferGroundGoals,100,100,100,50,50,10,100,1.5,1.5,1",
cgsw_pgg_2 => "ConjectureGeneralSymbolWeight(PreferGroundGoals,100,100,100,50,50,10,50,1.5,1.5,1",
cgsw_png_1 => "ConjectureGeneralSymbolWeight(PreferNonGoals,100,100,100,50,50,0,100,1.5,1.5,1",
cgsw_png_2 => "ConjectureGeneralSymbolWeight(PreferNonGoals,100,100,100,50,50,1,100,1.5,1.5,1",
cgsw_png_3 => "ConjectureGeneralSymbolWeight(PreferNonGoals,100,100,100,50,50,10,100,1.5,1.5,1",
cgsw_png_4 => "ConjectureGeneralSymbolWeight(PreferNonGoals,100,100,100,50,50,100,100,1.5,1.5,1",
cgsw_png_5 => "ConjectureGeneralSymbolWeight(PreferNonGoals,100,100,100,50,50,1000,100,1.5,1.5,1",
cgsw_png_6 => "ConjectureGeneralSymbolWeight(PreferNonGoals,100,100,100,50,50,5000,100,1.5,1.5,1",
cgsw_png_7 => "ConjectureGeneralSymbolWeight(PreferNonGoals,200,100,200,50,50,1,100,1.5,1.5,1",
cgsw_sos_1 => "ConjectureGeneralSymbolWeight(SimulateSOS, 100,100,100,50,50,0,50,1.5,1.5,1",
cgsw_sos_2 => "ConjectureGeneralSymbolWeight(SimulateSOS,100,100,100,50,50,10,50,1.5,1.5,1",
cgsw_sos_3 => "ConjectureGeneralSymbolWeight(SimulateSOS,100,100,100,50,50,20,50,1.5,1.5,1",
cgsw_sos_4 => "ConjectureGeneralSymbolWeight(SimulateSOS,100,100,100,50,50,50,50,1.5,1.5,1",
cgsw_sos_5 => "ConjectureGeneralSymbolWeight(SimulateSOS,100,100,100,50,50,5000,50,1.5,1.5,1",
crsw_cp_1 => "ConjectureRelativeSymbolWeight(ConstPrio,0.05, 100, 100, 100, 100, 1.5, 1.5, 1.5",
crsw_cp_2 => "ConjectureRelativeSymbolWeight(ConstPrio,0.1, 100, 100, 100, 100, 1.5, 1.5, 1.5",
crsw_cp_3 => "ConjectureRelativeSymbolWeight(ConstPrio,0.2, 100, 100, 100, 100, 1.5, 1.5, 1",
crsw_cp_4 => "ConjectureRelativeSymbolWeight(ConstPrio,0.2, 100, 100, 100, 100, 1.5, 1.5, 1.5",
crsw_cp_5 => "ConjectureRelativeSymbolWeight(ConstPrio,0.3, 100, 100, 100, 100, 2.5, 1, 1",
crsw_cp_6 => "ConjectureRelativeSymbolWeight(ConstPrio,0.5, 100, 100, 100, 100, 1.5, 1.5, 1",
crsw_cp_7 => "ConjectureRelativeSymbolWeight(ConstPrio,0.5, 100, 100, 100, 50, 1.5, 0.5, 1.5",
crsw_cp_8 => "ConjectureRelativeSymbolWeight(ConstPrio,0.5, 100, 100, 100, 50, 1.5, 1.5, 1",
crsw_cp_9 => "ConjectureRelativeSymbolWeight(ConstPrio,0.5, 100, 100, 100, 50, 1.5, 1.5, 1.5",
crsw_cp_10 => "ConjectureRelativeSymbolWeight(ConstPrio,0.7, 100, 100, 100, 100, 1.5, 1.5, 1",
crsw_png => "ConjectureRelativeSymbolWeight(PreferNonGoals,0.5, 100, 100, 100, 100, 1.5, 1.5, 1",
crsw_sos_1 => "ConjectureRelativeSymbolWeight(SimulateSOS,0.05, 100, 100, 100, 100, 1.5, 1.5, 1.5",
crsw_sos_2 => "ConjectureRelativeSymbolWeight(SimulateSOS,0.5, 100, 100, 100, 100, 1.5, 1.5, 1",
csw_cp => "ConjectureSymbolWeight(ConstPrio,10,10,5,5,5,1.5,1.5,1.5",
dw_sos => "Defaultweight(SimulateSOS",
=> "F1",
=> "F2",
fw_bnlt => "FIFOWeight(ByNegLitDist",
fw_cp => "FIFOWeight(ConstPrio",
fw_pp => "FIFOWeight(PreferProcessed",
fw_pu => "FIFOWeight(PreferUnits",
fw_sos => "FIFOWeight(SimulateSOS",
=> "Garity",
=> "Ginvarity",
=> "Ginvfreq",
=> "Ginvfreqconstmin",
=> "Ginvfreqhack",
=> "Gunary_first",
olmw_cp => "OrientLMaxWeight(ConstPrio,2,1,2,1,1",
pnrw_pg_1 => "PNRefinedweight(PreferGoals,1,1,1,2,2,2,0.5",
pnrw_pg_2 => "PNRefinedweight(PreferGoals,5,2,2,5,2,1,0.5",
pnrw_png_1 => "PNRefinedweight(PreferNonGoals,2,1,1,1,2,2,2",
pnrw_png_2 => "PNRefinedweight(PreferNonGoals,4,5,5,4,2,1,1",
rw_pg_1 => "Refinedweight(PreferGoals,1,2,2,1,0.8",
rw_pg_2 => "Refinedweight(PreferGoals,1,2,2,2,0.5",
rw_pg_3 => "Refinedweight(PreferGoals,1,2,2,2,2",
rw_pgg => "Refinedweight(PreferGroundGoals,2,1,2,1.0,1",
rw_png_1 => "Refinedweight(PreferNonGoals,1,1,2,1.5,1.5",
rw_png_2 => "Refinedweight(PreferNonGoals,2,1,2,2,0.5",
rw_png_3 => "Refinedweight(PreferNonGoals,2,1,2,2,2",
rw_png_4 => "Refinedweight(PreferNonGoals,2,1,2,3,0.8",
rw_sos => "Refinedweight(SimulateSOS,1,1,2,1.5,2",
stw_cp_1 => "SymbolTypeweight(ConstPrio,1,200,2,30,1.5,1.5,0.8",
stw_cp_2 => "SymbolTypeweight(ConstPrio,7,20,0,0,1.5,5,0.8",
=> "WNoGeneration",
=> "WNoSelection",
=> "WPSelectComplexExceptRRHorn",
=> "WPSelectNewComplexAHPExceptRRHorn",
=> "WPSelectNewComplexAHPExceptUniqMaxHorn",
=> "WSelectComplexAHP",
=> "WSelectComplexAHPExceptRRHorn",
=> "WSelectComplexG",
=> "WSelectMaxLComplexAvoidPosPred",
=> "WSelectNewComplex",
=> "WSelectNewComplexAHP",
=> "WSelectNewComplexAHPExceptRRHorn",
=> "WSelectNewComplexAHPExceptUniqMaxHorn",
=> "backward-context-sr",
=> "c1",
=> "destructive-er",
=> "destructive-er-aggressive",
=> "forward-context-sr",
=> "forward-context-sr-aggressive",
=> "oriented-simul-paramod",
=> "simul-paramod",
=> "prefer-initial-clauses",
=> "select-on-processing-only",
=> "simplify-with-unprocessed-units",
=> "sos-uses-input-types",
=> "split-aggressive",
=> "split-clauses=12",
=> "split-clauses=2",
=> "split-clauses=4",
=> "split-clauses=7",
=> "split-clauses=8",
=> "split-reuse-defs",
=> "tLPO4",
=> "warity",
=> "waritysquared",
=> "wconstant",
=> "winvfreqrank",
=> "winvmodfreqrankmax0",
=> "wmodarity",
=> "wprecedence"


"definitional-cnf=24"
"delete-bad-limit=150000000"
"tstp-in"
"memory-limit=192"
"print-pid"
"print-statistics"
"resources-info"
"s"





























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
  if ARGV[a] == "-rwsos"
    rwsos = ARGV[a+1]
    if rwsos.to_i > 0
      rwsos_s = "#{rwsos}*Refinedweight(SimulateSOS,1,1,2,1.5,2)"
      heurparms << rwsos_s
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

params1 = " -s -R --memory-limit=Auto --print-statistics --definitional-cnf=24 --tstp-format #{splaggr_s} #{splcl_s}  #{simparamod_s} --forward-context-sr --destructive-er-aggressive --destructive-er --prefer-initial-clauses -t#{tord} #{prord} -F1 --delete-bad-limit=150000000 -W#{sel} -H'(" + heur + ")' --cpu-limit=#{cutoff_time} #{infilename}"

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
