#!/usr/bin/env ruby

$prologRef = nil
$prologTempDir = "/Users/daniel/Desktop/Temp/"
$prologExe = "/opt/local/bin/swipl"
$isHalted = false

def StartProlog
    $prologRef = IO.popen($prologExe,"w")
    $isHalted = false
end

def StopProlog
    $prologRef.close_write
    $isHalted = true
end

def CallProlog(statement)
    $prologRef.puts "call((" + statement + "))."
end

def IsTrueProlog(statement)
        t = Time.now
    srand(t.to_i)
    randNum = rand(10000)
    $prologRef.puts "tell('" + $prologTempDir + "TEST" + randNum.to_s + "')."
    $prologRef.puts "(" + statement + ") -> write('true') ; write('false')."
    $prologRef.puts "told."
    accumulate = ""
    isRead = false
    while not isRead
        if $isHalted == true
            return false
        end        
        begin
            File.open($prologTempDir + "TEST" + randNum.to_s) do |query_result|
                query_result.each do |line|
                accumulate += line
                end
                isRead = true
            end
        rescue
            
        end
    end
    File.delete($prologTempDir + "TEST" + randNum.to_s)
    if accumulate != "true"
        return false
    else
        return true
    end
end

def QueryProlog(variablelist,query)
    t = Time.now
    srand(t.to_i)
    randNum = rand(10000)
    $prologRef.puts "tell('" + $prologTempDir + "QUERY" + randNum.to_s + "')."
    $prologRef.puts "findall((" + variablelist + "),(" + query + "),Z),write(Z),fail."
    $prologRef.puts "told."
    accumulate = ""
    isRead = false
    while not isRead
        if $isHalted == true
            return ""
        end        
        begin
            File.open($prologTempDir + "QUERY" + randNum.to_s) do |query_result|
                query_result.each do |line|
                    accumulate += line
                end
            isRead = true
            end
        rescue
            puts "error"
        end
    end
    File.delete($prologTempDir + "QUERY" + randNum.to_s)
    return accumulate
end


StartProlog()
CallProlog("assert(dog(dan))")
CallProlog("assert(dog(jim))")
CallProlog("assert(dog(pete))")

for i in 0 ... 100
    CallProlog("assert(person(fname(dan" + i.to_s + "),lname(sull" + i.to_s + ")))")
end

f = File.open("/Users/daniel/Desktop/Temp/results.txt","w")

f.puts QueryProlog("X,Y","person(X,Y)")

f.puts QueryProlog("X","dog(X)")

f.puts IsTrueProlog("dog(ddddddan)")

f.close_write

StopProlog()

