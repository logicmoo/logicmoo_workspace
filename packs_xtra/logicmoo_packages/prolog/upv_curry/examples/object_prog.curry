-- Example about Object Programming

-- Bank account system defined using messages between objects

-- Messages to send and receive by objects
data Message = Deposit Int | Withdraw Int | Balance Int

-- Bank account process
account :: [Message] -> Int -> Constraint
account eval rigid -- account is a consumer
account []                 _ = success
account ((Deposit  a): ms) n = account ms (n+a)
account ((Withdraw a): ms) n = account ms (n-a)
account ((Balance  b): ms) n = b=:=n & account ms n

-- Install an initial account with message stream s:
make_account s = account s 0

-- Send a message to an object identified by its message stream obj:
sendMsg :: [msg] -> msg -> [msg]
sendMsg obj msg | obj =:= msg:obj1 = obj1 where obj1 free

-- Client process of a bank account
client s | s1 =:= sendMsg s (Balance b) =
   if (b==50) then (s1=:=[])                             -- stop
   else if (b>50) then client (sendMsg s1 (Withdraw 30)) -- buy
        else client (sendMsg s1 (Deposit 70))            -- work
   where s1,b free

-- Example goal
bank_account Actions = 
    make_account Actions &                 -- Start bank object
    client (sendMsg Actions (Deposit 100)) -- Start client object with an initial deposit
