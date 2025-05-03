data Message = Deposit Int | Withdraw Int | Balance Int

account :: Int -> [Message] -> Success
account _ []                = success
account n (Deposit a : ms)  = account (n + a) ms
account n (Withdraw a : ms) = account (n - a) ms
account n (Balance b : ms)  = b =:= n & account n ms

make_account s = account 0 (ensureSpine s)

goal1 b = let s free in
    make_account s & s =:= [Deposit 200, Deposit 50, Balance b]
goal2 b = let s free in
    make_account s & s =:= [Deposit 200, Withdraw 100, Deposit 50, Balance b]

sendMsg msg obj | obj =:= msg:obj1 = obj1 where obj1 free

client s | s1 =:= sendMsg(Balance b) s =
    if b == 50 then
        s1 =:= []
    else if b > 50 then
        client (sendMsg (Withdraw 30) s1)
    else
        client (sendMsg (Deposit 70) s1)
    where s1, b free

goal3 s = make_account s & client (sendMsg (Deposit 100) s)

main = do
    print (goal1 250)
    print (goal2 150)
    print (goal3 [Deposit 100, Balance 100, Withdraw 30, Balance 70, Withdraw 30, Balance 40, Deposit 70, Balance 110, Withdraw 30, Balance 80, Withdraw 30, Balance 50])
