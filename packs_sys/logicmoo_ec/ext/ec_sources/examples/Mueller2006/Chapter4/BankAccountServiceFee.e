;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort account
sort value: integer

account Account1, Account2

predicate EndOfMonth(time)
function ServiceFee(account): value
function MinimumBalance(account): value

fluent ServiceFeeCharged(account)
fluent Balance(account,value)

event Transfer(account,account,value)
event MonthlyReset(account)
event ChargeServiceFee(account)

; Sigma

[account1,account2,value1,value2,value3,value4,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 &
value4=(value2+value3) ->
Initiates(Transfer(account1,account2,value3),Balance(account2,value4),time).

[account1,account2,value1,value2,value3,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 ->
Terminates(Transfer(account1,account2,value3),Balance(account2,value2),time).

[account1,account2,value1,value2,value3,value4,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 &
value4=(value1-value3) ->
Initiates(Transfer(account1,account2,value3),Balance(account1,value4),time).

[account1,account2,value1,value2,value3,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 ->
Terminates(Transfer(account1,account2,value3),Balance(account1,value1),time).

[account,time]
Initiates(ChargeServiceFee(account),ServiceFeeCharged(account),time).

[account,time]
Terminates(MonthlyReset(account),ServiceFeeCharged(account),time).

[account,value1,value2,time]
HoldsAt(Balance(account,value1),time) &
value2 = (value1-ServiceFee(account)) ->
Initiates(ChargeServiceFee(account),
          Balance(account,value2),
          time).

[account,value,time]
HoldsAt(Balance(account,value),time) ->
Terminates(ChargeServiceFee(account),Balance(account,value),time).

; Delta

[account,value,time]
HoldsAt(Balance(account,value),time) &
value<MinimumBalance(account) &
!HoldsAt(ServiceFeeCharged(account),time) ->
Happens(ChargeServiceFee(account),time).

[account,time]
EndOfMonth(time) ->
Happens(MonthlyReset(account),time).

Happens(Transfer(Account1,Account2,1),0).
Happens(Transfer(Account1,Account2,1),0).

; Psi

[account,value1,value2,time]
HoldsAt(Balance(account,value1),time) &
HoldsAt(Balance(account,value2),time) ->
value1=value2.

; Gamma

!HoldsAt(ServiceFeeCharged(Account1),0).
!HoldsAt(ServiceFeeCharged(Account2),0).
HoldsAt(Balance(Account1,3),0).
HoldsAt(Balance(Account2,1),0).
MinimumBalance(Account1)=3.
MinimumBalance(Account2)=1.
ServiceFee(Account1)=1.
ServiceFee(Account2)=1.
[time] !EndOfMonth(time).

completion Happens

range time 0 3
range value 1 3
range offset 1 1

; End of file.
