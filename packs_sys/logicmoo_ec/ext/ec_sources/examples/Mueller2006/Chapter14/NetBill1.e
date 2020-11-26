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
; @inproceedings{SirbuTygar:1995,
;   author = "Marvin A. Sirbu and J. D. Tygar",
;   year = "1995",
;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
;   editor = "
;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
;   pages = "20--25",
;   publisher = "
;   address = "
; }
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

sort agent
agent MusicStore, Jen

sort product
product BritneyCD

sort f
f PurchaseRequestedJenMusicStoreBritneyCD1
f DeliveredMusicStoreJenBritneyCD
f EPOSentJenMusicStore1

sort amount: integer

fluent C(agent,agent,f)
fluent CC(agent,agent,f,f)

event CreateC(agent,agent,f)
event CreateCC(agent,agent,f,f)
event DischargeC(agent,agent,f)
event DischargeCC(agent,agent,f,f)

fluent QuoteSent(agent,agent,product,amount)
fluent PurchaseRequested(agent,agent,product,amount)
fluent Delivered(agent,agent,product)
fluent EPOSent(agent,agent,amount)

event SendQuote(agent,agent,product,amount)
event RequestPurchase(agent,agent,product,amount)
event Deliver(agent,agent,product)
event SendEPO(agent,agent,amount)

; Sigma

[agent1,agent2,f,time]
Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,f,time]
Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,product,amount,time]
Initiates(SendQuote(agent1,agent2,product,amount),
          QuoteSent(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,amount,time]
Initiates(RequestPurchase(agent1,agent2,product,amount),
          PurchaseRequested(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,time]
Initiates(Deliver(agent1,agent2,product),
          Delivered(agent1,agent2,product),
          time).

[agent1,agent2,amount,time]
Initiates(SendEPO(agent1,agent2,amount),
          EPOSent(agent1,agent2,amount),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=MusicStore &
agent2=Jen &
product=BritneyCD &
amount=1 &
f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
f2=DeliveredMusicStoreJenBritneyCD ->
Initiates(SendQuote(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=Jen &
agent2=MusicStore &
product=BritneyCD &
amount=1 &
f1=DeliveredMusicStoreJenBritneyCD &
f2=EPOSentJenMusicStore1 &
!HoldsAt(Delivered(agent2,agent1,product),time) ->
Initiates(RequestPurchase(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

; Delta

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: Happens(SendQuote(MusicStore,Jen,BritneyCD,1),0).
Delta: Happens(RequestPurchase(Jen,MusicStore,BritneyCD,1),1).
Delta: Happens(Deliver(MusicStore,Jen,BritneyCD),3).
Delta: Happens(SendEPO(Jen,MusicStore,1),5).

; Gamma

[agent1,agent2,product,amount]
!HoldsAt(QuoteSent(agent1,agent2,product,amount),0).

[agent1,agent2,product,amount]
!HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).

[agent1,agent2,product]
!HoldsAt(Delivered(agent1,agent2,product),0).

[agent1,agent2,f]
!HoldsAt(C(agent1,agent2,f),0).

[agent1,agent2,f1,f2]
!HoldsAt(CC(agent1,agent2,f1,f2),0).

[agent1,agent2,amount]
!HoldsAt(EPOSent(agent1,agent2,amount),0).

completion Delta Happens

range time 0 7
range offset 1 1
range amount 1 1

; End of file.
