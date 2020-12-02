
:-module(agent,[]).



rule_set(RuleSet,MaxRuleID):-
  MaxRuleID = 7,
  RuleSet = [
    [1,def_father_1,[-[child,Y,X],-[male,X],+[father,X,Y]]],
    [2,def_mother_1,[-[child,Y,X],-[female,X],+[mother,X,Y]]],
    [3,def_parent_1,[-[father,X,Y],+[parent,X,Y]]],
    [4,def_parent_2,[-[mother,X,Y],+[parent,X,Y]]],
    [5,fact_lucy_1,[+[female,lucy]]],
    [6,fact_alice_1,[+[child,alice,bob]]],
    [7,fact_bob_1,[+[child,bob,lucy]]]
  ].

symbol_set(SymbolSet,MaxSymbolID):-
  MaxSymbolID = 10,
  SymbolSet = [
    [1,vble],[2,father],[3,child],[4,male],[5,female],
    [6,parent],[7,lucy],[8,alice],[9,bob],[10,mother]
  ].


nnet_definition(NNet):-
  InDim = 70,
  HidDim = 32,
  OutDim = 7,
  layer_init(layer1,InDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer2,HidDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer3,HidDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer4,HidDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer5,HidDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer6,HidDim,OutDim,softmax,[-0.2,0.2]),
  NNet = [layer1,layer2,layer3,layer4,layer5,layer6].

run_main:-
  init_search_time,
  nnet_definition(NNet),
  rule_set(RS,NR),
  symbol_set(SS,NS),
  LM = method(learning(100,0.01),input(2,2),output(7)),
  RM = method(reasoning,input(2,2),output(7)),
  G1 = [-[female,lucy]],
  dnn_sl_resolution(G1,[RS,NR],[SS,NS],NNet,LM,100,_),
  G2 = [-[mother,lucy,bob]],
  dnn_sl_resolution(G2,[RS,NR],[SS,NS],NNet,LM,100,_),
  G3 = [-[mother,lucy,bob],-[female,lucy]],
  dnn_sl_resolution(G3,[RS,NR],[SS,NS],NNet,LM,100,_),
  G4 = [-[parent,lucy,bob]],
  dnn_sl_resolution(G4,[RS,NR],[SS,NS],NNet,RM,100,Path4),
  nl,print_by_line(Path4),nl.


