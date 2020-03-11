/*
algorithm tarjan is
    input: graph G = (V, E)
    output: set of strongly connected components (sets of vertices)
   
    index := 0
    S := empty stack
    for each v in V do
        if v.index is undefined then
            strongconnect(v)
        end if
    end for
   
    function strongconnect(v)
        // Set the depth index for v to the smallest unused index
        v.index := index
        v.lowlink := index
        index := index + 1
        S.push(v)
        v.onStack := true
      
        // Consider successors of v
        for each (v, w) in E do
            if w.index is undefined then
                // Successor w has not yet been visited; recurse on it
                strongconnect(w)
                v.lowlink := min(v.lowlink, w.lowlink)
            else if w.onStack then
                // Successor w is in stack S and hence in the current SCC
                // If w is not on stack, then (v, w) is a cross-edge in the DFS tree and must be ignored
                // Note: The next line may look odd - but is correct.
                // It says w.index not w.lowlink; that is deliberate and from the original paper
                v.lowlink := min(v.lowlink, w.index)
            end if
        end for
      
        // If v is a root node, pop the stack and generate an SCC
        if v.lowlink = v.index then
            start a new strongly connected component
            repeat
                w := S.pop()
                w.onStack := false
                add w to current strongly connected component
            while w ≠ v
            output the current strongly connected component
        end if
    end function
*/



tarjan((V,E),SCC):-
    IDX0 = 0,
    Stack0 = [],
    SCC0 = [],
    initVInfo(V,_{},VInfo0),
    % VInfo is a dictionary containing V.info{} for each V
    vertices_cycle(V,E,VInfo0,_VInfo,IDX0,_IDX,Stack0,_Stack,SCC0,SCC).

initVInfo([],VInfo,VInfo).

initVInfo([V|OV],VInfo0,VInfo):-
    VInfo1=VInfo0.put(V,info{}),
    initVInfo(OV,VInfo1,VInfo).


vertices_cycle([],_,VInfo,VInfo,IDX,IDX,Stack,Stack,SCC,SCC).

vertices_cycle([V|OV],E,VInfo0,VInfo,IDX0,IDX,Stack0,Stack,SCC0,SCC):-
    _=VInfo0.get(V).get(idx),!,
    vertices_cycle(OV,E,VInfo0,VInfo,IDX0,IDX,Stack0,Stack,SCC0,SCC).

vertices_cycle([V|OV],E,VInfo0,VInfo,IDX0,IDX,Stack0,Stack,SCC0,SCC):-
    strongconnect(V,E,VInfo0,VInfo1,IDX0,IDX1,Stack0,Stack1,SCC0,SCC1),
    vertices_cycle(OV,E,VInfo1,VInfo,IDX1,IDX,Stack1,Stack,SCC1,SCC).



strongconnect(V,E,VInfo0,VInfo,IDX0,IDX,Stack0,Stack,SCC0,SCC):-
    VStruct0=VInfo0.get(V),
    VStruct1=VStruct0.put(idx,IDX0),
    VStruct2=VStruct1.put(lowlink,IDX0),
    IDX1 is IDX0 + 1,
    Stack1=[V|Stack0],
    VStruct3=VStruct2.put(onStack,true),
    VInfo1=VInfo0.put(V,VStruct3),
    findall(W,member(V-W,E),WS),
    check_successors(WS,V,E,VInfo1,VInfo2,IDX1,IDX,Stack1,Stack2,SCC0,SCC1),
    check_node_for_SCC(V,VInfo2,VInfo,Stack2,Stack,SCC1,SCC).


check_successors([],_,_,VInfo,VInfo,IDX,IDX,Stack,Stack,SCC,SCC).

check_successors([W|OW],V,E,VInfo0,VInfo,IDX0,IDX,Stack0,Stack,SCC0,SCC):-
    \+ (_=VInfo0.get(W).get(idx)),!,
    strongconnect(W,E,VInfo0,VInfo1,IDX0,IDX1,Stack0,Stack1,SCC0,SCC1),
    VStruct0=VInfo1.get(V),
    VStruct0LowLink=VStruct0.get(lowlink),
    WStruct0LowLink=VInfo1.get(W).get(lowlink),
    MinLowLink is min(VStruct0LowLink,WStruct0LowLink),
    VStruct1=VStruct0.put(lowlink,MinLowLink),
    VInfo2=VInfo1.put(V,VStruct1),
    check_successors(OW,V,E,VInfo2,VInfo,IDX1,IDX,Stack1,Stack,SCC1,SCC).

check_successors([W|OW],V,E,VInfo0,VInfo,IDX0,IDX,Stack0,Stack,SCC0,SCC):-
    true = VInfo0.get(W).get(onStack),!,
    VStruct0=VInfo0.get(V),
    VStruct0LowLink=VStruct0.get(lowlink),
    WStruct0IDX=VInfo0.get(W).get(idx),
    MinLowLink is min(VStruct0LowLink,WStruct0IDX),
    VStruct1=VStruct0.put(lowlink,MinLowLink),
    VInfo1=VInfo0.put(V,VStruct1),
    check_successors(OW,V,E,VInfo1,VInfo,IDX0,IDX,Stack0,Stack,SCC0,SCC).

check_successors([_|OW],V,E,VInfo0,VInfo,IDX0,IDX,Stack0,Stack,SCC0,SCC):-
    check_successors(OW,V,E,VInfo0,VInfo,IDX0,IDX,Stack0,Stack,SCC0,SCC).


check_node_for_SCC(V,VInfo0,VInfo,Stack0,Stack,SCC,[SCCNew|SCC]):-
    VLowLinkIDX=VInfo0.get(V).get(lowlink),
    VLowLinkIDX=VInfo0.get(V).get(idx),!,
    SCCInt=[],
    stack_cycle(V,VInfo0,VInfo,Stack0,Stack,SCCInt,SCCNew).

check_node_for_SCC(_,VInfo,VInfo,Stack,Stack,SCC,SCC).

stack_cycle(V,VInfo0,VInfo,[W|Stack0],Stack,SCCInt,SCCNew):-
    WStruct=VInfo0.get(W).put(onStack,false),
    VInfo1=VInfo0.put(W,WStruct),
    stack_cycle_check(V,W,VInfo1,VInfo,Stack0,Stack,[W|SCCInt],SCCNew).

stack_cycle_check(V,W,VInfo1,VInfo,Stack0,Stack,SCCInt,SCCNew):-
    dif(W,V),!,
    stack_cycle(V,VInfo1,VInfo,Stack0,Stack,SCCInt,SCCNew).

stack_cycle_check(V,V,VInfo,VInfo,Stack,Stack,SCCNew,SCCNew).

