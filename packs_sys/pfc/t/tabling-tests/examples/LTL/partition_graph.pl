   % NOTICE: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %                                                                      %
   %  COPYRIGHT (2009) University of Dallas at Texas.                     %
   %                                                                      %
   %  Developed at the Applied Logic, Programming Languages and Systems   %
   %  (ALPS) Laboratory at UTD by Feliks Kluzniak.                        %
   %                                                                      %
   %  Permission is granted to modify this file, and to distribute its    %
   %  original or modified contents for non-commercial purposes, on the   %
   %  condition that this notice is included in all copies in its         %
   %  original form.                                                      %
   %                                                                      %
   %  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,     %
   %  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES     %
   %  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE AND     %
   %  NON-INFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR        %
   %  ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE FOR ANY DAMAGES OR       %
   %  OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE, ARISING    %
   %  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR       %
   %  OTHER DEALINGS IN THE SOFTWARE.                                     %
   %                                                                      %
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Given a graph in the form of a set of facts with nodes (state/1) and arcs
%% (trans/2) produce a list of components, where each component is a maximum
%% connected subgraph, and is presented in the form of a list of nodes.
%% (If a graph has more than one such component, then it is disconnected.)

partition( Components ) :-
        findall( N, state( N ), Nodes ),
        partition_( Nodes, [], Components ).

%
partition_( [], ComponentsSoFar, ComponentsSoFar ).

partition_( [ Node | Nodes ], ComponentsSoFar, Components ) :-
        (
            member( Component, ComponentsSoFar ),
            member( Node, Component )
        ->
            NewComponentsSoFar = ComponentsSoFar
        ;
            component_of( Node, NewComponent ),
            NewComponentsSoFar = [ NewComponent | ComponentsSoFar ]
        ),
        partition_( Nodes, NewComponentsSoFar, Components ).


%% Given a node, compute the maximum connected component to which this node
%% belongs.

component_of( Node, Component ) :-
        component_of_( [ [ Node ] ], [], Component ).


% In this auxiliary procedure the nodes are represented not by a list, but by a
% list of lists: this allows us to avoid the costs of appending lists.

component_of_( [], ComponentSoFar, ComponentSoFar ).

component_of_( [ [] | Lists ], ComponentSoFar, Component ) :-
        component_of_( Lists, ComponentSoFar, Component ).

component_of_( [ [ Node | Nodes ] | Lists ], ComponentSoFar, Component ) :-
        (
            member( Node, ComponentSoFar )
        ->
            NewComponentSoFar = ComponentSoFar,
            NewLists          = Lists
        ;
            findall( N, (trans( Node, N ) ; trans( N, Node )), Ns ),
            NewComponentSoFar = [ Node | ComponentSoFar ],
            NewLists          = [ Ns | Lists ]
        ),
        component_of_( [ Nodes | NewLists ], NewComponentSoFar, Component ).

%-------------------------------------------------------------------------------
