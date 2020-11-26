
%******************************************************************************
%*
%*
%*  This file describes the learning operators of miles called by xmiles
%*  when a specific button is pressed 
%*
%*
%******************************************************************************

 

   
%******************************************************************************
%*
%*        groups(<list of groups>).
%*
%******************************************************************************

groups([preprocess,g1,g2,gencon,lgg,refinement,evaluation,truncation]). 



%******************************************************************************
%*
%*        groupdef(<groupname>,<buttonname-list>). 
%*
%******************************************************************************

groupdef(preprocess,['argument types','clause heads','flatten kb','flatten rules',
	'unflatten kb']).
groupdef(g1,['g1 op','apply g1',identify,absorb,'inv derivate',
	'most spec v',saturate,'elem saturate']).
groupdef(g2,['intraconstruct 1','intraconstruct 2','g2 op','apply g2']).
groupdef(gencon,['learn constrained', 'learn foil', 'learn rul']).
groupdef(lgg,['gen msg',rlgg,lgg,'headed lgg','nr lgg',gti]).
groupdef(refinement,['unify vars','instantiate vars','add body lit','new predicate']).
groupdef(evaluation,['correct check','complete check','evaluate examples',fp,
	  'covered examples']).
groupdef(truncation,[reduce,'unconnected','redundant','flat redundant','unconnecting',
         'negation based','flat negation based','facts']).
         



%******************************************************************************
%*
%*         operatordef(<buttonname>,<operatorname>,<in-out-pattern>,
%*	               <in-type-checks>,<out-display-functions>,
%*	               <refresh-list>,).  
%*
%******************************************************************************

operatordef('argument types',argument_types,[],[],[],[rules]).
operatordef('clause heads',clause_heads,[],[],[],[rules]).
operatordef('flatten kb',flatten_kb,[],[],[],[rules]).
operatordef('flatten rules',flatten_rules,[],[],[],[rules]).
operatordef('unflatten kb',unflatten_kb,[],[],[],[rules]).

operatordef('g1 op',g1_op,[xmarg1,xmarg2,xmout1],[isRule,isRule],
	    [resultAddRule],[]).
operatordef('apply g1',apply_g1,[xmarg1,xmout1],[isRule],
	    [resultAddRuleList],[]).
operatordef(identify,identify,[xmarg1,xmarg2,xmout1],[isExOrRule,true],
	    [resultAddRule],[]).
operatordef(absorb,absorb,[xmarg1,xmarg2,xmout1],[isExOrRule,isExOrRule],
	    [resultAddRule],[]).

operatordef('inv derivate',inv_derivate,[xmarg1,xmout1],[isExOrRule],
	    [resultAddRule],[]).
operatordef('most spec v',most_spec_v,[xmarg1,xmarg2,xmout1],
	    [isExOrRule,isExOrRule],[resultAddRule],[]).
operatordef(saturate,saturate,[xmarg1,xmout1,xmoptdepth],[isExOrRule,isDepth],
	    [resultAddRule],[]).
operatordef('elem saturate',elem_saturate,[xmarg1,xmarg2,xmout1],
	    [isExOrRule,true],[resultAddRule],[]).

operatordef('intraconstruct 1',intra_construct1,
	     [xmarg1,xmarg2,xmout1,xmout2,xmout3],
	     [isRule,isRule],[resultAddRule,resultAddRule,resultAddRule],[]).
operatordef('intraconstruct 2',intra_construct2,
	    [xmarg1,xmarg2,xmout1,xmout2,xmout3],
	    [isRule,isRule],[resultAddRule,resultAddRule,resultAddRule],[]).
operatordef('g2 op',g2_op,[xmarg1,xmarg2,xmout1,xmout2,xmout3],
	    [isRule,isRule],[resultAddRule,resultAddRule,resultAddRule],[]).
operatordef('apply g2',apply_g2,[xmarg1,xmarg2,xmout1,xmout2,xmout3],
	    [isRule,isRule],[resultAddRule,resultAddRule,resultAddRule],[]).

operatordef('learn constrained', learn_constrained,[],[],[],[rules]).
operatordef('learn foil', learn_foil,[],[],[],[rules]).
operatordef('learn rul', learn_rul,[],[],[],[rules]).


operatordef('gen msg',gen_msg,[xmarg1,xmarg2,xmout1],[isRule,isRule],
	    [resultAddRule],[]).
operatordef(rlgg,rlgg,[xmarg1,xmarg2,xmout1],[isRule,isRule],[resultAddRule],[]).
operatordef(lgg,lgg,[xmarg1,xmarg2,xmout1],[isRule,isRule],[resultAddRule],[]).
operatordef('headed lgg',headed_lgg,[xmarg1,xmarg2,xmout1],[isRule,true],
	    [resultAddRule],[]).
operatordef('nr lgg',nr_lgg,[xmarg1,xmarg2,xmout1],[isRule,isRule],
	    [resultAddRule],[]).
operatordef(gti,gti,[xmarg1,xmarg2,xmout1],[isRule,isRule],
	    [resultAddRule],[]).

operatordef('unify vars',refinement_unify_variables,[xmarg1,xmout1],
            [isRule],[resultAddSpec],[]).
operatordef('instantiate vars',refinement_instantiate_variables,[xmarg1,xmout1],
            [isRule],[resultAddSpec],[]).
operatordef('add body lit',refinement_add_body_literal,[xmarg1,xmout1],
            [isRule],[resultAddSpec],[]).
operatordef('new predicate',specialize_with_newpred,[xmarg1,xmout1],
            [isRule],[resultAddNewpreds],[]).

operatordef('correct check',correct_chk,[],[],[],[]).
operatordef('complete check',complete_chk,[],[],[],[]).
operatordef('evaluate examples',eval_examples,[],[],[],[]).
operatordef(fp,fp,[xmout1],[],[resultSelectRules],[]).
operatordef('covered examples',all_covered_examples,[xmout1],[],[resultSelectExamples],[]).

operatordef(reduce,reduce_complete,[xmarg1],[isRule],[],[rules]).
operatordef('unconnected',truncate_unconnected,[xmarg1],[isRule],[],[rules]).
operatordef('redundant',truncate_r,[xmarg1],[isRule],[],[rules]).
operatordef('flat redundant',truncate_flat_r,[xmarg1],[isRule],[],[rules]).
operatordef('unconnecting',truncate_unconnecting,[xmarg1],[isRule],[],[rules]).
operatordef('negation based',truncate_neg_based,[xmarg1],[isRule],[],[rules]).
operatordef('flat negation based',truncate_flat_neg_based,[xmarg1],[isRule],[],[rules]).
operatordef('facts',truncate_facts,[xmarg1],[isRule],[],[rules]).

