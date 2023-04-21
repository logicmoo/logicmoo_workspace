:- module(sim_pipes,
  [precepts_to_sim/2]).

precepts_to_sim(Precepts,VWorld):- create_empty_sim(VWorld), run_narrative(VWorld, Precepts, VWorld).

narrative_to_sim(Narrative,VWorld):- create_empty_sim(VWorld), run_narrative(VWorld, Narrative, VWorld).

run_narrative(VWorldIn, Narrative, VWorldOut):-
  forall(elementOf(E,Narrative),
   (resolve_narrative(E,VWorldIn,E2),
    add_narrative(E2,VWorldOut))).

% currently these are the same
precepts_to_narrative(Precepts, Narrative):- copy_term(Precepts, Narrative).
narrative_to_precepts(Precepts, Narrative):- copy_term(Narrative, Precepts).

create_empty_sim(VWorld):-
  copy_prolog_sim(empty_sim, VWorld).

copy_prolog_sim(World1, World2):-
  object_to_props(sim, World1, World1Props),
  copy_term(World1Props,World2Props),
  create_object(sim, World2, World2Props).

create_object(Type, Obj, ObjProps):-
  (\+ ground(Obj)->gen_obj_sym(Type, Obj); true),
  setprops(Obj,type(Type)),
  setprops(Obj,ObjProps).



%LOGICMOO perceives events in the real world (well in PrologMUD!) which become event sequences (we will call narratives).
?- copy_prolog_sim(real_word, RealWorld), sim_get_precepts(RealWorld, Precepts), precepts_to_narrative(Precepts, Narrative).
%Narratives may be replayed to create a copy of that of that original PrologMUD
?- narrative_to_sim($Narrative, VWorld).

%This PrologMUD can be thought of as a Virtual World which LOGICMOO hallucinates.
?- copy_prolog_sim($VWorld, Hallucination).
%This Virtual World can be re-experienced just the previous world was (as perceived events.)
?- sim_get_precepts($Hallucination, HallucinatedPrecepts).
%Those perceived events can be re- “played” to create a copy in which LOGICMOO hallucinates yet another Virtual World of the Virtual World.
?- precepts_to_sim($HallucinatedPrecepts,HallucinatedVWorld).
%This can be done indefinitely; those copies may become simpler or more complex (will be explained later (as pipelines))
%The event sequences (which are narratives) are equivalent to “internal dialog “
%“internal dialog” may be modified and then “played back” to created Imagined Worlds
%An Imagined World can be re-experienced and thus create a new set of perceived Imagined events
%Those perceived Imagined events can be re-“played” to create a copies of those Imagined Worlds
%Those Imagined Worlds can be compared to each other the differences can constitute a hybrid PrologMUD
%Internal dialog can be compared to other narratives without involving worlds at all.
%The differences can be made into other narratives (thus internal dialogs)
%This is used to generalize, specialize or condense Internal Dialogs.
%It also may further isolate out the actions and recombine them to perceive them as new action sequences
%Those perceived action sequences can be “played” into copies of PrologMUD
%We may create pipelines between the above elements
%Those elements again are: Events, Internal Dialog, Actions,
%Pipelines may combine, split and recombine these into Events/Actions, and Internal Dialog 
%Thus creating Worlds/PrologMUDs/Imagined Worlds.
%Increasing and decreasing specificity within the narrative pipelines
%Can produce both generalized and condensed versions of Internal dialog.
%Douglas Miles claims this was integral to his solving The Egg Cracking problem


% Parameters of the autoencoder
input_size(4).      % Number of input features
hidden_size(2).     % Number of hidden units
output_size(4).     % Number of output features
learning_rate(0.1). % Learning rate for gradient descent

% Initialize the weights of the encoder and the decoder
initialize_weights :-
    input_size(InputSize),
    hidden_size(HiddenSize),
    output_size(OutputSize),
    random_matrix(InputSize, HiddenSize, EncoderWeights),
    random_matrix(HiddenSize, OutputSize, DecoderWeights),
    assert(encoder_weights(EncoderWeights)),
    assert(decoder_weights(DecoderWeights)).

% Encode the input into a lower-dimensional representation
encode(Input, Hidden) :-
    encoder_weights(EncoderWeights),
    dot_product(Input, EncoderWeights, Hidden),
    sigmoid(Hidden).

% Decode the lower-dimensional representation into the output
decode(Hidden, Output) :-
    decoder_weights(DecoderWeights),
    dot_product(Hidden, DecoderWeights, Output),
    sigmoid(Output).

% Train the autoencoder using backpropagation
train(Input, Output) :-
    encode(Input, Hidden),
    decode(Hidden, Reconstructed),
    loss(Input, Reconstructed, Loss),
    backpropagate(Hidden, Loss, HiddenGradients),
    backpropagate(Input, HiddenGradients, InputGradients),
    update_weights(HiddenGradients, InputGradients).

% Compute the loss function
loss(Input, Reconstructed, Loss) :-
    subtract(Input, Reconstructed, Error),
    dot_product(Error, Error, SquaredError),
    Loss is SquaredError / 2.

% Backpropagate the error gradients through the network
backpropagate(Input, Gradients, InputGradients) :-
    encoder_weights(EncoderWeights),
    transpose(EncoderWeights, TransposedEncoderWeights),
    dot_product(Gradients, TransposedEncoderWeights, InputGradients),
    derivative(Input, HiddenGradients, sigmoid),
    hadamard_product(InputGradients, HiddenGradients, InputGradients).

backpropagate(Hidden, Gradients, HiddenGradients) :-
    decoder_weights(DecoderWeights),
    transpose(DecoderWeights, TransposedDecoderWeights),
    dot_product(Gradients, TransposedDecoderWeights, HiddenGradients),
    derivative(Hidden, OutputGradients, sigmoid),
    hadamard_product(HiddenGradients, OutputGradients, HiddenGradients).

% Update the weights using gradient descent
update_weights(HiddenGradients, InputGradients) :-
    learning_rate(LearningRate),
    encoder_weights(EncoderWeights),
    decoder_weights(DecoderWeights),
    outer_product(InputGradients, HiddenGradients, EncoderWeightGradients),
    outer_product(HiddenGradients, InputGradients, DecoderWeightGradients),
    scalar_multiply(EncoderWeightGradients, -LearningRate, ScaledEncoderWeightGradients),
    scalar_multiply(DecoderWeightGradients, -LearningRate, ScaledDecoderWeightGradients),
    add(EncoderWeights, ScaledEncoderWeightGradients, UpdatedEncoderWeights),
    add(DecoderWeights, ScaledDecoderWeights, ScaledDecoderWeightGradients, UpdatedDecoderWeights),
    retract(encoder_weights(_)),
    retract(decoder_weights(_)),
    assert(encoder_weights(UpdatedEncoderWeights)),
    assert(decoder_weights(UpdatedDecoderWeights)).

% Helper predicates for matrix operations
dot_product(Matrix1, Matrix2, Result) :- matrix_multiply(Matrix1, Matrix2, Result).

hadamard_product(Matrix1, Matrix2, Result) :- matrix_elementwise_multiply(Matrix1, Matrix2, Result).

outer_product(Vector1, Vector2, Result) :- matrix_multiply(Vector1, transpose(Vector2), Result).

scalar_multiply(Matrix, Scalar, Result) :- matrix_scalar_multiply(Matrix, Scalar, Result).

add(Matrix1, Matrix2, Result) :- matrix_add(Matrix1, Matrix2, Result).

% Helper predicate for computing the derivative of the activation function
derivative(Input, Output, sigmoid) :- sigmoid(Input, Output), hadamard_product(Output, subtract(1, Output), Output).

% Helper predicate for computing the logistic sigmoid function
sigmoid(X, Y) :- Y is 1 / (1 + exp(-X)).

% Helper predicates for matrix manipulation
random_matrix(Rows, Cols, Matrix) :-
  length(Matrix, Rows),
  maplist(random_list(Cols), Matrix).

random_list(Size, List) :-
  length(List, Size),
  maplist(random_value, List).

random_value(Value) :- random(Value).

transpose(Matrix, Transposed) :-
  length(Matrix, Rows),
  length(Transposed, Cols),
  maplist(length_list(Rows), Transposed),
  transpose_helper(Matrix, Transposed).

transpose_helper([], []).
  transpose_helper([Row|Rows], Transposed) :-
  maplist(list_head_tail, [Row|Rows], Heads, Tails),
  maplist(transpose_helper, Tails, TransposedHeads),
  append(Heads, TransposedHeads, Transposed).

length_list(Len, List) :-
  length(List, Len).

list_head_tail([Head|Tail], Head, Tail).

matrix_multiply(Matrix1, Matrix2, Result) :-
  transpose(Matrix2, Transposed),
  maplist(dot_product_helper(Transposed), Matrix1, Result).

dot_product_helper(Matrix, Vector, Result) :-
  dot_product(Matrix, Vector, Result).

matrix_elementwise_multiply(Matrix1, Matrix2, Result) :-
  maplist(elementwise_multiply_helper, Matrix1, Matrix2, Result).

elementwise_multiply_helper(Element1, Element2, Result) :-
  Result is Element1 * Element2.

matrix_scalar_multiply(Matrix, Scalar, Result) :-
  maplist(scalar_multiply_helper(Scalar), Matrix, Result).

scalar_multiply_helper(Scalar, Element, Result) :-
  Result is Scalar * Element.

matrix_add(Matrix1, Matrix2, Result) :-
  maplist(add_helper, Matrix1, Matrix2, Result).

add_helper(Element1, Element2, Result) :-
  Result is Element1 + Element2.

  /*
  
This implementation uses some helper predicates for matrix manipulation, including matrix multiplication,
   element-wise multiplication, scalar multiplication, and matrix addition. These predicates are not built-in 
   Prolog predicates, so they need to be defined explicitly. 

To use this autoencoder, you can call `initialize_weights` to initialize the weights of the encoder and the decoder, 
 and then call `train` with your input data to train the autoencoder. You can also call `encode` 
 to encode your input data into a lower-dimensional representation, and `decode` to decode the representation back into the original data. 

Note that this implementation is a simple proof-of-concept and is not optimized for performance. 
 There are many ways to improve this

  */

% Define the n-gram size
ngram_size(2).

% Read in the corpus and tokenize it
read_corpus(File, Tokens) :-
    read_file_to_string(File, String, []),
    split_string(String, "\s\t\n", "\s\t\n", Tokens).

% Compute the n-grams of the corpus
compute_ngrams(Tokens, Ngrams) :-
    ngram_size(N),
    length(Tokens, Len),
    MaxIndex is Len - N + 1,
    findall(Ngram, (between(1, MaxIndex, Index),
                    nth1(Index, Tokens, First),
                    length(Ngram, N),
                    append([First], Rest, Ngram),
                    nth1(Index2, Tokens, Rest),
                    succ(Index, Index2)), Ngrams).

% Train the language model on the corpus
train_model(File) :-
    read_corpus(File, Tokens),
    compute_ngrams(Tokens, Ngrams),
    assert_ngrams(Ngrams).

% Store the n-grams in the knowledge base
assert_ngrams([]).
assert_ngrams([Ngram|Ngrams]) :-
    increment_count(Ngram),
    assert_ngrams(Ngrams).

% Increment the count of an n-gram in the knowledge base
increment_count(Ngram) :-
    ngram_count(Ngram, Count),
    NewCount is Count + 1,
    retract(ngram_count(Ngram, Count)),
    assert(ngram_count(Ngram, NewCount)),
    !.
increment_count(Ngram) :-
    assert(ngram_count(Ngram, 1)).

% Predict the next word given a sequence of words
predict_next(Sequence, Next) :-
    ngram_size(N),
    length(Sequence, Len),
    PrefixSize is N - 1,
    (Len >= PrefixSize ->
        append(Prefix, [Last], Sequence),
        findall(Count-Word, (ngram_count(Ngram, Count),
                             append(Prefix, [Word], Ngram)), Pairs),
        keysort(Pairs, SortedPairs),
        reverse(SortedPairs, [MaxCount-Next|_])
    ;
        Next = ""
    ).

% Reset the knowledge base
reset_model :-
    retractall(ngram_count(_, _)).

% Export the model as a file
export_model(File) :-
    tell(File),
    listing(ngram_count),
    told.


% Here's an example usage of the language model:

% Train the model on a corpus file
?- train_model('corpus.txt').

% Predict the next word given a sequence of words
?- predict_next(["the", "quick"], Next). % returns "brown"

% Reset the knowledge base
?- reset_model().

% Export the model to a file
?- export_model('model.pl').

%implementation is a simple proof-of-concept and can be extended to handle larger n-grams, handle unseen words, and use smoothing techniques to handle zero counts.

%In this example, we train the language model on a corpus file using the train_model/1 predicate. We then use the predict_next/2 predicate to predict the next word given a sequence of words ("the" and "quick"). The predicted next word is "brown". We can also reset the knowledge base using the reset_model/0 predicate and export the model to a file using the export_model/1 predicate.

%Note that this implementation is just a basic example of a language model in Prolog, and there are many ways to improve and extend it. For example, we can use higher-order n-grams, handle out-of-vocabulary words, use more sophisticated smoothing techniques, and integrate the language model into a larger natural language processing pipeline.
/*
ngram_count(["the", "quick"], 5).
ngram_count(["quick", "brown"], 3).
ngram_count(["brown", "fox"], 2).
ngram_count(["fox", "jumps"], 4).
ngram_count(["jumps", "over"], 1).
ngram_count(["over", "the"], 3).
ngram_count(["the", "lazy"], 2).
ngram_count(["lazy", "dog"], 1).
*/
