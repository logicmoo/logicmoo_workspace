/*
 * Author: Sebastian Reichelt (SebastianR@gmx.de)
 * Based on work by Josef Urban
 * Licence: MIT
 */

CodeMirror.defineMode("mizar", function(cmCfg, modeCfg) 
{
    var keywords = {
	theorem: "main",
	scheme: "main",
	definition: "main",
	registration: "main",
	notation: "main",
	schemes: "main",
	constructors: "main",
	definitions: "main",
	theorems: "main",
	vocabulary: "main",
	clusters: "main",
	signature: "main",
	requirements: "main",

	proof: "block",
	now: "block",
	end: "block",
	hereby: "block",
	case: "block",
	suppose: "block",

	for: "formula",
	ex: "formula",
	not: "formula",
	or: "formula",
	implies: "formula",
	iff: "formula",
	st: "formula",
	holds: "formula",
	being: "formula",

	assume: "skeleton",
	cases: "skeleton",
	given: "skeleton",
	hence: "skeleton",
	let: "skeleton",
	per: "skeleton",
	take: "skeleton",
	thus: "skeleton",

	and: "normal",
	antonym: "normal",
	attr: "normal",
	as: "normal",
	be: "normal",
	begin: "normal",
	canceled: "normal",
	cluster: "normal",
	coherence: "normal",
	compatibility: "normal",
	consider: "normal",
	consistency: "normal",
	contradiction: "normal",
	correctness: "normal",
	def: "normal",
	deffunc: "normal",
	defpred: "normal",
	environ: "normal",
	equals: "normal",
	existence: "normal",
	func: "normal",
	if: "normal",
	irreflexivity: "normal",
	it: "normal",
	means: "normal",
	mode: "normal",
	of: "normal",
	otherwise: "normal",
	over: "normal",
	pred: "normal",
	provided: "normal",
	qua: "normal",
	reconsider: "normal",
	redefine: "normal",
	reflexivity: "normal",
	reserve: "normal",
	struct: "normal",
	such: "normal",
	synonym: "normal",
	that: "normal",
	then: "normal",
	thesis: "normal",
	where: "normal",
	associativity: "normal",
	commutativity: "normal",
	connectedness: "normal",
	irreflexivity: "normal",
	reflexivity: "normal",
	symmetry: "normal",
	uniqueness: "normal",
	transitivity: "normal",
	idempotence: "normal",
	asymmetry: "normal",
	projectivity: "normal",
	involutiveness: "normal"
    };

    return {
	token: function(stream, state) {
	    if (stream.match(/^::.*$/)) {
		return "comment";
	    } else if (stream.match(/^[A-Za-z]+/)) {
		var keyword = stream.current().toLowerCase();
		var kind = keywords[keyword];
		if (kind !== undefined) {
		    return "keyword-mizar-" + kind;
		} else if (keyword === "by") {
		    while (!stream.eol()) {
			var c = stream.next();
			if (c === ';') {
			    break;
			} else if (c === ':' && stream.peek() === ':') {
			    stream.backUp(1);
			    break;
			}
		    }
		    return "mizar-by";
		}
	    } else {
		var c = stream.next();
		if (c === '&') {
		    return "keyword-mizar-formula";
		}
	    }
	    return null;
	}
    };
});

CodeMirror.defineMIME("text/x-mizar", "mizar");
