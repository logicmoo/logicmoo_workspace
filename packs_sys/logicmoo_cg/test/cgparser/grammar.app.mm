.EQ
define DS '"$"'
delim $$
define all '\(fa'
.EN
.H 1 "Conceptual Graph Grammars"
In this appendix we present three different approaches to specifying
context-free grammars for the linear form of a conceptual graph.
The basic core of each is based on the sample grammar presented
in [SOW84] (p. 395). Each grammar is presented here using a form of
Extended Backus Naur Form that is summarised in the following table:
.TS
box tab(&);
lfB | lfB
c | l.
Syntactic Object&Meaning
=
"$text$"&literal sequence of symbols \fItext\fP
\fIsyntactic-term\fP&member of this syntactic class
$object sub 1 | object sub 2$&$object sub 1$ or $object sub 2$
[$object$]&$object$ or nothing
{$object$}&sequence of zero or more $objects$
($object$)&$object$ (used only for grouping)
_
\fBid\fP&identifier
\fBnumber\fP&an integer
.TE
.P
.Ws
Besides the context-free conditions specified by these grammars, there are other,
context-sensitive, constraints to be considered.
Firstly, a string of one or more commas immediately before a period or semi-colon
may be deleted. Also, each $n$-adic relation must have exactly $n$ adjacent arcs,
with $n - 1$ arcs pointing toward the relation and one arc pointing away from it.
.We
.H 2 "CoGNO"
.TS
center tab(&);
lcl.
<element>&::=&<cg> ("." | ";")

<cg>&::=&<cgraph> | <abstraction> | <subtype>

<abstraction>&::=&"type" \fBid\fP "(" \fBid\fP ")" "is" <cgraph>
&|&"relation" \fBid\fP "(" <varlist> ")" "is" <cgraph>
&|&"individual" "is" "(" \fBid\fP ")" "is" <cgraph>
&|&"prototype" "for" \fBid\fP "(" \fBid\fP ")" "is" <cgraph>
&|&"schema" "for" "(" \fBid\fP ")" "is" <cgraph>

<cgraph>&::=&<concept> [<rlink>] | <relation> <conlink>

<rlink>&::=&<arc> <relation> [<conlink>] | "-" <rlist> ","

<conlink>&::=&<arc> <concept> [<rlink>] | "-" <conlist> ","

<rlist>&::=&"newline" <relation> [<conlink>] {"newline" <relation> [<conlink>]}

<conlist>&::=&"newline" <arc> <concept> [<rlink>] {"newline" <arc> <concept> [<rlink>]}

<concept>&::=&"[" <typefield> [":" <reffield> "]"]
&|&"["<typefield> [":" <cgraph> {<cgraph>} "]"]
&|&"[" <cgraph> {<cgraph>} "]"

<relation>&::=&"("<typelabel>")"

<typefield>&::=&\fBid\fP

<typelabel>&::=&\fBid\fP

<reffield>&::=&\fBid\fP
&|&"*" [\fBid\fP]
&|&"$DS$"* [\fBid\fP]
&|&[\fBid\fP] "#" [\fBnumber\fP]
&|&["#" \fBnumber\fP] "@" \fBnumber\fP \fBid\fP
&|&<set> ["@" \fBnumber\fP]

<set>\(dg&::=&["dist"] "{" [(<sequential> | <disjunctive>)] "}"
&|&"resp" "<" [<internal>] ">"

<sequential>&::=&"*"
&|&\fBid\fP ["," <sequential>]

<disjunctive>&::=&"*"
&|&\fBid\fP ["|" <disjunctive>]

<arc>&::=&[\fBnumber\fP] {"<-" | "->"}

<subtype>\(dd&::=&"subtype" "of" \fBid\fP "is" \fBid\fP

<varlist>&::=&\fBid\fP ["," <varlist>]
.TE
.FS *
Used to represent $all$.
.FE
.FS \(dg
Sets are not implemented in the current version of CoGNO.
.FE
.FS \(dd
Added to the linear form to help in constructing the concept type hierarchy.
.FE
.H 2 "CONGRES"
The following is the grammar for CONGRES[RAO87] derived from the PROLOG source
code for CONGRES.
.TS
tab(&);
lll.
<defn_context>&::=&"concept_type" \fBid\fP "is" <cgraph>
&|&"relation_type \fBid\fP "is" <cgraph>
&|&"subtype" "of" \fBid\fP "is" \fBid\fP
&|&"type" \fBid\fP "(" <paramlist> ")" "is" <cgraph>
&|&"individual" \fBid\fP "(" <paramlist> ")" "is" <cgraph>
&|&"relation" \fBid\fP "(" <paramlist> ")" "is" <cgraph>
&|&"schema" "for" \fBid\fP "(" <paramlist> ")" "is" <cgraph>
&|&"prototype" "for" \fBid\fP "(" <paramlist> ")" "is" <cgraph>
&|&<outer_context>

<outer_context>&::=&<cgraph> ["<=" {<cgraph>}] "."

<cgraph>&::=&[<relation> (<arc> | <conlist>)] <con_rlink> <sym>

<con_rlink>&::=&<concept> [(<arc> <rel_conlink> | "-" <rlink>)]

<rel_conlink>&::=&<relation> [(<arc> <con_rlink> | "-" <conlist>)]

<rlist>&::=&"," | "]" | "." | <rel_conlink> <rlist>

<conlist>&::=&"," | "]" | "." | <con_rlink> <conlist>

<concept>&::=&<context_type_field> [":" <reffield>] "]"

<context_type_field>&::=&\fBid\fP
&|&"proposition" ":" {<cgraph>}
&|&"[" {<cgraph>} "]"

<reffield>&::=&"$DS$" [\fBid\fP]
&|&"*" [\fBid\fP]
&|&"#" [[\fBnumber\fP] "@" \fBnumber id\fP]
&|&"@" \fBnumber id\fP
&|&\fBid\fP

<relation>&::=&"(" \fBid\fP ")"

<arc>&::=&"<-" | "->"

<sym>&::=&"." | ";" | "[" | "]" | "(" | ")" | "<=" | ","

<paramlist>&::=&\fBid\fP | \fBid\fP "," <paramlist>
.TE
.H 2 "Knowledge Representation Environment (KRE)"
The following is an adaption of the YACC grammar given for KRE [JOY88] (Appendix A).
.TS
tab(&);
lll.
<graphlist>&::=&<cgraph> | <graphlist> ";" <cgraph>

<cgraph>&::=&<concept [<rlink>] | <relation> <conlink>

<conlink>&::=&<arc> <concept> [<rlink>] | "-" <conlist>

<conlist>&::=&<arc> <concept> [<rlink>] ":" {<arc> <concept> [<rlink>] ":"}

<concept>&::=&"[" <typelabel [":" <referent> "]"

<rlink>&::=&<arc> <relation> [<conlink>] | "-" <rlist>

<rlist>&::=&<arc> <relation> [<conlink>] ":" | {<arc> <relation> [<conlink>] ":"}

<relation>&::=& "(" <typelabel> ")"

<arc>&::=&"<-" | "->"

<referent>&::=&<id> | <number> | <variable> | <cgraph>

<typelabel>&::=&<id>

<variable>&::=&"*" <id>

<id>&::=&\fBid\fP

<number>&::=&\fBnumber\fP
.TE
