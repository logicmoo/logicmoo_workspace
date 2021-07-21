/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2016, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(swish_render_sldnf,
	  [ term_rendering//3,			% +Term, +Vars, +Options
	    svg//2				% +String, +Options
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(process)).
:- use_module(library(sgml)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module('../render').

:- register_renderer(sldnf, "Render SLDNF trees").

/** <module> Render SLDNF trees using latex

This renderer exploits latex to render SLDNF trees as SVG images
*/


%%	term_rendering(+Term, +Vars, +Options)//
%
%	Renders data using graphviz.  Options:
%
%	  - svg(+Mode)
%	  One of `inline` (default) or `object`, rendering the SVG using
%	  an HTML <object> element.

term_rendering(Data, Vars, Options) -->
	{ debug(sldnf(vars), 'Data: ~q, vars: ~p', [Data, Vars])
	},
	render_latex(Data, Options).

%%	render_latex(+LatexString, +Program, +Options)// is det.
%
%	Render a latex doc. First checks whether Program is available.
%	It produces  inline   SVG

render_latex(_LatexString,_Options) -->
	{ \+ has_latex_renderer(latex) }, !,
	no_latex(latex).

render_latex(LatexString, _Options) -->	% <svg> rendering
  { latex_stream(LatexString,SVG0),
    random(0,100000,R),
    number_string(R,Rs),
    string_concat(Rs,"-glyph",NewGlyph),
    string_codes(NewGlyph,NewGlyphCodes0),
    append(NewGlyphCodes0,T,NewGlyphCodes),
    string_codes(SVG0,SVG0Codes),
    phrase(rename_ids(NewGlyphCodes,T,SVGCodes),SVG0Codes,_),
    string_codes(SVG,SVGCodes)
          
	},
  html(div([ class(['render-latex', 'reactive-size']),
		       'data-render'('As tree')
		     ],
		     \svg(SVG, []))).

      
rename_ids(NewGlyph,O0,O) -->
  "glyph",!,
  {copy_term((NewGlyph,O0),(O,T))},
  rename_ids(NewGlyph,O0,T).

rename_ids(NewGlyph,O,[C|T])-->
  [C],!,
  rename_ids(NewGlyph,O,T).

rename_ids(_NewGlyph,_O,[])-->
  [].
      


rename_ids(NewGlyph,[NewGlyph|T]) -->
  "glyph",!,
  rename_ids(NewGlyph,T).

rename_ids(NewGlyph,[C|T])-->
  [C],
  rename_ids(NewGlyph,T).


%%	svg(+SVG:string, +Options:list)//
%
%	Include SVG as pan/zoom image. Must be  embedded in a <div> with
%	class 'reactive-size'.

svg(SVG, _Options) -->
	html([ 
  \[SVG],
	       \js_script({|javascript||
(function() {
   if ( $.ajaxScript ) {
     var div  = $.ajaxScript.parent();
     var svg  = div.find("svg");
     var data = { w0: svg.width(),
		  h0: svg.height()
		};
     var pan;

     function updateSize() {
       var w = svg.closest("div.answer").innerWidth();

       function reactive() {
	 if ( !data.reactive ) {
	   data.reactive = true;
	   div.on("reactive-resize", updateSize);
	 }
       }

       w = Math.max(w*0.85, 100);
       if ( w < data.w0 ) {
	 svg.width(w);
	 svg.height(w = Math.max(w*data.h0/data.w0, w/4));
	 reactive();
	 if ( pan ) {
	   pan.resize();
	   pan.fit();
	   pan.center();
	 }
       }
     }

     require(["svg-pan-zoom"], function(svgPanZoom) {
       updateSize()
       pan = svgPanZoom(svg[0], {
			  // controlIconsEnabled: true
			  minZoom: 0.1,
			  maxZoom: 50
			});
    });
   }
 })();

		      |})
	     ]).


latex_stream(Latex, SVG) :-
  tmp_file_stream(utf8,File,Stream),
  write(Stream,
"\\documentclass{article}
\\pagestyle{empty}
\\usepackage{epic,eepic}
\\usepackage{ecltree}
\\begin{document}
"),
  write(Stream,Latex),
  write(Stream,
"\\end{document}
"),
  close(Stream),
  file_directory_name(File, Directory),
  atom_concat('-output-directory=',Directory,OutDirOp),
  process_create(path(latex), [OutDirOp,'-interaction=nonstopmode',File], [stdout(null)]),
  delete_file(File),
  atom_concat(File,'.aux',FileAux),
  atom_concat(File,'.log',FileLog),
  delete_file(FileAux),
  delete_file(FileLog),
  atom_concat(File,'.dvi',FileDvi),
  atom_concat(File,'.pdf',FilePdf),
  process_create(path(dvipdf),[FileDvi,FilePdf], [stdout(null)]),
  atom_concat(File,'cropped.pdf',FileCroppedPdf),
  atomic_list_concat([pdfcrop,FilePdf,FileCroppedPdf,'>/dev/null'],' ',ShellComm),
  shell(ShellComm),
%  process_create(path(pdfcrop), [FilePdf,FileCroppedPdf], [stdout(null)]),
  delete_file(FilePdf),
  atom_concat(File,'.svg',FileSvg),
  process_create(path(pdf2svg), [FileCroppedPdf,FileSvg], [stdout(null)]),
  delete_file(FileCroppedPdf),
  open(FileSvg,read,StreamSvg),
  read_string(StreamSvg, _Length, SVG),
  close(StreamSvg),
  delete_file(FileSvg).


rewrite_sgv_dom([element(svg, Attrs, Content)],
		[element(svg, Attrs,
			 [ element(script, ['xlink:href'=SVGPan], []),
			   element(g, [ id=viewport
				      ],
				   Content)
			 ])]) :-
	http_absolute_location(js('SVGPan.js'), SVGPan, []).
rewrite_sgv_dom(DOM, DOM).

send_to_dot(Data, Out) :-
	call_cleanup(format(Out, '~s', [Data]),
		     close(Out)), !.

%%	remove_old_data(+Now)
%
%	Remove data that are older than 15 minutes.

remove_old_data(Time) :-
	(   dot_data(Hash, _, Stamp),
	    Time > Stamp+900,
	    retract(dot_data(Hash, _, Stamp)),
	    fail
	;   true
	).

has_latex_renderer(Renderer) :-
	exe_options(ExeOptions),
	absolute_file_name(path(Renderer), _,
			   [ file_errors(fail)
			   | ExeOptions
			   ]).

exe_options(Options) :-
	current_prolog_flag(windows, true), !,
	Options = [ extensions(['',exe,com]), access(read) ].
exe_options(Options) :-
	Options = [ access(execute) ].

no_latex(Renderer) -->
	html(div([ class('no-latex'),
		   style('color:red;')
		 ],
		 [ 'The server does not have the latex program ',
		   code(Renderer), ' installed in PATH. '
		 ])).


%%	add_defaults(Statements0, Statements) is det.

add_defaults(Statements0, Statements) :-
	\+ memberchk(bgcolor=_, Statements0), !,
	Statements = [bgcolor=transparent|Statements0].
add_defaults(Statements, Statements).
