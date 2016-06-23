/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2016, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(codes_html,
	  [code_html/3,		% ?Code,  ?Html, ?Tail
	   codes_html/2,	% +Codes, ?Html
	   codes_html/3		% +Codes, ?Html, ?Tail
	  ]).

codes_html(Codes, Html) :-
    codes_html(Codes, Html, []).

codes_html(Codes) -->
    foldl(code_html_nf, Codes).

code_html_nf(Code) --> code_html(Code), !.
code_html_nf(Code) --> [Code].

% escape characters taken from:
% http://www.theukwebdesigncompany.com/articles/entity-escape-characters.php

code_html(0'") --> "&quot;". % quotation mark
code_html(0'') --> "&apos;". % apostrophe
code_html(0'&) --> "&amp;". % ampersand
code_html(0'<) --> "&lt;". % less-than
code_html(0'>) --> "&gt;". % greater-than
code_html(0' ) --> "&nbsp;". % non-breaking space
code_html(0'¡) --> "&iexcl;". % inverted exclamation mark
code_html(0'¢) --> "&cent;". % cent
code_html(0'£) --> "&pound;". % pound
code_html(0'¤) --> "&curren;". % currency
code_html(0'¥) --> "&yen;". % yen
code_html(0'¦) --> "&brvbar;". % broken vertical bar
code_html(0'§) --> "&sect;". % section
code_html(0'¨) --> "&uml;". % spacing diaeresis
code_html(0'©) --> "&copy;". % copyright
code_html(0'ª) --> "&ordf;". % feminine ordinal indicator
code_html(0'«) --> "&laquo;". % angle quotation mark (left)
code_html(0'¬) --> "&not;". % negation
code_html(0'&) --> "hy;". % soft hyphen
code_html(0'®) --> "&reg;". % registered trademark
code_html(0'¯) --> "&macr;". % spacing macron
code_html(0'°) --> "&deg;". % degree
code_html(0'±) --> "&plusmn;". % plus-or-minus 
code_html(0'²) --> "&sup2;". % superscript 2
code_html(0'³) --> "&sup3;". % superscript 3
code_html(0'´) --> "&acute;". % spacing acute
code_html(0'µ) --> "&micro;". % micro
code_html(0'¶) --> "&para;". % paragraph
code_html(0'·) --> "&middot;". % middle dot
code_html(0'¸) --> "&cedil;". % spacing cedilla
code_html(0'¹) --> "&sup1;". % superscript 1
code_html(0'º) --> "&ordm;". % masculine ordinal indicator
code_html(0'») --> "&raquo;". % angle quotation mark (right)
code_html(0'¼) --> "&frac14;". % fraction 1/4
code_html(0'½) --> "&frac12;". % fraction 1/2
code_html(0'¾) --> "&frac34;". % fraction 3/4
code_html(0'¿) --> "&iquest;". % inverted question mark
code_html(0'×) --> "&times;". % multiplication
code_html(0'÷) --> "&divide;". % division
code_html(0'À) --> "&Agrave;". % capital a, grave accent
code_html(0'Á) --> "&Aacute;". % capital a, acute accent
code_html(0'Â) --> "&Acirc;". % capital a, circumflex accent
code_html(0'Ã) --> "&Atilde;". % capital a, tilde
code_html(0'Ä) --> "&Auml;". % capital a, umlaut mark
code_html(0'Å) --> "&Aring;". % capital a, ring
code_html(0'Æ) --> "&AElig;". % capital ae
code_html(0'Ç) --> "&Ccedil;". % capital c, cedilla
code_html(0'È) --> "&Egrave;". % capital e, grave accent
code_html(0'É) --> "&Eacute;". % capital e, acute accent
code_html(0'Ê) --> "&Ecirc;". % capital e, circumflex accent
code_html(0'Ë) --> "&Euml;". % capital e, umlaut mark
code_html(0'Ì) --> "&Igrave;". % capital i, grave accent
code_html(0'Í) --> "&Iacute;". % capital i, acute accent
code_html(0'Î) --> "&Icirc;". % capital i, circumflex accent
code_html(0'Ï) --> "&Iuml;". % capital i, umlaut mark
code_html(0'Ð) --> "&ETH;". % capital eth, Icelandic
code_html(0'Ñ) --> "&Ntilde;". % capital n, tilde
code_html(0'Ò) --> "&Ograve;". % capital o, grave accent
code_html(0'Ó) --> "&Oacute;". % capital o, acute accent
code_html(0'Ô) --> "&Ocirc;". % capital o, circumflex accent
code_html(0'Õ) --> "&Otilde;". % capital o, tilde
code_html(0'Ö) --> "&Ouml;". % capital o, umlaut mark
code_html(0'Ø) --> "&Oslash;". % capital o, slash
code_html(0'Ù) --> "&Ugrave;". % capital u, grave accent
code_html(0'Ú) --> "&Uacute;". % capital u, acute accent
code_html(0'Û) --> "&Ucirc;". % capital u, circumflex accent
code_html(0'Ü) --> "&Uuml;". % capital u, umlaut mark
code_html(0'Ý) --> "&Yacute;". % capital y, acute accent
code_html(0'Þ) --> "&THORN;". % capital THORN, Icelandic
code_html(0'ß) --> "&szlig;". % small sharp s, German
code_html(0'à) --> "&agrave;". % small a, grave accent
code_html(0'á) --> "&aacute;". % small a, acute accent
code_html(0'â) --> "&acirc;". % small a, circumflex accent
code_html(0'ã) --> "&atilde;". % small a, tilde
code_html(0'ä) --> "&auml;". % small a, umlaut mark
code_html(0'å) --> "&aring;". % small a, ring
code_html(0'æ) --> "&aelig;". % small ae
code_html(0'ç) --> "&ccedil;". % small c, cedilla
code_html(0'è) --> "&egrave;". % small e, grave accent
code_html(0'é) --> "&eacute;". % small e, acute accent
code_html(0'ê) --> "&ecirc;". % small e, circumflex accent
code_html(0'ë) --> "&euml;". % small e, umlaut mark
code_html(0'ì) --> "&igrave;". % small i, grave accent
code_html(0'í) --> "&iacute;". % small i, acute accent
code_html(0'î) --> "&icirc;". % small i, circumflex accent
code_html(0'ï) --> "&iuml;". % small i, umlaut mark
code_html(0'ð) --> "&eth;". % small eth, Icelandic
code_html(0'ñ) --> "&ntilde;". % small n, tilde
code_html(0'ò) --> "&ograve;". % small o, grave accent
code_html(0'ó) --> "&oacute;". % small o, acute accent
code_html(0'ô) --> "&ocirc;". % small o, circumflex accent
code_html(0'õ) --> "&otilde;". % small o, tilde
code_html(0'ö) --> "&ouml;". % small o, umlaut mark
code_html(0'ø) --> "&oslash;". % small o, slash
code_html(0'ù) --> "&ugrave;". % small u, grave accent
code_html(0'ú) --> "&uacute;". % small u, acute accent
code_html(0'û) --> "&ucirc;". % small u, circumflex accent
code_html(0'ü) --> "&uuml;". % small u, umlaut mark
code_html(0'ý) --> "&yacute;". % small y, acute accent
code_html(0'þ) --> "&thorn;". % small thorn, Icelandic
code_html(0'ÿ) --> "&yuml;". % small y, umlaut mark
