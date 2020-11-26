%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: modal_ops.pl,v 1.5 1995/01/27 13:45:38 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1995 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************

/*%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\chapter
[Die Datei {\tt tom\_ops}]
{Die Datei {\Huge \tt tom\_ops}}

We introduce the input language to the system.

The language should agree with the language of the problem collection to be
developed. (This is a project under discussion.)

The precedences we have set are compared to the predecences of the
corresponding Otter operators. We have chosen the precedences of our
operators according to the standard colon in Prolog, the precendece of
which is 600.

\makevertother
\begin{center}
\begin{tabular}{l|l|l|l}
         &            & corresponding &       \\
operator & precedence & Otter operator & precedence \\ \hline
{\tt equivalent} & 1150 &            & \\
{\tt implies}  & 1100 &              & \\
{\tt and}      & 1050 & {\tt \&}     & 780 \\
{\tt or}       & 1050 & {\tt |}     & 790\\
{\tt not}      &  400 & {\tt -}      & 500\\
{\tt forall}   &  600 & {\tt all}    & \\
{\tt exists}   &  600 & {\tt exists} & \\
{\tt box}      &  600 &              & \\ 
{\tt diamond}  &  600 &              & \\
\end{tabular}
\end{center}
\makevertactive

\PL*/
:- op(1150, xfy, 'equivalent'),
	op(1100, xfy, 'implies'),
	op(1000, xfy, 'and'),
	op(1050, xfy, 'or'),
	op( 400,  fy, 'not').

:- op( 600,  fy, 'forall'),
	op( 600,  fy, 'exists').

:- op( 600,  fy, 'box'),
	op( 600,  fy, 'diamond').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */

