% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(soap_utils, [
      get_element/3,           % +XMLStructure, +ElementName, -Element
      create_soap_message/2,   % +Content, -SOAPMessage
      create_fault_element/3   % +MessageCode, +Message, -FaultElement
   ]).

:- use_module('../list_utils').

/** <module> SOAP utils

This module contails some utility predicates for handling SOAP messages.

@author Tobias Kuhn
@version 2007-08-15
*/


env_ns('http://schemas.xmlsoap.org/soap/envelope/').
ar_ns('http://attempto.ifi.uzh.ch/acerules').


%% get_element(+XMLStructure, +ElementName, -Element)
%
% Returns an element of XMLStructure that has the name ElementName. XMLStructure can be an
% XML element like element(_,_,_), or it can be a list of XML elements. The predicate
% returns all solutions, if backtracking is forced.
% Only elements that occur on the top-most level are found (shallow search).

get_element(element(_, _, Content), ElementName, Element) :-
    get_element(Content, ElementName, Element).

get_element(XMLList, ElementName, Element) :-
    member(element(NS:ElementName, P, C), XMLList),
    Element = element(NS:ElementName, P, C).

get_element(XMLList, ElementName, Element) :-
    member(element(ElementName, P, C), XMLList),
    ElementName \= _:_,
    Element = element(ElementName, P, C).


%% create_soap_message(+Content, -SOAPMessage)
%
% Generates a complete SOAP message. Content has to be an atom and is inserted into the
% body-part of the envelope. SOAPMessage is returned as an atom.

create_soap_message(Content, SOAPMessage) :-
    env_ns(EnvNS),
    ar_ns(ARNS),
    SOAPElement =
    	element(EnvNS:'Envelope', [xmlns:env=EnvNS, xmlns:ar=ARNS], [
    		element(EnvNS:'Body', [], [Content])
    	]),
    xmlterm_to_xmlatom(SOAPElement, SOAPMessage).


%% create_fault_element(+MessageCode, +Message, -FaultElement)
%
% Generates a SOAP fault element with the specified MessageCode and message text (Message).
% FaultElement is returned as an atom.

create_fault_element(MessageCode, Message, FaultElement) :-
    env_ns(EnvNS),
    FaultElement =
    	element(EnvNS:'Fault', [], [
    		element(EnvNS:'faultcode', [], [MessageCode]),
    		element(EnvNS:'faultstring', [], [Message])
    	]).


xmlterm_to_xmlatom(XMLTerm, XMLAtom) :-
    new_memory_file(MemHandle),
    open_memory_file(MemHandle, write, S),
    xml_write(S, XMLTerm, []),
    close(S),
    memory_file_to_atom(MemHandle, XMLAtom).
