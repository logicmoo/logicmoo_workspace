/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Matt Lilley
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, CWI, Amsterdam
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

:- module(xmldsig,
          [ xmld_signed_DOM/3,                  % +DOM, -SignedDOM, +Options
            xmld_verify_signature/4             % +DOM, +Signature, -Certificate, +Options
          ]).
:- autoload(library(base64),[base64/3,base64/2]).
:- autoload(library(c14n2),[xml_write_canonical/3]).
:- autoload(library(crypto),
	    [crypto_data_hash/3,rsa_sign/4,hex_bytes/2,rsa_verify/4]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(error),
	    [type_error/2,domain_error/2,existence_error/2]).
:- autoload(library(lists),[member/2]).
:- autoload(library(option),[option/3,option/2]).
:- autoload(library(sha),[sha_hash/3]).
:- autoload(library(ssl),[load_private_key/3,certificate_field/2]).
:- autoload(library(xmlenc),[load_certificate_from_base64_string/2]).

/** <module> XML Digital signature

This library deals with _XMLDSIG_, RSA signed XML documents.

@see http://www.di-mgt.com.au/xmldsig.html
@see https://www.bmt-online.org/geekisms/RSA_verify
@see http://stackoverflow.com/questions/5576777/whats-the-difference-between-nid-sha-and-nid-sha1-in-openssl

*/

xmldsig_ns('http://www.w3.org/2000/09/xmldsig#').

%!  xmld_signed_DOM(+DOM, -SignedDOM, +Options) is det.
%
%   Translate an XML DOM structure in a signed version.  Options:
%
%     - key_file(+File)
%     File holding the private key needed to sign
%     - key_password(+Password)
%     String holding the password to op the private key.
%
%   The   SignedDOM   must   be   emitted   using   xml_write/3   or
%   xml_write_canonical/3.  If  xml_write/3  is   used,  the  option
%   layout(false) is needed to avoid  changing   the  layout  of the
%   =SignedInfo= element and the signed DOM,   which  will cause the
%   signature to be invalid.

xmld_signed_DOM(DOM, SignedDOM, Options) :-
    dom_hash(DOM, ODOM, Hash, Options),
    signed_info(Hash, Signature, SDOM, KeyDOM, Options),
    signed_xml_dom(ODOM, SDOM, KeyDOM, Signature, SignedDOM, Options).


%!  dom_hash(+DOM, -ODOM, -Hash, +Options) is det.
%
%   Compute the digest for DOM.
%
%   @arg Hash is the base64  encoded   version  of  the selected SHA
%   algorithm.

dom_hash(DOM, ODOM, Hash, Options) :-
    object_c14n(DOM, ODOM, C14N),
    hash(C14N, Hash, Options).

object_c14n(DOM, ODOM, C14N) :-
    object_dom(DOM, ODOM),
    with_output_to(
        string(C14N),
        xml_write_canonical(current_output, ODOM, [])).

object_dom(DOM0,
           element(NS:'Object', ['Id'='object', xmlns=NS], DOM)) :-
    xmldsig_ns(NS),
    to_list(DOM0, DOM).

to_list(DOM, DOM) :- DOM = [_|_].
to_list(DOM, [DOM]).

hash(C14N, Hash, Options) :-
    option(hash(Algo), Options, sha1),
    sha_hash(C14N, HashCodes, [algorithm(Algo)]),
    phrase(base64(HashCodes), Base64Codes),
    string_codes(Hash, Base64Codes).

%!  signed_info(+Hash, -Signature, -SDOM, -KeyDOM, +Options)

signed_info(Hash, Signature, SDOM, KeyDOM, Options) :-
    signed_info_dom(Hash, SDOM, Options),
    with_output_to(
        string(SignedInfo),
        xml_write_canonical(current_output, SDOM, [])),
    rsa_signature(SignedInfo, Signature, KeyDOM, Options).

%!  signed_info_dom(+Hash, -SDOM, +Options) is det.
%
%   True when SDOM is the xmldsign:Signature  DOM for an object with
%   the given Hash.

signed_info_dom(Hash, SDOM, _Options) :-
    SDOM = element(NS:'SignedInfo', [xmlns=NS],
                   [ '\n  ',
                     element(NS:'CanonicalizationMethod',
                             ['Algorithm'=C14NAlgo], []),
                     '\n  ',
                     element(NS:'SignatureMethod',
                             ['Algorithm'=SignatureMethod], []),
                     '\n  ',
                     Reference,
                     '\n'
                   ]),
    Reference = element(NS:'Reference', ['URI'='#object'],
                        [ '\n    ',
                          element(NS:'DigestMethod',
                                  ['Algorithm'=DigestMethod], []),
                          '\n    ',
                          element(NS:'DigestValue', [], [Hash]),
                          '\n  '
                        ]),
    xmldsig_ns(NS),
    DigestMethod='http://www.w3.org/2000/09/xmldsig#sha1',
    C14NAlgo='http://www.w3.org/TR/2001/REC-xml-c14n-20010315',
    SignatureMethod='http://www.w3.org/2000/09/xmldsig#rsa-sha1'.

%!  rsa_signature(+SignedInfo:string, -Signature, -KeyDOM, +Options)

rsa_signature(SignedInfo, Signature, KeyDOM, Options) :-
    option(algorithm(Algorithm), Options, sha1),
    crypto_data_hash(SignedInfo, Digest, [algorithm(Algorithm)]),
    string_upper(Digest, DIGEST),
    debug(xmldsig, 'SignedInfo ~w digest = ~p', [Algorithm, DIGEST]),
    private_key(Key, Options),
    rsa_key_dom(Key, KeyDOM),
    rsa_sign(Key, Digest, String,
             [ type(Algorithm)
             ]),
    string_length(String, Len),
    debug(xmldsig, 'RSA signatute length: ~p', [Len]),
    string_codes(String, Codes),
    phrase(base64(Codes), Codes64),
    string_codes(Signature, Codes64).

private_key(Key, Options) :-
    option(key_file(File), Options),
    option(key_password(Password), Options),
    !,
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        load_private_key(In, Password, Key),
        close(In)).
private_key(_Key, Options) :-
    \+ option(key_file(_), Options),
    !,
    throw(error(existence_error(option, key_file, Options),_)).
private_key(_Key, Options) :-
    throw(error(existence_error(option, key_password, Options),_)).



%!  rsa_key_dom(+Key, -DOM) is det.
%
%   Produce the KeyInfo node from the private key.

rsa_key_dom(Key,
            element(NS:'KeyInfo', [xmlns=NS],
                    [ element(NS:'KeyValue', [],
                              [ '\n  ',
                                element(NS:'RSAKeyValue', [],
                                        [ '\n    ',
                                          element(NS:'Modulus', [], [Modulus]),
                                          '\n    ',
                                          element(NS:'Exponent', [], [Exponent]),
                                          '\n  '
                                        ]),
                                '\n'
                              ])
                    ])) :-
    key_info(Key, Info),
    _{modulus:Modulus, exponent:Exponent} :< Info,
    xmldsig_ns(NS).


%!  key_info(+Key, -Info) is det.
%
%   Extract the RSA modulus and exponent   from a private key. These
%   are the first end  second  field  of   the  rsa  term.  They are
%   represented as hexadecimal encoded bytes. We must recode this to
%   base64.
%
%   @tbd    Provide better support from library(ssl).

key_info(private_key(Key), rsa{modulus:Modulus, exponent:Exponent}) :-
    !,
    base64_bignum_arg(1, Key, Modulus),
    base64_bignum_arg(2, Key, Exponent).
key_info(Key, _) :-
    type_error(private_key, Key).

base64_bignum_arg(I, Key, Value) :-
    arg(I, Key, HexModulesString),
    string_codes(HexModulesString, HexModules),
    hex_bytes(HexModules, Bytes),
    phrase(base64(Bytes), Bytes64),
    string_codes(Value, Bytes64).


signed_xml_dom(ObjectDOM, SDOM, KeyDOM, Signature, SignedDOM, _Options) :-
    SignedDOM = element(NS:'Signature', [xmlns=NS],
                        [ '\n', SDOM,
                          '\n', element(NS:'SignatureValue', [], [Signature]),
                          '\n', KeyDOM,
                          '\n', ObjectDOM,
                          '\n'
                        ]),
    xmldsig_ns(NS).



%!  xmld_verify_signature(+DOM, +SignatureDOM, -Certificate, +Options) is det.
%
%   Confirm  that  an  `ds:Signature`  element    contains  a  valid
%   signature. Certificate is bound to  the certificate that appears
%   in the element if the signature is valid. It is up to the caller
%   to determine if the certificate is trusted   or not.
%
%   *Note*: The DOM and SignatureDOM must   have been obtained using
%   the load_structure/3 option keep_prefix(true)   otherwise  it is
%   impossible to generate an identical   document  for checking the
%   signature. See also xml_write_canonical/3.

xmld_verify_signature(DOM, SignatureDOM, Certificate, Options) :-
    signature_info(DOM, SignatureDOM, SignedInfo, Algorithm, Signature,
                   PublicKey, Certificate, CanonicalizationMethod),
    base64(RawSignature, Signature),
    (   Algorithm = rsa(HashType)
    ->  with_output_to(string(C14N),
                       xml_write_canonical(current_output, SignedInfo,
                                           [method(CanonicalizationMethod)|Options])),
        crypto_data_hash(C14N, Digest, [algorithm(HashType)]),
        atom_codes(RawSignature, Codes),
        hex_bytes(HexSignature, Codes),
        rsa_verify(PublicKey, Digest, HexSignature, [type(HashType)])
    ;   domain_error(supported_signature_algorithm, Algorithm)
    ).

ssl_algorithm('http://www.w3.org/2000/09/xmldsig#rsa-sha1', rsa(sha1)).
ssl_algorithm('http://www.w3.org/2000/09/xmldsig#dsa-sha1', dsa(sha1)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#hmac-md5', hmac(md5)).       % NB: Requires a parameter
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#hmac-sha224', hmac(sha224)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#hmac-sha256', hmac(sha256)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#hmac-sha384', hmac(sha384)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#hmac-sha512', hmac(sha512)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#rsa-md5', rsa(md5)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#rsa-sha256', rsa(sha256)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#rsa-sha384', rsa(sha384)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#rsa-sha512', rsa(sha512)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#rsa-ripemd160', rsa(ripemd160)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha1', ecdsa(sha1)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha224', ecdsa(sha224)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha256', ecdsa(sha256)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha384', ecdsa(sha384)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha512', ecdsa(sha512)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#esign-sha1', esign(sha1)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#esign-sha224', esign(sha224)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#esign-sha256', esign(sha256)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#esign-sha384', esign(sha384)).
ssl_algorithm('http://www.w3.org/2001/04/xmldsig-more#esign-sha512', esign(sha512)).

digest_method('http://www.w3.org/2000/09/xmldsig#sha1', sha1).
digest_method('http://www.w3.org/2001/04/xmlenc#sha256', sha256).

signature_info(DOM, Signature, SignedData, Algorithm, SignatureValue,
               PublicKey, Certificate, CanonicalizationMethod) :-
    xmldsig_ns(NSRef),
    memberchk(element(ns(_, NSRef):'SignatureValue', _, [RawSignatureValue]), Signature),
    atom_codes(RawSignatureValue, RawSignatureCodes),
    delete_newlines(RawSignatureCodes, SignatureCodes),
    string_codes(SignatureValue, SignatureCodes),
    memberchk(element(ns(Prefix, NSRef):'SignedInfo', SignedInfoAttributes, SignedInfo), Signature),
    SignedData = element(ns(Prefix, NSRef):'SignedInfo', SignedInfoAttributes, SignedInfo),
    memberchk(element(ns(_, NSRef):'CanonicalizationMethod', CanonicalizationMethodAttributes, _), SignedInfo),
    memberchk('Algorithm'=CanonicalizationMethod, CanonicalizationMethodAttributes),
    forall(memberchk(element(ns(_, NSRef):'Reference', ReferenceAttributes, Reference), SignedInfo),
           verify_digest(ReferenceAttributes, CanonicalizationMethod, Reference, DOM)),
    memberchk(element(ns(_, NSRef):'SignatureMethod', SignatureMethodAttributes, []), SignedInfo),
    memberchk('Algorithm'=XMLAlgorithm, SignatureMethodAttributes),
    ssl_algorithm(XMLAlgorithm, Algorithm),
    memberchk(element(ns(_, NSRef):'KeyInfo', _, KeyInfo), Signature),
    ( memberchk(element(ns(_, NSRef):'X509Data', _, X509Data), KeyInfo),
      memberchk(element(ns(_, NSRef):'X509Certificate', _, [X509Certificate]), X509Data)->
        load_certificate_from_base64_string(X509Certificate, Certificate),
        certificate_field(Certificate, public_key(PublicKey))
    ; throw(not_implemented)
    ).


delete_newlines([], []):- !.
delete_newlines([13|As], B):- !, delete_newlines(As, B).
delete_newlines([10|As], B):- !, delete_newlines(As, B).
delete_newlines([A|As], [A|B]):- !, delete_newlines(As, B).


verify_digest(ReferenceAttributes, CanonicalizationMethod, Reference, DOM):-
    xmldsig_ns(NSRef),
    memberchk('URI'=URI, ReferenceAttributes),
    atom_concat('#', Id, URI),
    % Find the relevant bit of the DOM
    resolve_reference(DOM, Id, Digestible, _NSMap),
    (  memberchk(element(ns(_, NSRef):'Transforms', _, Transforms), Reference)
    -> findall(TransformAttributes-Transform,
               member(element(ns(_, NSRef):'Transform', TransformAttributes, Transform), Transforms),
               TransformList)
    ;  TransformList = []
    ),
    apply_transforms(TransformList, Digestible, TransformedDigestible),
    memberchk(element(ns(_, NSRef):'DigestMethod', DigestMethodAttributes, _), Reference),
    memberchk(element(ns(_, NSRef):'DigestValue', _, [DigestBase64]), Reference),
    memberchk('Algorithm'=Algorithm, DigestMethodAttributes),
    (  digest_method(Algorithm, DigestMethod)
    -> true
    ;  domain_error(supported_digest_method, DigestMethod)
    ),
    with_output_to(string(XMLString), xml_write_canonical(current_output, TransformedDigestible, [method(CanonicalizationMethod)])),
    sha_hash(XMLString, DigestBytes, [algorithm(DigestMethod)]),
    base64(ExpectedDigest, DigestBase64),
    atom_codes(ExpectedDigest, ExpectedDigestBytes),
    (  ExpectedDigestBytes == DigestBytes
    -> true
    ;  throw(error(invalid_digest, _))
    ).

resolve_reference([element(Tag, Attributes, Children)|_], ID, element(Tag, Attributes, Children), []):-
    memberchk('ID'=ID, Attributes),
    !.
resolve_reference([element(_, Attributes, Children)|Siblings], ID, Element, Map):-
    ( findall(xmlns:Prefix=URI,
              member(xmlns:Prefix=URI, Attributes),
              Map,
              Tail),
          resolve_reference(Children, ID, Element, Tail)
    ; resolve_reference(Siblings, ID, Element, Map)
    ).


apply_transforms([], X, X):- !.
apply_transforms([Attributes-Children|Transforms], In, Out):-
    memberchk('Algorithm'=Algorithm, Attributes),
    (  apply_transform(Algorithm, Children, In, I1)
    -> true
    ;  existence_error(transform_algorithm, Algorithm)
    ),
    apply_transforms(Transforms, I1, Out).

apply_transform('http://www.w3.org/2001/10/xml-exc-c14n#', [], X, X).

apply_transform('http://www.w3.org/2000/09/xmldsig#enveloped-signature', [], element(Tag, Attributes, Children), element(Tag, Attributes, NewChildren)):-
    delete_signature_element(Children, NewChildren).

delete_signature_element([element(ns(_, 'http://www.w3.org/2000/09/xmldsig#'):'Signature', _, _)|Siblings], Siblings):- !.
delete_signature_element([A|Siblings], [A|NewSiblings]):-
    delete_signature_element(Siblings, NewSiblings).
