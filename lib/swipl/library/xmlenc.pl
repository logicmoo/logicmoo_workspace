/*  Part of SWI-Prolog

    Author:        Matt Lilley
    E-mail:        thetrime@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2016, SWI-Prolog Foundation
                              VU University Amsterdam
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


:-module(xmlenc,
         [ decrypt_xml/4,   % +EncryptedXML, -DecryptedXML, :KeyCallback, +Options
           load_certificate_from_base64_string/2 % +Base64String, -Certificate
         ]).
:- autoload(library(base64),[base64/2]).
:- autoload(library(crypto),
	    [crypto_data_decrypt/6,rsa_private_decrypt/4,hex_bytes/2]).
:- autoload(library(error),[existence_error/2,domain_error/2]).
:- autoload(library(lists),[append/3]).
:- autoload(library(sgml),[load_structure/3]).
:- autoload(library(ssl),[load_certificate/2]).
:- autoload(library(uri),[uri_components/2]).
:- autoload(library(http/http_open),[http_open/3]).

:- meta_predicate
    decrypt_xml(+, -, 3, +).

/** <module> XML encryption library

This library is a partial implementation of the XML encryption standard.
It implements the _decryption_ part, which is needed by SAML clients.

@see https://www.w3.org/TR/xmlenc-core1/
@see https://en.wikipedia.org/wiki/Security_Assertion_Markup_Language
*/

% These are the 4 mandatory block cipher algorithms
% (actually aes-192-cbc is not mandatory, but it is easy to support)
ssl_algorithm('http://www.w3.org/2001/04/xmlenc#tripledes-cbc', 'des3',         8).
ssl_algorithm('http://www.w3.org/2001/04/xmlenc#aes128-cbc',    'aes-128-cbc', 16).
ssl_algorithm('http://www.w3.org/2001/04/xmlenc#aes256-cbc',    'aes-256-cbc', 32).
ssl_algorithm('http://www.w3.org/2001/04/xmlenc#aes192-cbc',    'aes-192-cbc', 24).

%!  decrypt_xml(+DOMIn, -DOMOut, :KeyCallback, +Options) is det.
%
%   @arg KeyCallback may be called as follows:
%           - call(KeyCallback, name,        KeyName,         Key)
%           - call(KeyCallback, public_key,  public_key(RSA), Key)
%           - call(KeyCallback, certificate, Certificate,     Key)

decrypt_xml([], [], _, _):- !.
decrypt_xml([element(ns(_, 'http://www.w3.org/2001/04/xmlenc#'):'EncryptedData',
                     Attributes, EncryptedData)|Siblings],
            [Decrypted|NewSiblings], KeyCallback, Options) :-
    !,
    decrypt_element(Attributes, EncryptedData, Decrypted, KeyCallback, Options),
    decrypt_xml(Siblings, NewSiblings, KeyCallback, Options).

decrypt_xml([element(Tag, Attributes, Children)|Siblings],
            [element(Tag, Attributes, NewChildren)|NewSiblings], KeyCallback, Options) :-
    !,
    decrypt_xml(Children, NewChildren, KeyCallback, Options),
    decrypt_xml(Siblings, NewSiblings, KeyCallback, Options).
decrypt_xml([Other|Siblings], [Other|NewSiblings], KeyCallback, Options):-
    decrypt_xml(Siblings, NewSiblings, KeyCallback, Options).

%!   decrypt_element(+Attributes,
%!                   +EncryptedData,
%!                   -DecryptedElement,
%!                   +Options).
%
%    Decrypt an EncryptedData element  with   Attributes  and  child
%    EncryptedData DecryptedElement will either be an element/3 term
%    or a string as dictacted by   the Type attribute in Attributes.
%    If Attributes does not contain a  Type attribute then we assume
%    it is a string

:-meta_predicate(decrypt_element(+, +, -, 3, +)).

decrypt_element(Attributes, EncryptedData, Decrypted, KeyCallback, Options):-
    XENC = ns(_, 'http://www.w3.org/2001/04/xmlenc#'),
    (  memberchk(element(XENC:'CipherData', _, CipherData), EncryptedData)
    -> true
    ;  existence_error(cipher_data, EncryptedData)
    ),
    % The Type attribute is not mandatory. However, 3.1 states that
    % "Without this information, the decryptor will be unable to automatically restore the XML document to its original cleartext form."
    (  memberchk('Type'=Type, Attributes)
    -> true
    ;  Type = 'http://www.w3.org/2001/04/xmlenc#Content'
    ),

    % First of all, determine the algorithm used to encrypt the data
    determine_encryption_algorithm(EncryptedData, Algorithm, IVSize),

    % There are now two tasks remaining, and they seem like they ought to be quite simple, but unfortunately they are not
    % First, we must determine the key used to encrypt the message
    determine_key(EncryptedData, Key, KeyCallback, Options),

    % Then, we must determine what the encrypted data even IS
    % If the message includes a CipherValue then this is straightfoward - the encrypted data is the base64-encoded child
    % of this element.
    (  memberchk(element(XENC:'CipherValue', _, CipherValueElement), CipherData)
    -> base64_element(CipherValueElement, CipherValueWithIV),
           string_codes(CipherValueWithIV, CipherValueWithIVCodes),
           length(IVCodes, IVSize),
           append(IVCodes, CipherCodes, CipherValueWithIVCodes),
           string_codes(IV, IVCodes),
           string_codes(CipherText, CipherCodes),
           length(CipherValueWithIVCodes, _),
           crypto_data_decrypt(CipherText, Algorithm, Key, IV, DecryptedStringWithPadding, [padding(none), encoding(octet)])
    ;  memberchk(element(XENC:'CipherReference', CipherReferenceAttributes, CipherReference), CipherData)->
           % However, it is allowed to include CipherReference instead. This is an arbitrary URI and a list of transforms to convert the
           % data identified by that URI into the raw octets that represent the encrypted data
           % The URI attribute of the CipherReference element is mandatory
           memberchk('URI'=CipherURI, CipherReferenceAttributes),
           % The transforms attribute is optional, though.
           (  memberchk(element('Transforms', _, Transforms), CipherReference)
           -> true
           ;  Transforms = []
           ),
           uri_components(CipherURI, uri_components(Scheme, _, _, _, _)),
           (  ( Scheme == 'http' ; Scheme == 'https')
              % FIXME: URI may not be an *absolute* URL
           ->  with_output_to(string(RawCipherValue),
                          setup_call_cleanup(http_open(CipherURI, HTTPStream, []),
                                             copy_stream_data(HTTPStream, current_output),
                                             close(HTTPStream)))
           ;  domain_error(resolvable_uri, CipherURI)
           ),
           apply_ciphertext_transforms(RawCipherValue, Transforms, CipherValue),
           sub_string(CipherValue, 0, IVSize, _, IV),
           sub_string(CipherValue, IVSize, _, 0, CipherText),
           crypto_data_decrypt(CipherText, Algorithm, Key, IV, DecryptedStringWithPadding, [padding(none), encoding(octet)])
    ),
    % The XML-ENC padding scheme does not comply with RFC-1423. This has been noted a few times by people trying to write
    % XML-ENC decryptors backed by OpenSSL, which insists on compliance. The only recourse we have is to disable padding entirely
    % and do it in our application
    xmlenc_padding(DecryptedStringWithPadding, DecryptedString),
    % Now that we have the decrypted data, we can decide whether to turn it into an element or leave it as
    % content
    (  Type == 'http://www.w3.org/2001/04/xmlenc#Element'
    -> setup_call_cleanup(open_string(DecryptedString, StringStream),
                          load_structure(StringStream, [Decrypted], [dialect(xmlns), keep_prefix(true)]),
                          close(StringStream))
    ;  Decrypted = DecryptedString
    ).

xmlenc_padding(DecryptedStringWithPadding, DecryptedString):-
    string_length(DecryptedStringWithPadding, _),
    string_codes(DecryptedStringWithPadding, Codes),
    append(_, [LastCode], Codes),
    length(Padding, LastCode),
    append(DecryptedCodes, Padding, Codes),
    !,
    string_codes(DecryptedString, DecryptedCodes).

apply_ciphertext_transforms(CipherValue, [], CipherValue):- !.
apply_ciphertext_transforms(_, [_AnythingElse|_], _):-
    % FIXME: Not implemented
    throw(error(implementation_missing('CipherReference transforms are not implemented', _))).

:- meta_predicate determine_key(+,-,3,+).
determine_key(EncryptedData, Key, KeyCallback, Options):-
    DS = ns(_, 'http://www.w3.org/2000/09/xmldsig#'),
    (  memberchk(element(DS:'KeyInfo', _, KeyInfo), EncryptedData)
    -> true
    ;  % Technically the KeyInfo is not mandatory. However, without a key we cannot decrypt
           % so raise an error. In the future Options could contain a key if it is agreed upon
           % by some other channel
           existence_error(key_info, EncryptedData)
    ),
    resolve_key(KeyInfo, Key, KeyCallback, Options).

:- meta_predicate resolve_key(+,-,3,+).

resolve_key(Info, Key, KeyCallback, Options):-
    % EncryptedKey
    XENC = 'http://www.w3.org/2001/04/xmlenc#',
    memberchk(element(ns(_, XENC):'EncryptedKey', _KeyAttributes, EncryptedKey), Info),
    !,
    % The EncryptedKey is slightly different to EncryptedData. For a start, the algorithms used to decrypt the
    % key are orthogonal to those used for EncryptedData. However we can recursively search for the keys then
    % decrypt them using the different algorithms as needed
    memberchk(element(ns(_, XENC):'EncryptionMethod', MethodAttributes, EncryptionMethod), EncryptedKey),
    memberchk('Algorithm'=Algorithm, MethodAttributes),

    % Now find the KeyInfo
    determine_key(EncryptedKey, PrivateKey, KeyCallback, Options),

    memberchk(element(ns(_, XENC):'CipherData', _, CipherData), EncryptedKey),
    memberchk(element(ns(_, XENC):'CipherValue', _, CipherValueElement), CipherData),
    base64_element(CipherValueElement, CipherValue),
    (  Algorithm == 'http://www.w3.org/2001/04/xmlenc#rsa-oaep-mgf1p'
    -> rsa_private_decrypt(PrivateKey, CipherValue, Key, [encoding(octet), padding(pkcs1_oaep)])
    ;  Algorithm == 'http://www.w3.org/2009/xmlenc11#rsa-oaep',
           memberchk(element(ns(_, 'http://www.w3.org/2009/xmlenc11#'):'MGF', MGFAttributes, _), EncryptionMethod),
           memberchk('Algorithm'='http://www.w3.org/2009/xmlenc11#mgf1sha1', MGFAttributes)   % This is just the same as rsa-oaep-mgf1p!
    -> rsa_private_decrypt(PrivateKey, CipherValue, Key, [encoding(octet), padding(pkcs1_oaep)])
    ;  Algorithm == 'http://www.w3.org/2001/04/xmlenc#rsa-1_5'
    -> rsa_private_decrypt(PrivateKey, CipherValue, Key, [encoding(octet), padding(pkcs1)])
    ;  domain_error(key_transport, Algorithm)
    ).
resolve_key(KeyInfo, _Key, _KeyCallback, _Options):-
    % AgreementMethod. FIXME: Not implemented
    XENC = ns(_, 'http://www.w3.org/2001/04/xmlenc#'),
    memberchk(element(XENC:'AgreementMethod', _KeyAttributes, _AgreementMethod), KeyInfo),
    !,
    throw(not_implemented).
% Additionally, we are allowed to use any elements from XML-DSIG
resolve_key(KeyInfo, Key, KeyCallback, _Options):-
    % KeyName. Use the callback with type=name and hint=KeyName
    DS = ns(_, 'http://www.w3.org/2000/09/xmldsig#'),
    memberchk(element(DS:'KeyName', _KeyAttributes, [KeyName]), KeyInfo),
    !,
    call(KeyCallback, name, KeyName, Key).
resolve_key(KeyInfo, _Key, _KeyCallback, _Options):-
    % RetrievalMethod. FIXME: Not implemented
    DS = ns(_, 'http://www.w3.org/2000/09/xmldsig#'),
    memberchk(element(DS:'RetrievalMethod', _KeyAttributes, _RetrievalMethod), KeyInfo),
    !,
    throw(not_implemented).
resolve_key(KeyInfo, Key, KeyCallback, _Options):-
    % KeyValue.
    DS = ns(_, 'http://www.w3.org/2000/09/xmldsig#'),
    memberchk(element(DS:'KeyValue', _KeyAttributes, KeyValue), KeyInfo),
    !,
    (  memberchk(element(DS:'RSAKeyValue', _, RSAKeyValue), KeyInfo)
    -> memberchk(element(DS:'Modulus', _, [ModulusBase64]), RSAKeyValue),
           memberchk(element(DS:'Exponent', _, [ExponentBase64]), RSAKeyValue),
           base64_to_hex(ModulusBase64, Modulus),
           base64_to_hex(ExponentBase64, Exponent),
           call(KeyCallback, public_key, public_key(rsa(Modulus, Exponent, -, -, -, -, -, -)), Key)
    ;  memberchk(element(DS:'DSAKeyValue', _, _DSAKeyValue), KeyInfo)
    -> throw(error(not_implemented(dsa_key), _)) % FIXME: Not implemented
    ;  existence_error(usable_key_value, KeyValue)
    ).
resolve_key(KeyInfo, Key, KeyCallback, _Options):-
    % X509Data.
    DS = ns(_, 'http://www.w3.org/2000/09/xmldsig#'),
    memberchk(element(DS:'X509Data', _, X509Data), KeyInfo),
    memberchk(element(DS:'X509Certificate', _, [X509Certificate]), X509Data),
    !,
    load_certificate_from_base64_string(X509Certificate, Certificate),
    call(KeyCallback, certificate, Certificate, Key).
resolve_key(KeyInfo, _Key, _KeyCallback, _Options):-
    % PGPData. FIXME: Not implemented
    DS = ns(_, 'http://www.w3.org/2000/09/xmldsig#'),
    memberchk(element(DS:'PGPData', _KeyAttributes, _PGPData), KeyInfo),
    !,
    throw(not_implemented).
resolve_key(KeyInfo, _Key, _KeyCallback, _Options):-
    % SPKIData. FIXME: Not implemented
    DS = ns(_, 'http://www.w3.org/2000/09/xmldsig#'),
    memberchk(element(DS:'SPKIData', _KeyAttributes, _SPKIData), KeyInfo),
    !,
    throw(not_implemented).
resolve_key(KeyInfo, _Key, _KeyCallback, _Options):-
    % MgmtData. FIXME: Not implemented
    DS = ns(_, 'http://www.w3.org/2000/09/xmldsig#'),
    memberchk(element(DS:'MgmtData', _KeyAttributes, _SPKIData), KeyInfo),
    !,
    throw(not_implemented).
resolve_key(Info, _, _, _):-
    % The XML-ENC standard allows for arbitrary other means of transmitting keys in application-specific
    % protocols. This is not supported here, though. In the future a callback could be provided in Options
    % to obtain the key information from a KeyInfo structure.
    existence_error(usable_key, Info).


base64_to_hex(Base64, Hex):-
    base64(Raw, Base64),
    atom_codes(Raw, Codes),
    hex_bytes(Hex0, Codes),
    string_upper(Hex0, Hex).


determine_encryption_algorithm(EncryptedData, Algorithm, IVSize):-
    XENC = ns(_, 'http://www.w3.org/2001/04/xmlenc#'),
    (  memberchk(element(XENC:'EncryptionMethod', EncryptionMethodAttributes, _), EncryptedData)
    -> % This is a mandatory attribute
           memberchk('Algorithm'=XMLAlgorithm, EncryptionMethodAttributes),
           (  ssl_algorithm(XMLAlgorithm, Algorithm, IVSize)
           -> true
           ; domain_error(block_cipher, XMLAlgorithm)
           )
        % In theory the EncryptionMethod is optional. In pracitse though, if the method is not supplied we
        % cannot decrypt the data. In the future we could support encryption_algorithm/1 as an option to
        % decrypt_element/3 but for now raise an exception
    ; existence_error(encryption_method, EncryptedData)
    ).

base64_element([CipherValueElement], CipherValue):-
    atom_codes(CipherValueElement, Base64Codes),
    delete_newlines(Base64Codes, TrimmedCodes),
    string_codes(Trimmed, TrimmedCodes),
    base64(CipherValue, Trimmed).

delete_newlines([], []):- !.
delete_newlines([13|As], B):- !, delete_newlines(As, B).
delete_newlines([10|As], B):- !, delete_newlines(As, B).
delete_newlines([A|As], [A|B]):- !, delete_newlines(As, B).



%!	load_certificate_from_base64_string(+String, -Certificate) is det.
%
%	Loads a certificate from a string, adding newlines and header
%       where appropriate so that OpenSSL 1.0.1+ will be able to parse it

load_certificate_from_base64_string(UnnormalizedData, Certificate):-
    normalize_space(codes(Codes), UnnormalizedData),
    % Break into 64-byte chunks
    chunk_certificate(Codes, Chunks),
    atomics_to_string(["-----BEGIN CERTIFICATE-----"|Chunks], '\n', CompleteCertificate),
    setup_call_cleanup(open_string(CompleteCertificate, StringStream),
                       load_certificate(StringStream, Certificate),
                       close(StringStream)).

chunk_certificate(Codes, [Chunk|Chunks]):-
    length(ChunkCodes, 64),
    append(ChunkCodes, Rest, Codes),
    !,
    string_codes(Chunk, ChunkCodes),
    chunk_certificate(Rest, Chunks).
chunk_certificate([], ["-----END CERTIFICATE-----\n"]):- !.
chunk_certificate(LastCodes, [LastChunk, "-----END CERTIFICATE-----\n"]):-
    string_codes(LastChunk, LastCodes).
