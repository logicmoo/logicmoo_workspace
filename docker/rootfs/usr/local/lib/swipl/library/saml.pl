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

:-module(saml,
         [saml_authenticate/4]).

:- autoload(library(base64),[base64/2]).
:- autoload(library(crypto),[rsa_sign/4,hex_bytes/2]).
:- autoload(library(debug),[debug/3,debugging/1]).
:- autoload(library(error),
	    [domain_error/2,existence_error/2,permission_error/3]).
:- autoload(library(lists),[member/2,subtract/3,select/3]).
:- autoload(library(memfile),
	    [ new_memory_file/1,
	      open_memory_file/4,
	      memory_file_to_atom/2,
	      free_memory_file/1
	    ]).
:- autoload(library(quintus),[otherwise/0]).
:- autoload(library(sgml),[load_structure/3]).
:- autoload(library(sgml_write),[xml_write/3]).
:- autoload(library(sha),[sha_hash/3]).
:- autoload(library(ssl),
	    [load_private_key/3,load_certificate/2,same_certificate/2]).
:- autoload(library(url),[parse_url/2,parse_url_search/2]).
:- autoload(library(uuid),[uuid/1]).
:- autoload(library(xmldsig),[xmld_verify_signature/4]).
:- autoload(library(xmlenc),
	    [load_certificate_from_base64_string/2,decrypt_xml/4]).
:- autoload(library(zlib),[zopen/3]).
:- autoload(library(http/http_client),[http_read_data/3]).
:- autoload(library(http/http_dispatch),[http_redirect/3]).
:- autoload(library(http/http_path),[http_absolute_location/3]).
:- autoload(library(http/http_open),[http_open/3]).

/** <module> SAML Authentication

This library uses SAML to exchange messages with an Identity Provider to establish
assertions about the current user's session. It operates only as the service end, not
the identity provider end.

@see https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf

There are four primary integration points for applications to use this code:
   1) You must declare at least one service provider (SP)
   2) You must declare at least one identity provider (IdP) per SP
   3) Finally, you can call saml_authenticate(+SP, +IdP, +Callback, +Request) to obtain assertions
      The asynchronous nature of the SAML process means that a callback must be used. Assuming
      that the IdP was able to provide at least some valid assertions about the user, after calling
      Callback with 2 extra arguments (a list of the assertion terms and the URL being request by
      the user), the user will be redirected back to their original URL. It is therefore up to the
      callback to ensure that this does not simply trigger another round of SAML negotiations - for
      example, by throwing http_reply(forbidden(RequestURL)) if the assertions are not strong enough
   4) Finally, your SP metadata will be available from the web server directly. This is required to
      configure the IdP. This will be available at './metadata.xml', relative to the LocationSpec
      provided when the SP was declared.

   Configuring an SP:
   To declare an SP, use the declaration
      :-saml_sp(+ServiceProvider: atom,
                +LocationSpec:    term,
                +PrivateKeySpec:  term,
                +Password:        atom
                +CertificateSpec: term,
                +Options:         list).

   The ServiceProvider is the identifier of your service. Ideally, this should be a fully-qualified URI
   The LocationSpec is a location that the HTTP dispatch layer will understand
      for example '.' or root('saml').
   The Private KeySpec is a 'file specifier' that resolves to a private key (see below for specifiers)
   The Password is a password used for reading the private key. If the key is not encrypted, any atom
      can be supplied as it will be ignored
   The CertificateSpec is a file specifier that resolves to a certificate holding the public key
      corresponding to PrivateKeySPec
   There are currently no implemented options (the list is ignored).

   Configuring an IdP:
   To declare an IdP, use the declaration
      :-saml_idp(+ServiceProvider: atom,
                 +MetadataSpec:    term).
   ServiceProvider is the identifier used when declaring your SP. You do not need to declare them in a
      particular order, but both must be present in the system before running saml_authenticate/4.
   MetadataSpec is a file specifier that resolves to the metadata for the IdP. Most IdPs will be able
      to provide this on request


   File Specifiers:
   The following specifiers are supported for locating files:
      * file(Filename): The local file Filename
      * resource(Resource): The prolog resource Resource. See resource/3
      * url(URL): The file identified by the HTTP (or HTTPS if you have the HTTPS plugin loaded) URL



*/

user:term_expansion(:-saml_idp(ServiceProvider, MetadataFile), Clauses):-
    saml_idp_clauses(ServiceProvider, MetadataFile, Clauses).

user:term_expansion(:-saml_sp(ServiceProvider, Spec, KeyFile, Password, CertFile, Options),
                    [saml:saml_acs_path(ServiceProvider, ACSPath),
                     saml:saml_sp_certificate(ServiceProvider, Certificate, PEMData, PrivateKey),
                     ( :-http_handler(MetadataPath, saml:saml_metadata(ServiceProvider, Options), [])),
                     ( :-http_handler(ACSPath, saml:saml_acs_handler(ServiceProvider, Options), []))]):-
    http_absolute_location(Spec, Root, []),
    atom_concat(Root, '/auth', ACSPath),
    atom_concat(Root, '/metadata.xml', MetadataPath),
    read_key(KeyFile, Password, PrivateKey),
    read_certificate(CertFile, Certificate, PEMData).

read_key(Spec, Password, Key):-
    setup_call_cleanup(open_spec(Spec, Stream),
                       load_private_key(Stream, Password, Key),
                       close(Stream)).

read_certificate(Spec, Certificate, PEMData):-
    setup_call_cleanup(open_spec(Spec, Stream1),
                       read_string(Stream1, _, PEMData),
                       close(Stream1)),
    setup_call_cleanup(open_string(PEMData, Stream2),
                       load_certificate(Stream2, Certificate),
                       close(Stream2)).

open_spec(Spec, Stream):-
    (  Spec = file(Filename)
    -> open(Filename, read, Stream)
    ;  Spec = resource(Name)
    -> open_resource(Name, read, Stream)
    ;  Spec = url(URL)
    -> http_open(URL, Stream, [])
    ;  domain_error(file_specification, Spec)
    ).

:-multifile(saml:saml_sp_certificate/4).
:-multifile(saml:saml_idp/3).
:-multifile(saml:saml_idp_certificate/4).
:-multifile(saml:saml_idp_binding/4).
:-multifile(saml:saml_acs_path/2).

saml_idp_clauses(ServiceProvider, MetadataSpec, Clauses):-
    setup_call_cleanup(open_spec(MetadataSpec, Stream),
                       load_structure(Stream, Metadata, [dialect(xmlns)]),
                       close(Stream)),
    (  memberchk(element('urn:oasis:names:tc:SAML:2.0:metadata':'EntitiesDescriptor', _, EntitiesDescriptor), Metadata)
    -> (  memberchk(element('urn:oasis:names:tc:SAML:2.0:metadata':'EntityDescriptor', EntityDescriptorAttributes, EntityDescriptor), EntitiesDescriptor),
              memberchk(element('urn:oasis:names:tc:SAML:2.0:metadata':'IDPSSODescriptor', IDPSSODescriptorAttributes, IDPSSODescriptor), EntityDescriptor)
           -> trust_saml_idp_descriptor(ServiceProvider, EntityDescriptorAttributes, IDPSSODescriptorAttributes, IDPSSODescriptor, Clauses)
           ;  existence_error(idp_descriptor, MetadataSpec)
           )
    ;  memberchk(element('urn:oasis:names:tc:SAML:2.0:metadata':'EntityDescriptor', EntityDescriptorAttributes, EntityDescriptor), Metadata),
           memberchk(element('urn:oasis:names:tc:SAML:2.0:metadata':'IDPSSODescriptor', IDPSSODescriptorAttributes, IDPSSODescriptor), EntityDescriptor)
    -> trust_saml_idp_descriptor(ServiceProvider, EntityDescriptorAttributes, IDPSSODescriptorAttributes, IDPSSODescriptor, Clauses)
    ;  existence_error(idp_descriptor, MetadataSpec)
    ).

trust_saml_idp_descriptor(ServiceProvider,
                          EntityDescriptorAttributes,
                          IDPSSODescriptorAttributes,
                          IDPSSODescriptor,
                          [saml:saml_idp(ServiceProvider, EntityID, MustSign)|Clauses]):-
    memberchk(entityID=EntityID, EntityDescriptorAttributes),
    findall(saml:saml_idp_binding(ServiceProvider, EntityID, Binding, BindingInfo),
            ( member(element('urn:oasis:names:tc:SAML:2.0:metadata':'SingleSignOnService', SingleSignOnServiceAttributes, SingleSignOnService), IDPSSODescriptor),
              process_saml_binding(SingleSignOnServiceAttributes, SingleSignOnService, Binding, BindingInfo)
            ),
            Clauses,
            Tail),
    (  Tail == Clauses
    -> existence_error(supported_binding, IDPSSODescriptor)
    ;  true
    ),
    findall(saml:saml_idp_certificate(ServiceProvider, EntityID, CertificateUse, Certificate),
            idp_certificate(IDPSSODescriptor, CertificateUse, Certificate),
            Tail),
    (  memberchk('WantAuthnRequestsSigned'=true, IDPSSODescriptorAttributes)
    -> MustSign = true
    ;  MustSign = false
    ).

idp_certificate(IDPSSODescriptor, CertificateUse, Certificate):-
    member(element('urn:oasis:names:tc:SAML:2.0:metadata':'KeyDescriptor', KeyDescriptorAttributes, KeyDescriptor), IDPSSODescriptor),
    memberchk(use=CertificateUse, KeyDescriptorAttributes),
    memberchk(element('http://www.w3.org/2000/09/xmldsig#':'KeyInfo', _, KeyInfo), KeyDescriptor),
    memberchk(element('http://www.w3.org/2000/09/xmldsig#':'X509Data', _, X509Data), KeyInfo),
    memberchk(element('http://www.w3.org/2000/09/xmldsig#':'X509Certificate', _, [X509CertificateData]), X509Data),
    load_certificate_from_base64_string(X509CertificateData, Certificate).


process_saml_binding(SingleSignOnServiceAttributes, _, 'urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect', Location):-
    memberchk('Binding'='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect', SingleSignOnServiceAttributes),
    !,
    memberchk('Location'=Location, SingleSignOnServiceAttributes).

process_saml_binding(SingleSignOnServiceAttributes, _, 'urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST', Location):-
    memberchk('Binding'='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST', SingleSignOnServiceAttributes),
    !,
    memberchk('Location'=Location, SingleSignOnServiceAttributes).



form_authn_request(Request, ID, Destination, Date, ServiceProvider, ExtraElements, XML):-
    saml_acs_path(ServiceProvider, Path),
    subtract(Request, [path(_), search(_)], Request1),
    parse_url(ACSURL, [path(Path)|Request1]),
    SAMLP = 'urn:oasis:names:tc:SAML:2.0:protocol',
    SAML = 'urn:oasis:names:tc:SAML:2.0:assertion',
    XML = element(SAMLP:'AuthnRequest', ['ID'=ID,
                                         'Version'='2.0',
                                         'IssueInstant'=Date,
                                         'Destination'=Destination,
                                         'IsPassive'=false,
                                         'ProtocolBinding'='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST',
                                         'AssertionConsumerServiceURL'=ACSURL],
                  [element(SAML:'Issuer', [], [ServiceProvider]),
                   element(SAMLP:'NameIDPolicy', ['AllowCreate'=true,
                                                  'Format'='urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified'], [])|ExtraElements]).


:-meta_predicate(saml_authenticate(+, +, 2, +)).
saml_authenticate(ServiceProvider, IdentityProvider, Callback, Request):-
    memberchk(request_uri(RequestingURI), Request),
    format(atom(RelayState), '~q', [saml(RequestingURI, Callback)]),
    get_xml_timestamp(Date),
    uuid(UUID),
    % the ID must start with a letter but the UUID may start with a number. Resolve this by prepending an 'a'
    atom_concat(a, UUID, ID),
    saml_idp(ServiceProvider, IdentityProvider, _MustSign),
    % Always sign the request
    MustSign = true,
    XMLOptions = [header(false), layout(false)],
    (  saml_idp_binding(ServiceProvider, IdentityProvider, 'urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect', BaseURL)
    -> parse_url(BaseURL, Parts),
           form_authn_request(Request, ID, BaseURL, Date, ServiceProvider, [], XML),
           with_output_to(string(XMLString), xml_write(current_output, XML, XMLOptions)),
           debug(saml, 'XML:~n~s~n', [XMLString]),
           setup_call_cleanup(new_memory_file(MemFile),
                          (setup_call_cleanup(open_memory_file(MemFile, write, MemWrite, [encoding(octet)]),
                                               (setup_call_cleanup(zopen(MemWrite, Write, [format(raw_deflate), level(9), close_parent(false)]),
                                                               format(Write, '~s', [XMLString]),
                                                               close(Write))
                                               ),
                                           close(MemWrite)),
                            memory_file_to_atom(MemFile, SAMLRequestRaw)
                          ),
                          free_memory_file(MemFile)),
           base64(SAMLRequestRaw, SAMLRequest),
           debug(saml, 'Encoded request: ~w~n', [SAMLRequest]),
           (  MustSign == true
           -> saml_sp_certificate(ServiceProvider, _, _, PrivateKey),
              saml_sign(PrivateKey, XMLString, SAMLRequest, RelayState, ExtraParameters)
           ;  ExtraParameters = []
           )
    ; domain_error(supported_binding, IdentityProvider) % Other bindings could be implemented here, most obviously HTTP-POST and HTTP-POST-SimpleSign
    ),
    parse_url(IdPURL, [search(['SAMLRequest'=SAMLRequest, 'RelayState'=RelayState|ExtraParameters])|Parts]),
    debug(saml, 'Redirecting user to~n~w~n', [IdPURL]),
    http_redirect(moved_temporary, IdPURL, Request).

saml_simple_sign(PrivateKey, XMLString, _SAMLRequest, RelayState, ['SigAlg'=SigAlg,'Signature'=Signature]):-
    SigAlg = 'http://www.w3.org/2000/09/xmldsig#rsa-sha1',
    format(string(DataToSign), 'SAMLRequest=~s&RelayState=~w&SigAlg=~w', [XMLString, RelayState, SigAlg]),
    debug(saml, 'Data to sign with HTTP-Redirect-SimpleSign:~n~s~n', [DataToSign]),
    sha_hash(DataToSign, Digest, [algorithm(sha1)]),
    rsa_sign(PrivateKey, Digest, RawSignature,
             [ type(sha1),
               encoding(octet)
             ]),
    base64(RawSignature, Signature),
    debug(saml, 'Signature:~n~w~n', [Signature]).

saml_sign(PrivateKey, _XMLString, SAMLRequest, RelayState, ['SigAlg'=SigAlg,'Signature'=Base64Signature]):-
    SigAlg = 'http://www.w3.org/2000/09/xmldsig#rsa-sha1',
    parse_url_search(CodesToSign, ['SAMLRequest'=SAMLRequest, 'RelayState'=RelayState, 'SigAlg'=SigAlg]),
    string_codes(DataToSign, CodesToSign),
    debug(saml, 'Data to sign with HTTP-Redirect binding:~n~s~n', [DataToSign]),
    sha_hash(DataToSign, Digest, [algorithm(sha1)]),
    rsa_sign(PrivateKey, Digest, HexSignature,
             [ type(sha1),
               encoding(octet)
             ]),
    hex_bytes(HexSignature, SignatureBytes),
    atom_codes(SignatureAtom, SignatureBytes),
    base64(SignatureAtom, Base64Signature),
    debug(saml, '~nSignature:~n~w~n', [Base64Signature]).

saml_acs_handler(ServiceProvider, Options, Request):-
    debug(saml, 'Got a message back from IdP!~n', []),
    http_read_data(Request, PostedData, []),
    debug(saml, '~w~n', [PostedData]),
    memberchk('SAMLResponse'=Atom, PostedData),
    memberchk('RelayState'=Relay, PostedData),
    (  atom_to_term(Relay, saml(OriginalURI, Callback), _)
    -> true
    ;  throw(error(invalid_request, _))
    ),
    base64(RawData, Atom),
    atom_string(RawData, RawString),
    setup_call_cleanup(open_string(RawString, Stream),
                       load_structure(Stream, XML, [dialect(xmlns), keep_prefix(true)]),
                       close(Stream)),
    (  debugging(saml)
    -> xml_write(user_error, XML, [])
    ;  true
    ),
    process_saml_response(XML, ServiceProvider, Callback, OriginalURI, Options),
    debug(saml, 'Redirecting successfully authenticated user to ~w~n', [OriginalURI]),
    http_redirect(moved_temporary, OriginalURI, Request).


propagate_ns([], _, []):- !.
propagate_ns([element(Tag, Attributes, Children)|Siblings],
             NS,
             [element(Tag, NewAttributes, NewChildren)|NewSiblings]):-
    !,
    merge_ns(NS, Attributes, NewAttributes, NewNS),
    propagate_ns(Children, NewNS, NewChildren),
    propagate_ns(Siblings, NS, NewSiblings).
propagate_ns([X|Siblings], NS, [X|NewSiblings]):-
    propagate_ns(Siblings, NS, NewSiblings).

merge_ns([xmlns:Prefix=Value|NS], Attributes, NewAttributes, NewNS):-
    (  select(xmlns:Prefix=NewValue, Attributes, A1)
    -> NewNS = [xmlns:Prefix=NewValue|T],
           NewAttributes = [xmlns:Prefix=NewValue|N]
    ;  A1 = Attributes,
           NewNS = [xmlns:Prefix=Value|T],
           NewAttributes = [xmlns:Prefix=Value|N]
    ),
    merge_ns(NS, A1, N, T).

merge_ns([], A, A, NS):-
    findall(xmlns:Prefix=Value, member(xmlns:Prefix=Value, A), NS).


:-meta_predicate(process_saml_response(+, +, 2, +, +)).
process_saml_response(XML0, ServiceProvider, Callback, RequestURL, Options):-
    SAMLP = 'urn:oasis:names:tc:SAML:2.0:protocol',
    SAML = 'urn:oasis:names:tc:SAML:2.0:assertion',
    DS = 'http://www.w3.org/2000/09/xmldsig#',
    propagate_ns(XML0, [], XML),
    XML = [element(ns(_, SAMLP):'Response', _, Response)],
    % Response MAY  contain the following elements  : Issuer, Signature, Extensions
    % Response MAY  contain the following attributes: InResponseTo, Destination, Consent
    % Response MUST contain the following elements  : Status
    % Response MUST contain the following attributes: ID, IssueInstant, Version
    ( memberchk(element(ns(_, SAMLP):'Status', _StatusAttributes, Status), Response)->
        % Status MUST contain a StatusCode element, and MAY contain a StatusMessage and or StatusDetail element
        ( memberchk(element(ns(_, SAMLP):'StatusCode', StatusCodeAttributes, _StatusCode), Status)->
            % StatusCode MUST contain a Value attribute
            ( memberchk('Value'=StatusCodeValue, StatusCodeAttributes)->
                true
            ; domain_error(legal_saml_response, XML0)
            )
        ; domain_error(legal_saml_response, XML0)
        )
    ; domain_error(legal_saml_response, XML0)
    ),
    (  memberchk(element(ns(_, SAML):'Issuer', _, [IssuerName]), Response)
    -> true
    ;  IssuerName = {null}
    ),

    ( member(element(ns(_, DS):'Signature', _, Signature), Response)->
        xmld_verify_signature(XML, Signature, Certificate, []),
        % Check that the certificate used to sign was one in the metadata
        (  saml_idp_certificate(ServiceProvider, IssuerName, signing, IDPCertificate),
           same_certificate(Certificate, IDPCertificate)
        -> true
        ;  domain_error(trusted_certificate, Certificate)
        )
    ; otherwise->
        % Warning: Message is not signed. Assertions may be though
        % FIXME: Determine a policy for handling this - if the SP wants them signed, we must make sure they are
        true
    ),

    ( StatusCodeValue == 'urn:oasis:names:tc:SAML:2.0:status:Success'->
        % The user has authenticated in some capacity.
        % Note that we cannot say anything ABOUT the user yet. That will come once we process the assertions
        true
    ; StatusCodeValue == 'urn:oasis:names:tc:SAML:2.0:status:Requester'->
        throw(saml_rejected(requester))
    ; StatusCodeValue == 'urn:oasis:names:tc:SAML:2.0:status:Responder'->
        throw(saml_rejected(responder))
    ; StatusCodeValue == 'urn:oasis:names:tc:SAML:2.0:status:VersionMismatch'->
        throw(saml_rejected(version_mismatch))
    ; throw(saml_rejected(illegal_response))
    ),

    % Response MAY also contain 0..N of the following elements: Assertion, EncryptedAssertion.
    findall(Attribute,
            ( ( member(element(ns(SAMLPrefix, SAML):'Assertion', AssertionAttributes, Assertion), Response),
                process_assertion(ServiceProvider, IssuerName, XML, AssertionAttributes, Assertion, Attribute))
            ; member(element(ns(SAMLPrefix, SAML):'EncryptedAssertion', _, EncryptedAssertion), Response),
              decrypt_xml(EncryptedAssertion, DecryptedAssertion, saml:saml_key_callback(ServiceProvider), Options),
              member(element(ns(_, SAML):'Assertion', AssertionAttributes, Assertion), DecryptedAssertion),
              process_assertion(ServiceProvider, IssuerName, XML, AssertionAttributes, Assertion, Attribute)
            ),
            AcceptedAttributes),
    debug(saml, 'Calling SAML callback with these attributes: ~w', [AcceptedAttributes]),
    call(Callback, RequestURL, AcceptedAttributes).

process_assertion(ServiceProvider, _EntityID, Document, Attributes, Assertion, AssertedAttribute):-
    SAML = ns(_, 'urn:oasis:names:tc:SAML:2.0:assertion'),
    DS = ns(_, 'http://www.w3.org/2000/09/xmldsig#'),
    ( memberchk('ID'=_AssertionID, Attributes)->
        true
    ; throw(missing_assertion_id)
    ),
    % An Assertion MUST contain an Issuer, and MAY contain a Signature, Subject, Conditions, Advice, plus 0..N of the following:
    %   Statement
    %   AuthnStatement
    %   AuthzDecisionStatement
    %   AttributeStatement
    % It must also have all the following attributes, Version, ID, IssueInstant
    memberchk(element(SAML:'Issuer', _, [IssuerName]), Assertion),
    debug(saml, 'Received assertion from IdP ~w', [IssuerName]),
    ( member(element(DS:'Signature', _, Signature), Assertion)->
        xmld_verify_signature(Document, Signature, Certificate, []),
        % Check that the certificate used to sign was one in the metadata
        (  saml_idp_certificate(ServiceProvider, IssuerName, signing, IDPCertificate),
           same_certificate(Certificate, IDPCertificate)
        -> true
        ;  domain_error(trusted_certificate, Certificate)
        )
    ; otherwise->
        % Technically the standard allows this, but it seems like practically it would be useless?
        % Which part of the response SHOULD be signed? The entire thing or the assertions?
        true
        %throw(unsigned_response)
    ),
    ( memberchk(element(SAML:'Conditions', ConditionsAttributes, Conditions), Assertion)->
        % If conditions are present, we MUST check them. These can include arbitrary, user-defined conditions
        % and things like ProxyRestriction and OneTimeUse
        get_xml_timestamp(Date),
        ( memberchk('NotOnOrAfter'=Expiry, ConditionsAttributes)->
            Date @< Expiry
        ; true
        ),
        ( memberchk('NotBefore'=Expiry, ConditionsAttributes)->
            Date @> Expiry
        ; true
        ),
        forall(member(element(SAML:'Condition', ConditionAttributes, Condition), Conditions),
               condition_holds(ConditionAttributes, Condition)),
        forall(member(element(SAML:'AudienceRestriction', _AudienceRestrictionAttributes, AudienceRestriction), Conditions),
               (  member(element(SAML:'Audience', _, [Audience]), AudienceRestriction),
                  Audience == ServiceProvider
               -> true
               ;  permission_error(accept, assertion, AudienceRestriction)
               )),
        ( memberchk(element(SAML:'OneTimeUse', _, _), Conditions)->
            throw(one_time_use_not_supported)
        ; true
        ),
        ( memberchk(element(SAML:'ProxyRestriction', _, _), Conditions)->
            throw(proxy_restriction_not_supported)
        ; true
        )
    ; true
    ),
    % The Subject element is not mandatory. In the introduction to section 2, the specification states
    % "the <Subject> element is optional, and other specifications and profiles may utilize the SAML assertion
    % structure to make similar statements without specifying a subject, or possibly specifying the subject in an
    % alternate way"
    % However, 2.3.3 goes on to say that
    % "SAML itself defines no such statements, and an assertion without a subject has no defined meaning in this specification."
    % Specifically, 2.7.2, 2.7.3, 2.7.4 enumerate all the SAML-defined statements, and all of them say that the assertion MUST
    % contain a subject
    ( memberchk(element(SAML:'Subject', _, Subject), Assertion)->
        memberchk(element(SAML:'NameID', _, [IdPName]), Subject),
        debug(saml, 'Assertion is for subject ~w', [IdPName]),
        % Note that it is not mandatory for there to be any SubjectConfirmation in the message, however, since we must verify at least one
        % confirmation in order to trust that the subject has really associated with the IdP, a subject with no confirmations is useless anyway
        ( member(element(SAML:'SubjectConfirmation', SubjectConfirmationAttributes, SubjectConfirmation), Subject),
              subject_confirmation_is_valid(SubjectConfirmationAttributes, SubjectConfirmation)->
            debug(saml, 'Subject is confirmed', [])
        ; debug(saml, 'No valid subject confirmation could be found', []),
              throw(no_subject_confirmation)
        )
    ; throw(not_supported(assertion_without_subject))
    ),
    !,
    memberchk(element(SAML:'AttributeStatement', _, AttributeStatement), Assertion),
    member(element(SAML:'Attribute', AttributeAttributes, Attribute), AttributeStatement),
    memberchk('Name'=AttributeName, AttributeAttributes),
    (  memberchk('FriendlyName'=FriendlyName, AttributeAttributes)
    -> true
    ;  FriendlyName = ''
    ),
    memberchk(element(SAML:'AttributeValue', _, [AttributeValue]), Attribute),
    AssertedAttribute = attribute(AttributeName, FriendlyName, AttributeValue).

process_assertion(_Attributes, _Assertion, _, _, _, _):-
    debug(saml, 'Warning: Assertion was not valid', []).

condition_holds(_ConditionAttributes, _Condition):-
    throw(conditions_not_implemented).

get_xml_timestamp(Date):-
    get_time(Time),
    stamp_date_time(Time, date(Y, M, D, HH, MM, SSF, _, 'UTC', _), 'UTC'),
    SS is floor(SSF),
    format(atom(Date), '~w-~|~`0t~w~2+-~|~`0t~w~2+T~|~`0t~w~2+:~|~`0t~w~2+:~|~`0t~w~2+Z', [Y,M,D,HH,MM,SS]).


subject_confirmation_is_valid(SubjectConfirmationAttributes, SubjectConfirmation):-
    SAML = ns(_, 'urn:oasis:names:tc:SAML:2.0:assertion'),
    memberchk('Method'='urn:oasis:names:tc:SAML:2.0:cm:bearer', SubjectConfirmationAttributes), % this is the only method we support
    memberchk(element(SAML:'SubjectConfirmationData', Attributes, _SubjectConfirmationData), SubjectConfirmation),
    get_xml_timestamp(Date),
    ( memberchk('NotOnOrAfter'=Expiry, Attributes)->
        Date @< Expiry
    ; true
    ),
    ( memberchk('NotBefore'=Expiry, Attributes)->
        Date @> Expiry
    ; true
    ),
    ( memberchk('InResponseTo'=_InResponseTo, Attributes)->
        % FIXME: Check that we sent the message, somehow?
        true
    ; true
    ),
    ( memberchk('Recipient'=_Recipient, Attributes)->
        % FIXME: Check that this is us, somehow?
        true
    ; true
    ),
    % FIXME: We can also have other arbitrary elements and attributes in here for user-defined extensions. These are ignored.
    true.

saml_key_callback(ServiceProvider, certificate, KeyHint, Key):-
    saml_sp_certificate(ServiceProvider, KeyHint, _, Key),
    !.


saml_metadata(ServiceProvider, _Options, Request):-
    MD = 'urn:oasis:names:tc:SAML:2.0:metadata',
    DS = 'http://www.w3.org/2000/09/xmldsig#',
    saml_sp_certificate(ServiceProvider, _X509Certificate, X509Certificate, _PrivateKey),

    % All of this should be configurable, eventually?
    EncryptionMethod = 'http://www.w3.org/2009/xmlenc11#rsa-oaep',
    NameIDFormat = 'urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified',
    ACSBinding = 'urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST',

    parse_url(RequestURL, Request),
    http_absolute_location('./auth', ACSLocation, [relative_to(RequestURL)]),

    % Extract the part of the certificate between the BEGIN and END delimiters
    ( sub_string(X509Certificate, CertMarkerStart, CertMarkerLength, _, "-----BEGIN CERTIFICATE-----\n"),
      sub_string(X509Certificate, CertEnd, _, _, "\n-----END CERTIFICATE-----"),
      CertStart is CertMarkerStart + CertMarkerLength,
      CertEnd > CertStart->
        CertLength is CertEnd - CertStart,
        sub_string(X509Certificate, CertStart, CertLength, _, PresentableCertificate)
    ; existence_error(certificate_data, X509Certificate)
    ),
    format(current_output, 'Content-type: text/xml~n~n', []),
    XML = [element(MD:'EntitiesDescriptor', [], [EntityDescriptor])],
    EntityDescriptor = element(MD:'EntityDescriptor', [entityID=ServiceProvider], [SPSSODescriptor]),
    SPSSODescriptor = element(MD:'SPSSODescriptor', ['AuthnRequestsSigned'=true,
                                                     protocolSupportEnumeration='urn:oasis:names:tc:SAML:2.0:protocol'], [EncryptionKeyDescriptor,
                                                                                                                          SigningKeyDescriptor,
                                                                                                                          element(MD:'NameIDFormat', [], [NameIDFormat]),
                                                                                                                          AssertionConsumerService]),
    EncryptionKeyDescriptor = element(MD:'KeyDescriptor', [use=encryption], [KeyInfo,
                                                                             element(MD:'EncryptionMethod', ['Algorithm'=EncryptionMethod], [])]),
    SigningKeyDescriptor = element(MD:'KeyDescriptor', [use=signing], [KeyInfo,
                                                                          element(MD:'EncryptionMethod', ['Algorithm'=EncryptionMethod], [])]),

    KeyInfo = element(DS:'KeyInfo', [], [X509Data]),
    X509Data = element(DS:'X509Data', [], [element(DS:'X509Certificate', [], [PresentableCertificate])]),
    AssertionConsumerService = element(MD:'AssertionConsumerService', [index='0', isDefault=true, 'Binding'=ACSBinding, 'Location'=ACSLocation], []),
    xml_write(current_output, XML, []).

