
:- expects_dialect(lps).

% slamodel.lps SLA Model as a smart contract
% version alfa 1.0 Jacinto Dávila
% 
% % based on
% Study report "Standards terms and performances criteria in Service
% Level Agreements for cloud computing services" (SMART 2013/0039)
% https://ec.europa.eu/digital-single-market/en/news/study-report-standards-terms-and-performances-criteria-service-level-agreements-cloud-computing
%
% The final report has a "MODEL SLA" (page 43) a sort of template to
% create contracts for Cloud Service Level Agreements in Europe 
% (see tables pag 59 onwards).
% 
% http://ec.europa.eu/newsroom/dae/document.cfm?doc_id=10860
%
% "INPUT FOR THE DEVELOPMENT OF MODEL TERMS FOR CLOUD COMPUTING
% SERVICE LEVEL AGREEMENTS FOR CONTRACTS BETWEEN
% PROVIDERS AND PROFESSIONAL CLOUD USERS - MODEL SLA"
% This study was carried out for the European Commission by
% time.lex CVBA - Hans Graux and Jos Dumortier and
% Spark Ltd – Patricia Ypma, Jasmine Simpson, Peter McNally, and Marc de Vries
%
% The party (or parties) that is required under the Services Agreement to provide
% cloud services will be referred to as the ‘Cloud Service Provider’ or ‘CSP’,
% whereas the party (or parties) that will be permitted under the Services
% Agreement to use such services will be referred to as the ‘Customer’.
% This SLA will define the specific service level objectives (SLOs) and service
% quality assurances which the CSP has committed to providing 23 . It also sets
% out any exceptions to these obligations, specifies measurement and evaluation
% procedures, and identifies any remedies to which the Customer is entitled. It
% should be noted that the obligations in the SLA relate to the services as
% described in the Services Agreement, irrespective of whether the CSP relies
% on a third party service providers to offer those services. Therefore, the
% obligations as set out herein shall include any service providers contracted
% by the CSP to offer the services to the Customer.
%
maxTime(15).

fluents 
   % state variable to indicate how much down time has been accummulated
   total_time_of_scheduled_maintenance(TT), 
   % binary flag indicating that support has been requested
   support_requested(_), 
   % it time to check
   time_to_test, 
   % to check availabilty, this variables set the size of downloads
   sample_benchmark(_), 
   % a message for a client has been issued and confirmed
   notified_of_breach(_,_,_),
   % has the customer claimed her/his credit
   claimed(Customer, Amount, Month), 
   % a record of the last time the system was tested
   last_time_tested(_).

events 
   % a message to request down time for maintenance
   request_maintenance(_), 
   % an event to decide, this month, on the extend of the breach if any
   in_breach_by(_,_), 
   % a message reporting measured availability at that Month
   measured_availability(_Month, _Percentage),
   % a request for support
   request_support(_), 
   % a message indicating that a request is being served
   attending_request(_), 
   % a message indicating that the end of Month is being reached
   end_of_month(_Month). 

actions 
   % an order to schedule the given time for a maintenance downtime
   schedule_maintenance(_),
   % a notification for customers of Breach at Month
   notify_customer_of_breach(_Breach,_Month),  
   % a statement of the charge due to some discount credit for the customer
   apply_credit_charge(_),
   % a statment of the charge due to some discount credit for customers
   apply_support_credit_charge(_,_),
   % an indication that Customer must claim Amount at Month
   claim_remedy(Customer, Amount, Month), 
   % an order to test and report back on availability measures
   test_and_report_availability(_).

initially sample_benchmark(10000).
initially total_time_of_scheduled_maintenance(0).
initially last_time_tested(0). 

observe end_of_month(june) from 0 to 1. 
observe end_of_month(july) from 6 to 7. 

% from section 1.8 Applicability and exceptions
% If the total time of scheduled maintenance exceed 8 hours in a single
% calendar month, any hours in excess of 8 hours shall however be considered
% as being in breach of the SLA commitments.

if end_of_month(Month) from T1 to T2 
then in_breach_by(Breach_of, Month) from T2 to T3.

in_breach_by(Breach, Month) if 
   total_time_of_scheduled_maintenance(TT), 
   TT > 8, 
   Breach is TT-8,
   notify_customer_of_breach(Breach,Month).

in_breach_by(Breach, Month) if 
   total_time_of_scheduled_maintenance(TT), 
   TT =< 8.

% end_of_month indicates that time T is the end of the month Month or not
% end_of_month(X, anymonth) :- 0 is X mod 30. 
% 
observe request_maintenance(9) from 2 to 3. 

if request_maintenance(Expected_Time) from T to T1 
then schedule_maintenance(Expected_Time) from T2 to T3, T1 + 1 < T2.

schedule_maintenance(ET) initiates total_time_of_scheduled_maintenance(TT) if
   total_time_of_scheduled_maintenance(T0),
   TT is T0 + ET.

schedule_maintenance(ET) terminates total_time_of_scheduled_maintenance(TO)  if
   total_time_of_scheduled_maintenance(T0).

% from section 1.9 Measurement and reporting of SLA compliance
% Reporting periods have a duration of one month, i.e. the CSP must make any
% reports available on at least a monthly basis.

% in_breach_by/2 would be an event reported into the blockchain

% from section 1.10 Change management – revisions of the SLA
% The Customer has the right to terminate the Services Agreement free of
% charge and without creating any right to compensation for the CSP if any
% communicated changes in the SLA will have or are reasonably likely to have a
% materially adverse effect on the Customer. The Customer must exercise this
% right within one month after being informed of the communicated changes;
% otherwise the Customer will be deemed to have accepted the changes.

% too difficult for short example!

% from section 1.11 Breaches of the SLA
% If a breach of a service level objective as stipulated below is measured and
% reported by the CSP, then the remedy will be provided automatically by the CSP.

% this is automatically achieved with condition-action rules

% If the breach is found to be established, the Customer will be entitled to
% the service credits set out in the sections below. If more than one service
% level objective is not met in relation to the same incident, only the highest
% service credit shall apply (i.e. they shall not apply cumulatively).

% If a breach of a service quality assurance as stipulated below is measured and
% reported by the CSP, then the CSP shall be required to provide notice of the
% breach to the Customer using a communications channel that is habitually used
% between the parties. The notice shall describe the nature and cause of the
% breach.

% this rule is being subsumed above
% if notify_of_breach(Amount, Month) 
% then notify_customer(Amount, Month).

% The Customer is thereafter entitled to claim a remedy from the CSP in
% accordance with the provisions of the Services Agreement and taking into
% account any liability restrictions which may be set out therein.
% The breach shall however be qualified as a substantive breach of the
% Services Agreement.

% this is where I think we need to model the other agent in the contract

% Customer {
% events notified_of_breach/2.
% actions claim_remedy/2.

% meanwhile a centralized solution
% 

notify_customer_of_breach(Amount,Month) 
initiates notified_of_breach(customer, Amount, Month).

notify_customer_of_breach(Amount,Month) 
terminates notified_of_breach(customer, OAmount, OMonth) if
   notified_of_breach(customer, OAmount, OMount), Month \== OMonth.

claim_remedy(Customer, Amount, Month) 
initiates claimed(Customer, Amount, Month). 

if notified_of_breach(Customer, Amount, Month) at T1,
   not claimed(Customer, Amount, Month) at T1
then claim_remedy(Customer, Amount, Month) from T1 to T2.

% }

% from section 1.12 Availability service level objective
% If the service level objective is not met, the following service credits
% shall apply (for the avoidance of doubt, only the highest credit in the
% table shall apply):

% service_credit(Measured availability percentage, Applicable service credit %)
service_credit(99.9, 10).
service_credit(99, 25).
service_credit(95, 50).
service_credit(90, 100).

if measured_availability(Month, Percentage) from T to T1,
  highest_credit(Percentage, Credit_to_apply)
then apply_credit_charge(Credit_to_apply) from T1 to T2.

highest_credit(P, C) :- 
   service_credit(R, C),
   P =< R, 
   not(higher_credit_than(P, C)).

higher_credit_than(P, C) :- 
    service_credit(R, C2), 
    P =< R, C < C2. 

observe measured_availability(june, 94) from 1 to 2. 
observe measured_availability(july, 99.9) from 5 to 6. 

% this is where a systematic logical approach could be very useful for
% the customer. In this case, the contract states that the CSP is responsible
% for measuring availability. But a more sparse, forgetful measuring will
% work on its advantage. Presumably the customer would want some
% particular enforcing to be in place

if time_to_test at T,
   sample_benchmark(Size) at T
then test_and_report_availability(Size) from T to T2.

time_to_test at T if
    last_time_tested(T0) at T,
    inter_test_time(Delta),
    T is T0 + Delta.

% inter_test_time(2 days)
inter_test_time(2).

test_and_report_availability(_) initiates last_time_tested(T2) if
   current_time(T2). 

test_and_report_availability(_) terminates last_time_tested(TX).

% with test_and_report_availability/1 implemented as some low level function

% from section 1.13. Support service level objective
% The support services as defined by the Services Agreements shall be
% offered in accordance with the service levels below.
% For the purposes of this SLA, ‘response time’ refers to the duration of time
% between the submission of a support request by the Customer and the receipt
% of a substantive and non-automated response on behalf of the CSP
% (irrespective of whether the response resolves the issue reported by
% the Customer).
% If no support requests are made, the objective will be automatically met.

% Measured response time Applicable service credit

% support_service_credit(4 hours, 10%).
support_service_credit(4, 10).
% support_service_credit(one business day, 25%).
support_service_credit(24, 25).
% support_service_credit(three business days, 50%).
support_service_credit(72, 50).

if request_support(ID) from T1 to T2,
   support_service_credit(Time, Credit),
   TA is T2 + Time,
   support_requested(ID) at TA
then apply_support_credit_charge(ID, Credit) from T4 to T5.

request_support(ID) initiates support_requested(ID).
attending_request(ID) terminates support_requested(ID).
% apply_support_credit_charge(ID, _) terminates support_requested(ID).

observe request_support(jacinto001) from 2 to 3. 
observe attending_request(jacinto001) from 10 to 11. 

% from section 1.15
% a first attempt to model Service quality assurances which describe
% conditions which either hold or not and must be enforced by the contract

% Data Portability Quality Assurance (cells to be marked in green if applicable,
% red if not applicable). To be selected as apply among the following:

% 1. Customer data is not retrievable by the Customer via a single download link
% or documented API interface.

% false
%    customer_data_retrievable_via(Data_ID, single_link, Customer) at T,
%    owner(Data_ID, Customer) at T.
% false
%     customer_data_retrievable_via(Data_ID, documented_API, Customer) at T,
%    owner(Data_ID, Customer) at T.

% 2. Customer data is not available via the Internet; however, it is available
% via a physical carrier.

% customer_data_available_via(DataID, physical_carrier, Customer) at T if
%    owner(DataID, Customer) at T.
% false
%    customer_data_available_via(Data_ID, internet, Customer) at T,
%    owner(Data_ID, Customer) at T.

% presumably:
% customer_data_available_via(Data_ID, internet, Customer) at T if
%    customer_data_retrievable_via(Data_ID, single_link, Customer) at T.
%
% customer_data_available_via(Data_ID, internet, Customer) at T if
%    customer_data_retrievable_via(Data_ID, documented_API, Customer) at T.

% 3. Customer data is retrievable by the Customer via a single
% download link or documented API interface. The data format is not
% documented in a manner that allows the Customer to understand the structure
% and semantic meaning of the data.

% customer_data_retrievable_via(Data_ID, single_link, Customer) at T if
%   unformated(Data_ID),
%   owner(Data_ID, Customer) at T.

% customer_data_retrievable_via(Data_ID, documented_API, Customer) at T if
%   unformated(Data_ID),
%   owner(Data_ID, Customer) at T.

% 4. Customer data is retrievable by the Customer via a single
% download link or documented API interface. The data format is
% structured and documented in a sufficient manner to allow the
% Customer to re-use it or to restructure it into a different data
% format if desired.

% customer_data_retrievable_via(Data_ID, single_link, Customer) at T if
%   well_formated(Data_ID),
%   owner(Data_ID, Customer) at T.

% customer_data_retrievable_via(Data_ID, documented_API, Customer) at T if
%   well_formated(Data_ID),
%   owner(Data_ID, Customer) at T.
