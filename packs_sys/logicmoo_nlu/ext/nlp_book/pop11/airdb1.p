;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; airdb1.p [Chapter  9] Example database about airline companies

;;; [located COMPANY COUNTRY]
;;; [bought COMPANY COMPANY]
;;; [subsidiary COMPANY COMPANY]

vars database;
  [
  [located Air_Cal US]
  [located American_Airlines US]
  [located British_Airways England]
  [located British_Caledonian England]
  [located Cambrian_Airways Scotland]
  [located Carl_Icahn US]
  [located Continental US]
  [located Delta US]
  [located Eastern_Airlines US]
  [located Empire US]
  [located Frontier US]
  [located Hughes_Airwest US]
  [located National_Airlines US]
  [located Northwest US]
  [located Ozark US]
  [located Pan_Am_Pacific US]
  [located Pan_Am US]
  [located People_Express US]
  [located Piedmont US]
  [located Republic US]
  [located Scotair Scotland]
  [located Scottish_Airways Scotland]
  [located Texas_Air US]
  [located Trans_World_Airlines US]
  [located US_Air US]
  [located United_Airlines US]
  [located Virgin_Airways England]
  [located Western US]

  [bought American_Airlines Air_Cal]
  [bought British_Airways British_Caledonian]
  [bought Carl_Icahn Trans_World_Airlines]
  [bought Delta Western]
  [bought Northwest Republic]
  [bought Pan_Am National_Airlines]
  [bought People_Express Frontier]
  [bought Piedmont Empire]
  [bought Republic Hughes_Airwest]
  [bought Texas_Air Continental]
  [bought Texas_Air Eastern_Airlines]
  [bought Texas_Air People_Express]
  [bought Trans_World_Airlines Ozark]
  [bought United_Airlines Pan_Am_Pacific]
  [bought US_Air Piedmont]

  [subsidiary Scotair British_Caledonian]
  [subsidiary Scotair British_Airways]
  [subsidiary Cambrian_Airways British_Airways]
  [subsidiary Scottish_Airways British_Airways]

] -> database;

vars airdb1; true -> airdb1;
