;;; excorporate-time-zones.el --- time zone conversion *- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Keywords: calendar

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Hash tables and functions that map (current-time-zone) values to
;; time zone names recognized by Exchange servers.

;; For example:
;; (current-time-zone) => (-14400 "EDT")
;; (exco-time-zone)    => "Eastern Standard Time"

;;; Code:

(require 'cl-lib)

;; Generated with:

;;(insert
;; (concat "(defvar exco--time-zone-olson-to-server "
;;	 (with-current-buffer
;;	     (url-retrieve-synchronously
;;	      (concat "https://raw.githubusercontent.com/unicode-org/cldr/"
;;		      "master/common/supplemental/windowsZones.xml"))
;;	   (goto-char (point-min))
;;	   (search-forward-regexp "^<!DOC")
;;	   (move-beginning-of-line nil)
;;	   (delete-region (point-min) (point))
;;	   (let ((table (make-hash-table :test #'equal))
;;		 (zone-lines
;;		  (nth 3 (nth 5 (car (xml-parse-region
;;				      (point-min) (point-max)))))))
;;	     (dolist (zone-line zone-lines)
;;	       (when (and (listp zone-line) (equal (car zone-line) 'mapZone))
;;		 (let ((names (split-string
;;			       (cdaddr (car (cdr zone-line))) " "))
;;		       (zone (cdaar (cdr zone-line))))
;;		   (dolist (name names)
;;		     (puthash name zone table)))))
;;	     (format "%S" table)))
;;	 " \"A hash table mapping IANA/Olson time zone"
;;	 " names to server time zone names.\")"))

(defvar exco--time-zone-olson-to-server
  #s(hash-table
     size 487
     test equal
     rehash-size 1.5
     rehash-threshold 0.8125
     data
     ("Etc/GMT+12"                     "Dateline Standard Time"
      "Etc/GMT+11"                     "UTC-11"
      "Pacific/Pago_Pago"              "UTC-11"
      "Pacific/Niue"                   "UTC-11"
      "Pacific/Midway"                 "UTC-11"
      "America/Adak"                   "Aleutian Standard Time"
      "Pacific/Honolulu"               "Hawaiian Standard Time"
      "Pacific/Rarotonga"              "Hawaiian Standard Time"
      "Pacific/Tahiti"                 "Hawaiian Standard Time"
      "Pacific/Johnston"               "Hawaiian Standard Time"
      "Etc/GMT+10"                     "Hawaiian Standard Time"
      "Pacific/Marquesas"              "Marquesas Standard Time"
      "America/Anchorage"              "Alaskan Standard Time"
      "America/Juneau"                 "Alaskan Standard Time"
      "America/Metlakatla"             "Alaskan Standard Time"
      "America/Nome"                   "Alaskan Standard Time"
      "America/Sitka"                  "Alaskan Standard Time"
      "America/Yakutat"                "Alaskan Standard Time"
      "Etc/GMT+9"                      "UTC-09"
      "Pacific/Gambier"                "UTC-09"
      "America/Tijuana"                "Pacific Standard Time (Mexico)"
      "America/Santa_Isabel"           "Pacific Standard Time (Mexico)"
      "Etc/GMT+8"                      "UTC-08"
      "Pacific/Pitcairn"               "UTC-08"
      "America/Los_Angeles"            "Pacific Standard Time"
      "America/Vancouver"              "Pacific Standard Time"
      "PST8PDT"                        "Pacific Standard Time"
      "America/Phoenix"                "US Mountain Standard Time"
      "America/Creston"                "US Mountain Standard Time"
      "America/Dawson_Creek"           "US Mountain Standard Time"
      "America/Fort_Nelson"            "US Mountain Standard Time"
      "America/Hermosillo"             "US Mountain Standard Time"
      "Etc/GMT+7"                      "US Mountain Standard Time"
      "America/Chihuahua"              "Mountain Standard Time (Mexico)"
      "America/Mazatlan"               "Mountain Standard Time (Mexico)"
      "America/Denver"                 "Mountain Standard Time"
      "America/Edmonton"               "Mountain Standard Time"
      "America/Cambridge_Bay"          "Mountain Standard Time"
      "America/Inuvik"                 "Mountain Standard Time"
      "America/Yellowknife"            "Mountain Standard Time"
      "America/Ojinaga"                "Mountain Standard Time"
      "America/Boise"                  "Mountain Standard Time"
      "MST7MDT"                        "Mountain Standard Time"
      "America/Whitehorse"             "Yukon Standard Time"
      "America/Dawson"                 "Yukon Standard Time"
      "America/Guatemala"              "Central America Standard Time"
      "America/Belize"                 "Central America Standard Time"
      "America/Costa_Rica"             "Central America Standard Time"
      "Pacific/Galapagos"              "Central America Standard Time"
      "America/Tegucigalpa"            "Central America Standard Time"
      "America/Managua"                "Central America Standard Time"
      "America/El_Salvador"            "Central America Standard Time"
      "Etc/GMT+6"                      "Central America Standard Time"
      "America/Chicago"                "Central Standard Time"
      "America/Winnipeg"               "Central Standard Time"
      "America/Rainy_River"            "Central Standard Time"
      "America/Rankin_Inlet"           "Central Standard Time"
      "America/Resolute"               "Central Standard Time"
      "America/Matamoros"              "Central Standard Time"
      "America/Indiana/Knox"           "Central Standard Time"
      "America/Indiana/Tell_City"      "Central Standard Time"
      "America/Menominee"              "Central Standard Time"
      "America/North_Dakota/Beulah"    "Central Standard Time"
      "America/North_Dakota/Center"    "Central Standard Time"
      "America/North_Dakota/New_Salem" "Central Standard Time"
      "CST6CDT"                        "Central Standard Time"
      "Pacific/Easter"                 "Easter Island Standard Time"
      "America/Mexico_City"            "Central Standard Time (Mexico)"
      "America/Bahia_Banderas"         "Central Standard Time (Mexico)"
      "America/Merida"                 "Central Standard Time (Mexico)"
      "America/Monterrey"              "Central Standard Time (Mexico)"
      "America/Regina"                 "Canada Central Standard Time"
      "America/Swift_Current"          "Canada Central Standard Time"
      "America/Bogota"                 "SA Pacific Standard Time"
      "America/Rio_Branco"             "SA Pacific Standard Time"
      "America/Eirunepe"               "SA Pacific Standard Time"
      "America/Coral_Harbour"          "SA Pacific Standard Time"
      "America/Guayaquil"              "SA Pacific Standard Time"
      "America/Jamaica"                "SA Pacific Standard Time"
      "America/Cayman"                 "SA Pacific Standard Time"
      "America/Panama"                 "SA Pacific Standard Time"
      "America/Lima"                   "SA Pacific Standard Time"
      "Etc/GMT+5"                      "SA Pacific Standard Time"
      "America/Cancun"                 "Eastern Standard Time (Mexico)"
      "America/New_York"               "Eastern Standard Time"
      "America/Nassau"                 "Eastern Standard Time"
      "America/Toronto"                "Eastern Standard Time"
      "America/Iqaluit"                "Eastern Standard Time"
      "America/Montreal"               "Eastern Standard Time"
      "America/Nipigon"                "Eastern Standard Time"
      "America/Pangnirtung"            "Eastern Standard Time"
      "America/Thunder_Bay"            "Eastern Standard Time"
      "America/Detroit"                "Eastern Standard Time"
      "America/Indiana/Petersburg"     "Eastern Standard Time"
      "America/Indiana/Vincennes"      "Eastern Standard Time"
      "America/Indiana/Winamac"        "Eastern Standard Time"
      "America/Kentucky/Monticello"    "Eastern Standard Time"
      "America/Louisville"             "Eastern Standard Time"
      "EST5EDT"                        "Eastern Standard Time"
      "America/Port-au-Prince"         "Haiti Standard Time"
      "America/Havana"                 "Cuba Standard Time"
      "America/Indianapolis"           "US Eastern Standard Time"
      "America/Indiana/Marengo"        "US Eastern Standard Time"
      "America/Indiana/Vevay"          "US Eastern Standard Time"
      "America/Grand_Turk"             "Turks And Caicos Standard Time"
      "America/Asuncion"               "Paraguay Standard Time"
      "America/Halifax"                "Atlantic Standard Time"
      "Atlantic/Bermuda"               "Atlantic Standard Time"
      "America/Glace_Bay"              "Atlantic Standard Time"
      "America/Goose_Bay"              "Atlantic Standard Time"
      "America/Moncton"                "Atlantic Standard Time"
      "America/Thule"                  "Atlantic Standard Time"
      "America/Caracas"                "Venezuela Standard Time"
      "America/Cuiaba"                 "Central Brazilian Standard Time"
      "America/Campo_Grande"           "Central Brazilian Standard Time"
      "America/La_Paz"                 "SA Western Standard Time"
      "America/Antigua"                "SA Western Standard Time"
      "America/Anguilla"               "SA Western Standard Time"
      "America/Aruba"                  "SA Western Standard Time"
      "America/Barbados"               "SA Western Standard Time"
      "America/St_Barthelemy"          "SA Western Standard Time"
      "America/Kralendijk"             "SA Western Standard Time"
      "America/Manaus"                 "SA Western Standard Time"
      "America/Boa_Vista"              "SA Western Standard Time"
      "America/Porto_Velho"            "SA Western Standard Time"
      "America/Blanc-Sablon"           "SA Western Standard Time"
      "America/Curacao"                "SA Western Standard Time"
      "America/Dominica"               "SA Western Standard Time"
      "America/Santo_Domingo"          "SA Western Standard Time"
      "America/Grenada"                "SA Western Standard Time"
      "America/Guadeloupe"             "SA Western Standard Time"
      "America/Guyana"                 "SA Western Standard Time"
      "America/St_Kitts"               "SA Western Standard Time"
      "America/St_Lucia"               "SA Western Standard Time"
      "America/Marigot"                "SA Western Standard Time"
      "America/Martinique"             "SA Western Standard Time"
      "America/Montserrat"             "SA Western Standard Time"
      "America/Puerto_Rico"            "SA Western Standard Time"
      "America/Lower_Princes"          "SA Western Standard Time"
      "America/Port_of_Spain"          "SA Western Standard Time"
      "America/St_Vincent"             "SA Western Standard Time"
      "America/Tortola"                "SA Western Standard Time"
      "America/St_Thomas"              "SA Western Standard Time"
      "Etc/GMT+4"                      "SA Western Standard Time"
      "America/Santiago"               "Pacific SA Standard Time"
      "America/St_Johns"               "Newfoundland Standard Time"
      "America/Araguaina"              "Tocantins Standard Time"
      "America/Sao_Paulo"              "E. South America Standard Time"
      "America/Cayenne"                "SA Eastern Standard Time"
      "Antarctica/Rothera"             "SA Eastern Standard Time"
      "Antarctica/Palmer"              "SA Eastern Standard Time"
      "America/Fortaleza"              "SA Eastern Standard Time"
      "America/Belem"                  "SA Eastern Standard Time"
      "America/Maceio"                 "SA Eastern Standard Time"
      "America/Recife"                 "SA Eastern Standard Time"
      "America/Santarem"               "SA Eastern Standard Time"
      "Atlantic/Stanley"               "SA Eastern Standard Time"
      "America/Paramaribo"             "SA Eastern Standard Time"
      "Etc/GMT+3"                      "SA Eastern Standard Time"
      "America/Buenos_Aires"           "Argentina Standard Time"
      "America/Argentina/La_Rioja"     "Argentina Standard Time"
      "America/Argentina/Rio_Gallegos" "Argentina Standard Time"
      "America/Argentina/Salta"        "Argentina Standard Time"
      "America/Argentina/San_Juan"     "Argentina Standard Time"
      "America/Argentina/San_Luis"     "Argentina Standard Time"
      "America/Argentina/Tucuman"      "Argentina Standard Time"
      "America/Argentina/Ushuaia"      "Argentina Standard Time"
      "America/Catamarca"              "Argentina Standard Time"
      "America/Cordoba"                "Argentina Standard Time"
      "America/Jujuy"                  "Argentina Standard Time"
      "America/Mendoza"                "Argentina Standard Time"
      "America/Godthab"                "Greenland Standard Time"
      "America/Montevideo"             "Montevideo Standard Time"
      "America/Punta_Arenas"           "Magallanes Standard Time"
      "America/Miquelon"               "Saint Pierre Standard Time"
      "America/Bahia"                  "Bahia Standard Time"
      "Etc/GMT+2"                      "UTC-02"
      "America/Noronha"                "UTC-02"
      "Atlantic/South_Georgia"         "UTC-02"
      "Atlantic/Azores"                "Azores Standard Time"
      "America/Scoresbysund"           "Azores Standard Time"
      "Atlantic/Cape_Verde"            "Cape Verde Standard Time"
      "Etc/GMT+1"                      "Cape Verde Standard Time"
      "Etc/GMT"                        "UTC"
      "America/Danmarkshavn"           "UTC"
      "Etc/UTC"                        "UTC"
      "Europe/London"                  "GMT Standard Time"
      "Atlantic/Canary"                "GMT Standard Time"
      "Atlantic/Faeroe"                "GMT Standard Time"
      "Europe/Guernsey"                "GMT Standard Time"
      "Europe/Dublin"                  "GMT Standard Time"
      "Europe/Isle_of_Man"             "GMT Standard Time"
      "Europe/Jersey"                  "GMT Standard Time"
      "Europe/Lisbon"                  "GMT Standard Time"
      "Atlantic/Madeira"               "GMT Standard Time"
      "Atlantic/Reykjavik"             "Greenwich Standard Time"
      "Africa/Ouagadougou"             "Greenwich Standard Time"
      "Africa/Abidjan"                 "Greenwich Standard Time"
      "Africa/Accra"                   "Greenwich Standard Time"
      "Africa/Banjul"                  "Greenwich Standard Time"
      "Africa/Conakry"                 "Greenwich Standard Time"
      "Africa/Bissau"                  "Greenwich Standard Time"
      "Africa/Monrovia"                "Greenwich Standard Time"
      "Africa/Bamako"                  "Greenwich Standard Time"
      "Africa/Nouakchott"              "Greenwich Standard Time"
      "Atlantic/St_Helena"             "Greenwich Standard Time"
      "Africa/Freetown"                "Greenwich Standard Time"
      "Africa/Dakar"                   "Greenwich Standard Time"
      "Africa/Lome"                    "Greenwich Standard Time"
      "Africa/Sao_Tome"                "Sao Tome Standard Time"
      "Africa/Casablanca"              "Morocco Standard Time"
      "Africa/El_Aaiun"                "Morocco Standard Time"
      "Europe/Berlin"                  "W. Europe Standard Time"
      "Europe/Andorra"                 "W. Europe Standard Time"
      "Europe/Vienna"                  "W. Europe Standard Time"
      "Europe/Zurich"                  "W. Europe Standard Time"
      "Europe/Busingen"                "W. Europe Standard Time"
      "Europe/Gibraltar"               "W. Europe Standard Time"
      "Europe/Rome"                    "W. Europe Standard Time"
      "Europe/Vaduz"                   "W. Europe Standard Time"
      "Europe/Luxembourg"              "W. Europe Standard Time"
      "Europe/Monaco"                  "W. Europe Standard Time"
      "Europe/Malta"                   "W. Europe Standard Time"
      "Europe/Amsterdam"               "W. Europe Standard Time"
      "Europe/Oslo"                    "W. Europe Standard Time"
      "Europe/Stockholm"               "W. Europe Standard Time"
      "Arctic/Longyearbyen"            "W. Europe Standard Time"
      "Europe/San_Marino"              "W. Europe Standard Time"
      "Europe/Vatican"                 "W. Europe Standard Time"
      "Europe/Budapest"                "Central Europe Standard Time"
      "Europe/Tirane"                  "Central Europe Standard Time"
      "Europe/Prague"                  "Central Europe Standard Time"
      "Europe/Podgorica"               "Central Europe Standard Time"
      "Europe/Belgrade"                "Central Europe Standard Time"
      "Europe/Ljubljana"               "Central Europe Standard Time"
      "Europe/Bratislava"              "Central Europe Standard Time"
      "Europe/Paris"                   "Romance Standard Time"
      "Europe/Brussels"                "Romance Standard Time"
      "Europe/Copenhagen"              "Romance Standard Time"
      "Europe/Madrid"                  "Romance Standard Time"
      "Africa/Ceuta"                   "Romance Standard Time"
      "Europe/Warsaw"                  "Central European Standard Time"
      "Europe/Sarajevo"                "Central European Standard Time"
      "Europe/Zagreb"                  "Central European Standard Time"
      "Europe/Skopje"                  "Central European Standard Time"
      "Africa/Lagos"                   "W. Central Africa Standard Time"
      "Africa/Luanda"                  "W. Central Africa Standard Time"
      "Africa/Porto-Novo"              "W. Central Africa Standard Time"
      "Africa/Kinshasa"                "W. Central Africa Standard Time"
      "Africa/Bangui"                  "W. Central Africa Standard Time"
      "Africa/Brazzaville"             "W. Central Africa Standard Time"
      "Africa/Douala"                  "W. Central Africa Standard Time"
      "Africa/Algiers"                 "W. Central Africa Standard Time"
      "Africa/Libreville"              "W. Central Africa Standard Time"
      "Africa/Malabo"                  "W. Central Africa Standard Time"
      "Africa/Niamey"                  "W. Central Africa Standard Time"
      "Africa/Ndjamena"                "W. Central Africa Standard Time"
      "Africa/Tunis"                   "W. Central Africa Standard Time"
      "Etc/GMT-1"                      "W. Central Africa Standard Time"
      "Asia/Amman"                     "Jordan Standard Time"
      "Europe/Bucharest"               "GTB Standard Time"
      "Asia/Nicosia"                   "GTB Standard Time"
      "Asia/Famagusta"                 "GTB Standard Time"
      "Europe/Athens"                  "GTB Standard Time"
      "Asia/Beirut"                    "Middle East Standard Time"
      "Africa/Cairo"                   "Egypt Standard Time"
      "Europe/Chisinau"                "E. Europe Standard Time"
      "Asia/Damascus"                  "Syria Standard Time"
      "Asia/Hebron"                    "West Bank Standard Time"
      "Asia/Gaza"                      "West Bank Standard Time"
      "Africa/Johannesburg"            "South Africa Standard Time"
      "Africa/Bujumbura"               "South Africa Standard Time"
      "Africa/Gaborone"                "South Africa Standard Time"
      "Africa/Lubumbashi"              "South Africa Standard Time"
      "Africa/Maseru"                  "South Africa Standard Time"
      "Africa/Blantyre"                "South Africa Standard Time"
      "Africa/Maputo"                  "South Africa Standard Time"
      "Africa/Kigali"                  "South Africa Standard Time"
      "Africa/Juba"                    "South Africa Standard Time"
      "Africa/Mbabane"                 "South Africa Standard Time"
      "Africa/Lusaka"                  "South Africa Standard Time"
      "Africa/Harare"                  "South Africa Standard Time"
      "Etc/GMT-2"                      "South Africa Standard Time"
      "Europe/Kiev"                    "FLE Standard Time"
      "Europe/Mariehamn"               "FLE Standard Time"
      "Europe/Sofia"                   "FLE Standard Time"
      "Europe/Tallinn"                 "FLE Standard Time"
      "Europe/Helsinki"                "FLE Standard Time"
      "Europe/Vilnius"                 "FLE Standard Time"
      "Europe/Riga"                    "FLE Standard Time"
      "Europe/Uzhgorod"                "FLE Standard Time"
      "Europe/Zaporozhye"              "FLE Standard Time"
      "Asia/Jerusalem"                 "Israel Standard Time"
      "Europe/Kaliningrad"             "Kaliningrad Standard Time"
      "Africa/Khartoum"                "Sudan Standard Time"
      "Africa/Tripoli"                 "Libya Standard Time"
      "Africa/Windhoek"                "Namibia Standard Time"
      "Asia/Baghdad"                   "Arabic Standard Time"
      "Europe/Istanbul"                "Turkey Standard Time"
      "Asia/Riyadh"                    "Arab Standard Time"
      "Asia/Bahrain"                   "Arab Standard Time"
      "Asia/Kuwait"                    "Arab Standard Time"
      "Asia/Qatar"                     "Arab Standard Time"
      "Asia/Aden"                      "Arab Standard Time"
      "Europe/Minsk"                   "Belarus Standard Time"
      "Europe/Moscow"                  "Russian Standard Time"
      "Europe/Kirov"                   "Russian Standard Time"
      "Europe/Simferopol"              "Russian Standard Time"
      "Africa/Nairobi"                 "E. Africa Standard Time"
      "Antarctica/Syowa"               "E. Africa Standard Time"
      "Africa/Djibouti"                "E. Africa Standard Time"
      "Africa/Asmera"                  "E. Africa Standard Time"
      "Africa/Addis_Ababa"             "E. Africa Standard Time"
      "Indian/Comoro"                  "E. Africa Standard Time"
      "Indian/Antananarivo"            "E. Africa Standard Time"
      "Africa/Mogadishu"               "E. Africa Standard Time"
      "Africa/Dar_es_Salaam"           "E. Africa Standard Time"
      "Africa/Kampala"                 "E. Africa Standard Time"
      "Indian/Mayotte"                 "E. Africa Standard Time"
      "Etc/GMT-3"                      "E. Africa Standard Time"
      "Asia/Tehran"                    "Iran Standard Time"
      "Asia/Dubai"                     "Arabian Standard Time"
      "Asia/Muscat"                    "Arabian Standard Time"
      "Etc/GMT-4"                      "Arabian Standard Time"
      "Europe/Astrakhan"               "Astrakhan Standard Time"
      "Europe/Ulyanovsk"               "Astrakhan Standard Time"
      "Asia/Baku"                      "Azerbaijan Standard Time"
      "Europe/Samara"                  "Russia Time Zone 3"
      "Indian/Mauritius"               "Mauritius Standard Time"
      "Indian/Reunion"                 "Mauritius Standard Time"
      "Indian/Mahe"                    "Mauritius Standard Time"
      "Europe/Saratov"                 "Saratov Standard Time"
      "Asia/Tbilisi"                   "Georgian Standard Time"
      "Europe/Volgograd"               "Volgograd Standard Time"
      "Asia/Yerevan"                   "Caucasus Standard Time"
      "Asia/Kabul"                     "Afghanistan Standard Time"
      "Asia/Tashkent"                  "West Asia Standard Time"
      "Antarctica/Mawson"              "West Asia Standard Time"
      "Asia/Oral"                      "West Asia Standard Time"
      "Asia/Aqtau"                     "West Asia Standard Time"
      "Asia/Aqtobe"                    "West Asia Standard Time"
      "Asia/Atyrau"                    "West Asia Standard Time"
      "Indian/Maldives"                "West Asia Standard Time"
      "Indian/Kerguelen"               "West Asia Standard Time"
      "Asia/Dushanbe"                  "West Asia Standard Time"
      "Asia/Ashgabat"                  "West Asia Standard Time"
      "Asia/Samarkand"                 "West Asia Standard Time"
      "Etc/GMT-5"                      "West Asia Standard Time"
      "Asia/Yekaterinburg"             "Ekaterinburg Standard Time"
      "Asia/Karachi"                   "Pakistan Standard Time"
      "Asia/Qyzylorda"                 "Qyzylorda Standard Time"
      "Asia/Calcutta"                  "India Standard Time"
      "Asia/Colombo"                   "Sri Lanka Standard Time"
      "Asia/Katmandu"                  "Nepal Standard Time"
      "Asia/Almaty"                    "Central Asia Standard Time"
      "Antarctica/Vostok"              "Central Asia Standard Time"
      "Asia/Urumqi"                    "Central Asia Standard Time"
      "Indian/Chagos"                  "Central Asia Standard Time"
      "Asia/Bishkek"                   "Central Asia Standard Time"
      "Asia/Qostanay"                  "Central Asia Standard Time"
      "Etc/GMT-6"                      "Central Asia Standard Time"
      "Asia/Dhaka"                     "Bangladesh Standard Time"
      "Asia/Thimphu"                   "Bangladesh Standard Time"
      "Asia/Omsk"                      "Omsk Standard Time"
      "Asia/Rangoon"                   "Myanmar Standard Time"
      "Indian/Cocos"                   "Myanmar Standard Time"
      "Asia/Bangkok"                   "SE Asia Standard Time"
      "Antarctica/Davis"               "SE Asia Standard Time"
      "Indian/Christmas"               "SE Asia Standard Time"
      "Asia/Jakarta"                   "SE Asia Standard Time"
      "Asia/Pontianak"                 "SE Asia Standard Time"
      "Asia/Phnom_Penh"                "SE Asia Standard Time"
      "Asia/Vientiane"                 "SE Asia Standard Time"
      "Asia/Saigon"                    "SE Asia Standard Time"
      "Etc/GMT-7"                      "SE Asia Standard Time"
      "Asia/Barnaul"                   "Altai Standard Time"
      "Asia/Hovd"                      "W. Mongolia Standard Time"
      "Asia/Krasnoyarsk"               "North Asia Standard Time"
      "Asia/Novokuznetsk"              "North Asia Standard Time"
      "Asia/Novosibirsk"               "N. Central Asia Standard Time"
      "Asia/Tomsk"                     "Tomsk Standard Time"
      "Asia/Shanghai"                  "China Standard Time"
      "Asia/Hong_Kong"                 "China Standard Time"
      "Asia/Macau"                     "China Standard Time"
      "Asia/Irkutsk"                   "North Asia East Standard Time"
      "Asia/Singapore"                 "Singapore Standard Time"
      "Asia/Brunei"                    "Singapore Standard Time"
      "Asia/Makassar"                  "Singapore Standard Time"
      "Asia/Kuala_Lumpur"              "Singapore Standard Time"
      "Asia/Kuching"                   "Singapore Standard Time"
      "Asia/Manila"                    "Singapore Standard Time"
      "Etc/GMT-8"                      "Singapore Standard Time"
      "Australia/Perth"                "W. Australia Standard Time"
      "Asia/Taipei"                    "Taipei Standard Time"
      "Asia/Ulaanbaatar"               "Ulaanbaatar Standard Time"
      "Asia/Choibalsan"                "Ulaanbaatar Standard Time"
      "Australia/Eucla"                "Aus Central W. Standard Time"
      "Asia/Chita"                     "Transbaikal Standard Time"
      "Asia/Tokyo"                     "Tokyo Standard Time"
      "Asia/Jayapura"                  "Tokyo Standard Time"
      "Pacific/Palau"                  "Tokyo Standard Time"
      "Asia/Dili"                      "Tokyo Standard Time"
      "Etc/GMT-9"                      "Tokyo Standard Time"
      "Asia/Pyongyang"                 "North Korea Standard Time"
      "Asia/Seoul"                     "Korea Standard Time"
      "Asia/Yakutsk"                   "Yakutsk Standard Time"
      "Asia/Khandyga"                  "Yakutsk Standard Time"
      "Australia/Adelaide"             "Cen. Australia Standard Time"
      "Australia/Broken_Hill"          "Cen. Australia Standard Time"
      "Australia/Darwin"               "AUS Central Standard Time"
      "Australia/Brisbane"             "E. Australia Standard Time"
      "Australia/Lindeman"             "E. Australia Standard Time"
      "Australia/Sydney"               "AUS Eastern Standard Time"
      "Australia/Melbourne"            "AUS Eastern Standard Time"
      "Pacific/Port_Moresby"           "West Pacific Standard Time"
      "Antarctica/DumontDUrville"      "West Pacific Standard Time"
      "Pacific/Truk"                   "West Pacific Standard Time"
      "Pacific/Guam"                   "West Pacific Standard Time"
      "Pacific/Saipan"                 "West Pacific Standard Time"
      "Etc/GMT-10"                     "West Pacific Standard Time"
      "Australia/Hobart"               "Tasmania Standard Time"
      "Australia/Currie"               "Tasmania Standard Time"
      "Antarctica/Macquarie"           "Tasmania Standard Time"
      "Asia/Vladivostok"               "Vladivostok Standard Time"
      "Asia/Ust-Nera"                  "Vladivostok Standard Time"
      "Australia/Lord_Howe"            "Lord Howe Standard Time"
      "Pacific/Bougainville"           "Bougainville Standard Time"
      "Asia/Srednekolymsk"             "Russia Time Zone 10"
      "Asia/Magadan"                   "Magadan Standard Time"
      "Pacific/Norfolk"                "Norfolk Standard Time"
      "Asia/Sakhalin"                  "Sakhalin Standard Time"
      "Pacific/Guadalcanal"            "Central Pacific Standard Time"
      "Antarctica/Casey"               "Central Pacific Standard Time"
      "Pacific/Ponape"                 "Central Pacific Standard Time"
      "Pacific/Kosrae"                 "Central Pacific Standard Time"
      "Pacific/Noumea"                 "Central Pacific Standard Time"
      "Pacific/Efate"                  "Central Pacific Standard Time"
      "Etc/GMT-11"                     "Central Pacific Standard Time"
      "Asia/Kamchatka"                 "Russia Time Zone 11"
      "Asia/Anadyr"                    "Russia Time Zone 11"
      "Pacific/Auckland"               "New Zealand Standard Time"
      "Antarctica/McMurdo"             "New Zealand Standard Time"
      "Etc/GMT-12"                     "UTC+12"
      "Pacific/Tarawa"                 "UTC+12"
      "Pacific/Majuro"                 "UTC+12"
      "Pacific/Kwajalein"              "UTC+12"
      "Pacific/Nauru"                  "UTC+12"
      "Pacific/Funafuti"               "UTC+12"
      "Pacific/Wake"                   "UTC+12"
      "Pacific/Wallis"                 "UTC+12"
      "Pacific/Fiji"                   "Fiji Standard Time"
      "Pacific/Chatham"                "Chatham Islands Standard Time"
      "Etc/GMT-13"                     "UTC+13"
      "Pacific/Enderbury"              "UTC+13"
      "Pacific/Fakaofo"                "UTC+13"
      "Pacific/Tongatapu"              "Tonga Standard Time"
      "Pacific/Apia"                   "Samoa Standard Time"
      "Pacific/Kiritimati"             "Line Islands Standard Time"
      "Etc/GMT-14"                     "Line Islands Standard Time"))
  "A hash table mapping IANA/Olson time zone names to server time zone names.")

;; Generated with:
;;(defun zdump-line-to-current-time-zone-value ()
;;  "Convert a zdump line to the format returned by `current-time-zone'."
;;  (unless (eobp)
;;    (search-forward "	")
;;    (search-forward "	")
;;    (let ((start (point)))
;;      (forward-word)
;;      (let* ((offset-string (buffer-substring-no-properties start (point)))
;;	     (offset-number-hours
;;	      (* (string-to-number (substring offset-string 0 3)) 3600))
;;	     (offset-number
;;	      (if (= (length offset-string) 3)
;;		  offset-number-hours
;;		(let ((offset-number-minutes
;;		       (string-to-number (substring offset-string 3))))
;;		  (if (< offset-number-hours 0)
;;		      (- offset-number-hours (* offset-number-minutes 60))
;;		    (+ offset-number-hours (* offset-number-minutes 60)))))))
;;	(prog1
;;	    (cond ((eolp)
;;		   (list offset-number offset-string))
;;		  ((progn (search-forward "	") (=  (char-after (point)) 9))
;;		   (list offset-number offset-string))
;;		  (t
;;		   (let ((start (point)))
;;		     (forward-word)
;;		     (list offset-number
;;			   (buffer-substring-no-properties start (point))))))
;;	  (move-end-of-line nil)
;;	  (forward-char))))))
;;
;;(insert
;; (concat
;;  "(defvar exco--time-zone-emacs-to-olson "
;;  (let ((table (make-hash-table :test #'equal)))
;;    (dolist (file
;;	     (split-string
;;	      (org-trim (shell-command-to-string
;;			 "find /usr/share/zoneinfo/right -type f"))
;;	      "\n"))
;;      (with-temp-buffer
;;	(goto-char (point-min))
;;	(shell-command (format "zdump -c 2020,2021 -i %S" file) t)
;;	(next-line)
;;	(next-line)
;;	(while (not (eobp))
;;	  (let* ((key (zdump-line-to-current-time-zone-value))
;;		 (value (substring file (length "/usr/share/zoneinfo/right/")))
;;		 (values (gethash key table)))
;;	    (if values
;;		(unless (member value values)
;;		  (puthash key (cons value values) table))
;;	      (puthash key (list value) table))))))
;;    (format "%S" table))
;;  "(concat \"A hash table mapping `current-time-zone' values to\"
;;	  \" IANA/Olson time zone names.\")"))

;; `exco-time-zone' only uses one of these, but I thought knowing this
;; mapping might be generally useful.
(defvar exco--time-zone-emacs-to-olson
  #s(hash-table
     size 97
     test equal
     rehash-size 1.5
     rehash-threshold 0.8125
     data
     ((-43200 "-12")   ("Etc/GMT+12")
      (-39600 "SST")   ("Pacific/Pago_Pago")
      (-39600 "-11")   ("Etc/GMT+11" "Pacific/Niue")
      (-36000 "HST")   ("HST" "America/Adak" "Pacific/Honolulu")
      (-36000 "-10")   ("Etc/GMT+10" "Pacific/Tahiti" "Pacific/Rarotonga")
      (-34200 "-0930") ("Pacific/Marquesas")
      (-32400 "HDT")   ("America/Adak")
      (-32400 "AKST")  ("America/Sitka" "America/Anchorage" "America/Nome"
			"America/Metlakatla" "America/Yakutat" "America/Juneau")
      (-32400 "-09")   ("Etc/GMT+9" "Pacific/Gambier")
      (-28800 "PST")   ("America/Dawson" "America/Vancouver"
			"America/Whitehorse" "America/Tijuana"
			"America/Los_Angeles" "PST8PDT")
      (-28800 "AKDT")  ("America/Sitka" "America/Anchorage" "America/Nome"
			"America/Metlakatla" "America/Yakutat" "America/Juneau")
      (-28800 "-08")   ("Etc/GMT+8" "Pacific/Pitcairn")
      (-25200 "PDT")   ("America/Dawson" "America/Vancouver"
			"America/Whitehorse" "America/Tijuana"
			"America/Los_Angeles" "PST8PDT")
      (-25200 "MST")   ("MST7MDT" "America/Dawson" "America/Yellowknife"
			"America/Fort_Nelson" "America/Creston"
			"America/Cambridge_Bay" "America/Boise"
			"America/Mazatlan" "America/Chihuahua" "America/Phoenix"
			"America/Inuvik" "America/Hermosillo" "America/Edmonton"
			"America/Whitehorse" "America/Dawson_Creek"
			"America/Ojinaga" "America/Denver" "MST")
      (-25200 "-07")   ("Etc/GMT+7")
      (-21600 "MDT")   ("MST7MDT" "America/Yellowknife" "America/Cambridge_Bay"
			"America/Boise" "America/Mazatlan" "America/Chihuahua"
			"America/Inuvik" "America/Edmonton" "America/Ojinaga"
			"America/Denver")
      (-21600 "CST")   ("CST6CDT" "America/Managua" "America/Belize"
			"America/Regina" "America/Rankin_Inlet"
			"America/Tegucigalpa" "America/Resolute"
			"America/North_Dakota/Beulah"
			"America/North_Dakota/New_Salem"
			"America/North_Dakota/Center" "America/Merida"
			"America/Guatemala" "America/Winnipeg" "America/Chicago"
			"America/Swift_Current" "America/Menominee"
			"America/Monterrey" "America/Mexico_City"
			"America/El_Salvador" "America/Indiana/Tell_City"
			"America/Indiana/Knox" "America/Costa_Rica"
			"America/Matamoros" "America/Bahia_Banderas"
			"America/Rainy_River")
      (-21600 "-06")   ("Etc/GMT+6" "Pacific/Easter" "Pacific/Galapagos")
      (-18000 "EST")   ("EST5EDT" "America/Detroit" "America/Port-au-Prince"
			"America/Jamaica" "America/Iqaluit" "America/Grand_Turk"
			"America/Kentucky/Monticello"
			"America/Kentucky/Louisville" "America/Cancun"
			"America/Toronto" "America/Atikokan" "America/Panama"
			"America/Pangnirtung" "America/Indiana/Indianapolis"
			"America/Indiana/Vevay" "America/Indiana/Vincennes"
			"America/Indiana/Petersburg" "America/Indiana/Winamac"
			"America/Indiana/Marengo" "America/Nipigon"
			"America/Nassau" "America/New_York"
			"America/Thunder_Bay" "EST")
      (-18000 "CST")   ("America/Havana")
      (-18000 "CDT")   ("CST6CDT" "America/Rankin_Inlet" "America/Resolute"
			"America/North_Dakota/Beulah"
			"America/North_Dakota/New_Salem"
			"America/North_Dakota/Center" "America/Merida"
			"America/Winnipeg" "America/Chicago" "America/Menominee"
			"America/Monterrey" "America/Mexico_City"
			"America/Indiana/Tell_City" "America/Indiana/Knox"
			"America/Matamoros" "America/Bahia_Banderas"
			"America/Rainy_River")
      (-18000 "-05")   ("America/Rio_Branco" "America/Eirunepe" "America/Lima"
			"America/Bogota" "America/Guayaquil" "Etc/GMT+5"
			"Pacifibc/Easter")
      (-14400 "EDT")   ("EST5EDT" "America/Detroit" "America/Port-au-Prince"
			"America/Iqaluit" "America/Grand_Turk"
			"America/Kentucky/Monticello"
			"America/Kentucky/Louisville" "America/Toronto"
			"America/Pangnirtung" "America/Indiana/Indianapolis"
			"America/Indiana/Vevay" "America/Indiana/Vincennes"
			"America/Indiana/Petersburg" "America/Indiana/Winamac"
			"America/Indiana/Marengo" "America/Nipigon"
			"America/Nassau" "America/New_York"
			"America/Thunder_Bay")
      (-14400 "CDT")   ("America/Havana")
      (-14400 "AST")   ("Atlantic/Bermuda" "America/Thule" "America/Glace_Bay"
			"America/Goose_Bay" "America/Puerto_Rico"
			"America/Santo_Domingo" "America/Curacao"
			"America/Barbados" "America/Blanc-Sablon"
			"America/Martinique" "America/Moncton" "America/Halifax"
			"America/Port_of_Spain")
      (-14400 "-04")   ("America/Santiago" "America/Cuiaba" "America/Caracas"
			"America/La_Paz" "America/Porto_Velho" "America/Guyana"
			"America/Campo_Grande" "America/Manaus"
			"America/Asuncion" "America/Boa_Vista" "Etc/GMT+4")
      (-12600 "NST")   ("America/St_Johns")
      (-10800 "ADT")   ("Atlantic/Bermuda" "America/Thule" "America/Glace_Bay"
			"America/Goose_Bay" "America/Moncton" "America/Halifax")
      (-10800 "-03")   ("Atlantic/Stanley" "America/Miquelon" "America/Santiago"
			"America/Santarem" "America/Argentina/Ushuaia"
			"America/Argentina/Jujuy"
			"America/Argentina/Rio_Gallegos"
			"America/Argentina/La_Rioja"
			"America/Argentina/San_Juan" "America/Argentina/Salta"
			"America/Argentina/San_Luis" "America/Argentina/Tucuman"
			"America/Argentina/Mendoza"
			"America/Argentina/Catamarca"
			"America/Argentina/Cordoba"
			"America/Argentina/Buenos_Aires"
			"America/Bahia" "America/Cayenne" "America/Sao_Paulo"
			"America/Maceio" "America/Fortaleza"
			"America/Paramaribo" "America/Araguaina" "America/Belem"
			"America/Nuuk" "America/Punta_Arenas"
			"America/Montevideo" "America/Asuncion" "America/Recife"
			"Etc/GMT+3" "Antarctica/Palmer" "Antarctica/Rothera")
      ( -9000 "NDT")   ("America/St_Johns")
      ( -7200 "-02")   ("Atlantic/South_Georgia" "America/Miquelon"
			"America/Nuuk" "America/Noronha" "Etc/GMT+2")
      ( -3600 "-01")   ("Atlantic/Cape_Verde" "Atlantic/Azores"
			"America/Scoresbysund" "Etc/GMT+1")
      (     0 "+00")   ("Atlantic/Azores" "Africa/El_Aaiun" "Africa/Casablanca"
			"America/Scoresbysund" "Antarctica/Troll")
      (     0 "-00")   ("Factory")
      (     0 "GMT")   ("Atlantic/Reykjavik" "Africa/Abidjan" "Africa/Monrovia"
			"Africa/Sao_Tome" "Africa/Bissau" "Africa/Accra"
			"Europe/London" "Europe/Dublin" "America/Danmarkshavn"
			"Etc/GMT")
      (     0 "UTC")   ("Etc/UTC")
      (     0 "WET")   ("Atlantic/Faroe" "Atlantic/Canary" "Atlantic/Madeira"
			"Europe/Lisbon" "WET")
      ( +3600 "+01")   ("Africa/El_Aaiun" "Africa/Casablanca" "Etc/GMT-1")
      ( +3600 "BST")   ("Europe/London")
      ( +3600 "CET")   ("Africa/Tunis" "Africa/Ceuta" "Africa/Algiers" "CET"
			"Europe/Oslo" "Europe/Berlin" "Europe/Amsterdam"
			"Europe/Rome" "Europe/Budapest" "Europe/Tirane"
			"Europe/Copenhagen" "Europe/Belgrade" "Europe/Malta"
			"Europe/Warsaw" "Europe/Vienna" "Europe/Stockholm"
			"Europe/Paris" "Europe/Andorra" "Europe/Brussels"
			"Europe/Madrid" "Europe/Gibraltar" "Europe/Zurich"
			"Europe/Monaco" "Europe/Prague" "Europe/Luxembourg")
      ( +3600 "IST")   ("Europe/Dublin")
      ( +3600 "MET")   ("MET")
      ( +3600 "WAT")   ("Africa/Ndjamena" "Africa/Lagos")
      ( +3600 "WEST")  ("Atlantic/Faroe" "Atlantic/Canary" "Atlantic/Madeira"
			"Europe/Lisbon" "WET")
      ( +7200 "+02")   ("Etc/GMT-2" "Antarctica/Troll")
      ( +7200 "CAT")   ("Africa/Khartoum" "Africa/Maputo" "Africa/Windhoek")
      ( +7200 "CEST")  ("Africa/Ceuta" "CET" "Europe/Oslo" "Europe/Berlin"
			"Europe/Amsterdam" "Europe/Rome" "Europe/Budapest"
			"Europe/Tirane" "Europe/Copenhagen" "Europe/Belgrade"
			"Europe/Malta" "Europe/Warsaw" "Europe/Vienna"
			"Europe/Stockholm" "Europe/Paris" "Europe/Andorra"
			"Europe/Brussels" "Europe/Madrid" "Europe/Gibraltar"
			"Europe/Zurich" "Europe/Monaco" "Europe/Prague"
			"Europe/Luxembourg")
      ( +7200 "EET")   ("Africa/Cairo" "Africa/Tripoli" "Europe/Zaporozhye"
			"Europe/Kiev" "Europe/Riga" "Europe/Chisinau"
			"Europe/Athens" "Europe/Helsinki" "Europe/Uzhgorod"
			"Europe/Bucharest" "Europe/Kaliningrad" "Europe/Sofia"
			"Europe/Vilnius" "Europe/Tallinn" "EET" "Asia/Beirut"
			"Asia/Amman" "Asia/Damascus" "Asia/Nicosia"
			"Asia/Hebron" "Asia/Famagusta" "Asia/Gaza")
      ( +7200 "IST")   ("Asia/Jerusalem")
      ( +7200 "MEST")  ("MET")
      ( +7200 "SAST")  ("Africa/Johannesburg")
      (+10800 "+03")   ("Europe/Volgograd" "Europe/Minsk" "Europe/Istanbul"
			"Europe/Kirov" "Etc/GMT-3" "Asia/Riyadh" "Asia/Baghdad"
			"Asia/Qatar" "Antarctica/Syowa")
      (+10800 "EAT")   ("Africa/Nairobi" "Africa/Juba")
      (+10800 "EEST")  ("Europe/Zaporozhye" "Europe/Kiev" "Europe/Riga"
			"Europe/Chisinau" "Europe/Athens" "Europe/Helsinki"
			"Europe/Uzhgorod" "Europe/Bucharest" "Europe/Sofia"
			"Europe/Vilnius" "Europe/Tallinn" "EET" "Asia/Beirut"
			"Asia/Amman" "Asia/Damascus" "Asia/Nicosia"
			"Asia/Hebron" "Asia/Famagusta" "Asia/Gaza")
      (+10800 "IDT")   ("Asia/Jerusalem")
      (+10800 "MSK")   ("Europe/Moscow" "Europe/Simferopol")
      (+12600 "+0330") ("Asia/Tehran")
      (+14400 "+04")   ("Indian/Mauritius" "Indian/Reunion" "Indian/Mahe"
			"Europe/Samara" "Europe/Volgograd" "Europe/Astrakhan"
			"Europe/Saratov" "Europe/Ulyanovsk" "Etc/GMT-4"
			"Asia/Baku" "Asia/Yerevan" "Asia/Dubai" "Asia/Tbilisi")
      (+16200 "+0430") ("Asia/Tehran" "Asia/Kabul")
      (+18000 "+05")   ("Indian/Maldives" "Indian/Kerguelen" "Etc/GMT-5"
			"Asia/Aqtobe" "Asia/Oral" "Asia/Aqtau" "Asia/Tashkent"
			"Asia/Dushanbe" "Asia/Atyrau" "Asia/Yekaterinburg"
			"Asia/Samarkand" "Asia/Ashgabat" "Asia/Qyzylorda"
			"Antarctica/Mawson")
      (+18000 "PKT")   ("Asia/Karachi")
      (+19800 "+0530") ("Asia/Colombo")
      (+19800 "IST")   ("Asia/Kolkata")
      (+20700 "+0545") ("Asia/Kathmandu")
      (+21600 "+06")   ("Indian/Chagos" "Etc/GMT-6" "Asia/Qostanay"
			"Asia/Thimphu" "Asia/Bishkek" "Asia/Urumqi" "Asia/Omsk"
			"Asia/Almaty" "Asia/Dhaka" "Antarctica/Vostok")
      (+23400 "+0630") ("Indian/Cocos" "Asia/Yangon")
      (+25200 "+07")   ("Indian/Christmas" "Etc/GMT-7" "Asia/Hovd"
			"Asia/Novokuznetsk" "Asia/Bangkok" "Asia/Tomsk"
			"Asia/Barnaul" "Asia/Ho_Chi_Minh" "Asia/Novosibirsk"
			"Asia/Krasnoyarsk" "Antarctica/Davis")
      (+25200 "WIB")   ("Asia/Jakarta" "Asia/Pontianak")
      (+28800 "+08")   ("Etc/GMT-8" "Asia/Brunei" "Asia/Kuching"
			"Asia/Ulaanbaatar" "Asia/Kuala_Lumpur" "Asia/Singapore"
			"Asia/Choibalsan" "Asia/Irkutsk" "Antarctica/Casey")
      (+28800 "AWST")  ("Australia/Perth")
      (+28800 "CST")   ("Asia/Macau" "Asia/Shanghai" "Asia/Taipei")
      (+28800 "HKT")   ("Asia/Hong_Kong")
      (+28800 "PST")   ("Asia/Manila")
      (+28800 "WITA")  ("Asia/Makassar")
      (+31500 "+0845") ("Australia/Eucla")
      (+32400 "+09")   ("Etc/GMT-9" "Asia/Yakutsk" "Asia/Chita" "Asia/Khandyga"
			"Asia/Dili" "Pacific/Palau")
      (+32400 "JST")   ("Asia/Tokyo")
      (+32400 "KST")   ("Asia/Seoul" "Asia/Pyongyang")
      (+32400 "WIT")   ("Asia/Jayapura")
      (+34200 "ACST")  ("Australia/Darwin" "Australia/Adelaide"
			"Australia/Broken_Hill")
      (+36000 "+10")   ("Etc/GMT-10" "Asia/Ust-Nera" "Asia/Vladivostok"
			"Pacific/Chuuk" "Pacific/Port_Moresby"
			"Antarctica/DumontDUrville")
      (+36000 "AEST")  ("Australia/Melbourne" "Australia/Lindeman"
			"Australia/Hobart" "Australia/Brisbane"
			"Australia/Sydney" "Antarctica/Macquarie")
      (+36000 "ChST")  ("Pacific/Guam")
      (+37800 "+1030") ("Australia/Lord_Howe")
      (+37800 "ACDT")  ("Australia/Adelaide" "Australia/Broken_Hill")
      (+39600 "+11")   ("Australia/Lord_Howe" "Etc/GMT-11" "Asia/Magadan"
			"Asia/Sakhalin" "Asia/Srednekolymsk" "Pacific/Norfolk"
			"Pacific/Pohnpei" "Pacific/Guadalcanal" "Pacific/Kosrae"
			"Pacific/Bougainville" "Pacific/Noumea" "Pacific/Efate"
			"Antarctica/Casey")
      (+39600 "AEDT")  ("Australia/Melbourne" "Australia/Hobart"
			"Australia/Sydney" "Antarctica/Macquarie")
      (+43200 "+12")   ("Etc/GMT-12" "Asia/Anadyr" "Asia/Kamchatka"
			"Pacific/Wake" "Pacific/Majuro" "Pacific/Norfolk"
			"Pacific/Nauru" "Pacific/Funafuti" "Pacific/Fiji"
			"Pacific/Kwajalein" "Pacific/Tarawa" "Pacific/Wallis")
      (+43200 "NZST")  ("Pacific/Auckland")
      (+45900 "+1245") ("Pacific/Chatham")
      (+46800 "+13")   ("Etc/GMT-13" "Pacific/Tongatapu" "Pacific/Enderbury"
			"Pacific/Fiji" "Pacific/Fakaofo" "Pacific/Apia")
      (+46800 "NZDT")  ("Pacific/Auckland")
      (+49500 "+1345") ("Pacific/Chatham")
      (+50400 "+14")   ("Etc/GMT-14" "Pacific/Kiritimati" "Pacific/Apia")))
  (concat "A hash table mapping `current-time-zone' values to"
	  " IANA/Olson time zone names."))

(defcustom excorporate-time-zone nil
  "The server-style time zone.
If this variable is nil, Excorporate will compute a time zone
automatically based on `current-time-zone'.  If that doesn't
work, or you want to specify the time zone directly, run
`excorporate-customize-time-zone' to customize this variable from
a list of valid values."
  :type '(choice :menu-tag "Server-style time zone"
		 :tag "Server-style time zone"
		 (const :tag "Compute from Emacs time zone" nil)
		 string)
  :group 'excorporate)

(defun excorporate-customize-time-zone ()
  "Prompt for a server-style time zone from a list of valid values."
  (interactive)
  (let ((zone (completing-read
	       "Excorporate time zone: "
	       (cons "Emacs Built-in"
		     (cl-loop ; hash-table-values when Emacs < 24.4
		      for v being the hash-values of
		      exco--time-zone-olson-to-server
		      collect v))
	       nil t)))
    (unless (equal zone "")
      (customize-save-variable 'excorporate-time-zone
			       (if (equal zone "Emacs Built-in") nil zone)))))

(defun exco-time-zone (&optional emacs-time-zone)
  "Return server style time zone string.
Return `excorporate-time-zone' if it is non-nil, or look up the
time zone based on `current-time-zone' otherwise.
If EMACS-TIME-ZONE is specified, convert it to a server time zone."
  (if (and (not emacs-time-zone) excorporate-time-zone)
      excorporate-time-zone
    (or
     (catch 'found
       (dolist (zone
		(gethash (or emacs-time-zone (current-time-zone))
			 exco--time-zone-emacs-to-olson))
	 (let ((server-zone (gethash zone exco--time-zone-olson-to-server)))
	   (when server-zone (throw 'found server-zone)))))
     (error (concat "Excorporate: Could not compute server time zone; "
		    "Run `excorporate-customize-time-zone'")))))

(provide 'excorporate-time-zones)

;;; excorporate-time-zones.el ends here
