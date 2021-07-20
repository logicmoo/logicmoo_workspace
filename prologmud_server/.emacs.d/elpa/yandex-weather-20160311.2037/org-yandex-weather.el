;;; org-yandex-weather.el -- Show Yandex Weather forecasts in Org Agenda.

;; Copyright (C) 2013-2015 Whitesquall

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; Commentary:

;; The facility for the org-mode agenda.
;; This script based on google-weather.el originally written by Julien Danjou.

;;; Code:


(require 'cl-lib)
(require 'yandex-weather)
(require 'image)
(require 'format-spec)
(require 'solar)
(require 'parse-time)

(defgroup org-yandex-weather nil
  "Yandex Weather for Org mode."
  :group 'comm
  :group 'org)

(defcustom org-yandex-weather-location "27612"
  "Default location for org-yandex-weather."
  :group 'org-yandex-weather)

(defcustom org-yandex-weather-format "%C: %i %c, [%l,%h]%s"
  "String to return to describe the weather.
Valid %-sequences are:
   - %i the icon;
   - %c means the weather condition;
   - %C the city the weather is for;
   - %l the lower temperature;
   - %h the higher temperature;
   - %p the pressure in mmHg;
   - %d the wind direction;
   - %w the wind speed;
   - %H the humidity;
   - %s the temperature unit symbol.")

(defcustom org-yandex-weather-cache-time 7200
  "Define how many seconds we should cache the weather forecast."
  :group 'org-yandex-weather)

(defcustom org-yandex-weather-cache-icon-time 15552000
  "Define how many seconds we should cache icons for the forecast."
  :group 'org-yandex-weather)

(defcustom org-yandex-weather-display-icon-p t
  "Display icons."
  :group 'org-yandex-weather)

(defvar org-yandex-weather-wind-direction-symbols
  '(("n" . "↓") ("ne" . "↙")
    ("e" . "←") ("se" . "↖")
    ("s" . "↑") ("sw" . "↗")
    ("w" . "→") ("nw" . "↘"))
  "The arrows for wind directions.")

(defun org-yandex-weather-get-wind-direction-arrow-by-symbol (symbol)
  "Return the arrow of wind direction by SYMBOL."
  (cdr (assoc symbol org-yandex-weather-wind-direction-symbols)))

(defun org-yandex-weather-check-interval (date)
  "Return t if DATE places between current day and current day
plus 10 days. Else return nil."
  (let* ((low-days (time-to-days (current-time)))
         (high-days (+ low-days 10))
         (days-of-date
          (calendar-absolute-from-gregorian
           date)))
    (and
     (>= days-of-date low-days)
     (< days-of-date high-days))))

(defun org-yandex-weather-create-icon-if-need (forecast)
  "Create image for the forecast according to the value of
`org-yandex-weather-display-icon-p'."
  (when org-yandex-weather-display-icon-p
    (create-image
     (yandex-weather-get-icon
      (yandex-weather-forecast->icon forecast)
      org-yandex-weather-cache-icon-time)
     'png t)))

(defun org-yandex-weather-build-org-ret-string (data forecast)
  "Build and return forecast string for the agenda."
  (let ((condition (yandex-weather-forecast->condition forecast))
        (low (yandex-weather-forecast->avg-night-temperature forecast))
        (high (yandex-weather-forecast->avg-day-temperature forecast))
        (humidity (yandex-weather-forecast->humidity forecast))
        (pressure (yandex-weather-forecast->pressure forecast))
        (wind-speed (yandex-weather-forecast->wind-speed forecast))
        (wind-direction
         (org-yandex-weather-get-wind-direction-arrow-by-symbol
          (yandex-weather-forecast->wind-direction forecast)))
        (city (yandex-weather-data->city data))
        (icon (org-yandex-weather-create-icon-if-need forecast)))
    (format-spec org-yandex-weather-format
                 `((?i . ,(if icon
                              (propertize "icon"
                                          'display
                                          (append
                                           icon '(:ascent center))
                                          'rear-nonsticky '(display))
                            ""))
                   (?c . ,condition)
                   (?l . ,low)
                   (?h . ,high)
                   (?p . ,pressure)
                   (?d . ,wind-direction)
                   (?w . ,wind-speed)
                   (?H . ,humidity)
                   (?C . ,city)
                   (?s . ,yandex-weather-temperature-symbol)))))

;;;###autoload
(defun org-yandex-weather (&optional location)
  "Return Org entry with the weather for LOCATION.
If LOCATION isn't set, use org-yandex-weather-location."
  (when (org-yandex-weather-check-interval (with-no-warnings date))
    (let* ((location (or location org-yandex-weather-location))
           (data (ignore-errors
                   (yandex-weather-get-data location
                                            org-yandex-weather-cache-time)))
           (forecast (when data
                       (yandex-weather-data->forecast-by-date
                        data (with-no-warnings date)))))
      (when forecast
        (org-yandex-weather-build-org-ret-string data forecast)))))

(provide 'org-yandex-weather)


;;; org-yandex-weather.el ends here
