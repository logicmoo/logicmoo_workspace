;;; yandex-weather.el --- Fetch Yandex Weather forecasts.

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

;;; Commentary:

;; Parser for the yandex weather forecasts for the org-mode Agenda.
;; This script based on google-weather.el originally written by Julien Danjou.
;;
;; How to use.
;;
;; - Copy project files in your .emacs.d.
;; - Add this lines in your emacs config:
;;
;; (load-file "~/.emacs.d/yandex-weather.el")
;; (load-file "~/.emacs.d/org-yandex-weather.el")
;;
;; - Add this line in your agenda's org file.
;;
;; %%(org-yandex-weather "27612")
;;
;; Where '27612' is ID of your city from:
;; http://weather.yandex.ru/static/cities.xml
;;
;; Also you can use MELPA for the installation.

;;; Code:


(require 'cl-lib)
(require 'url)
(require 'url-cache)
(require 'xml)
(require 'time-date)

(defgroup yandex-weather nil
  "Yandex Weather."
  :group 'comm)

(defcustom yandex-weather-use-https t
  "Default protocol to use to access the Yandex Weather API."
  :group 'yandex-weather
  :type 'boolean)

(defconst yandex-weather-forecast-url
  "export.yandex.ru/weather-ng/forecasts/"
  "URL of the API.")

(defconst yandex-weather-icon-url
  "yandex.st/weather/1.1.86/i/icons/22x22/"
  "URL of the icons.")

(defconst yandex-weather-temperature-symbol "°C"
  "Temperature symbol.")

(defun yandex-weather-cache-expired (url expire-time)
  "Check if URL is cached for more than EXPIRE-TIME."
  (cond (url-standalone-mode
         (not (file-exists-p (url-cache-create-filename url))))
        (t (let ((cache-time (url-is-cached url)))
             (if cache-time
                 (time-less-p
                  (time-add
                   cache-time
                   (seconds-to-time expire-time))
                  (current-time))
               t)))))

(defun yandex-weather-cache-fetch (url)
  "Fetch URL from the cache."
  (with-current-buffer (generate-new-buffer " *temp*")
    (url-cache-extract (url-cache-create-filename url))
    (current-buffer)))

(defun yandex-weather-retrieve-data-raw (url &optional expire-time)
  "Retrieve URL and return its data as string.
If EXPIRE-TIME is set, the data will be fetched from the cache if
their are not older than EXPIRE-TIME seconds. Otherwise, they
will be fetched and then cached. Therefore, setting EXPIRE-TIME
to 0 force a cache renewal."
  (let* ((expired (if expire-time
                      (yandex-weather-cache-expired url expire-time)
                    t))
         (buffer (if expired
                     (url-retrieve-synchronously url)
                   (yandex-weather-cache-fetch url)))
         data)
    (when (and expired expire-time)
      (url-store-in-cache buffer))
    buffer))

(defun yandex-weather-retrieve-data (url &optional expire-time)
  "Retrieve URL and return its data as string.
If EXPIRE-TIME is set, the data will be fetched from the cache if
their are not older than EXPIRE-TIME seconds. Otherwise, they
will be fetched and then cached. Therefore, setting EXPIRE-TIME
to 0 force a cache renewal."
  (with-current-buffer (yandex-weather-retrieve-data-raw
                        url expire-time)
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (error "Data not found."))
    (decode-coding-region
     (point) (point-max)
     (detect-coding-region (point) (point-max) t))
    (set-buffer-multibyte t)
    (let ((data (xml-parse-region (point) (point-max))))
      (kill-buffer (current-buffer))
      data)))

(defun yandex-weather-retrieve-icon (url &optional expire-time)
  (with-current-buffer (yandex-weather-retrieve-data-raw url expire-time)
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (error "Data not found."))
    (set-buffer-multibyte nil)
    (let ((data (buffer-substring (point) (point-max))))
      (kill-buffer (current-buffer))
      data)))

(defun yandex-weather-get-icon (icon-name &optional expire-time)
  (yandex-weather-retrieve-icon
   (yandex-weather-build-icon-url icon-name)
   expire-time))

(defun yandex-weather-build-forecast-url (location)
  "Build URL to retrieve weather for LOCATION.
LOCATION can be finded http://weather.yandex.ru/static/cities.xml .
We need 'id' field in the 'city' tag."
  (concat "http" (when yandex-weather-use-https "s")
          "://" yandex-weather-forecast-url location ".xml"))

(defun yandex-weather-build-icon-url (icon-num)
  "Build URL to retrieve icon for weather."
  (concat "http" (when yandex-weather-use-https "s")
          "://" yandex-weather-icon-url icon-num ".png"))

(defun yandex-weather-get-data (location &optional expire-time)
  "Get weather data for LOCATION.
See `yandex-weather-retrieve-data' for the use of EXPIRE-TIME."
  (yandex-weather-retrieve-data
   (yandex-weather-build-forecast-url location) expire-time))

(defun yandex-weather-data->all-info (data)
  "Return all weather information from DATA."
  (cdr (assq 'forecast data)))

(defun yandex-weather-data->city (data)
  "Return the city where the DATA come from."
  (cdr (assq 'city (car (yandex-weather-data->all-info data)))))

(defun yandex-weather-data->forecasts (data)
  "Return forecasts for all days from the DATA."
  (xml-get-children (yandex-weather-data->all-info data) 'day))

(defun yandex-weather-data->forecast-by-date (data date)
  "Return the forecast of the weather for the DATA for the DATE."
  (let ((forecast-date (format "%.4d-%.2d-%.2d"
                               (nth 2 date)
                               (nth 0 date)
                               (nth 1 date)))
        (forecasts (yandex-weather-data->forecasts data))
        (retvalue nil))
    ; Now we got the formated date and forecasts for all days.
    (mapc (lambda (x)
            (when (equal (cdr (assq 'date (xml-node-attributes x)))
                         forecast-date)
              (setq retvalue x)))
          forecasts)
    retvalue))

(defun yandex-weather-forecast->day-part (forecast day-part)
  "Return required DAY-PART for the FORECAST."
  (let ((retvalue nil))
    (mapc (lambda (x)
            (when (equal (cdr (assq 'type (xml-node-attributes x)))
                         day-part)
              (setq retvalue x)))
          (xml-get-children forecast 'day_part))
    retvalue))

(defun yandex-weather-forecast->avg-temperature (forecast day-part)
  "Return the average temperature for the FORECAST and day DAY-PART."
  (nth 2 (car (xml-get-children
               (car
                (xml-get-children
                 (yandex-weather-forecast->day-part forecast day-part)
                 'temperature-data))
               'avg))))

(defun yandex-weather-forecast->avg-night-temperature (forecast)
  "Return the average night temperature for the FORECAST."
  (yandex-weather-forecast->avg-temperature forecast "night"))

(defun yandex-weather-forecast->avg-day-temperature (forecast)
  "Return the average day temperature for the FORECAST."
  (yandex-weather-forecast->avg-temperature forecast "day"))

(defun yandex-weather-forecast->get-characteristic (forecast characteristic)
  "Return the value of CHARACTERISTIC of FORECAST."
  (nth 2 (car (xml-get-children (yandex-weather-forecast->day-part
                                 forecast "day") characteristic))))

(defun yandex-weather-forecast->condition (forecast)
  "Return the condition for the FORECAST."
  (yandex-weather-forecast->get-characteristic
   forecast 'weather_type))

(defun yandex-weather-forecast->pressure (forecast)
  "Return the pressure for the FORECAST."
  (yandex-weather-forecast->get-characteristic
   forecast 'pressure))

(defun yandex-weather-forecast->humidity (forecast)
  "Return the humidity for the FORECAST."
  (yandex-weather-forecast->get-characteristic
   forecast 'humidity))

(defun yandex-weather-forecast->wind-speed (forecast)
  "Return the speed of wind for the FORECAST."
  (yandex-weather-forecast->get-characteristic
   forecast 'wind_speed))

(defun yandex-weather-forecast->wind-direction (forecast)
  "Return the wind direction for the FORECAST."
  (yandex-weather-forecast->get-characteristic
   forecast 'wind_direction))

(defun yandex-weather-forecast->icon (forecast)
  "Return the name of the icon for the FORECAST."
  (yandex-weather-forecast->get-characteristic
   forecast 'image-v2))

(provide 'yandex-weather)


;;; yandex-weather.el ends here
