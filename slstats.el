;;; slstats.el --- Acquire and display stats about Second Life
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: games
;; URL: https://github.com/davep/slstats.el
;; Package-Requires: ((cl-lib "0.5"))

;; slstats.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; slstats.sl provides commands that make it easy to load and view the basic
;; stats about the Second Life grid and its economy.

;;; Code:

(require 'url)
(require 'cl-lib)

(defconst slstats-lab-url "http://secondlife.com/httprequest/homepage.php"
  "The URL that contains the SL statistics.")

(defconst slstats-grid-size-url "http://api.gridsurvey.com/metricquery.php?metric=grid_size"
  "The URL that contains grid size data.")

(defun slstats-get (key stats)
  "Get a value associated with KEY from STATS."
  (cdr (assoc key stats)))

(defun slstats-to-alist (stats)
  "Turn raw STATS list into an alist."
  (when stats
    (cons
     (cons (intern (concat ":" (car stats))) (cadr stats))
     (slstats-to-alist (cddr stats)))))

(defun slstats-load-data (url &optional sep)
  "Load data about SL from URL.

SEP is an optional separator that is passed to `split-string'."
  (with-current-buffer (url-retrieve-synchronously url t)
    (setf (point) (point-min))
    (when (search-forward-regexp "^$" nil t)
      (slstats-to-alist
       (cl-remove-if
        (lambda (s)
          (zerop (length s)))
        (split-string
         (buffer-substring-no-properties (point) (point-max))
         sep))))))

(defun slstats-load-lab-data ()
  "Load the raw statistics about Second Life from Linden Lab."
  (slstats-load-data slstats-lab-url "\n"))

(defun slstats-load-grid-size-data ()
  "Load the grid size data."
  (slstats-load-data slstats-grid-size-url))

(defun slstats-format-time (time stats)
  "Format TIME from STATS as a string."
  (format-time-string "%F %T%z" (string-to-number (slstats-get time stats))))

(defun slstats-message (name data time)
  "Show a Second Life statistic as a message.

NAME is the title to give the statistic. DATA is the keyword for
finding the statistic. TIME is the keyword for finding the
last-update time for the statistic."
  (let ((stats (slstats-load-lab-data)))
    (message "%s: %s (as of %s)"
             name
             (slstats-get data stats)
             (slstats-format-time time stats))))

;;;###autoload
(defun slstats-signups ()
  "Display the Second Life sign-up count."
  (interactive)
  (slstats-message "Sign-ups" :signups :signups_updated_unix))

;;;###autoload
(defun slstats-exchange-rate ()
  "Display the L$ -> $ exchange rate."
  (interactive)
  (slstats-message "L$/$" :exchange_rate :exchange_rate_updated_unix))

;;;###autoload
(defun slstats-inworld ()
  "Display how many avatars are in-world in Second Life."
  (interactive)
  (slstats-message "Avatars in-world" :inworld :inworld_updated_unix))

;;;###autoload
(defun slstats-grid-size ()
  "Display the grid size data for Second Life."
  (interactive)
  (let ((stats (slstats-load-grid-size-data)))
    (message "Regions: Total: %s, Private: %s, Linden: %s, Adult: %s, Mature: %s, PG: %s, Linden Homes: %s"
             (slstats-get :total stats)
             (slstats-get :private stats)
             (slstats-get :linden stats)
             (slstats-get :adult stats)
             (slstats-get :mature stats)
             (slstats-get :pg stats)
             (slstats-get :linden_homes stats))))

(defun slstats-format-grid-size-total (title size stats)
  "Format a grid size total.

TITLE is the title to give the size. SIZE is the keyword of the
size we're going to format, and STATS is the stats list we'll
pull it from."
  (format "%s: %s\n" title (slstats-get size stats)))

;;;###autoload
(defun slstats ()
  "Display available statistics about Second Life.

This includes information available about the state of the grid and the SL economy."
  (interactive)
  (let ((lab-stats (slstats-load-lab-data))
        (grid-size (slstats-load-grid-size-data)))
    (with-help-window "*Second Life Stats*"
      (princ "Total sign-ups..: ")
      (princ (slstats-get :signups lab-stats))
      (princ "\n")
      (princ "Last updated....: ")
      (princ (slstats-format-time :signups_updated_unix lab-stats))
      (princ "\n\n")
      (princ "Exchange rate...: ")
      (princ (slstats-get :exchange_rate lab-stats))
      (princ "\n")
      (princ "Last updated....: ")
      (princ (slstats-format-time :exchange_rate_updated_unix lab-stats))
      (princ "\n\n")
      (princ "Avatars in-world: ")
      (princ (slstats-get :inworld lab-stats))
      (princ "\n")
      (princ "Last updated....: ")
      (princ (slstats-format-time :inworld_updated_unix lab-stats))
      (princ "\n\n")
      (princ "Grid size:\n")
      (princ (slstats-format-grid-size-total "Total......." :total grid-size))
      (princ (slstats-format-grid-size-total "Private....." :private grid-size))
      (princ (slstats-format-grid-size-total "Linden......" :linden grid-size))
      (princ (slstats-format-grid-size-total "Adult......." :adult grid-size))
      (princ (slstats-format-grid-size-total "Mature......" :mature grid-size))
      (princ (slstats-format-grid-size-total "PG.........." :pg grid-size))
      (princ (slstats-format-grid-size-total "Linden Homes" :linden_homes grid-size)))))

(provide 'slstats)

;;; slstats.el ends here
