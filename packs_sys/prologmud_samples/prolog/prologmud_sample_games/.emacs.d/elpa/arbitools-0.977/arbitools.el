;;; arbitools.el --- Package for chess tournaments administration

;; Copyright 2016-2019 Free Software Foundation, Inc.

;; Author: David Gonzalez Gandara <dggandara@member.fsf.org>
;; Version: 0.977
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; REQUIRES:
;; ---------------------------
;; Some functions require the arbitools python package, written by myself
;; you can install it by: "pip3 install arbitools"
;;
;; "pdflatex" by Han The Thanh is necessary in case you want to get pdfs.
;;            It is distributed under a GPL license.
;;            https://www.tug.org/applications/pdftex/
;;
;; "bbpPairings.exe" by Bierema Boyz Programming is necessary to do the
;;                   pairings. Copy the file to an executable folder,
;;                   for example /usr/bin.
;;                   Find bbpPairings in
;;                   https://github.com/BieremaBoyzProgramming/bbpPairings
;;                   under GPL license.
;;
;; USAGE:
;; ---------------------------
;; arbitools.el is an interface for the python package "arbitools",
;; designed to manage chess tournament reports.  If you don't install the
;; python package you can still have the syntax colouring and some native
;; functions. In the future, all the functions will be translated to ELISP.
;;
;; FEATURES:
;; ----------------------------
;; - Syntax colouring for the official trf FIDE files.  This facilitates
;; manual edition of the files.
;;
;; - Updating the players ratings. - with python
;;
;; - Adding players to an existing file. - with python
;;
;; - Getting standings from a tournament file. -with python
;;
;; - Getting IT3 Tournament report form. - with python
;;
;; - Deleting a round. - Native
;;
;; - Insert result. - Native
;;
;; - Insert player. - Native
;;
;; - Insert bye. - Native
;;
;; - Get the pairing list or results of a round - Native
;;
;; - Get the list of the players - Native
;;
;; - Delete player. Adjust all rank numbers - Native
;;
;; - Adjust points for each player, according to results of rounds - Native
;;
;; - Print standings - Native
;;
;; - Calculate performance and ARPO (Average Rating Performance of Opponents -Native
;;   ARPO calcuations are based on the ideas of Miguel Brozos, Marco A. Campo,
;;   Carlos Díaz and Julio González
;;   eio.usc.es/pub/julio/desempate/Performance_Recursiva.htm
;;
;; - Export ELO in FEDA (Spanish Chess Federation) format - with python
;;
;; - Do pairings - with bbpPairings.exe. In order for this to work,
;;                 remember to add XXR and XXCfields in the file with the number
;;                 of rounds of the tournament.
;;
;; TODO:
;; ---------------------------------
;;
;; - Write the add players from file function in ELISP.
;; - Insert results from a results file created with a pairing program.
;;   Add the date in the "132" line and the results in the "001" lines.
;; - Add empty round. Ask for date create empty space in the players lines.
;;   Add the date in the "132" line.
;; - Add the rank number and the position automatically when adding players.
;; - Add team.
;; - Add player to team. Prompt for team and player number.
;; - Generate pgn file for a round or the whole tournament.
;; - Reorder the players list
;; - Error handling
;; - Make the interface more friendly
;; You will find more information in www.dggandara.eu/arbitools.htm

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'seq)

(defvar arbitools-verbose nil)
(defvar arbitools-elo-floor 1000 "Rating floor for calculations")
(defvar arbitools-arpo-cutworst t "Cut the worst result for ARPO calculations")
(defvar arbitools-arpo-cutbest t "Cut the best results for ARPO calculations")
(defvar arbitools-performancetable (list -800 -677 -589 -538 -501 -470 -444 -422 -401 -383 -366 -351 -336 -322 -309 -296 -284 -273 -262 -251 -240 -230 -220 -211 -202 -193 -184 -175 -166 -158 -149 -141 -133 -125 -117 -110 -102 -95 -87 -80 -72 -65 -57 -50 -43 -36 -29 -21 -14 -7 0 7 14 21 29 36 43 50 57 65 72 80 87 95 102 110 117 125 133 141 149 158 166 175 184 193 202 211 220 230 240 251 262 273 284 296 309 322 336 351 366 383 401 422 444 470 501 538 589 677 800)
  "Table of expected results according to FIDE ratings rules")
(defvar arbitools-players-info nil
  "Table to store rank numbers, names, ratings and expected results")

;; TODO Implement a hashtable to parse the file and store the data
;; TODO Implement the performance table as a vector variable

(defun arbitools-fill-players-info ()
  "Fill `arbitools-players-info' with the information from the main buffer"
  (save-excursion
    (goto-char (point-min))
    (setq arbitools-players-info nil)
    (while (re-search-forward "^001" nil t)
      (let* ((linestring (thing-at-point 'line))
	     (rankstring (substring-no-properties linestring 5 8))
	     (namestring (substring-no-properties linestring 14 47))
	     (elostring (substring-no-properties linestring 48 52))
	     (playerinfo))
	
        (push rankstring playerinfo)
        (push namestring playerinfo)
        (push elostring playerinfo)
        (push '0 playerinfo)
	  (add-to-list 'arbitools-players-info (reverse playerinfo) t)))))

(defun arbitools-do-pairings (round)
  "Use bbpPairings to do the pairings for the next round.
   You need a XXR section followed by the number of rounds.
   If you have any players that are not going to be paired,
   insert 0000 - H in the column, for a half point bye and
   0000 - F for full point bye. You can do that with
   arbitools-insert-bye. For the first round you will need a
   XXC section followed by white1 or black1, which will force
   the corresponding colour.
   If the program throws an error you will find it in the
   Pairings-output buffer."
  ;; TODO: if there is no XXR entry, error and prompt to write one.
  ;; TODO: right now, the program writes "0" as an opponent for allocated bye:
  ;;       replace this with "0000 - U".
  (interactive "sWhich round do you need to generate the pairings for?: ")
  (with-current-buffer "Pairings-output"
    (erase-buffer))
  (call-process "bbpPairings.exe" nil "Pairings-output" nil  "--dutch" buffer-file-name "-p")

  (let* ((numberoftables 0)
         (actualtable 0)
         (white 0)
         (black 0)
         (positiontowrite (+ 89 (* (- (string-to-number round) 1) 10)))
         (endoflinecolumn 0))

    (with-current-buffer "Pairings-output"
      (goto-char (point-min))
      (setq numberoftables (string-to-number (thing-at-point 'word))))
    (while (<= actualtable numberoftables)
      (with-current-buffer "Pairings-output"
        (forward-line)
        (setq actualtable (+ actualtable 1))
        (setq white (thing-at-point 'word))
        (forward-word)
        (forward-word)
        (setq black (thing-at-point 'word)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^001" nil t)
          (forward-char 4) ;; go to rank number
          (when (string= white (thing-at-point 'word))
            (end-of-line)
            (setq endoflinecolumn (current-column))
            (beginning-of-line)
            (forward-char positiontowrite)
            (unless (= positiontowrite endoflinecolumn) ;; check if there is something and
              (delete-char (- endoflinecolumn positiontowrite))) ;; erase it
            (insert "     ") ;; replace the first positions with spaces
            (cond ((= 2 (length black)) (backward-char 1)) ;; make room for bigger numbers
                  ((= 3 (length black)) (backward-char 2)))
            (insert (format "%s w  " black))
            (cond ((= 2 (length black)) (delete-char 1)) ;; adjust when numbers are longer
                  ((= 3 (length black)) (delete-char 2))))
          (when (string= black (thing-at-point 'word))
            (end-of-line)
            (setq endoflinecolumn (current-column))
            (beginning-of-line)
            (forward-char positiontowrite)
            (unless (= positiontowrite endoflinecolumn) ;; check if there is something and
              (delete-char (- endoflinecolumn positiontowrite))) ;; erase it
            (insert "     ") ;; replace the first positions with spaces
            (cond ((= 2 (length white)) (backward-char 1)) ;; make room for bigger numbers
                  ((= 3 (length white)) (backward-char 2)))
            (insert (format "%s b  " white))
            (cond ((= 2 (length white)) (delete-char 1)) ;; adjust when numbers are longer
                  ((= 3 (length white)) (delete-char 2)))))))))

(defun arbitools-prepare-file-DOS ()
  "Prepare file for DOS: add carriage return at the end of lines.
   For some administrators, like the ones in FEDA, the files need
   to be in this format or they will not allow them."
  (interactive)
  ;; FIXME: Most likely this should be replaced by something like
  ;; (set-buffer-file-coding-system 'dos)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))))

(defun arbitools-update (elolist)
  "Update the players ratings in a database file based on a elo list file."
  (interactive "selolist:")
  ;; FIXME: What if `list' is "foo; bar"?
  (call-process "arbitools-run.py" nil "Arbitools-output" nil "update" buffer-file-name "-l" elolist))

(defun arbitools-add (addfile)
  "Add players to an existing database file."
  (interactive "faddfile: ")
  ;; FIXME: What if `addlist' is "foo; bar"?
  (call-process "arbitools-add.py" nil "Arbitools-output" nil "-a" addfile "-i" buffer-file-name))

(defun arbitools-trim-left (s)
            "Remove whitespace at the beginning of S."
            (if (string-match "\\`[ \t\n\r]+" s)
              (replace-match "" t t s)
              s))
(defun arbitools-trim-right (s)
            "Remove whitespace at the end of S."
            (if (string-match "[ \t\n\r]+\\'" s)
              (replace-match "" t t s)
              s))

(defun arbitools-arpo-vega ()
  "Create userTB.txt file for file generated with ARPO app.
   Use in crosstable.txt generated in Vega.
   You need to open the ARPO1.txt file in another buffer."

  ;; FIXME: Right now the buffer is modified to perform the operations.
  ;; It should be done without modifying
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 7) ;; where the data starts in crosstable.txt. This can be improved with regex
    (let* ((continue t)
	   (arpodata "data")
	   (arpopoint "point")
	   (name " ")
	   namesplit)

      (when (not (get-buffer "userTB.txt")) (generate-new-buffer "userTB.txt"))
      (with-current-buffer "userTB.txt"
        (erase-buffer) (insert "  User Tie-Break  ;"))
      (let ((case-fold-search t)) ;; removing the string "(W)" in players who withdrew. This should be replaced afterwards

	(goto-char (point-min))
	(forward-line 7)
	(while (search-forward "(W)" nil t)
	  (replace-match "   "))
	(goto-char (point-min))
	(forward-line 7))
      (while continue ;; loop over crosstable.txt
        (beginning-of-line) (forward-word)
	(if (thing-at-point 'word)
	    ;; if statement
	    (progn
	      (clear-string name)
	      (setq name (substring-no-properties (thing-at-point 'line) 4 24)) ;; read the players name
	      (setq namesplit (split-string name ",")) ;; remove the comma, which is not in ARPO1
	      (setq name (mapconcat #'identity namesplit "" )) ;; remove the comma
	      (setq name (arbitools-trim-right name)) ;; remove the comma

	      (with-current-buffer "ARPO1.txt"
		(goto-char (point-min))

		(if (search-forward name) ;; find the name from crosstable
		    ;; then
		    (progn
                      (end-of-line)(backward-word) ;; go to the end of line, where the ARPO is
		      (setq arpopoint (thing-at-point 'word))(backward-word) ;; get decimal figures
		      (setq arpodata (thing-at-point 'word)) ;;get integer part

                      (with-current-buffer "userTB.txt"
			(insert arpodata)(insert ".")
			(insert arpopoint) (insert ";"))) ;; insert the ARPO in userTB.txt
                  ;; else
		  (with-current-buffer "userTB.txt"
		    (insert "0.0;")))) ;; in case the player has not got an ARPO, write a 0
	      (forward-line))
	  ;;else statement
	  (setq continue nil)))))) ;; if no more players, tell the while to stop


(defun arbitools-list-pairing (round)
  "Get the pairings and/or results of the given round. It will
   only work with the current round. Some player's names will be
   missing if you try a finished round, and the order of the tables
   will be wrong.
   You will find the pairings in the Pairing List buffer."
  ;; TODO: Fix the finished rounds issue.
  ;; TODO: There is an issue with the table number for floats.
  (interactive "sFor which round?: ")
  (arbitools-calculate-standings)
  (save-excursion
    (let* ((tournamentnamestring)
           (numberofplayers)
           (tablenumber 1)
           (tablenumberstring)
           (paired '())
           (saveposition)
           (savepositionstandings)
           (namestring nil)
           (opponentstring nil)
           (rankstring nil)
           (fideidstring nil)
           (opponent nil)
           (color nil)
           (result nil))

      (goto-char (point-min))
      (re-search-forward "^012" nil t)
      (setq tournamentnamestring (substring-no-properties (thing-at-point 'line) 4 (end-of-line)))
      (goto-char (point-min))
      (re-search-forward "^062" nil t)
      (setq numberofplayers (string-to-number (substring-no-properties
        (thing-at-point 'line) 4 (end-of-line))))
      (with-current-buffer "Pairings List"
        (erase-buffer)
        (insert (format "%s" tournamentnamestring))
        (insert (format "Pairings for round %s\n\n" round)))
      (with-current-buffer "Standings"
        (goto-char (point-min)) (forward-line 4) (setq savepositionstandings (point)))
      (while (>= numberofplayers 1)
         (with-current-buffer "Standings"
           (goto-char savepositionstandings)
           (forward-line 1)
           (setq fideidstring (arbitools-trim-left (substring-no-properties (thing-at-point 'line) 50 58)))
           (setq savepositionstandings (point)))
         (goto-char (point-min))
         (search-forward fideidstring)
         (setq rankstring (arbitools-trim-left (substring-no-properties (thing-at-point 'line) 4 8)))
         (setq namestring (substring-no-properties (thing-at-point 'line) 14 46))
         (setq opponent (arbitools-trim-left (substring-no-properties (thing-at-point 'line)
                        (+ 91 (* (- (string-to-number round) 1)10 ))
                        (+ 95(* (- (string-to-number round) 1) 10 )))))
         (setq color (substring-no-properties (thing-at-point 'line)
                        (+ 96 (* (- (string-to-number round) 1)10 ))
                        (+ 97(* (- (string-to-number round) 1) 10 ))))
         (setq result (substring-no-properties (thing-at-point 'line)
                        (+ 98 (* (- (string-to-number round) 1)10 ))
                        (+ 99(* (- (string-to-number round) 1) 10 ))))

         (setq saveposition (point))
         (goto-char (point-min))
         (while (re-search-forward "^001" nil t)
           (forward-char 4)
           (when (string= (arbitools-trim-left opponent) (thing-at-point 'word))
             (setq opponentstring (substring-no-properties (thing-at-point 'line) 14 46))
             (with-current-buffer "Arbitools-output" (insert (format "%s" opponentstring)))))
         (goto-char saveposition)
         (unless (or (member rankstring paired) (member opponent paired))
           (cl-pushnew rankstring paired :test #'equal)
           (with-current-buffer "Pairings List"
             (setq tablenumberstring (number-to-string tablenumber))
             (when (< (length tablenumberstring)  2)
               (setq tablenumberstring (concat " " tablenumberstring)))
             (when (< (length rankstring)  2)
               (setq rankstring (concat rankstring " ")))
             (when (< (length opponent)  2)
               (setq opponent (concat opponent " ")))
             (cond ((string= color "w")
                     (cond ((string= result "1")
                             (insert (format "%s. %s %s 1-0 %s %s\n" tablenumberstring rankstring
                              namestring opponent opponentstring)))
                           ((string= result "0")
                             (insert (format "%s. %s %s 0-1 %s %s\n" tablenumberstring rankstring
                              namestring opponent opponentstring)))
                           ((string= result "+")
                             (insert (format "%s. %s %s + - %s %s\n" tablenumberstring rankstring
                              namestring opponent opponentstring)))
                           ((string= result "-")
                             (insert (format "%s. %s %s - + %s %s\n" tablenumberstring rankstring
                              namestring opponent opponentstring)))
                           ((string= result " ")
                             (insert (format "%s. %s %s  -  %s %s\n" tablenumberstring rankstring
                              namestring opponent opponentstring)))
                           ((string= result "=")
                             (insert (format "%s. %s %s 1/2 %s %s\n" tablenumberstring rankstring
                              namestring opponent opponentstring)))))
                   ((string= color "b")
                     (cond ((string= result "1")
                           (insert (format "%s. %s %s 0-1 %s %s\n" tablenumberstring opponent opponentstring
                            rankstring namestring)))
                          ((string= result "0")
                           (insert (format "%s. %s %s 1-0 %s %s\n" tablenumberstring opponent opponentstring
                            rankstring namestring)))
                          ((string= result "+")
                           (insert (format "%s. %s %s - + %s %s\n" tablenumberstring opponent opponentstring
                            rankstring namestring)))
                          ((string= result "-")
                           (insert (format "%s. %s %s + - %s %s\n" tablenumberstring opponent opponentstring
                            rankstring namestring)))
                          ((string= result " ")
                           (insert (format "%s. %s %s  -  %s %s\n" tablenumberstring opponent opponentstring
                            rankstring namestring)))
                          ((string= result "=")
                           (insert (format "%s. %s %s 1/2 %s %s\n" tablenumberstring opponent opponentstring
                            rankstring namestring))))))))
         (setq tablenumber (+ tablenumber 1))
         (setq numberofplayers (- numberofplayers 1)))))
   (switch-to-buffer "Pairings List"))

(defun arbitools-list-players ()
  "Put the list of players in two buffers, one in plain text and another
   in a beautiful LaTeX.
   You will also find a list in the List of players buffer."
  ;; TODO: the beautiful LaTeX
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (with-current-buffer "List of players" (erase-buffer))
   (while (re-search-forward "^001" nil t)
     (let* ((linestring (thing-at-point 'line))
           (rankstring (substring linestring 5 8)))

       (with-current-buffer "List of players"
         (insert (format " %s " rankstring))))

     (let* ((linestring (thing-at-point 'line))
           (namestring (substring linestring 14 47)))

       (with-current-buffer "List of players"
         (insert (format "%s " namestring))))

     (let* ((linestring (thing-at-point 'line))
            (elostring (substring linestring 48 52)))

       (with-current-buffer "List of players"
         (insert (format "%s\n" elostring))))))
  (with-current-buffer "List of players"
    (remove-text-properties (point-min)(point-max) '(face nil))))

(defun arbitools-new-trf ()
  "Create an empty trf file"
  ;; TODO prompt for the data of the tournament to create the structure
  (interactive)
  (generate-new-buffer "New trf")
  (switch-to-buffer "New trf")
  (set-buffer "New trf")
  (arbitools-mode)
  (insert "012 NAME OF THE TOURNAMENT\n")
  (insert "022 PLACE\n")
  (insert "032 FEDERATION\n")
  (insert "042 STARTING DATE (YYYY/MM/DD)\n")
  (insert "052 ENDING DATE (YYYY/MM/DD)\n")
  (insert "062 NUMBER OF PLAYERS\n")
  (insert "072 NUMBER OF RATED PLAYERS\n")
  (insert "082 NUMBER OF TEAMS\n")
  (insert "092 TYPE OF TOURNAMENT\n")
  (insert "102 CHIEF ARBITER\n")
  (insert "112 DEPUTY CHIEF ARBITER\n")
  (insert "122 ALLOTED TIMES PER MOVE/GAME\n")
  (insert "XXC COLOR FOR THE FIRST ROUND (white1 or black1)\n")
  (insert "XXR NUMBER OF ROUNDS\n")
  (insert "132 DATES                                                                                  YY/MM/DD  YY/MM/DD\n")
  (insert "001  000 GTIT NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN RAT. FED   0000000000 YYYY/MM/DD 00.0  RNK  0000 C R  0000 C R\n")
  ;; (insert "013 NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN  0000 0000\n")
)

 (defun arbitools-number-of-rounds ()
   "Get the number of rounds in the tournament. It has to be executed in the
    principal buffer."
   (let* ((numberofrounds 0))

     (save-excursion
       (if (re-search-forward "^XXR" nil t)
         (progn
            (beginning-of-line)
            (forward-char 5)
            (setq numberofrounds (string-to-number (thing-at-point 'word))))

         (goto-char (point-min))
         (re-search-forward "^132" nil t)
         (let* ((linestringrounds (thing-at-point 'line))
              (beginning-of-round 91)
              (end-of-round 99)
              (continue t))
            (while continue
              (if (< end-of-round (length linestringrounds))

                (progn
                   (setq numberofrounds (+ numberofrounds 1))
                   (setq beginning-of-round (+ beginning-of-round 10))
                   (setq end-of-round (+ end-of-round 10)))

                   (setq continue nil))))))
     numberofrounds))

(defun arbitools-actual-round ()
  "Calculate the actual round. It has to be run on the principal buffer."
  ;; TODO this function can be improved by checking all the players lines
  ;; instead of just the first one
  (let* ((actualround 0))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^001" nil t)
      (end-of-line)
      (setq actualround (- (current-column) 89))
      ;; 89 is the position of the initial data
      (when (> (current-column) 89)
        (setq actualround (/ (current-column) 10)))
      (when (< actualround 0)
        (setq actualround 0)))
    actualround))

(defun arbitools-calculate-points (round)
  "Automatically calculate the points of each player and adjust the
   corresponding column.
   Don't use this function when the round doesn't include all the results."
  (interactive "sUp to which round?: ")
  (save-excursion
    (let ( ;; (numberofrounds (arbitools-number-of-rounds))
           (points         0.0)
           (pointstosum    0.0)
           (roundcount     1))
      (goto-char (point-min))
      (while (re-search-forward "^001" nil t)
        (setq points 0.0)
        (setq roundcount 1)
        (while (<= roundcount (string-to-number round))
          (beginning-of-line)
	  (forward-char (+ 98 (* (- roundcount 1) 10))) ;; go to where the result is for each round
          (let ((sym (thing-at-point 'symbol)))
            ;; FIXME: If `sym' doesn't match any of those, we keep the previous
            ;; value of `pointstosum', which seems wrong.
	    ;; ::: pointstosum should stay the same if none of the conditions
	    ;;     is met
            (cond ((string= sym "1") (setq pointstosum 1.0))
                  ((string= sym "+") (setq pointstosum 1.0))
                  ((string= sym "=") (setq pointstosum 0.5))
                  ((string= sym "0") (setq pointstosum 0.0))
                  ((string= sym "-") (setq pointstosum 0.0))
                  ((string= sym "F") (setq pointstosum 1.0))
                  ((string= sym "H") (setq pointstosum 0.5))
                  ((string= sym "Z") (setq pointstosum 0.0))
                  ((string= sym "U") (setq pointstosum 1.0))
                  ((string= sym nil) (setq pointstosum 0.0))))
          (setq points (+ points pointstosum))
          (setq roundcount (+ roundcount 1)))
        (beginning-of-line)
        (forward-char 84)
        (forward-char -3)
        (delete-char 3)
        (insert-char ?\s (- 3 (length (format "%s" points))))
        (insert (format "%s" points))))))

(defun arbitools-calculate-standings ()
  "Write the standings in the Standings buffer. Update the POS field in the
   file.
   You might need to run arbitools-calculate-points before using this
   function."
  ;; TODO: Write tiebreaks. Write a new function that organize the standings according
  ;; to a given tiebreak.
  ;; Also, make it possible to print standings for past rounds.
  (interactive)
  (save-excursion
    (with-current-buffer "Standings"
      (erase-buffer))
    (arbitools-list-players)
    (let* ((tournamentnamestring)
           (numberofplayers 0)
           (round "round")
           (datachunk "")
	   (name)
	   (arpo)
           (newpos 0)
           (beg)
           (end))
      (goto-char (point-min))
      (re-search-forward "^062" nil t)
      (forward-char 1)
      (setq numberofplayers (thing-at-point 'word)) ;; get the number of players
      (goto-char (point-min))
      (re-search-forward "^012" nil t) ;; Get the name of the tournament
      (setq tournamentnamestring (thing-at-point 'line))
      (with-current-buffer "Standings" ;; write the headings
	(erase-buffer)
        (insert (format "%s" tournamentnamestring))
        (insert (format "Standings for round %s\n\n" round))
        (insert (format "POS. PTS.   No. Name                              ID         ARPO\n")))
      (goto-char (point-min))
      (while (re-search-forward "^001" nil t) ;; loop the players in the main buffer
        (beginning-of-line)
        (setq datachunk (substring-no-properties (thing-at-point 'line) 80 84)) ;; get points
        (with-current-buffer "Standings"
          (insert (format "%s" datachunk))
          (insert-char ?\s (- 4 (length datachunk))) ;; insert spaces to make list beautiful
          (insert "  "))
        (setq datachunk (substring-no-properties (thing-at-point 'line) 4 8)) ;; get rank
        (with-current-buffer "Standings"
          (insert (format "%s" datachunk))
          (insert-char ?\s (- 4 (length datachunk)))
          (insert "  "))
        (setq datachunk (substring-no-properties (thing-at-point 'line) 14 47)) ;; get name
	(setq name datachunk)
        (with-current-buffer "Standings"
          (insert (format "%s" datachunk))
          (insert-char ?\s (- 33 (length datachunk)))
          (insert " "))
        (beginning-of-line)
        (forward-char 67)
        (setq datachunk (thing-at-point 'word)) ;; get idfide
        (with-current-buffer "Standings"
          (insert (format "%s" datachunk))
          (insert-char ?\s (- 10 (length datachunk)))
	  (insert " "))
	(with-current-buffer "ARPO"
	  (goto-char (point-min))
	  (re-search-forward name nil t)
	  (forward-word)
	  (setq arpo (thing-at-point 'word))
	  (forward-word)
	  (setq arpo (format "%s.%s" arpo (thing-at-point 'word))))
	(with-current-buffer "Standings"
          (insert (format "%s" arpo)) ;; fix tabs for sorting to work fine
	  (insert "\n"))
        )
      (with-current-buffer "Standings" ;;  sorting
        (goto-char (point-min))
        (forward-line 4)
        (setq beg (point))
        (goto-char (point-max))
        (setq end (point))
        (reverse-region beg end) ;; make sure it respects the ranking
        (sort-numeric-fields 1 beg end)
        (goto-char (point-min))
        (forward-line 4)
        (while (>= (string-to-number numberofplayers) 1) ;; write the positions
          (insert (format "%s" numberofplayers))
          (insert-char ?\s (- 3 (length numberofplayers)))
          (insert " ")
          (setq numberofplayers (number-to-string(- (string-to-number numberofplayers) 1)))
          (forward-line 1))
        (goto-char (point-min))
        (forward-line 4)
        (setq beg (point))
        (goto-char (point-max))
        (setq end (point))
        (reverse-region beg end)) ;; get this to sort in reverse
      (goto-char (point-min))
      (while (re-search-forward "^001" nil t)
	(setq datachunk (substring-no-properties (thing-at-point 'line) 14 47))
        (with-current-buffer "Standings"
          (goto-char (point-min))
          (search-forward datachunk nil t)
          (setq newpos (- (line-number-at-pos) 4))) ;; the number of gives as the pos field
        ;; minus 4 because of the first two lines
        (beginning-of-line)
        (forward-char 89) ;; go to POS field
        (forward-char -3)
        (delete-char 3)
        (insert-char ?\s (- 3 (length (format "%s" newpos))))
        (insert (format "%s" newpos))))))

(defun arbitools-delete-player (player)
   "Delete a player. Adjust all the rank numbers accordingly."
   (interactive "sInsert rank number of the player: ")
   (let ((numberofrounds 0)
         (elo            ""))

    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^132" nil t)
        (let* ((linestringrounds   (thing-at-point 'line))
               (beginning-of-round 91)
               (end-of-round       99)
               (continue           t))
           (while continue
             (if (< end-of-round (length linestringrounds))
               (progn
                  ;; (setq actualround (substring-no-properties linestringrounds beginning-of-round end-of-round))
                  (setq numberofrounds (+ numberofrounds 1))
                  (setq beginning-of-round (+ beginning-of-round 10))
                  (setq end-of-round (+ end-of-round 10)))
               (setq continue nil)))))
    (save-excursion
     (goto-char (point-min))
     (while (re-search-forward "^001" nil t)
       (let* ((linestring (thing-at-point 'line))
              (rankstring (substring linestring 5 8)))
         (when (= (string-to-number rankstring) (string-to-number player))
           (forward-char 1)
           (delete-char 4)
           (insert " DEL")
           (setq elo (substring linestring 48 52))
           (with-current-buffer "Arbitools-output" (insert (format "%s" elo))))
         (when (> (string-to-number rankstring)(string-to-number player))
           (forward-char 1)
           (delete-char 4)
           (insert-char ?\s (- 4 (length (format "%s" (- (string-to-number rankstring) 1)))))
           (insert (format "%s" (- (string-to-number rankstring) 1)))
           (save-excursion
             (goto-char (point-min))
             (while (re-search-forward "^001" nil t)
               (let* ((roundcount          1))
                  (while (<= roundcount numberofrounds)
                    (beginning-of-line)
                    (forward-char (+ 95 (* (- roundcount 1) 10)))
                    (when (string= (format "%s" (string-to-number rankstring)) (thing-at-point 'word))
                      (forward-char -4) ;; go back to the beginning of the opponent's number
                      (delete-char 4) ;; remove the original opponent's number
                      (insert-char ?\s (- 4 (length (format "%s" (- (string-to-number rankstring) 1)))))
                      (insert (format "%s" (- (string-to-number rankstring) 1))))
                    (setq roundcount (+ roundcount 1))))
               ;;(condition-case nil ;; TODO: fix teams info
                 (save-excursion
                   (while (re-search-forward "^013" nil t)
                    (let* ((linestringteam (thing-at-point 'line))
                          (integrantcount 0)
                          (members 0))

                        ;; to find the end of the line, the number is length -2, for some reason
                        (setq members (/ (- (- (length linestringteam) 2) 34) 5)) ;; calculate number of members

                      (while (< integrantcount members)
                       (beginning-of-line)
                       (forward-char (+ 40 (* (- integrantcount 1) 5)))
                       (when (string= (format "%s" (string-to-number rankstring)) (thing-at-point 'word))
                         (forward-char -4)
                         (delete-char 4)
                         (insert-char ?\s (- 4 (length (format "%s" (- (string-to-number rankstring) 1)))))
                         (insert (format "%s" (- (string-to-number rankstring) 1))))
                       (setq integrantcount (+ integrantcount 1))))))))))))

     (save-excursion  ;; Actually delete the player's line
       (goto-char (point-min))
       (while (re-search-forward "^001  DEL" nil t)
         (beginning-of-line)
         (let ((beg (point)))
           (forward-line 1)
           (delete-region beg (point)))))
     ;; TODO delete the rank from teams section
     ;; TODO change number of players and number of rated players
     (save-excursion
       (with-current-buffer "Arbitools-output" (insert (format "%s" elo)))
       (goto-char (point-min))
       (re-search-forward "^062 ")
       (let* ((linestring      (thing-at-point 'line))
              (numberofplayers (substring linestring 4)))
        (delete-char (length numberofplayers))
        (setq numberofplayers (string-to-number numberofplayers))
        (setq numberofplayers (- numberofplayers 1))
        (insert (concat (number-to-string numberofplayers) "\n")))
       (re-search-forward "^072 ")
       (let* ((linestring           (thing-at-point 'line))
              (numberofratedplayers (substring linestring 4)))
        (unless (< (length elo) 2) ;; if elo is 0 or nonexistent
          (delete-char (length numberofratedplayers))
          (setq numberofratedplayers (string-to-number numberofratedplayers))
          (setq numberofratedplayers (- numberofratedplayers 1))
          (insert (concat (number-to-string numberofratedplayers) "\n")))))))

(defun arbitools-delete-round (round)
   "Delete a round."
   ;; TODO: It seems that it doesn't delete a previous bye inserted.
   (interactive "sround: ")
   (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^001" nil t)
     (forward-char (+ 88 (* (- (string-to-number round) 1) 10)))
     (delete-char 8)
     (insert "        "))))

(defun arbitools-insert-bye (player round type)
   "Insert bye for player. Types of byes are: H for Half point,
    F for Full point, Z for zero points of U for allocated by the system."
   (interactive "sRank number of the player: \nsround: \nstype (H, F, -, Z, U):")
   (let* ((pointtowrite (+ 89 (* (- (string-to-number round) 1) 10)))
       (positionendofline 0)
       (points 0.0))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "^001" nil t)
         (forward-char 4) ;; go to rank number
         (when (string= player (thing-at-point 'word))
          (end-of-line)
          (setq positionendofline (current-column))
          ;; create space if needed
          (when (< positionendofline pointtowrite)
            (end-of-line)
            (insert-char 32 (- pointtowrite positionendofline)))
          (beginning-of-line)
          (forward-char 84)
          (forward-char -3)
          (setq points (string-to-number (thing-at-point 'word)))
          (cond ((string= type "H")(setq points (+ points 0.5)))
            ((string= type "F")(setq points (+ points 1.0)))
            ((string= type "U")(setq points (+ points 1.0)))
            ((string= type "Z")(setq points (+ points 0.0)))
            ((string= type "-")(setq points (+ points 0.0))))
          (delete-char 3)
          (insert-char ?\s (- 3 (length (format "%s" points)))) ;; write extra empty spaces
          (insert (format "%s" points)) ;; write the points
          (beginning-of-line)
          (forward-char pointtowrite)
          (insert (format "  0000 - %s" type)))))))

(defun arbitools-replace-empty ()
   "Replace non played games with spaces."
   (interactive)
   (save-excursion
    (goto-char (point-min))
    (while (search-forward "0000 - 0" nil t)
      (replace-match "        "))))

(defun arbitools-insert-player (sex title name elo fed idfide year)
   "Insert a player. You will be prompted for the data."
   ;; TODO: automatically insert the player in a team
   (interactive "ssex: \nstitle: \nsname: \nselo: \nsfed: \nsidfide: \nsyear: ")
  (let ((playerlinelength nil)
        (thislinelength nil))
     (save-excursion
       (goto-char (point-min))
       (re-search-forward "^001 ")
       (let* ((linestring (thing-at-point 'line)))
         (setq playerlinelength (length linestring))))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "^001" nil t))
       (let* ((linestring (thing-at-point 'line))
              (rankstring (substring linestring 5 8)))

         (forward-line 1)
         (insert "\n")
         (forward-char -1)
         (insert (format "001 "))
         (insert-char ?\s (- 4 (length (format "%s" (+ (string-to-number rankstring) 1)))))
         (insert (format "%s" (+ (string-to-number rankstring) 1)))
         (insert (format " %s" sex))
         (when (= (length sex) 0) (insert " ")) ;; add extra space if the sex string is empty
         (insert-char ?\s (- 3 (length title)))
         (insert (format "%s " title))
         (insert (format "%s" name))
         (insert-char ?\s (- 34 (length name)))
         (when (= (length elo) 4) (insert (format "%s " elo)))
         (when (= (length elo) 0) (insert "     ")) ;; add extra space if the elo is empty
         (when (= (length elo) 1) (insert "   0 ")) ;; add extra space if the elo is a "0"
         (insert (format "%s" fed))
         (when (= (length fed) 0) (insert "   ")) ;; add extra space if fed is empty
         (insert-char ?\s (- 12 (length idfide)))
         (insert (format "%s " idfide))
         (insert (format "%s      " year))
         (when (= (length year) 0) (insert "    ")) ;; TODO: improve this to make it support different data formats
         (insert (format "  0.0 "))
         (insert-char ?\s (- 4 (length (format "%s" (+ (string-to-number rankstring) 1)))))
         (insert (format "%s" (+ (string-to-number rankstring) 1)))
         (setq thislinelength (length (thing-at-point 'line)))
         (insert-char ?\s (- playerlinelength thislinelength)))))
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^062 ")
    (let* ((linestring (thing-at-point 'line))
           (numberofplayers (substring linestring 4)))
      (delete-char (length numberofplayers))
      (setq numberofplayers (string-to-number numberofplayers))
      (setq numberofplayers (+ 1 numberofplayers))
      (insert (concat (number-to-string numberofplayers) "\n")))
    (re-search-forward "^072 ")
    (let* ((linestring (thing-at-point 'line))
        (numberofratedplayers (substring linestring 4)))
      (unless (< (length elo) 2)
        (delete-char (length numberofratedplayers))
        (setq numberofratedplayers (string-to-number numberofratedplayers))
        (setq numberofratedplayers (+ 1 numberofratedplayers))
        (insert (concat (number-to-string numberofratedplayers) "\n"))))))

(defun arbitools--verbose-output (buffer msg &rest args)
  "Insert MSG (formatted with ARGS) into BUFFER.
Only do it if `arbitools-verbose' is non-nil."
  (declare (indent 1))
  (when arbitools-verbose
    (with-current-buffer buffer
      (insert (apply #'format msg args)))))

(defun arbitools-insert-result (round white black result)
   "Insert a result. You will be prompetd for the white and black players
    rank numbers and the result (1, 0, =, +, -)"
   ;; TODO: It erases everything at the end. Fix this.
   (interactive "sround: \nswhite player's rank number: \nsblack player's rank number: \nsresult (1, 0, =, +, -): ")
   (let* ((pointtowrite (+ 89 (* (- (string-to-number round) 1) 10)))
     (positionendofline 0))
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward "^001" nil t)
       (forward-char 4) ;; go to rank number
       (when (string= white (thing-at-point 'word))
         ;; go to first round taking into account the cursor is in the rank number
         (end-of-line)
         (setq positionendofline (current-column))
         (beginning-of-line)
         (forward-char pointtowrite)
         (unless (= pointtowrite positionendofline) ;; check if there is something and
           (delete-char (- positionendofline pointtowrite)))   ;; erase it
         (insert "     ") ;; replace the first positions with spaces
         ;; make room for bigger numbers
         (cond ((= 2 (length black))
           (backward-char 1))
           ((= 3 (length black))
           (backward-char 2)))
         (insert (format "%s w %s" black result)))
       (when (string= black (thing-at-point 'word))
         ;; go to first round taking into account the cursor is in the rank number
         (end-of-line)
         (setq positionendofline (current-column))
         (beginning-of-line)
         (forward-char pointtowrite)
         (unless (= pointtowrite positionendofline) ;; check if there is something and
           (arbitools--verbose-output "Arbitools-output" "yes")
           (delete-char (- positionendofline pointtowrite)))   ;; erase it
         (insert "     ") ;; replace the first positions with spaces
         ;; make room for bigger numbers
         (cond ((= 2 (length white)) (backward-char 1))
           ((= 3 (length white)) (backward-char 2)))
         (cond ((string= "1" result) (insert (format "%s b 0" white)))
           ((string= "=" result) (insert (format "%s b =" white)))
           ((string= "+" result) (insert (format "%s b -" white)))
           ((string= "-" result) (insert (format "%s b +" white)))
           ((string= "0" result) (insert (format "%s b 1" white)))))))))

(defun arbitools-get-player-opponents-average (player)
  "Takes the player's rank as argument. Returns the average rating of the opponents"
  (save-excursion
    (let*((eloaverage 0.0)
	  (opponents (arbitools-get-player-opponents player)))
      (dolist (opponent opponents)
	(let* ((rating (string-to-number (nth 2
	  (nth (- (string-to-number opponent) 1) arbitools-players-info)))))
	  (when (not rating) (setq rating arbitools-elo-floor))
	  (when (not (numberp rating)) (setq rating arbitools-elo-floor))    ;; floor rating here
	  (when (= rating 0) (setq rating arbitools-elo-floor))              ;; floor rating here
	  (setq eloaverage (+ eloaverage rating))))
      (setq eloaverage (/ eloaverage (length opponents)))
      eloaverage)))

(defun arbitools-get-player-ci (player)
  "Takes the player's rank as argument. Returns the re-scaling of diff"
  (save-excursion
    (let*((sum_mi_ci  0.0)
	  (sum_mi     0.0)
	  (ci         0.0)
	  (opponents (arbitools-get-player-opponents player)))
      (dolist (opponent opponents)
        (setq sum_mi (+ sum_mi (length (arbitools-get-player-opponents (string-to-number opponent)))))
	(setq sum_mi_ci (+ sum_mi_ci (* (nth 3 (nth (- (string-to-number opponent) 1) arbitools-players-info)) (length (arbitools-get-player-opponents (string-to-number opponent))))))
	)
      (setq ci (- (nth 3 (nth (- player 1) arbitools-players-info)) (/ sum_mi_ci sum_mi)))
      (arbitools--verbose-output "Arbitools-output"
	"Player %d sum_mi_ci %d sum_mi %d ci %d\n"
	player sum_mi_ci sum_mi ci)
      ci)))

(defun arbitools-get-player-opponents (player)
  "Takes the player's rank as argument. Returns a list which contains the rank number
   of player opponents in the tournament. Worst passes the number of worst opponents
   to remove from the list"
  (save-excursion
    (let*((opps nil)
	  (oppspoints nil)
	  (opponent 000))
      (goto-char (point-min))

      (re-search-forward (format "^001[[:space:]]\\{1,4\\}%d" player))
      (end-of-line)

      (let* ((maxlength (current-column))
	     (numberofrounds (/ (- maxlength 89) 10))
	     (offset 0))

        (dotimes (roundcount-1 numberofrounds)
	  (setq offset (+ 94 (* roundcount-1 10)))
          (beginning-of-line)
	  (forward-char offset)
          (setq opponent (thing-at-point 'word))
          (when (not (member opponent '(" " "" nil "nil" "0000")))
	    (cl-pushnew opponent opps :test #'equal))))

      ;;do list opponents, create an alist with the points of each opponent
      (dolist (opp opps)
	(let* ((points (arbitools-get-player-played-points
                        (string-to-number opp))))
	  (cl-pushnew (cons opp points) oppspoints :test #'equal)))

      ;; Beware: `sort' "consumes" its argument, so it's indispensable to use
      ;; its return value.
      (setq oppspoints (sort oppspoints (lambda (a b) (< (cdr a) (cdr b)))))
      (when (and (> (length opps) 4) arbitools-arpo-cutworst)
	(setq opps (delete (car (car oppspoints)) opps))) ;; cut worst opponent
      (when (and (> (length opps) 4) arbitools-arpo-cutbest)
	(setq opps (delete (car (nth (- (length oppspoints) 1) oppspoints)) opps))) ;; cut best opponent
      (arbitools--verbose-output "Arbitools-output"
	"Player: %d opponents: %d: %s oppspoints %d: %s worst %s best %s\n"
	player
        (length opps) opps
        (length oppspoints) oppspoints
        (car (car oppspoints))
        (car (nth (- (length oppspoints) 1) oppspoints)))
      opps)))


(defun arbitools-get-player-played-points (player)
  "Take the player's rank as argument. Return points got by player in actual games"
  ;; TODO manage results such as 0000 - =
  (save-excursion
    (let*((points     0.0)
	  (result     0))

      (goto-char (point-min))
      (let* ((maxlength 0)
	     (numberofrounds)
	     (offset 0))

        (re-search-forward (format "^001[[:space:]]\\{1,4\\}%d" player))
	(end-of-line)
        (setq maxlength (+ maxlength (current-column)))
        (setq numberofrounds (/ (- maxlength 89) 10))
        (dotimes (roundcount-1 numberofrounds)
	  (setq offset (+ 98 (* roundcount-1 10)))
	  (beginning-of-line)
	  (forward-char offset)
	  (setq result (thing-at-point 'symbol))
          (when (not (member result '(nil "nil" "-" "F" "H" "Z")))
	    (cond
	     ((string= result "1")(setq points (+ 1 points)))
	     ((string= result "+")(setq points (+ 1 points)))
	     ((string= result "=")(setq points (+ 0.5 points)))))))
      points)))

(defun arbitools-get-player-performance (player)
  "Take the player's rank as argument. Return the performance of the player
   in the tournament"
  ;; TODO for some reason, get-player-opponents gets cutworst at always true
  (save-excursion
    (let* ((opponents         (arbitools-get-player-opponents player))
	   (points            (arbitools-get-player-played-points player))
	   (discard           0.0) ;;points to discard
	   (percentage        0.0)
	   (diff              0)
	   (eloaverage        0.0)
	   (performance       0.0)
	   (numberofopponents 0))
      ;; discard points against discarded opponents
      (goto-char (point-min))
      (let* ((maxlength 0)
	     (numberofrounds)
	     (opp 000)
	     (offset 0))

	(re-search-forward (format "^001[[:space:]]\\{1,4\\}%d" player))
	(end-of-line)
	(setq maxlength (+ maxlength (current-column)))
	(setq numberofrounds (/ (- maxlength 89) 10))
	(dotimes (roundcount-1 numberofrounds)
	  (setq offset (+ 94 (* roundcount-1 10)))
	  (beginning-of-line)
	  (forward-char offset)
	  (setq opp (thing-at-point 'word))
	  (when (not (member opp opponents))

            (forward-char 4)
	    (when (string= (thing-at-point 'symbol) "1")
	      (setq discard (+ discard 1.0)))
	    (when (string= (thing-at-point 'symbol) "+")
	      (setq discard (+ discard 1.0)))
	    (when (string= (thing-at-point 'symbol) "=")
	      (setq discard (+ discard 0.5))))))

      ;; loop over opponents and get their elo average
      (dolist (opponent opponents)
	(let* ((rating (string-to-number (nth 2
	                                      (nth (- (string-to-number opponent) 1) arbitools-players-info)))))
	  (when (not rating) (setq rating arbitools-elo-floor))
	  (when (not (numberp rating)) (setq rating arbitools-elo-floor)) ;; floor rating here
	  (when (= rating 0) (setq rating arbitools-elo-floor)) ;; floor rating here
	  (setq eloaverage (+ eloaverage rating))
	  (setq numberofopponents (+ numberofopponents 1))))
      ;; calculate performance
      (setq eloaverage (/ eloaverage numberofopponents))
      (setq points (- points discard))
      (setq percentage (/ points numberofopponents))
      (setq diff (nth (truncate (* 100 percentage)) arbitools-performancetable))
      (setf (nth 3 (nth (- player 1) arbitools-players-info)) diff)
      (when diff
        (arbitools--verbose-output "Arbitools-output"
          "Correct! player %d eloaverage: %d points:%f numberofopponents:%d  percentage: %d\n"
          player eloaverage points numberofopponents (* percentage 100))
        (arbitools--verbose-output "Arbitools-output"
          "Players's info player %s diff %d\n"
          (nth 1 (nth (- player 1) arbitools-players-info)) diff))
      (when (not diff)
        (setq diff 0)
	(with-current-buffer "Arbitools-output"
          (insert (format "Warning! player %d diff=0 eloaverage: %d points:%d numberofopponents:%d percentage: %d\n" player eloaverage points numberofopponents (* 100 percentage)))))
      (setq performance (+ eloaverage diff))
      performance)))



(defun arbitools-calculate-players-performance ()
  "Calculates the performance for all the players in the tourmanent and writes them in a buffer,
   ordered by player rank."
  (save-excursion
    (let* ((performance 0.0)
	   (performances))

      (when arbitools-verbose
        (with-current-buffer "Players performance"
          ;; FIXME: can't use arbitools--verbose-output here because of this
          ;; `delete-region'.
	  (delete-region (point-min)(point-max))
	  (insert "rank Name                           Performance\n")))
      ;; Loop over players and calculate performances
      (dotimes (iter (length arbitools-players-info))
	(let* (;; (rating (string-to-number
               ;;          (nth 2 (nth iter arbitools-players-info))))
	       (name (nth 1 (nth iter arbitools-players-info))))

	  (setq performance (arbitools-get-player-performance (+ iter 1)))
	  (push performance performances)
	  (when arbitools-verbose
            (with-current-buffer "Players performance"
              ;; FIXME: can't use arbitools--verbose-output here because of
              ;; this `goto-char': is it really needed here?
	      (goto-char (point-max))
	      (insert (format "%d %s %s\n" (+ iter 1)	name performance))))))
      performances)))

(defun arbitools-calculate-arpo ()
  "Calculates the ARPO for all the players and writes the results to a buffer.
   also it creates a userTB.txt buffer, so that it can be used in Vega."
  ;; TODO This algorythm is terribly inefficient, it should be improved
  (interactive)
  
  (save-excursion
    (let* ((iterand              )
	   (iterand_1            )
	   (sumiterand           0.0)
	   (converges            nil)
	   (iterations           0)
	   (opponents            )
	   (percentage           0.0)
	   (diff                 0)
	   (points               0.0)
	   (discard              0.0)
	   (performances (reverse (arbitools-calculate-players-performance))) ;;calculate performances
	   (performancesopponents)
	   (opponentsperformance 0.0)
	   (averageperformanceofopponents 0.0)
	   (differences)
	   (difference)
	   (continue t)
	   numberofplayers
	   numberofopponents)

      (setq iterand_1 performances) ;; store performance list in iterand_1 for the first iteration
      (setq numberofplayers (length performances))
      (while continue ;; iterate performances until the check is true
	(setq iterand iterand_1)               ;; fill iterand with iterand_1
	(setq iterand_1 nil)                   ;; reset iterand_1
	(setq sumiterand (apply #'+ iterand))  ;; sum elements in iterand
	(arbitools--verbose-output "Arbitools-output"
          "Starting run %d; iterand: %s sum_iterand: %d\n"
	  iterations iterand sumiterand)
	(dotimes (number numberofplayers) ;; loop the list of performances
	  (setq opponents (arbitools-get-player-opponents (+ number 1)))
	  (setq numberofopponents (length opponents))

          ;; get opponents performances from iterand list, store in a list. Reset performances list
	  (setq performancesopponents nil)
	  (setq averageperformanceofopponents 0.0)
          (dolist (opponent opponents) ;; loop the opponents of each player
	    (setq opponentsperformance (nth (- (string-to-number opponent) 1) iterand))

	    (setq averageperformanceofopponents (+ averageperformanceofopponents opponentsperformance))

	    (push opponentsperformance performancesopponents))

	  ;; calculate average of opponents performance + dp
          (setq averageperformanceofopponents (/ averageperformanceofopponents numberofopponents))

	  ;; calculate points to discard (points against discarded opponents)
	  (setq discard 0.0)
	  (goto-char (point-min))
	  (let* ((maxlength      0)
	         (numberofrounds)
	         (opp            000)
	         (offset         0))

	    (re-search-forward (format "^001[[:space:]]\\{1,4\\}%d" (+ number 1)))
	    (end-of-line)
	    (setq maxlength (+ maxlength (current-column)))
	    (setq numberofrounds (/ (- maxlength 89) 10))
	    (dotimes (roundcount-1 numberofrounds)
	      (setq offset (+ 94 (* roundcount-1 10)))
	      (beginning-of-line)
	      (forward-char offset)
	      (setq opp (thing-at-point 'word))
	      (when (not (member opp opponents))
                (forward-char 4)
                (cond
		 ((member (thing-at-point 'symbol) '("1" "+" "F"))
                  (setq discard (+ discard 1.0)))
		 ((member (thing-at-point 'symbol) '("H" "="))
                  (setq discard (+ discard 0.5)))))))

          ;; calculate percentage of points
	  (setq points (arbitools-get-player-played-points (+ number 1)))
	  (setq points (- points discard))
	  (when (> (length opponents) 0) (setq percentage (/ points numberofopponents)))
	  (setq diff (arbitools-get-player-ci (+ number 1)))
	  (setq averageperformanceofopponents (+ averageperformanceofopponents diff))
	  (arbitools--verbose-output "Arbitools-output" " c %d----\n" diff)

	  (arbitools--verbose-output "Arbitools-output"
	    "Success! player %d run %d points %f discard %f opponents %d: %s percentage %f ARPO averageperformanceofopponents %f performances %s diff %d\n"
	    (+ number 1) iterations points discard numberofopponents opponents percentage averageperformanceofopponents performancesopponents diff)

	  (push averageperformanceofopponents iterand_1))

	(setq iterand_1 (reverse iterand_1)) ;; reverse iterand_1
	(setq differences nil)

	;; write difference in a list to check for convergence
	(dotimes (number numberofplayers)
	  (setq difference (- (nth number iterand) (nth number iterand_1)))
	  (push difference differences))

	;; check if the model converges
	;;(when (and (< (abs (- (nth 1 differences) (nth 0 differences))) 0.0000000001) ;; define here the value of epsilon
	;;	   (< (abs (- (nth (- numberofplayers 1) differences) (nth 0 differences))) 0.0000000001))
	;;  (setq converges t)) ;; TODO: improve this to check more members
	(when (< (- (seq-max differences)(seq-min differences)) 0.0000000001)
	  (setq converges t))

	(setq iterations (+ iterations 1))
	(when (or converges (= iterations 300)) (setq continue nil))) ;; define here maximum number of iterations

      ;; write a buffer with rank, name and the value from the last list obtained
      (arbitools--verbose-output "Arbitools-output"
	"difference: %f differences: %s converges: %s"
	(- (nth 1 differences) (nth 0 differences))
	differences converges)

      ;; write the results in the corresponding buffer
      (with-current-buffer "ARPO"
	(goto-char (point-min))
	(delete-region (point-min)(point-max))
	(insert "rank Name                           ARPO\n"))
      (with-current-buffer "userTB.txt"
        (goto-char (point-min))
	(delete-region (point-min)(point-max))
	(insert "  User Tie-Break   ;"))

      (dotimes (iter (length iterand_1))
	(let* ((name (nth 1 (nth iter arbitools-players-info)))
	       (arpo (nth iter iterand_1)))
          (with-current-buffer "ARPO"
            (insert (format "%d %s %s\n" (+ iter 1) name arpo)))
	  (with-current-buffer "userTB.txt"
            (insert (format "%s;" arpo))))))))

(defun arbitools-it3 ()
   "Get the IT3 tournament report. You will get a .tex file, and a pdf
    if you have pdflatex installed."
   (interactive)
   (call-process "arbitools-run.py" nil "Arbitools-output" nil "it3" buffer-file-name))

;; TODO: New It3 function, usint it3.tex from home directory, replacing the data and pdflatex it

(defun arbitools-fedarating ()
   "Get the FEDA rating admin file."
   (interactive)
   (call-process "arbitools-run.py" nil "Arbitools-output" nil "fedarating" buffer-file-name))

(defvar arbitools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i") 'arbitools-insert-player)
    (define-key map (kbd "C-c r") 'arbitools-insert-result)
    (define-key map (kbd "C-c p") 'arbitools-do-pairing)
    (define-key map (kbd "C-c b") 'arbitools-insert-bye)
    map)
  "Keymap for Arbitools major mode.")


(easy-menu-define arbitools-mode-menu arbitools-mode-map
  "Menu for Arbitools mode"
  '("Arbitools"
    ["New Tournament header" arbitools-new-trf]
    "---"
    ["Insert Player" arbitools-insert-player]
    ["Delete Player" arbitools-delete-player]
    "---"
    ["Do Pairings" arbitools-do-pairings]
    ["Insert Result" arbitools-insert-result]
    ["Insert Bye" arbitools-insert-bye]
    ["Delete Round" arbitools-delete-round]
    "---"
    ["List Players" arbitools-list-players]
    ["List Pairings" arbitools-list-pairing]
    ["Recalculate Standings" arbitools-calculate-standings]
    ["Recalculate points" arbitools-calculate-points]
    ["Calculate ARPO" arbitools-calculate-arpo]
    "---"
    ["Print Standings to file" arbitools-standings]
    "---"
    ["Update Elo" arbitools-update]
    ["Get It3 form Report" arbitools-it3]
    ["Get FEDA Rating file" arbitools-fedarating]
    ["Prepare file for DOS" arbitools-prepare-file-DOS]
    ))


(defvar arbitools-highlights
 '(("^001" . font-lock-function-name-face) ; name of the tournament
    ("^012.*" . font-lock-comment-face)
    ("\\(^022\\|^032\\|^042\\|^052\\|^062\\|^072\\|^082\\|^092\\|^102\\|^112\\|^122\\).*" . font-lock-constant-face)
    ("^132.*" . font-lock-warning-face) ;dates
    ("^013" . font-lock-warning-face) ;teams
    ("\\(^013.\\{1\\}\\)\\(.\\{31\\}\\)" 2 font-lock-comment-face) ;; teams
    ;; (" [0-9]\\{6,\\} " . font-lock-variable-name-face) ;FIDE ID
    ("\\(^001.\\{11\\}\\)\\(.\\{32\\}\\)" 2 font-lock-string-face)  ;; Name of the player (by position)
    ("\\(^001.\\{55\\}\\)\\(.\\{10\\}\\)" 2 font-lock-function-name-face) ;; FIDE ID
    ("\\(^001.\\{88\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face) ;; round 1 opponent
    ;; ("\\(^132.\\{88\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face) ;; round 1 date line
    ("\\(^001.\\{93\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face) ;; round 1 colour
    ("\\(^001.\\{95\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face) ;; round 1 result
    ;; rest of rounds
    ("\\(^001.\\{98\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{98\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{103\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{105\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{108\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{108\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{113\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{115\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{118\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{118\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{123\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{125\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{128\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{128\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{133\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{135\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{138\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{138\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{143\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{145\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{148\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{148\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{153\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{155\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{158\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{158\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{163\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{165\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{168\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{168\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{173\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{175\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{178\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{178\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{183\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{185\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{188\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{188\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{193\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{195\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{198\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{198\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{203\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{205\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)))

;;;###autoload
(define-derived-mode arbitools-mode
  fundamental-mode
  "Arbitools"
  "Major mode for Chess Tournament Management."
  ;(setq font-lock-defaults '(arbitools-highlights))
  
  (get-buffer-create "Arbitools-output")
  (get-buffer-create "List of players")
  (get-buffer-create "Pairings List")
  (get-buffer-create "Standings")
  (get-buffer-create "Pairings-output")
  (get-buffer-create "Players performance")
  (get-buffer-create "ARPO")
  (get-buffer-create "userTB.txt")
  (column-number-mode)
  (arbitools-fill-players-info)
  (set (make-local-variable 'font-lock-defaults) '(arbitools-highlights)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.trf?\\'" . arbitools-mode))

;;;; ChangeLog:

;; 2019-07-08  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	*packages/arbitools: Fixed some bugs
;; 
;; 2019-05-12  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools/arbitools.el: Applied some suggested code tweaks
;; 
;; 2019-05-10  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/arbitools/arbitools.el (arbitools--verbose-output): New
;; 	function
;; 
;; 	(arbitools-insert-result, arbitools-get-player-ci)
;; 	(arbitools-get-player-opponents, arbitools-get-player-performance)
;; 	(arbitools-calculate-arpo): Use it.
;; 
;; 2019-05-10  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/arbitools/arbitools.el: Various code tweaks
;; 
;; 	Silence compiler warnings when compiling with lexical-binding, mostly by
;; 	removing unused variables. Remove redundant `save-excursion` around
;; 	`with-current-buffer`.
;; 	(arbitools-fill-players-info): Don't use `add-to-list` on local vars.
;; 	(arbitools-calculate-points): Don't repeatedly call thing-at-point.
;; 	(arbitools-get-player-opponents): Avoid let-binding `maxlength` and
;; 	`numberofrounds` to dummy initial values only to immediately set them. 
;; 	Use the loop's index variable instead of computing it by hand in
;; 	`roundcount`.  Don't use `add-to-list` on local vars.
;; 	(arbitools-get-player-played-points): Simplify with DeMorgan's
;; 	(AND (not A) (not B)) == (not (or A B)), and then consolidate
;; 	(or (= x ..) (= x ..) ...) into a `member` test. Use the loop's index
;; 	variable instead of computing it by hand in
;; 	`roundcount`.
;; 	(arbitools-get-player-performance): Use the loop's index variable 
;; 	instead of computing it by hand in `roundcount`.
;; 	(arbitools-calculate-arpo): Use the loop's index variable instead of 
;; 	computing it by hand in `roundcount`.  Also use `member`.
;; 
;; 2019-05-09  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools/arbitools.el: Fixed bugs in ARPO
;; 
;; 2019-01-02  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	arbitools.el: added support for ARPO tiebreak
;; 
;; 2018-06-22  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	 arbitools.el: added new function
;; 
;; 2018-01-10  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	arbitools.el: Improved functions, fixed bugs
;; 
;; 2017-12-31  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	*arbitools.el: Some functions improved
;; 
;; 2017-12-19  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* arbitools.el: added new functions, updated website
;; 
;; 2016-05-04  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Removed unused variables
;; 
;; 2016-05-03  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: minor fixes
;; 
;; 2016-04-24  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* arbitools/arbitools.el: Remove unused vars
;; 
;; 	* arbitools.el (arbitools-calculate-standings, arbitools-delete-player)
;; 	(arbitools-calculate-standings): Remove unused vars.
;; 
;; 2016-04-24  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Fixed some bugs
;; 
;; 2016-04-24  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Fixed some bugs
;; 
;; 2016-04-09  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Added new functions
;; 
;; 2016-03-27  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Applied suggestions, improved functions
;; 
;; 2016-03-25  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools: Added new functions
;; 
;; 2016-03-01  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools: Add functions and menus
;; 
;; 2016-02-28  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools: added menu option
;; 
;; 2016-02-28  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools: endoffile bug fixed
;; 
;; 2016-02-28  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools: added some new functions and menus
;; 
;; 2016-02-23  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools.el: fix coding issues
;; 
;; 2016-02-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* arbitools/arbitools.el: Fix checkdoc warnings and quoting problems
;; 
;; 	* arbitools/arbitools.el (arbitools-update, arbitools-add)
;; 	(arbitools-standings): Fix obvious quoting problems.  Add docstring.
;; 	(arbitools-mode): Use a more conventional mode-name.
;; 
;; 2016-02-22  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools.el: correct code syntax issues
;; 
;; 2016-02-21  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	[ELPA]: new package: arbitools
;; 
;; 2016-02-21  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	[ELPA]: new package: arbitools
;; 
;; 2016-02-21  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	[ELPA] new package: arbitools
;; 



(provide 'arbitools)

;;; arbitools.el ends here
