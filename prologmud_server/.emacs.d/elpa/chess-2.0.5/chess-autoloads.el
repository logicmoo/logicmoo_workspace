;;; chess-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "chess" "chess.el" (0 0 0 0))
;;; Generated autoloads from chess.el

(autoload 'chess "chess" "\
Start a game of chess, playing against ENGINE (a module name).
With prefix argument, prompt for the engine to play against.
Otherwise use `chess-default-engine' to determine the engine.

\(fn &optional ENGINE DISABLE-POPUP ENGINE-RESPONSE-HANDLER &rest ENGINE-CTOR-ARGS)" t nil)

(defalias 'chess-session 'chess)

(define-key menu-bar-games-menu [chess] '(menu-item "Chess" chess :help "Play Chess"))

(autoload 'chess-create-display "chess" "\
Create a display, letting the user's customization decide the style.
If MODULES-TOO is non-nil, also create and associate the modules
listed in `chess-default-modules'.

\(fn PERSPECTIVE &optional MODULES-TOO)" nil nil)

(register-definition-prefixes "chess" '("chess-"))

;;;***

;;;### (autoloads nil "chess-ai" "chess-ai.el" (0 0 0 0))
;;; Generated autoloads from chess-ai.el

(register-definition-prefixes "chess-ai" '("chess-ai-"))

;;;***

;;;### (autoloads nil "chess-algebraic" "chess-algebraic.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from chess-algebraic.el

(register-definition-prefixes "chess-algebraic" '("chess-"))

;;;***

;;;### (autoloads nil "chess-announce" "chess-announce.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from chess-announce.el

(register-definition-prefixes "chess-announce" '("chess-"))

;;;***

;;;### (autoloads nil "chess-autosave" "chess-autosave.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from chess-autosave.el

(register-definition-prefixes "chess-autosave" '("chess-"))

;;;***

;;;### (autoloads nil "chess-chat" "chess-chat.el" (0 0 0 0))
;;; Generated autoloads from chess-chat.el

(register-definition-prefixes "chess-chat" '("chess-chat-"))

;;;***

;;;### (autoloads nil "chess-clock" "chess-clock.el" (0 0 0 0))
;;; Generated autoloads from chess-clock.el

(register-definition-prefixes "chess-clock" '("chess-clock-"))

;;;***

;;;### (autoloads nil "chess-common" "chess-common.el" (0 0 0 0))
;;; Generated autoloads from chess-common.el

(register-definition-prefixes "chess-common" '("chess-"))

;;;***

;;;### (autoloads nil "chess-crafty" "chess-crafty.el" (0 0 0 0))
;;; Generated autoloads from chess-crafty.el

(register-definition-prefixes "chess-crafty" '("chess-crafty-"))

;;;***

;;;### (autoloads nil "chess-database" "chess-database.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from chess-database.el

(register-definition-prefixes "chess-database" '("chess-database-"))

;;;***

;;;### (autoloads nil "chess-display" "chess-display.el" (0 0 0 0))
;;; Generated autoloads from chess-display.el

(register-definition-prefixes "chess-display" '("chess-"))

;;;***

;;;### (autoloads nil "chess-eco" "chess-eco.el" (0 0 0 0))
;;; Generated autoloads from chess-eco.el

(register-definition-prefixes "chess-eco" '("chess-"))

;;;***

;;;### (autoloads nil "chess-engine" "chess-engine.el" (0 0 0 0))
;;; Generated autoloads from chess-engine.el

(register-definition-prefixes "chess-engine" '("chess-engine-"))

;;;***

;;;### (autoloads nil "chess-epd" "chess-epd.el" (0 0 0 0))
;;; Generated autoloads from chess-epd.el

(register-definition-prefixes "chess-epd" '("chess-"))

;;;***

;;;### (autoloads nil "chess-fen" "chess-fen.el" (0 0 0 0))
;;; Generated autoloads from chess-fen.el

(register-definition-prefixes "chess-fen" '("chess-"))

;;;***

;;;### (autoloads nil "chess-file" "chess-file.el" (0 0 0 0))
;;; Generated autoloads from chess-file.el

(register-definition-prefixes "chess-file" '("chess-file-"))

;;;***

;;;### (autoloads nil "chess-fruit" "chess-fruit.el" (0 0 0 0))
;;; Generated autoloads from chess-fruit.el

(register-definition-prefixes "chess-fruit" '("chess-fruit-"))

;;;***

;;;### (autoloads nil "chess-game" "chess-game.el" (0 0 0 0))
;;; Generated autoloads from chess-game.el

(register-definition-prefixes "chess-game" '("chess-game-"))

;;;***

;;;### (autoloads nil "chess-glaurung" "chess-glaurung.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from chess-glaurung.el

(register-definition-prefixes "chess-glaurung" '("chess-glaurung-"))

;;;***

;;;### (autoloads nil "chess-gnuchess" "chess-gnuchess.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from chess-gnuchess.el

(register-definition-prefixes "chess-gnuchess" '("chess-gnuchess-"))

;;;***

;;;### (autoloads nil "chess-ics" "chess-ics.el" (0 0 0 0))
;;; Generated autoloads from chess-ics.el

(autoload 'chess-ics "chess-ics" "\
Connect to an Internet Chess Server.

\(fn SERVER PORT &optional HANDLE PASSWORD-OR-FILENAME HELPER &rest HELPER-ARGS)" t nil)

(define-key menu-bar-games-menu [chess-ics] '(menu-item "Internet Chess Servers" chess-ics :help "Play Chess on the Internet"))

(register-definition-prefixes "chess-ics" '("chess-ic"))

;;;***

;;;### (autoloads nil "chess-ics1" "chess-ics1.el" (0 0 0 0))
;;; Generated autoloads from chess-ics1.el

(register-definition-prefixes "chess-ics1" '("chess-"))

;;;***

;;;### (autoloads nil "chess-images" "chess-images.el" (0 0 0 0))
;;; Generated autoloads from chess-images.el

(register-definition-prefixes "chess-images" '("chess-images-"))

;;;***

;;;### (autoloads nil "chess-input" "chess-input.el" (0 0 0 0))
;;; Generated autoloads from chess-input.el

(register-definition-prefixes "chess-input" '("chess-"))

;;;***

;;;### (autoloads nil "chess-irc" "chess-irc.el" (0 0 0 0))
;;; Generated autoloads from chess-irc.el

(register-definition-prefixes "chess-irc" '("chess-irc-"))

;;;***

;;;### (autoloads nil "chess-kibitz" "chess-kibitz.el" (0 0 0 0))
;;; Generated autoloads from chess-kibitz.el

(register-definition-prefixes "chess-kibitz" '("chess-kibitz-"))

;;;***

;;;### (autoloads nil "chess-link" "chess-link.el" (0 0 0 0))
;;; Generated autoloads from chess-link.el

(autoload 'chess-link "chess-link" "\
Play out a game between two engines, and watch the progress.
If you want to run an engine as a bot, make the transport the first
engine, and the computer the second engine.

\(fn FIRST-ENGINE-TYPE SECOND-ENGINE-TYPE)" t nil)

(register-definition-prefixes "chess-link" '("chess-link-"))

;;;***

;;;### (autoloads nil "chess-log" "chess-log.el" (0 0 0 0))
;;; Generated autoloads from chess-log.el

(register-definition-prefixes "chess-log" '("chess-log"))

;;;***

;;;### (autoloads nil "chess-message" "chess-message.el" (0 0 0 0))
;;; Generated autoloads from chess-message.el

(register-definition-prefixes "chess-message" '("chess-"))

;;;***

;;;### (autoloads nil "chess-module" "chess-module.el" (0 0 0 0))
;;; Generated autoloads from chess-module.el

(register-definition-prefixes "chess-module" '("chess-"))

;;;***

;;;### (autoloads nil "chess-network" "chess-network.el" (0 0 0 0))
;;; Generated autoloads from chess-network.el

(register-definition-prefixes "chess-network" '("chess-network-"))

;;;***

;;;### (autoloads nil "chess-none" "chess-none.el" (0 0 0 0))
;;; Generated autoloads from chess-none.el

(register-definition-prefixes "chess-none" '("chess-none-"))

;;;***

;;;### (autoloads nil "chess-perft" "chess-perft.el" (0 0 0 0))
;;; Generated autoloads from chess-perft.el

(register-definition-prefixes "chess-perft" '("chess-perft"))

;;;***

;;;### (autoloads nil "chess-pgn" "chess-pgn.el" (0 0 0 0))
;;; Generated autoloads from chess-pgn.el

(autoload 'chess-pgn-read "chess-pgn" "\
Read and display a PGN game after point.

\(fn &optional FILE)" t nil)

(autoload 'chess-pgn-mode "chess-pgn" "\
A mode for editing chess PGN files.

\(fn)" t nil)

(defalias 'pgn-mode 'chess-pgn-mode)

(add-to-list 'auto-mode-alist '("\\.pgn\\'" . chess-pgn-mode))

(register-definition-prefixes "chess-pgn" '("chess-"))

;;;***

;;;### (autoloads nil "chess-phalanx" "chess-phalanx.el" (0 0 0 0))
;;; Generated autoloads from chess-phalanx.el

(register-definition-prefixes "chess-phalanx" '("chess-phalanx-"))

;;;***

;;;### (autoloads nil "chess-plain" "chess-plain.el" (0 0 0 0))
;;; Generated autoloads from chess-plain.el

(register-definition-prefixes "chess-plain" '("chess-plain-"))

;;;***

;;;### (autoloads nil "chess-ply" "chess-ply.el" (0 0 0 0))
;;; Generated autoloads from chess-ply.el

(register-definition-prefixes "chess-ply" '("chess-"))

;;;***

;;;### (autoloads nil "chess-polyglot" "chess-polyglot.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from chess-polyglot.el

(register-definition-prefixes "chess-polyglot" '("chess-polyglot-"))

;;;***

;;;### (autoloads nil "chess-pos" "chess-pos.el" (0 0 0 0))
;;; Generated autoloads from chess-pos.el

(register-definition-prefixes "chess-pos" '("chess-"))

;;;***

;;;### (autoloads nil "chess-puzzle" "chess-puzzle.el" (0 0 0 0))
;;; Generated autoloads from chess-puzzle.el

(autoload 'chess-puzzle "chess-puzzle" "\
Pick a random puzzle from FILE, and solve it against the default engine.
The spacebar in the display buffer is bound to `chess-puzzle-next',
making it easy to go on to the next puzzle once you've solved one.

\(fn FILE &optional INDEX)" t nil)

(register-definition-prefixes "chess-puzzle" '("chess-puzzle-"))

;;;***

;;;### (autoloads nil "chess-random" "chess-random.el" (0 0 0 0))
;;; Generated autoloads from chess-random.el

(autoload 'chess-fischer-random-position "chess-random" "\
Generate a Fischer Random style position." nil nil)

(register-definition-prefixes "chess-random" '("chess-shuffle-vector" "pieces-vector"))

;;;***

;;;### (autoloads nil "chess-scid" "chess-scid.el" (0 0 0 0))
;;; Generated autoloads from chess-scid.el

(register-definition-prefixes "chess-scid" '("chess-scid-"))

;;;***

;;;### (autoloads nil "chess-sjeng" "chess-sjeng.el" (0 0 0 0))
;;; Generated autoloads from chess-sjeng.el

(register-definition-prefixes "chess-sjeng" '("chess-sjeng-"))

;;;***

;;;### (autoloads nil "chess-sound" "chess-sound.el" (0 0 0 0))
;;; Generated autoloads from chess-sound.el

(register-definition-prefixes "chess-sound" '("chess-sound"))

;;;***

;;;### (autoloads nil "chess-stockfish" "chess-stockfish.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from chess-stockfish.el

(register-definition-prefixes "chess-stockfish" '("chess-stockfish-"))

;;;***

;;;### (autoloads nil "chess-transport" "chess-transport.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from chess-transport.el

(register-definition-prefixes "chess-transport" '("chess-transport-"))

;;;***

;;;### (autoloads nil "chess-tutorial" "chess-tutorial.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from chess-tutorial.el

(autoload 'chess-tutorial "chess-tutorial" "\
A simple chess training display." t nil)

(register-definition-prefixes "chess-tutorial" '("chess-tutorial-knight-1"))

;;;***

;;;### (autoloads nil "chess-ucb" "chess-ucb.el" (0 0 0 0))
;;; Generated autoloads from chess-ucb.el

(register-definition-prefixes "chess-ucb" '("chess-ucb-"))

;;;***

;;;### (autoloads nil "chess-uci" "chess-uci.el" (0 0 0 0))
;;; Generated autoloads from chess-uci.el

(register-definition-prefixes "chess-uci" '("chess-uci-"))

;;;***

;;;### (autoloads nil "chess-var" "chess-var.el" (0 0 0 0))
;;; Generated autoloads from chess-var.el

(register-definition-prefixes "chess-var" '("chess-var-"))

;;;***

;;;### (autoloads nil nil ("chess-german.el" "chess-pkg.el") (0 0
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; chess-autoloads.el ends here
