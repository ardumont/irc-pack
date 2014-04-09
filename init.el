;;; irc-pack.el --- IRC setup

;;; Commentary:

;;; Code:

(require 'erc)
(require 'erc-services)    ; for passwords

(setq *IRC-PACK-CREDENTIALS-FILE* "~/.netrc")

;; ===================== setup functions

(defvar irc-pack/login    "")
(defvar irc-pack/password "")
(defvar irc-pack/fullname "")

(defun irc-pack/log (str) "A log function for the pack."
  (message "irc-pack - %s" str))

(defun irc-pack/setup-possible-p (creds-file)
  "Check if the setup is possible by checking the existence of the file and that the entry 'jabber' exists."
  (let ((parsed-file (netrc-parse creds-file)))
    (and parsed-file ;; nil if the file does not exist
         (netrc-machine parsed-file "irc"))))

(defun irc-pack/setup (creds-file)
  ;;(require 'init-gpg)      ; my personal config that loads some passwords
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)

  ;; (if (boundp 'irc-freenode-nick-passwd)  ; from my personal pass.el.gpg
  ;;     (setq erc-nickserv-passwords
  ;;           `((freenode (("ardumont"     . ,irc-freenode-nick-passwd)
  ;;                        ;;("nick-2"   . ,irc-freenode-nick-passwd)
  ;;                        ))
  ;;             ;;(DALnet       (("nickname" . ,dalnet-pass)))
  ;;             )))

  ;; check channels
  (erc-track-mode t)
  ;; (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
  ;;                                 "324" "329" "332" "333" "353" "477"))
  ;; don't show any of this
  ;; (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

  ;; load the irc entry in the ~/.netrc, we obtain a hash-map with the needed data
  (setq cred (netrc-machine (netrc-parse creds-file) "irc" t))

  (setq irc-pack/login    (netrc-get cred "login"))
  (setq irc-pack/password (netrc-get cred "password"))
  (setq irc-pack/fullname (netrc-get cred "fullname"))

  (setq erc-nickserv-passwords `((freenode ((,irc-pack/login ,irc-pack/password))))))

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (progn
      (erc :server "irc.freenode.net" :port 6667 :nick irc-pack/login :full-name irc-pack/fullname))))

(global-set-key (kbd "C-c c i") 'erc-start-or-switch)

;; ===================== setup routine

(if (irc-pack/setup-possible-p *IRC-PACK-CREDENTIALS-FILE*)
    (progn (irc-pack/log (concat *IRC-PACK-CREDENTIALS-FILE* " found! Running setup..."))
           (irc-pack/setup *IRC-PACK-CREDENTIALS-FILE*)
           (irc-pack/log "Setup done!"))
  (irc-pack/log (concat "You need to setup the credentials file " *IRC-PACK-CREDENTIALS-FILE* " for this to work.\n"
                        "Here is the needed content to setup to your need into '" *IRC-PACK-CREDENTIALS-FILE* "':\n"
                        "machine irc login <your-login> password <your-password> fullname <your-full-name-in-quote-if-need-be>")))
