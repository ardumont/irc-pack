;;; irc-pack.el --- IRC setup

;;; Commentary:

;;; Code:

(require 'install-packages-pack)
(install-packages-pack/install-packs '(dash))

(require 'netrc)
(require 'erc)
(require 'erc-services)    ; for passwords
(require 'dash)

(defvar *IRC-PACK-CREDENTIALS-FILE* "~/.authinfo.gpg"
  "Default credentials file.
This could be a plain authinfo file too.")

(defvar irc-pack/login    nil
  "User's login.")

(defvar irc-pack/password nil
  "User's credentials.")

(defvar irc-pack/fullname nil
  "User's fullname.")

(defconst irc-pack/server "irc.freenode.net"
  "IRC server.")

(defconst irc-pack/port  6667
  "IRC server port.")

;; ===================== setup functions

(defun irc-pack/server-uri (server port)
  "Compute the irc SERVER uri from the SERVER and the PORT."
  (format "%s:%s" server port))

(defun irc-pack/log (&rest args)
  "Log the message ARGS in the mini-buffer."
  (apply #'message (format "IRC Pack - %s" (car args)) (cdr args)))

(defun irc-pack/setup-possible-p (creds-file)
  "Check if the setup is possible.
Check the existence of the CREDS-FILE and that the entry 'irc' exists.
If it does return such entry, nil otherwise."
  (let ((parsed-file (netrc-parse creds-file)))
    (-when-let (irc-creds (and parsed-file ;; nil if the file does not exist
                               (netrc-machine parsed-file "irc")))
      irc-creds)))

(defun irc-pack/erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer (irc-pack/server-uri irc-pack/server irc-pack/port)) ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (erc :server irc-pack/server :port irc-pack/port :nick irc-pack/login :full-name irc-pack/fullname)))

(defun irc-pack/setup (irc-creds)
  "Execute the setup from the IRC-CREDS."
  (let* ((login    (netrc-get irc-creds "login"))
         (password (netrc-get irc-creds "password"))
         (fullname (netrc-get irc-creds "fullname")))
    (irc-pack/log "Running irc-pack setup...")
    ;; activate modes
    (erc-services-mode 1)
    ;; check channels
    (erc-track-mode t)
    ;; some specific setup
    (setq ;; erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
     ;;                           "324" "329" "332" "333" "353" "477")
     ;; don't show any of this
     ;; erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
     erc-prompt-for-nickserv-password nil
     erc-nickserv-passwords `((freenode ((,login . ,password))))
     ;; keep the credentials
     irc-pack/login login
     irc-pack/password password
     irc-pack/fullname fullname)
    ;; add keybindings
    (irc-pack/log "Setup done!")))

;; ===================== setup routine

(defun irc-pack/load-pack! ()
  "(Re)load the irc-pack."
  (interactive)
  (-if-let (irc-creds (irc-pack/setup-possible-p *IRC-PACK-CREDENTIALS-FILE*))
      (irc-pack/setup irc-creds)
    (irc-pack/log (concat "You need to setup the credentials file " *IRC-PACK-CREDENTIALS-FILE* " for this to work.\n"
                          "Here is the needed content to setup to your need into '" *IRC-PACK-CREDENTIALS-FILE* "':\n"
                          "machine irc login <your-login> password <your-password> fullname <your-full-name-in-quote-if-need-be>"))))

(defvar irc-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i l") 'irc-pack/load-pack!)
    (define-key map (kbd "C-c i c") 'irc-pack/erc-start-or-switch)
    (define-key map (kbd "C-c i d") 'erc-quit-server)
    map)
  "Keymap for git-pack mode.")

(define-minor-mode irc-pack-mode
  "Minor mode to consolidate irc-pack extensions.

\\{irc-pack-mode-map}"
  :lighter " IP"
  :keymap irc-pack-mode-map)

(define-globalized-minor-mode global-irc-pack-mode irc-pack-mode irc-pack-on)

(defun irc-pack-on ()
  "Turn on `irc-pack-mode'."
  (irc-pack-mode +1))

(global-irc-pack-mode)

(provide 'irc-pack)
;;; irc-pack.el ends here
