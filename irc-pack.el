;;; irc-pack.el --- IRC setup

;;; Commentary:

;;; Code:

(require 'erc)
(require 'erc-services)    ; for passwords
(require 'dash)
(require 'creds)

;; default setup

(defconst irc-pack-default-port 6667
  "Default port connection.")

(defconst irc-pack-default-server "localhost"
  "Default server.")

(defconst irc-pack-default-connection-type nil
  "Default connection type.")

;; user customization

(defcustom irc-pack-credentials-file "~/.authinfo.gpg"
  "Default credentials file.
This could be a plain authinfo file too.")

(defcustom irc-pack-login    nil
  "User's login.")

(defcustom irc-pack-password nil
  "User's credentials.")

(defcustom irc-pack-fullname nil
  "User's fullname.")

(defcustom irc-pack-server "localhost" ;; "irc.freenode.net"
  "IRC server default.")

(defcustom irc-pack-port 6667
  "IRC server port default.")

(defcustom irc-pack-connection-type nil
  "IRC connection type, default to unsecure.")

;; ===================== setup functions

(defun irc-pack--log (&rest args)
  "Log the message ARGS in the mini-buffer."
  (apply #'message (format "IRC Pack - %s" (car args)) (cdr args)))

(defun irc-pack-setup-possible-p (creds-file)
  "Check if the setup is possible.
Check the existence of the CREDS-FILE and that the entry 'irc' exists.
If it does return such entry, nil otherwise."
  (let ((parsed-file (creds/read-lines creds-file)))
    (-when-let (irc-creds (and parsed-file ;; nil if the file does not exist
                               (creds/get parsed-file "irc")))
      irc-creds)))

(defun irc-pack-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer."
  (interactive)
  (-if-let (buffer (car (erc-buffer-list)))
      (switch-to-buffer buffer)
    (let ((conn-fn (if (null irc-pack-connection-type) 'erc 'erc-tls)))
      (funcall conn-fn :server irc-pack-server
               :port irc-pack-port
               :nick irc-pack-login
               :password irc-pack-password
               :full-name irc-pack-fullname))))

(defun irc-pack-setup (irc-creds)
  "Execute the setup from the IRC-CREDS."
  (let ((login           (creds/get-entry irc-creds "login"))
        (password        (creds/get-entry irc-creds "password"))
        (fullname        (creds/get-entry irc-creds "fullname"))
        (irc-server      (creds/get-entry irc-creds "server"))
        (irc-server-port (creds/get-entry irc-creds "port"))
        (irc-connection-type (creds/get-entry irc-creds "connection-type")))
    (irc-pack--log "Running irc-pack setup...")
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
     ;; erc-autojoin-channels-alist
     ;; erc-nickserv-passwords `((freenode ((,login . ,password))))
     ;; keep the credentials
     irc-pack-login login
     irc-pack-password (or password "")
     irc-pack-fullname fullname
     irc-pack-server (or irc-server irc-pack-default-server)
     irc-pack-port (or irc-server-port irc-pack-default-port)
     irc-pack-connection-type (or irc-connection-type irc-pack-default-connection-type))
    ;; add keybindings
    (irc-pack--log "Setup done!")))

;; ===================== setup routine

(defun irc-pack-load-pack ()
  "(Re)load the irc-pack."
  (interactive)
  (-if-let (irc-creds (irc-pack-setup-possible-p irc-pack-credentials-file))
      (irc-pack-setup irc-creds)
    (irc-pack--log "You need to setup the configuration file '%s' with the following content:
machine irc server <irc-server> port <irc-server-port> login <your-login> password <your-password> fullname <your-full-name-in-quote-when-spaces-inside>" irc-pack-credentials-file)))

(defvar irc-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i l") 'irc-pack-load-pack)
    (define-key map (kbd "C-c i c") 'irc-pack-erc-start-or-switch)
    (define-key map (kbd "C-c i d") 'erc-quit-server)
    (define-key map (kbd "C-c i L") (lambda () (interactive) (irc-pack--log "irc-buffers: %s" (erc-buffer-list))))
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
