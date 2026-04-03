;; ;; https://stackoverflow.com/questions/10787087/use-elpa-emacs-behind-a-proxy-requiring-authentication
;; (setq url-proxy-services
;;       '(("no_proxy" . "127.0.0.1,*.local,otherhost...")
;;         ("http" . "proxy.com:8080")
;;         ("https" . "proxy.com:8080")))

;; (setq url-http-proxy-basic-auth-storage
;;       (list (list "proxy.com:8080"
;;                   (cons "Input your LDAP UID !"
;;                         (base64-encode-string "LOGIN:PASSWORD")))))
