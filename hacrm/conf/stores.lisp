(in-package :hacrm)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.

(defvar *default-db-path*
  #P "~/.hacrm/db/"
  "This path is used for production data.")

(defvar *dev-db-path*
  #P"~/.hacrm/dev-db/"
  "This path is used for development"
)


(weblocks-stores:defstore *hacrm-store* :prevalence *default-db-path*)

