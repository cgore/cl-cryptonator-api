(defpackage :cryptonator/rest-api
  (:use :common-lisp))

(in-package :cryptonator/rest-api)

(defvar *api* "https://api.cryptonator.com/api")

(defvar *api-cache-timeout-seconds* 60)

(defun canonized-currency-code (code)
  (cond ((null code)
         "usd")
        ((symbolp code)
         (string-downcase (symbol-name code)))
        ((stringp code)
         (string-downcase code))))

(defun http-get (url-components)
  "Make an HTTP GET call to the Zapper.fi API."
  (dex:get (quri:make-uri :defaults (cond ((stringp url-components)
                                           (concatenate 'string *api* url-components))
                                          ((listp url-components)
                                           (apply 'concatenate 'string *api* url-components))))))

(function-cache:defcached
    (http-get-cached :timeout *api-cache-timeout-seconds*)
    (url-components)
  (http-get url-components))

(defun http-get-json (url-components)
  "Make an HTTP GET call to the Zapper.fi API, expecting JSON back, and parse."
  (let ((yason:*parse-json-arrays-as-vectors*   t)
        (yason:*parse-json-booleans-as-symbols* t))
    (yason:parse (http-get url-components))))

(function-cache:defcached
    (http-get-json-cached :timeout *api-cache-timeout-seconds*)
    (url-components &key (query-args '()) (final-hook nil))
  (let ((result (if query-args
                    (http-get-json url-components)
                    (http-get-json url-components))))
    (when final-hook (funcall final-hook result))
    result))

(defun get-currencies ()
  "Returns a list of all supported currencies."
  (let ((*api-cache-timeout-seconds* (* 60 60 24))) ; I assume this updates rarely.
    (http-get-json-cached "/currencies")))

(defun get-ticker (base &optional (target :usd))
  (http-get-json-cached `("/ticker/"
                          ,(canonized-currency-code base)
                          "-"
                          ,(canonized-currency-code target))))

(defun get-full (base &optional (target :usd))
  (http-get-json-cached `("/full/"
                          ,(canonized-currency-code base)
                          "-"
                          ,(canonized-currency-code target))))
