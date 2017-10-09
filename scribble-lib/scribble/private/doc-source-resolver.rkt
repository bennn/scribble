#lang racket/base

;; For converting a module path to a URL for the module's source.
;; TODO more docs

(require racket/contract)
(provide
  (contract-out
   [doc-source-resolver/catalogs
    (-> path-string? string?)]))

(require
  (only-in pkg/path
    path->pkg+subpath)
  (only-in pkg/lib
    get-pkg-details-from-catalogs)
  net/url)

;; -----------------------------------------------------------------------------

(define (doc-source-resolver/catalogs path-str)
  (define-values [pkg-name subpath] (path->pkg+subpath path-str))
  ;; TODO hard to get right path, try 'with-cache' vs. 'redex'
  (define subpath-str (path->string (build-path pkg-name subpath)))
  (let* ([catalog-url (get-pkg-source-from-catalogs pkg-name)]
         ;; TODO what if catalog-url is #false
         [catalog-url (url-update-scheme catalog-url "https")]
         [catalog-url (url-update-query catalog-url `((path . ,subpath-str)))])
(printf "url ~a~n" (url->string catalog-url))
    (url->string catalog-url)))

(define (get-pkg-source-from-catalogs pkg-name)
  (define details (get-pkg-details-from-catalogs pkg-name))
  (define source (hash-ref details 'source #f))
  (and source (string->url source)))

(define (url-update-scheme u new-scheme)
  (make-url
    new-scheme
    (url-user u)
    (url-host u)
    (url-port u)
    (url-path-absolute? u)
    (url-path u)
    (url-query u)
    (url-fragment u)))

(define (url-update-query u new-query)
  (make-url
    (url-scheme u)
    (url-user u)
    (url-host u)
    (url-port u)
    (url-path-absolute? u)
    (url-path u)
    new-query
    (url-fragment u)))

