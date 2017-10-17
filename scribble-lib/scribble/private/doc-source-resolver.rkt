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
    get-pkg-details-from-catalogs
    current-pkg-catalogs)
  net/url)

;; -----------------------------------------------------------------------------

(define (doc-source-resolver/catalogs path-str)
  (define-values [pkg-name subpath] (path->pkg+subpath path-str))
  (let* ([catalog-url (get-pkg-source-from-catalogs pkg-name)]
         ;; TODO what if catalog-url is #false
         [catalog-url (url-update-scheme catalog-url "https")]
         [catalog-url (url-extend-query-path catalog-url subpath)])
    (url->string catalog-url)))

(define get-pkg-source-from-catalogs
  (let ([url* (list (string->url "https://pkgs.racket-lang.org") (string->url "https://planet-compats.racket-lang.org"))])
    (λ (pkg-name)
      (define details
        (parameterize ([current-pkg-catalogs url*])
          (get-pkg-details-from-catalogs pkg-name)))
      (define source (hash-ref details 'source #f))
      (and source (string->url source)))))

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

(define (url-extend-query-path u subpath)
  (define old-query (url-query u))
  (define old-path (assoc 'path old-query))
  (define new-query
    (if old-path
      (for/list ([q (in-list old-query)])
        (if (eq? 'path (car q))
          `(path . ,(path->string (build-path (or (cdr old-path) ".") subpath)))
          q))
      (cons `(path . ,(path->string subpath)) old-query)))
  (make-url
    (url-scheme u)
    (url-user u)
    (url-host u)
    (url-port u)
    (url-path-absolute? u)
    (url-path u)
    new-query
    (url-fragment u)))

