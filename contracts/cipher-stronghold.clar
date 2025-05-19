;; Cipher Stronghold

;; ========== STATE VARIABLES AND DATA STRUCTURES ==========
(define-data-var asset-counter uint u0) ;; Tracks total registered assets

;; Core Registry Database - Maps asset identifiers to complete asset information
(define-map asset-registry
  { asset-id: uint } ;; Primary key is a unique asset identifier
  {
    label: (string-ascii 64),           ;; Human-readable asset name
    registrant: principal,              ;; Principal address of asset registrant
    volume: uint,                       ;; Digital representation of asset size
    registration-block: uint,           ;; Block height of registration
    collection: (string-ascii 32),      ;; Organizational collection name
    description: (string-ascii 128),    ;; Detailed asset description
    attributes: (list 10 (string-ascii 32)) ;; Associated metadata attributes
  }
)

;; Permission Control System - Defines who can view specific assets
(define-map viewing-permissions
  { asset-id: uint, viewer: principal } ;; Compound key of asset and user
  { 
    permitted: bool,                    ;; Access status flag
    grantor: principal,                 ;; Who granted the permission
    timestamp: uint                     ;; When permission was set
  }
)
