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

;; Personal Collections Tracker - Manages user's curated collections
(define-map personal-collections
  { curator: principal, asset-id: uint }  ;; User and asset compound key
  {
    addition-block: uint,                ;; When asset was added to collection
    last-interaction: uint               ;; Latest user interaction timestamp
  }
)

;; ========== ERROR CODE DEFINITIONS ==========
(define-constant ERROR_ASSET_NONEXISTENT (err u401)) ;; Asset with specified ID does not exist
(define-constant ERROR_ASSET_DUPLICATE (err u402)) ;; Attempt to create asset that already exists
(define-constant ERROR_LABEL_INVALID (err u403)) ;; Asset label does not meet requirements
(define-constant ERROR_VOLUME_INVALID (err u404)) ;; Asset volume outside acceptable range
(define-constant ERROR_PERMISSION_DENIED (err u405)) ;; Action requires higher permission level
(define-constant ERROR_COLLECTION_INVALID (err u406)) ;; Collection name does not meet requirements
(define-constant ERROR_OPERATION_BLOCKED (err u407)) ;; Operation cannot be performed
(define-constant ERROR_VIEWING_RESTRICTED (err u408)) ;; User lacks viewing permissions
(define-constant ERROR_SHARE_INVALID (err u409)) ;; Invalid sharing attempt
(define-constant ERROR_TARGET_INVALID (err u410)) ;; Invalid principal target
(define-constant ERROR_COLLECTION_DUPLICATE (err u411)) ;; Item already in collection
(define-constant ERROR_COLLECTION_MISSING (err u412)) ;; Item not in collection

;; ========== PERMISSION CONFIGURATIONS ==========
(define-constant REGISTRY_SUPERVISOR tx-sender) ;; Initial administrator of the registry


;; ========== UTILITY FUNCTIONS ==========

;; Verifies asset existence in registry
(define-private (asset-exists? (asset-id uint))
  (is-some (map-get? asset-registry { asset-id: asset-id }))
)

;; Validates ownership rights for an asset
(define-private (is-owner? (asset-id uint) (requester principal))
  (match (map-get? asset-registry { asset-id: asset-id })
    registry-entry (is-eq (get registrant registry-entry) requester)
    false
  )
)

;; Validates principal address
(define-private (is-valid-principal (target principal))
  (not (is-eq target 'ST000000000000000000002AMW42H))
)

;; Checks if viewer has permission to access asset
(define-private (has-viewing-permission? (asset-id uint) (viewer principal))
  (match (map-get? viewing-permissions { asset-id: asset-id, viewer: viewer })
    permission-data (get permitted permission-data)
    false
  )
)

;; Checks if asset is in user's personal collection
(define-private (in-collection? (asset-id uint) (curator principal))
  (is-some (map-get? personal-collections { curator: curator, asset-id: asset-id }))
)

;; Retrieves an asset's digital volume
(define-private (get-asset-volume (asset-id uint))
  (default-to u0 
    (get volume 
      (map-get? asset-registry { asset-id: asset-id })
    )
  )
)

;; ========== ATTRIBUTE VALIDATION FUNCTIONS ==========

;; Validates a single attribute name
(define-private (validate-attribute (attribute (string-ascii 32)))
  (and 
    (> (len attribute) u0)
    (< (len attribute) u33)
  )
)

;; Validates a complete set of attributes
(define-private (validate-attribute-set (attributes (list 10 (string-ascii 32))))
  (and
    (> (len attributes) u0)
    (<= (len attributes) u10)
    (is-eq (len (filter validate-attribute attributes)) (len attributes))
  )
)

;; ========== PUBLIC FUNCTIONS ==========

;; Register a new digital asset
(define-public (register-asset (label (string-ascii 64)) (volume uint) (collection (string-ascii 32)) (description (string-ascii 128)) (attributes (list 10 (string-ascii 32))))
  (let
    (
      (new-asset-id (+ (var-get asset-counter) u1))
    )
    ;; Validate input parameters
    (asserts! (> (len label) u0) ERROR_LABEL_INVALID)
    (asserts! (< (len label) u65) ERROR_LABEL_INVALID)
    (asserts! (> volume u0) ERROR_VOLUME_INVALID)
    (asserts! (< volume u1000000000) ERROR_VOLUME_INVALID)
    (asserts! (> (len collection) u0) ERROR_COLLECTION_INVALID)
    (asserts! (< (len collection) u33) ERROR_COLLECTION_INVALID)
    (asserts! (> (len description) u0) ERROR_LABEL_INVALID)
    (asserts! (< (len description) u129) ERROR_LABEL_INVALID)
    (asserts! (validate-attribute-set attributes) ERROR_LABEL_INVALID)

    ;; Register the new asset in the system
    (map-insert asset-registry
      { asset-id: new-asset-id }
      {
        label: label,
        registrant: tx-sender,
        volume: volume,
        registration-block: block-height,
        collection: collection,
        description: description,
        attributes: attributes
      }
    )

    ;; Grant viewing rights to registrant automatically
    (map-insert viewing-permissions
      { asset-id: new-asset-id, viewer: tx-sender }
      { 
        permitted: true,
        grantor: tx-sender,
        timestamp: block-height
      }
    )

    ;; Update global asset counter
    (var-set asset-counter new-asset-id)
    (ok new-asset-id)
  )
)

;; Add asset to personal collection
(define-public (add-to-collection (asset-id uint))
  (let
    (
      (registry-entry (unwrap! (map-get? asset-registry { asset-id: asset-id }) ERROR_ASSET_NONEXISTENT))
    )
    ;; Validate request
    (asserts! (asset-exists? asset-id) ERROR_ASSET_NONEXISTENT)
    (asserts! (has-viewing-permission? asset-id tx-sender) ERROR_VIEWING_RESTRICTED)
    (asserts! (not (in-collection? asset-id tx-sender)) ERROR_COLLECTION_DUPLICATE)

    ;; Add to personal collection
    (map-insert personal-collections
      { curator: tx-sender, asset-id: asset-id }
      {
        addition-block: block-height,
        last-interaction: block-height
      }
    )
    (ok true)
  )
)

;; Remove asset from personal collection
(define-public (remove-from-collection (asset-id uint))
  (let
    (
      (registry-entry (unwrap! (map-get? asset-registry { asset-id: asset-id }) ERROR_ASSET_NONEXISTENT))
    )
    ;; Validate asset existence and collection status
    (asserts! (asset-exists? asset-id) ERROR_ASSET_NONEXISTENT)
    (asserts! (in-collection? asset-id tx-sender) ERROR_COLLECTION_MISSING)

    ;; Remove from personal collection
    (map-delete personal-collections { curator: tx-sender, asset-id: asset-id })
    (ok true)
  )
)

;; Check if asset is in personal collection
(define-read-only (check-collection-status (asset-id uint))
  (ok (in-collection? asset-id tx-sender))
)

;; Grant viewing permission for a specific asset
(define-public (grant-viewing-permission (asset-id uint) (viewer principal))
  (let
    (
      (registry-entry (unwrap! (map-get? asset-registry { asset-id: asset-id }) ERROR_ASSET_NONEXISTENT))
    )
    ;; Validate request
    (asserts! (asset-exists? asset-id) ERROR_ASSET_NONEXISTENT)
    (asserts! (is-owner? asset-id tx-sender) ERROR_PERMISSION_DENIED)
    (asserts! (not (is-eq viewer tx-sender)) ERROR_SHARE_INVALID)

    ;; Set viewing permission
    (map-set viewing-permissions
      { asset-id: asset-id, viewer: viewer }
      { 
        permitted: true,
        grantor: tx-sender,
        timestamp: block-height
      }
    )
    (ok true)
  )
)

;; Revoke viewing permission for a specific asset
(define-public (revoke-viewing-permission (asset-id uint) (viewer principal))
  (let
    (
      (registry-entry (unwrap! (map-get? asset-registry { asset-id: asset-id }) ERROR_ASSET_NONEXISTENT))
      (permission-data (unwrap! (map-get? viewing-permissions { asset-id: asset-id, viewer: viewer }) ERROR_PERMISSION_DENIED))
    )
    ;; Validate request
    (asserts! (asset-exists? asset-id) ERROR_ASSET_NONEXISTENT)
    (asserts! (is-owner? asset-id tx-sender) ERROR_PERMISSION_DENIED)
    (asserts! (not (is-eq viewer tx-sender)) ERROR_SHARE_INVALID)

    ;; Revoke viewing permission
    (map-delete viewing-permissions { asset-id: asset-id, viewer: viewer })

    ;; Also remove from collection if present
    (if (in-collection? asset-id viewer)
      (map-delete personal-collections { curator: viewer, asset-id: asset-id })
      true
    )
    (ok true)
  )
)

;; Check if a user has viewing permission for an asset
(define-read-only (check-viewing-permission (asset-id uint) (viewer principal))
  (ok (has-viewing-permission? asset-id viewer))
)

;; Transfer asset ownership
(define-public (transfer-asset-ownership (asset-id uint) (new-owner principal))
  (let
    (
      (registry-details (unwrap! (map-get? asset-registry { asset-id: asset-id }) ERROR_ASSET_NONEXISTENT))
    )
    ;; Validate transfer request
    (asserts! (asset-exists? asset-id) ERROR_ASSET_NONEXISTENT)
    (asserts! (is-owner? asset-id tx-sender) ERROR_PERMISSION_DENIED)
    (asserts! (not (is-eq new-owner tx-sender)) ERROR_SHARE_INVALID)
    (asserts! (is-valid-principal new-owner) ERROR_TARGET_INVALID)

    ;; Update ownership in registry
    (map-set asset-registry
      { asset-id: asset-id }
      (merge registry-details { registrant: new-owner })
    )

    ;; Transfer viewing permissions to new owner
    (map-set viewing-permissions
      { asset-id: asset-id, viewer: new-owner }
      {
        permitted: true,
        grantor: tx-sender,
        timestamp: block-height
      }
    )
    (ok true)
  )
)

;; Update asset details
(define-public (update-asset (asset-id uint) (new-label (string-ascii 64)) (new-volume uint) (new-collection (string-ascii 32)) (new-description (string-ascii 128)) (new-attributes (list 10 (string-ascii 32))))
  (let
    (
      (registry-info (unwrap! (map-get? asset-registry { asset-id: asset-id }) ERROR_ASSET_NONEXISTENT))
    )
    ;; Validate update request
    (asserts! (asset-exists? asset-id) ERROR_ASSET_NONEXISTENT)
    (asserts! (is-eq (get registrant registry-info) tx-sender) ERROR_PERMISSION_DENIED)
    (asserts! (> (len new-label) u0) ERROR_LABEL_INVALID)
    (asserts! (< (len new-label) u65) ERROR_LABEL_INVALID)
    (asserts! (> new-volume u0) ERROR_VOLUME_INVALID)
    (asserts! (< new-volume u1000000000) ERROR_VOLUME_INVALID)
    (asserts! (> (len new-collection) u0) ERROR_COLLECTION_INVALID)
    (asserts! (< (len new-collection) u33) ERROR_COLLECTION_INVALID)
    (asserts! (> (len new-description) u0) ERROR_LABEL_INVALID)
    (asserts! (< (len new-description) u129) ERROR_LABEL_INVALID)
    (asserts! (validate-attribute-set new-attributes) ERROR_LABEL_INVALID)

    ;; Update asset record
    (map-set asset-registry
      { asset-id: asset-id }
      (merge registry-info { 
        label: new-label, 
        volume: new-volume, 
        collection: new-collection, 
        description: new-description, 
        attributes: new-attributes 
      })
    )
    (ok true)
  )
)

;; Remove asset from registry
(define-public (delete-asset (asset-id uint))
  (let
    (
      (registry-entry (unwrap! (map-get? asset-registry { asset-id: asset-id }) ERROR_ASSET_NONEXISTENT))
    )
    ;; Validate removal request
    (asserts! (asset-exists? asset-id) ERROR_ASSET_NONEXISTENT)
    (asserts! (is-eq (get registrant registry-entry) tx-sender) ERROR_PERMISSION_DENIED)

    ;; Remove asset from registry
    (map-delete asset-registry { asset-id: asset-id })
    (ok true)
  )
)

