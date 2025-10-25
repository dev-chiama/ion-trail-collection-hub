;; ion-trail-collection-hub

;; ========================================
;; ERROR CODE DEFINITIONS
;; ========================================

(define-constant ERR_ITEM_NONEXISTENT (err u401))
(define-constant ERR_ITEM_ALREADY_EXISTS (err u402)) 
(define-constant ERR_INVALID_ITEM_ID (err u403))
(define-constant ERR_MASS_BOUNDARY_VIOLATION (err u404))
(define-constant ERR_UNAUTHORIZED_ACCESS (err u405))
(define-constant ERR_INVALID_LOCATION_INFO (err u406))
(define-constant ERR_FORBIDDEN_OPERATION (err u407))
(define-constant ERR_VIEW_PERMISSION_DENIED (err u408))
(define-constant ERR_INVALID_TOKEN (err u409))
(define-constant ERR_PRINCIPAL_INVALID (err u410))
(define-constant ERR_BOOKMARK_ALREADY_SET (err u411))
(define-constant ERR_BOOKMARK_NOT_FOUND (err u412))

;; ========================================
;; DATA STORAGE MAPS
;; ========================================

(define-map collection-items
  { item-id: uint }
  {
    item-name: (string-ascii 64),
    current-custodian: principal,
    mass-grams: uint,
    block-registered: uint,
    source-location: (string-ascii 32),
    description: (string-ascii 128),
    classification-labels: (list 10 (string-ascii 32))
  }
)

(define-map custodian-bookmarks
  { bookmark-owner: principal, bookmarked-item: uint }
  {
    bookmark-timestamp: uint,
    last-updated: uint
  }
)

(define-map access-permissions
  { item-id: uint, permitted-user: principal }
  {
    has-access: bool,
    granted-by: principal,
    granted-at-block: uint
  }
)


;; ========================================
;; GLOBAL STATE TRACKING
;; ========================================

(define-data-var total-items-registered uint u0)

;; ========================================
;; CONTRACT AUTHORITY CONSTANTS
;; ========================================

(define-constant CONTRACT_OWNER tx-sender)

;; ========================================
;; CORE ITEM REGISTRATION
;; ========================================

(define-public (register-new-item (name (string-ascii 64)) (mass uint) (location (string-ascii 32)) 
                                   (desc (string-ascii 128)) (labels (list 10 (string-ascii 32))))
  (let
    (
      (new-id (+ (var-get total-items-registered) u1))
    )
    (asserts! (> (len name) u0) ERR_INVALID_ITEM_ID)
    (asserts! (< (len name) u65) ERR_INVALID_ITEM_ID)
    (asserts! (> mass u0) ERR_MASS_BOUNDARY_VIOLATION)
    (asserts! (< mass u1000000000) ERR_MASS_BOUNDARY_VIOLATION)
    (asserts! (> (len location) u0) ERR_INVALID_LOCATION_INFO)
    (asserts! (< (len location) u33) ERR_INVALID_LOCATION_INFO)
    (asserts! (> (len desc) u0) ERR_INVALID_ITEM_ID)
    (asserts! (< (len desc) u129) ERR_INVALID_ITEM_ID)
    (asserts! (verify-labels-valid labels) ERR_INVALID_ITEM_ID)

    (map-insert collection-items
      { item-id: new-id }
      {
        item-name: name,
        current-custodian: tx-sender,
        mass-grams: mass,
        block-registered: block-height,
        source-location: location,
        description: desc,
        classification-labels: labels
      }
    )

    (map-insert access-permissions
      { item-id: new-id, permitted-user: tx-sender }
      { 
        has-access: true,
        granted-by: tx-sender,
        granted-at-block: block-height
      }
    )

    (var-set total-items-registered new-id)
    (ok new-id)
  )
)

;; ========================================
;; ITEM METADATA UPDATES
;; ========================================

(define-public (update-item-metadata (id uint) (new-name (string-ascii 64)) (new-mass uint) 
                                      (new-location (string-ascii 32)) (new-desc (string-ascii 128)) 
                                      (new-labels (list 10 (string-ascii 32))))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (is-eq (get current-custodian item-data) tx-sender) ERR_UNAUTHORIZED_ACCESS)
    (asserts! (> (len new-name) u0) ERR_INVALID_ITEM_ID)
    (asserts! (< (len new-name) u65) ERR_INVALID_ITEM_ID)
    (asserts! (> new-mass u0) ERR_MASS_BOUNDARY_VIOLATION)
    (asserts! (< new-mass u1000000000) ERR_MASS_BOUNDARY_VIOLATION)
    (asserts! (> (len new-location) u0) ERR_INVALID_LOCATION_INFO)
    (asserts! (< (len new-location) u33) ERR_INVALID_LOCATION_INFO)
    (asserts! (> (len new-desc) u0) ERR_INVALID_ITEM_ID)
    (asserts! (< (len new-desc) u129) ERR_INVALID_ITEM_ID)
    (asserts! (verify-labels-valid new-labels) ERR_INVALID_ITEM_ID)

    (map-set collection-items
      { item-id: id }
      (merge item-data { 
        item-name: new-name, 
        mass-grams: new-mass, 
        source-location: new-location, 
        description: new-desc, 
        classification-labels: new-labels 
      })
    )
    (ok true)
  )
)

;; ========================================
;; ITEM DELETION
;; ========================================

(define-public (delete-item-from-registry (id uint))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (is-eq (get current-custodian item-data) tx-sender) ERR_UNAUTHORIZED_ACCESS)

    (map-delete collection-items { item-id: id })
    (ok true)
  )
)

;; ========================================
;; BOOKMARK SYSTEM FUNCTIONS
;; ========================================

(define-public (add-item-to-bookmarks (id uint))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (can-user-view id tx-sender) ERR_VIEW_PERMISSION_DENIED)
    (asserts! (not (is-item-bookmarked id tx-sender)) ERR_BOOKMARK_ALREADY_SET)

    (map-insert custodian-bookmarks
      { bookmark-owner: tx-sender, bookmarked-item: id }
      {
        bookmark-timestamp: block-height,
        last-updated: block-height
      }
    )
    (ok true)
  )
)

(define-public (remove-item-from-bookmarks (id uint))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (is-item-bookmarked id tx-sender) ERR_BOOKMARK_NOT_FOUND)

    (map-delete custodian-bookmarks { bookmark-owner: tx-sender, bookmarked-item: id })
    (ok true)
  )
)

(define-read-only (check-bookmark-status (id uint))
  (ok (is-item-bookmarked id tx-sender))
)

;; ========================================
;; ACCESS PERMISSION MANAGEMENT
;; ========================================

(define-public (grant-view-permission (id uint) (user principal))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (is-custodian-of-item id tx-sender) ERR_UNAUTHORIZED_ACCESS)
    (asserts! (not (is-eq user tx-sender)) ERR_INVALID_TOKEN)

    (map-set access-permissions
      { item-id: id, permitted-user: user }
      { 
        has-access: true,
        granted-by: tx-sender,
        granted-at-block: block-height
      }
    )
    (ok true)
  )
)

(define-public (remove-view-permission (id uint) (user principal))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
      (permission-data (unwrap! (map-get? access-permissions { item-id: id, permitted-user: user }) ERR_UNAUTHORIZED_ACCESS))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (is-custodian-of-item id tx-sender) ERR_UNAUTHORIZED_ACCESS)
    (asserts! (not (is-eq user tx-sender)) ERR_INVALID_TOKEN)

    (map-delete access-permissions { item-id: id, permitted-user: user })

    (if (is-item-bookmarked id user)
      (map-delete custodian-bookmarks { bookmark-owner: user, bookmarked-item: id })
      true
    )
    (ok true)
  )
)

(define-read-only (check-user-access (id uint) (user principal))
  (ok (can-user-view id user))
)

;; ========================================
;; CUSTODIAN TRANSFER OPERATIONS
;; ========================================

(define-public (transfer-custodianship (id uint) (new-custodian principal))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (is-custodian-of-item id tx-sender) ERR_UNAUTHORIZED_ACCESS)
    (asserts! (not (is-eq new-custodian tx-sender)) ERR_INVALID_TOKEN)
    (asserts! (is-principal-valid new-custodian) ERR_PRINCIPAL_INVALID)

    (map-set collection-items
      { item-id: id }
      (merge item-data { current-custodian: new-custodian })
    )

    (map-set access-permissions
      { item-id: id, permitted-user: new-custodian }
      {
        has-access: true,
        granted-by: tx-sender,
        granted-at-block: block-height
      }
    )
    (ok true)
  )
)

;; ========================================
;; VALIDATION HELPER FUNCTIONS
;; ========================================

(define-private (does-item-exist (id uint))
  (is-some (map-get? collection-items { item-id: id }))
)

(define-private (is-custodian-of-item (id uint) (user principal))
  (match (map-get? collection-items { item-id: id })
    item-record (is-eq (get current-custodian item-record) user)
    false
  )
)

(define-private (is-principal-valid (target principal))
  (not (is-eq target 'ST000000000000000000002AMW42H))
)

(define-private (can-user-view (id uint) (user principal))
  (match (map-get? access-permissions { item-id: id, permitted-user: user })
    permission-record (get has-access permission-record)
    false
  )
)

(define-private (is-item-bookmarked (id uint) (user principal))
  (is-some (map-get? custodian-bookmarks { bookmark-owner: user, bookmarked-item: id }))
)

(define-private (get-item-mass (id uint))
  (default-to u0 
    (get mass-grams 
      (map-get? collection-items { item-id: id })
    )
  )
)

;; ========================================
;; LABEL VALIDATION SYSTEM
;; ========================================

(define-private (is-single-label-valid (label (string-ascii 32)))
  (and 
    (> (len label) u0)
    (< (len label) u33)
  )
)

(define-private (verify-labels-valid (label-set (list 10 (string-ascii 32))))
  (and
    (> (len label-set) u0)
    (<= (len label-set) u10)
    (is-eq (len (filter is-single-label-valid label-set)) (len label-set))
  )
)

;; ========================================
;; SECURITY VERIFICATION PROTOCOL
;; ========================================

(define-public (perform-security-check (id uint) (verification-code uint) (operation-name (string-ascii 32)))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
      (permission-data (unwrap! (map-get? access-permissions { item-id: id, permitted-user: tx-sender }) ERR_VIEW_PERMISSION_DENIED))
      (computed-hash (+ id block-height (get mass-grams item-data)))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (can-user-view id tx-sender) ERR_VIEW_PERMISSION_DENIED)
    (asserts! (> (len operation-name) u0) ERR_INVALID_ITEM_ID)
    (asserts! (< (len operation-name) u33) ERR_INVALID_ITEM_ID)
    (asserts! (> verification-code u0) ERR_INVALID_TOKEN)

    (asserts! (is-eq (mod computed-hash u1000) (mod verification-code u1000)) ERR_INVALID_TOKEN)

    (map-set access-permissions
      { item-id: id, permitted-user: tx-sender }
      (merge permission-data { granted-at-block: block-height })
    )

    (ok computed-hash)
  )
)

;; ========================================
;; AUDIT TRAIL GENERATION
;; ========================================

(define-public (log-access-event (id uint) (event-type (string-ascii 32)) (event-info (string-ascii 96)))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (can-user-view id tx-sender) ERR_VIEW_PERMISSION_DENIED)
    (asserts! (> (len event-type) u0) ERR_INVALID_ITEM_ID)
    (asserts! (< (len event-type) u33) ERR_INVALID_ITEM_ID)
    (asserts! (> (len event-info) u0) ERR_INVALID_ITEM_ID)
    (asserts! (< (len event-info) u97) ERR_INVALID_ITEM_ID)

    (map-set access-permissions
      { item-id: id, permitted-user: tx-sender }
      { 
        has-access: true,
        granted-by: tx-sender,
        granted-at-block: block-height
      }
    )

    (ok block-height)
  )
)

;; ========================================
;; ITEM INTEGRITY VERIFICATION
;; ========================================

(define-public (verify-item-integrity (id uint))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
      (custodian-permission (map-get? access-permissions { item-id: id, permitted-user: (get current-custodian item-data) }))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (is-custodian-of-item id tx-sender) ERR_UNAUTHORIZED_ACCESS)

    (asserts! (is-some custodian-permission) ERR_VIEW_PERMISSION_DENIED)
    (asserts! (and (> (len (get item-name item-data)) u0) (< (len (get item-name item-data)) u65)) ERR_INVALID_ITEM_ID)
    (asserts! (and (> (get mass-grams item-data) u0) (< (get mass-grams item-data) u1000000000)) ERR_MASS_BOUNDARY_VIOLATION)
    (asserts! (and (> (len (get source-location item-data)) u0) (< (len (get source-location item-data)) u33)) ERR_INVALID_LOCATION_INFO)
    (asserts! (and (> (len (get description item-data)) u0) (< (len (get description item-data)) u129)) ERR_INVALID_ITEM_ID)
    (asserts! (verify-labels-valid (get classification-labels item-data)) ERR_INVALID_ITEM_ID)

    (ok true)
  )
)

;; ========================================
;; ADMINISTRATIVE OVERRIDE FUNCTIONS
;; ========================================

(define-public (force-custodian-change (id uint) (replacement-custodian principal) (reason (string-ascii 64)))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED_ACCESS)
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (is-principal-valid replacement-custodian) ERR_PRINCIPAL_INVALID)
    (asserts! (> (len reason) u0) ERR_INVALID_ITEM_ID)
    (asserts! (< (len reason) u65) ERR_INVALID_ITEM_ID)
    (asserts! (not (is-eq replacement-custodian (get current-custodian item-data))) ERR_INVALID_TOKEN)

    (map-set collection-items
      { item-id: id }
      (merge item-data { current-custodian: replacement-custodian })
    )

    (map-set access-permissions
      { item-id: id, permitted-user: replacement-custodian }
      {
        has-access: true,
        granted-by: CONTRACT_OWNER,
        granted-at-block: block-height
      }
    )
    (ok true)
  )
)

(define-public (revoke-all-permissions (id uint))
  (let
    (
      (item-data (unwrap! (map-get? collection-items { item-id: id }) ERR_ITEM_NONEXISTENT))
    )
    (asserts! (does-item-exist id) ERR_ITEM_NONEXISTENT)
    (asserts! (is-custodian-of-item id tx-sender) ERR_UNAUTHORIZED_ACCESS)

    (map-set access-permissions
      { item-id: id, permitted-user: tx-sender }
      { 
        has-access: true,
        granted-by: tx-sender,
        granted-at-block: block-height
      }
    )
    (ok true)
  )
)

