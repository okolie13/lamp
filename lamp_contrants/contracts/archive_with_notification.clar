;; Enhanced Archive with Notifications Smart Contract
;; Advanced contract with multiple storage types, access control, history tracking, and more

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-ADDRESS (err u101))
(define-constant ERR-ADDRESS-NOT-FOUND (err u102))
(define-constant ERR-INVALID-VALUE (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-CONTRACT-PAUSED (err u105))
(define-constant ERR-COOLDOWN-ACTIVE (err u106))
(define-constant ERR-INVALID-THRESHOLD (err u107))
(define-constant ERR-HISTORY-LIMIT-REACHED (err u108))
(define-constant ERR-INVALID-PERMISSIONS (err u109))
(define-constant ERR-SUBSCRIPTION-EXPIRED (err u110))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-HISTORY-ENTRIES u100)
(define-constant DEFAULT-COOLDOWN u10) ;; 10 blocks
(define-constant SUBSCRIPTION-COST u1000) ;; Cost in microSTX

;; Data variables
(define-data-var stored-value uint u0)
(define-data-var stored-string (string-utf8 256) u"")
(define-data-var stored-boolean bool false)
(define-data-var contract-paused bool false)
(define-data-var update-cooldown uint DEFAULT-COOLDOWN)
(define-data-var last-update-block uint u0)
(define-data-var notification-threshold uint u0)
(define-data-var total-updates uint u0)
(define-data-var emergency-admin (optional principal) none)

;; Permission levels
(define-constant PERMISSION-READ u1)
(define-constant PERMISSION-WRITE u2)
(define-constant PERMISSION-ADMIN u3)

;; Data maps
(define-map notification-addresses principal bool)
(define-map conditional-notifications principal {threshold-type: (string-ascii 10), threshold-value: uint})
(define-map user-permissions principal uint)
(define-map value-history uint {value: uint, block-height: uint, updated-by: principal, timestamp: uint})
(define-map user-balances principal uint)
(define-map subscription-expiry principal uint)
(define-map custom-storage (string-ascii 50) {data-type: (string-ascii 20), value: (string-utf8 500), owner: principal})
(define-map access-logs principal {last-access: uint, access-count: uint})
(define-map notification-preferences principal {email-enabled: bool, sms-enabled: bool, webhook-url: (optional (string-utf8 200))})

;; Lists for batch operations
(define-data-var authorized-updaters (list 20 principal) (list))

;; Read-only functions

;; Get the current stored value
(define-read-only (get-stored-value)
  (var-get stored-value)
)

;; Get stored string
(define-read-only (get-stored-string)
  (var-get stored-string)
)

;; Get stored boolean
(define-read-only (get-stored-boolean)
  (var-get stored-boolean)
)

;; Check contract status
(define-read-only (get-contract-status)
  {
    paused: (var-get contract-paused),
    total-updates: (var-get total-updates),
    last-update-block: (var-get last-update-block),
    cooldown-period: (var-get update-cooldown),
    notification-threshold: (var-get notification-threshold)
  }
)

;; Get user permissions
(define-read-only (get-user-permissions (user principal))
  (default-to u0 (map-get? user-permissions user))
)

;; Check if user has specific permission
(define-read-only (has-permission (user principal) (required-permission uint))
  (>= (get-user-permissions user) required-permission)
)

;; Get value history entry
(define-read-only (get-history-entry (index uint))
  (map-get? value-history index)
)

;; Get user balance
(define-read-only (get-user-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

;; Check subscription status
(define-read-only (is-subscription-active (user principal))
  (match (map-get? subscription-expiry user)
    expiry (> expiry block-height)
    false
  )
)

;; Get custom storage
(define-read-only (get-custom-storage (key (string-ascii 50)))
  (map-get? custom-storage key)
)

;; Get notification preferences
(define-read-only (get-notification-preferences (user principal))
  (map-get? notification-preferences user)
)

;; Check if address is registered for notifications
(define-read-only (is-notification-address (address principal))
  (default-to false (map-get? notification-addresses address))
)

;; Get access logs
(define-read-only (get-access-logs (user principal))
  (map-get? access-logs user)
)

;; Calculate statistics
(define-read-only (get-contract-statistics)
  {
    total-registered-addresses: (len (var-get authorized-updaters)),
    total-value-updates: (var-get total-updates),
    current-threshold: (var-get notification-threshold),
    contract-age-blocks: (- block-height (var-get last-update-block))
  }
)

;; Public functions

;; Emergency pause/unpause (only owner or emergency admin)
(define-public (toggle-contract-pause)
  (begin
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER)
                  (is-eq (some tx-sender) (var-get emergency-admin))) ERR-NOT-AUTHORIZED)
    (var-set contract-paused (not (var-get contract-paused)))
    (print {event: "contract-pause-toggled", paused: (var-get contract-paused), by: tx-sender})
    (ok (var-get contract-paused))
  )
)

;; Set emergency admin
(define-public (set-emergency-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set emergency-admin (some admin))
    (ok true)
  )
)

;; Grant permissions to user
(define-public (grant-permission (user principal) (permission uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= permission PERMISSION-ADMIN) ERR-INVALID-PERMISSIONS)
    (map-set user-permissions user permission)
    (print {event: "permission-granted", user: user, permission: permission})
    (ok true)
  )
)

;; Add user balance
(define-public (add-balance (amount uint))
  (let ((current-balance (get-user-balance tx-sender)))
    (begin
      (map-set user-balances tx-sender (+ current-balance amount))
      (ok true)
    )
  )
)

;; Subscribe to premium notifications
(define-public (subscribe-premium (duration uint))
  (let ((cost (* SUBSCRIPTION-COST duration))
        (current-balance (get-user-balance tx-sender)))
    (begin
      (asserts! (>= current-balance cost) ERR-INSUFFICIENT-BALANCE)
      (map-set user-balances tx-sender (- current-balance cost))
      (map-set subscription-expiry tx-sender (+ block-height duration))
      (print {event: "premium-subscription", user: tx-sender, expires: (+ block-height duration)})
      (ok true)
    )
  )
)

;; Set notification preferences
(define-public (set-notification-preferences (email bool) (sms bool) (webhook (optional (string-utf8 200))))
  (begin
    (map-set notification-preferences tx-sender {
      email-enabled: email,
      sms-enabled: sms,
      webhook-url: webhook
    })
    (ok true)
  )
)

;; Enhanced value setting with history tracking
(define-public (set-value (new-value uint))
  (let (
    (old-value (var-get stored-value))
    (current-updates (var-get total-updates))
  )
    (begin
      (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
      (asserts! (has-permission tx-sender PERMISSION-WRITE) ERR-NOT-AUTHORIZED)
      (asserts! (>= block-height (+ (var-get last-update-block) (var-get update-cooldown))) ERR-COOLDOWN-ACTIVE)
      
      ;; Update value and metadata
      (var-set stored-value new-value)
      (var-set last-update-block block-height)
      (var-set total-updates (+ current-updates u1))
      
      ;; Store in history (if under limit)
      (if (< current-updates MAX-HISTORY-ENTRIES)
        (map-set value-history current-updates {
          value: new-value,
          block-height: block-height,
          updated-by: tx-sender,
          timestamp: block-height
        })
        true
      )
      
      ;; Update access logs
      (update-access-log tx-sender)
      
      ;; Emit comprehensive event
      (print {
        event: "value-changed-enhanced",
        old-value: old-value,
        new-value: new-value,
        change-delta: (if (> new-value old-value) (- new-value old-value) (- old-value new-value)),
        changed-by: tx-sender,
        block-height: block-height,
        update-number: current-updates,
        threshold-crossed: (> new-value (var-get notification-threshold))
      })
      
      (ok true)
    )
  )
)

;; Set string value
(define-public (set-string-value (new-string (string-utf8 256)))
  (begin
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (has-permission tx-sender PERMISSION-WRITE) ERR-NOT-AUTHORIZED)
    (var-set stored-string new-string)
    (update-access-log tx-sender)
    (print {event: "string-value-changed", new-value: new-string, changed-by: tx-sender})
    (ok true)
  )
)

;; Set boolean value
(define-public (set-boolean-value (new-bool bool))
  (begin
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (has-permission tx-sender PERMISSION-WRITE) ERR-NOT-AUTHORIZED)
    (var-set stored-boolean new-bool)
    (update-access-log tx-sender)
    (print {event: "boolean-value-changed", new-value: new-bool, changed-by: tx-sender})
    (ok true)
  )
)

;; Conditional value update (only if condition is met)
(define-public (conditional-set-value (new-value uint) (condition (string-ascii 10)) (threshold uint))
  (let ((current-value (var-get stored-value)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
      (asserts! (has-permission tx-sender PERMISSION-WRITE) ERR-NOT-AUTHORIZED)
      
      ;; Check condition
      (asserts! (if (is-eq condition "greater")
                   (> new-value threshold)
                   (if (is-eq condition "less")
                      (< new-value threshold)
                      (is-eq new-value threshold))) ERR-INVALID-VALUE)
      
      (var-set stored-value new-value)
      (print {event: "conditional-value-set", condition: condition, threshold: threshold, new-value: new-value})
      (ok true)
    )
  )
)

;; Increment value by amount
(define-public (increment-value (amount uint))
  (let ((current-value (var-get stored-value)))
    (begin
      (asserts! (has-permission tx-sender PERMISSION-WRITE) ERR-NOT-AUTHORIZED)
      (var-set stored-value (+ current-value amount))
      (print {event: "value-incremented", amount: amount, new-value: (+ current-value amount)})
      (ok true)
    )
  )
)

;; Set custom storage entry
(define-public (set-custom-storage (key (string-ascii 50)) (data-type (string-ascii 20)) (value (string-utf8 500)))
  (begin
    (asserts! (has-permission tx-sender PERMISSION-WRITE) ERR-NOT-AUTHORIZED)
    (map-set custom-storage key {
      data-type: data-type,
      value: value,
      owner: tx-sender
    })
    (print {event: "custom-storage-set", key: key, data-type: data-type, owner: tx-sender})
    (ok true)
  )
)

;; Batch operations
(define-public (batch-add-notification-addresses (addresses (list 20 principal)))
  (begin
    (asserts! (has-permission tx-sender PERMISSION-ADMIN) ERR-NOT-AUTHORIZED)
    (ok (map add-single-address addresses))
  )
)

;; Set notification threshold
(define-public (set-notification-threshold (threshold uint))
  (begin
    (asserts! (has-permission tx-sender PERMISSION-ADMIN) ERR-NOT-AUTHORIZED)
    (var-set notification-threshold threshold)
    (print {event: "threshold-updated", new-threshold: threshold})
    (ok true)
  )
)

;; Set update cooldown period
(define-public (set-update-cooldown (cooldown uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= cooldown u100) ERR-INVALID-VALUE) ;; Max 100 blocks
    (var-set update-cooldown cooldown)
    (ok true)
  )
)

;; Advanced notification with conditions
(define-public (add-conditional-notification (address principal) (threshold-type (string-ascii 10)) (threshold-value uint))
  (begin
    (asserts! (has-permission tx-sender PERMISSION-ADMIN) ERR-NOT-AUTHORIZED)
    (map-set conditional-notifications address {
      threshold-type: threshold-type,
      threshold-value: threshold-value
    })
    (map-set notification-addresses address true)
    (ok true)
  )
)

;; Reset contract (emergency function)
(define-public (emergency-reset)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set stored-value u0)
    (var-set stored-string u"")
    (var-set stored-boolean false)
    (var-set total-updates u0)
    (var-set last-update-block block-height)
    (print {event: "emergency-reset", by: tx-sender, block: block-height})
    (ok true)
  )
)

;; Private helper functions

;; Helper function for batch adding addresses
(define-private (add-single-address (address principal))
  (map-set notification-addresses address true)
)

;; Update access logs
(define-private (update-access-log (user principal))
  (let ((current-log (default-to {last-access: u0, access-count: u0} (map-get? access-logs user))))
    (map-set access-logs user {
      last-access: block-height,
      access-count: (+ (get access-count current-log) u1)
    })
  )
)

;; Initialize the enhanced contract
(define-public (initialize-enhanced (initial-value uint) (initial-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (var-get stored-value) u0) ERR-NOT-AUTHORIZED)
    
    ;; Set initial values
    (var-set stored-value initial-value)
    (var-set notification-threshold initial-threshold)
    (var-set last-update-block block-height)
    
    ;; Grant owner full permissions
    (map-set user-permissions CONTRACT-OWNER PERMISSION-ADMIN)
    
    (print {
      event: "enhanced-contract-initialized",
      initial-value: initial-value,
      threshold: initial-threshold,
      initialized-by: tx-sender
    })
    (ok true)
  )
)