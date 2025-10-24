;; Enhanced Storage with Access Control List (ACL) Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-already-whitelisted (err u102))
(define-constant err-not-whitelisted (err u103))
(define-constant err-contract-paused (err u104))
(define-constant err-invalid-value (err u105))
(define-constant err-same-value (err u106))
(define-constant err-ownership-transfer-pending (err u107))
(define-constant err-no-pending-transfer (err u108))
(define-constant err-not-pending-owner (err u109))

;; Define data variables
(define-data-var stored-value uint u0)
(define-data-var contract-paused bool false)
(define-data-var min-value uint u0)
(define-data-var max-value uint u1000000)
(define-data-var total-updates uint u0)
(define-data-var pending-owner (optional principal) none)

;; Define data maps
(define-map whitelist principal bool)
(define-map user-update-count principal uint)
(define-map value-history uint {value: uint, updater: principal, timestamp: uint})

;; Initialize contract owner in whitelist
(map-set whitelist contract-owner true)

;; Events (using print for logging)
(define-private (emit-value-updated (old-value uint) (new-value uint) (updater principal))
  (print {
    event: "value-updated",
    old-value: old-value,
    new-value: new-value,
    updater: updater,
    timestamp: burn-block-height
  })
)

(define-private (emit-whitelist-updated (address principal) (action (string-ascii 10)))
  (print {
    event: "whitelist-updated",
    address: address,
    action: action,
    timestamp: burn-block-height
  })
)

;; ===== READ-ONLY FUNCTIONS =====

;; Get the stored value (anyone can call)
(define-read-only (get-value)
  (var-get stored-value)
)

;; Check if an address is whitelisted
(define-read-only (is-whitelisted (address principal))
  (default-to false (map-get? whitelist address))
)

;; Get the contract owner
(define-read-only (get-owner)
  contract-owner
)

;; Check if caller is the owner
(define-read-only (is-owner (address principal))
  (is-eq address contract-owner)
)

;; Check if contract is paused
(define-read-only (is-paused)
  (var-get contract-paused)
)

;; Get value constraints
(define-read-only (get-value-constraints)
  {
    min-value: (var-get min-value),
    max-value: (var-get max-value)
  }
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    current-value: (var-get stored-value),
    total-updates: (var-get total-updates),
    is-paused: (var-get contract-paused),
    min-value: (var-get min-value),
    max-value: (var-get max-value)
  }
)

;; Get user update count
(define-read-only (get-user-update-count (user principal))
  (default-to u0 (map-get? user-update-count user))
)

;; Get value history entry
(define-read-only (get-value-history (index uint))
  (map-get? value-history index)
)

;; Get pending owner
(define-read-only (get-pending-owner)
  (var-get pending-owner)
)

;; ===== PRIVATE HELPER FUNCTIONS =====

;; Validate value is within constraints
(define-private (is-valid-value (value uint))
  (and 
    (>= value (var-get min-value))
    (<= value (var-get max-value))
  )
)

;; Record value in history
(define-private (record-value-history (value uint) (updater principal))
  (let ((update-count (var-get total-updates)))
    (map-set value-history update-count {
      value: value,
      updater: updater,
      timestamp: burn-block-height
    })
  )
)

;; ===== PUBLIC FUNCTIONS =====

;; Update the stored value (only whitelisted addresses when not paused)
(define-public (set-value (new-value uint))
  (let ((old-value (var-get stored-value)))
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-whitelisted tx-sender) err-not-authorized)
    (asserts! (is-valid-value new-value) err-invalid-value)
    (asserts! (not (is-eq new-value old-value)) err-same-value)
    
    ;; Update value and statistics
    (var-set stored-value new-value)
    (var-set total-updates (+ (var-get total-updates) u1))
    
    ;; Update user statistics
    (map-set user-update-count tx-sender 
      (+ (get-user-update-count tx-sender) u1))
    
    ;; Record in history
    (record-value-history new-value tx-sender)
    
    ;; Emit event
    (emit-value-updated old-value new-value tx-sender)
    
    (ok new-value)
  )
)

;; Batch update multiple values (for efficiency)
(define-public (batch-set-values (values (list 10 uint)))
  (begin
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-whitelisted tx-sender) err-not-authorized)
    
    (fold batch-set-helper values (ok u0))
  )
)

(define-private (batch-set-helper (value uint) (previous-result (response uint uint)))
  (match previous-result
    success (set-value value)
    error (err error)
  )
)

;; Add an address to the whitelist (only contract owner)
(define-public (add-to-whitelist (address principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-whitelisted address)) err-already-whitelisted)
    
    (map-set whitelist address true)
    (emit-whitelist-updated address "added")
    (ok true)
  )
)

;; Remove an address from the whitelist (only contract owner)
(define-public (remove-from-whitelist (address principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-whitelisted address) err-not-whitelisted)
    
    (map-delete whitelist address)
    (emit-whitelist-updated address "removed")
    (ok true)
  )
)

;; Batch whitelist management
(define-public (batch-add-to-whitelist (addresses (list 20 principal)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (fold batch-add-helper addresses (ok true))
  )
)

(define-private (batch-add-helper (address principal) (previous-result (response bool uint)))
  (match previous-result
    success (if (is-whitelisted address)
              (ok true)
              (add-to-whitelist address))
    error (err error)
  )
)

;; Emergency pause/unpause (only contract owner)
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused true)
    (print {event: "contract-paused", timestamp: burn-block-height})
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused false)
    (print {event: "contract-unpaused", timestamp: burn-block-height})
    (ok true)
  )
)

;; Set value constraints (only contract owner)
(define-public (set-value-constraints (new-min uint) (new-max uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (< new-min new-max) err-invalid-value)
    
    (var-set min-value new-min)
    (var-set max-value new-max)
    
    (print {
      event: "constraints-updated",
      min-value: new-min,
      max-value: new-max,
      timestamp: burn-block-height
    })
    
    (ok true)
  )
)

;; Reset value to a specific number (only owner, emergency function)
(define-public (emergency-reset-value (reset-value uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-valid-value reset-value) err-invalid-value)
    
    (let ((old-value (var-get stored-value)))
      (var-set stored-value reset-value)
      (var-set total-updates (+ (var-get total-updates) u1))
      
      (record-value-history reset-value tx-sender)
      (emit-value-updated old-value reset-value tx-sender)
      
      (print {event: "emergency-reset", value: reset-value, timestamp: burn-block-height})
      (ok reset-value)
    )
  )
)

;; Two-step ownership transfer for security
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set pending-owner (some new-owner))
    (print {
      event: "ownership-transfer-initiated",
      pending-owner: new-owner,
      timestamp: burn-block-height
    })
    (ok true)
  )
)

(define-public (accept-ownership)
  (begin
    (asserts! (is-some (var-get pending-owner)) err-no-pending-transfer)
    (asserts! (is-eq tx-sender (unwrap-panic (var-get pending-owner))) err-not-pending-owner)
    
    (let ((new-owner tx-sender))
      ;; Add new owner to whitelist and remove old owner
      (map-set whitelist new-owner true)
      (map-delete whitelist contract-owner)
      
      ;; Clear pending transfer
      (var-set pending-owner none)
      
      (print {
        event: "ownership-transferred",
        old-owner: contract-owner,
        new-owner: new-owner,
        timestamp: burn-block-height
      })
      
      (ok true)
    )
  )
)

;; Cancel pending ownership transfer
(define-public (cancel-ownership-transfer)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (var-get pending-owner)) err-no-pending-transfer)
    
    (var-set pending-owner none)
    (print {event: "ownership-transfer-cancelled", timestamp: burn-block-height})
    (ok true)
  )
)