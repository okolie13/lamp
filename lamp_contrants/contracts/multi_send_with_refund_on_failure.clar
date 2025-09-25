;; Multi-Send with Refund on Failure Smart Contract
;; Allows batch sending of STX with automatic refunds if any transfer fails

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_TRANSFER_FAILED (err u103))
(define-constant ERR_EMPTY_RECIPIENTS (err u104))
(define-constant ERR_REFUND_FAILED (err u105))

;; Data Variables
(define-data-var contract-active bool true)

;; Data Maps
(define-map pending-refunds principal uint)

;; Private Functions

;; Calculate total amount needed for all transfers
(define-private (calculate-total (recipients (list 50 {recipient: principal, amount: uint})))
  (fold + (map get-amount recipients) u0)
)

;; Helper function to extract amount from recipient tuple
(define-private (get-amount (recipient {recipient: principal, amount: uint}))
  (get amount recipient)
)

;; Perform a single STX transfer with error handling
(define-private (safe-transfer (recipient-data {recipient: principal, amount: uint}))
  (let ((recipient (get recipient recipient-data))
        (amount (get amount recipient-data)))
    (if (> amount u0)
        (stx-transfer? amount tx-sender recipient)
        (ok true))))

;; Execute all transfers in sequence
(define-private (execute-transfers (recipients (list 50 {recipient: principal, amount: uint})))
  (fold check-transfer-result 
        (map safe-transfer recipients) 
        (ok true)))

;; Check if all transfers succeeded
(define-private (check-transfer-result (current-result (response bool uint)) (accumulator (response bool uint)))
  (match accumulator
    success (match current-result
              ok-result (ok true)
              err-result (err err-result))
    error (err error)))

;; Refund the total amount to sender
(define-private (refund-sender (sender principal) (total-amount uint))
  (if (> total-amount u0)
      (begin
        ;; Store pending refund
        (map-set pending-refunds sender total-amount)
        ;; Execute refund
        (match (as-contract (stx-transfer? total-amount tx-sender sender))
          success (begin
                    (map-delete pending-refunds sender)
                    (ok true))
          error (err ERR_REFUND_FAILED)))
      (ok true)))

;; Public Functions

;; Main multi-send function with refund mechanism
(define-public (multi-send-with-refund (recipients (list 50 {recipient: principal, amount: uint})))
  (let ((sender tx-sender)
        (total-amount (calculate-total recipients))
        (sender-balance (stx-get-balance sender)))
    
    ;; Input validation
    (asserts! (var-get contract-active) ERR_NOT_AUTHORIZED)
    (asserts! (> (len recipients) u0) ERR_EMPTY_RECIPIENTS)
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= sender-balance total-amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; First, transfer total amount to contract for escrow
    (match (stx-transfer? total-amount sender (as-contract tx-sender))
      transfer-success 
        ;; Execute all transfers
        (match (as-contract (execute-transfers recipients))
          success (ok {
            status: "success",
            total-sent: total-amount,
            recipients-count: (len recipients),
            refunded: u0
          })
          error (begin
            ;; If transfers failed, refund the sender
            (match (refund-sender sender total-amount)
              refund-success (ok {
                status: "failed-refunded", 
                total-sent: u0,
                recipients-count: (len recipients),
                refunded: total-amount
              })
              refund-error (ok {
                status: "refund-failed",
                total-sent: u0,
                recipients-count: (len recipients),
                refunded: u0
              }))))
      transfer-error (ok {
        status: "transfer-failed",
        total-sent: u0,
        recipients-count: (len recipients),
        refunded: u0
      }))))

;; Emergency refund function for stuck funds
(define-public (emergency-refund)
  (let ((sender tx-sender)
        (pending-amount (default-to u0 (map-get? pending-refunds sender))))
    (asserts! (> pending-amount u0) ERR_INVALID_AMOUNT)
    (match (as-contract (stx-transfer? pending-amount tx-sender sender))
      ok-value (begin
                 (map-delete pending-refunds sender)
                 (ok pending-amount))
      err-value (err err-value))))

;; Validate all amounts meet minimum requirement using fold
(define-private (validate-minimum-amount 
  (recipient-data {recipient: principal, amount: uint}) 
  (context {min-amount: uint, all-valid: bool}))
  {
    min-amount: (get min-amount context),
    all-valid: (and (get all-valid context) (>= (get amount recipient-data) (get min-amount context)))
  })

;; Check if all recipients meet minimum amount requirement
(define-private (all-amounts-valid (recipients (list 50 {recipient: principal, amount: uint})) (min-amount uint))
  (get all-valid 
    (fold validate-minimum-amount 
          recipients 
          {min-amount: min-amount, all-valid: true})))

;; Batch send with individual amount validation
(define-public (validated-multi-send (recipients (list 50 {recipient: principal, amount: uint})) (min-amount uint))
  (begin
    ;; Validate that all amounts meet minimum requirement
    (asserts! (all-amounts-valid recipients min-amount) ERR_INVALID_AMOUNT)
    (multi-send-with-refund recipients)))

;; Read-only Functions

;; Get pending refund amount for a user
(define-read-only (get-pending-refund (user principal))
  (default-to u0 (map-get? pending-refunds user)))

;; Check if contract is active
(define-read-only (is-contract-active)
  (var-get contract-active))

;; Calculate total cost for a batch before sending
(define-read-only (preview-total-cost (recipients (list 50 {recipient: principal, amount: uint})))
  (ok {
    total-amount: (calculate-total recipients),
    recipient-count: (len recipients),
    estimated-fees: (* (len recipients) u1000) ;; Rough fee estimation
  }))

;; Count zero amounts using fold
(define-private (count-zero-amounts 
  (recipient-data {recipient: principal, amount: uint}) 
  (zero-count uint))
  (if (is-eq (get amount recipient-data) u0)
      (+ zero-count u1)
      zero-count))

;; Validate recipients list
(define-read-only (validate-recipients (recipients (list 50 {recipient: principal, amount: uint})))
  (let ((zero-amount-count (fold count-zero-amounts recipients u0)))
    (ok {
      valid: (> (len recipients) u0),
      count: (len recipients),
      total-amount: (calculate-total recipients),
      has-zero-amounts: (> zero-amount-count u0)
    })))

;; Admin Functions (Contract Owner Only)

;; Toggle contract active status
(define-public (toggle-contract-status)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-active (not (var-get contract-active)))
    (ok (var-get contract-active))))

;; Emergency withdrawal (only if contract is deactivated)
(define-public (emergency-withdraw)
  (let ((contract-balance (stx-get-balance (as-contract tx-sender))))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (not (var-get contract-active)) ERR_NOT_AUTHORIZED)
    (asserts! (> contract-balance u0) ERR_INVALID_AMOUNT)
    (as-contract (stx-transfer? contract-balance tx-sender CONTRACT_OWNER))))