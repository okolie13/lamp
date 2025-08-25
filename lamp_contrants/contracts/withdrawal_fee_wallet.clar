;; Withdrawal Fee Wallet Smart Contract
;; This contract allows users to deposit STX and withdraw with a small fee

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_OWNER_ONLY (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_TRANSFER_FAILED (err u103))

;; Data Variables
(define-data-var withdrawal-fee-percentage uint u250) ;; 2.5% (250 basis points)

;; Data Maps
(define-map user-balances principal uint)

;; Read-only functions

;; Get user balance
(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

;; Get withdrawal fee percentage (in basis points, 10000 = 100%)
(define-read-only (get-withdrawal-fee-percentage)
  (var-get withdrawal-fee-percentage)
)

;; Calculate withdrawal fee for a given amount
(define-read-only (calculate-withdrawal-fee (amount uint))
  (/ (* amount (var-get withdrawal-fee-percentage)) u10000)
)

;; Calculate net withdrawal amount (amount minus fee)
(define-read-only (calculate-net-withdrawal (amount uint))
  (let ((fee (calculate-withdrawal-fee amount)))
    (- amount fee)
  )
)

;; Get contract owner
(define-read-only (get-contract-owner)
  CONTRACT_OWNER
)

;; Public functions

;; Deposit STX to the wallet
(define-public (deposit (amount uint))
  (let ((current-balance (get-balance tx-sender)))
    (begin
      ;; Transfer STX from user to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      ;; Update user balance
      (map-set user-balances tx-sender (+ current-balance amount))
      (ok amount)
    )
  )
)

;; Withdraw STX with fee deduction
(define-public (withdraw (amount uint))
  (let (
    (user-balance (get-balance tx-sender))
    (withdrawal-fee (calculate-withdrawal-fee amount))
    (net-amount (- amount withdrawal-fee))
  )
    (begin
      ;; Check if user has sufficient balance
      (asserts! (>= user-balance amount) ERR_INSUFFICIENT_BALANCE)
      ;; Check if amount is valid (greater than 0)
      (asserts! (> amount u0) ERR_INVALID_AMOUNT)
      
      ;; Update user balance (deduct full amount)
      (map-set user-balances tx-sender (- user-balance amount))
      
      ;; Transfer net amount to user
      (try! (as-contract (stx-transfer? net-amount tx-sender tx-sender)))
      
      ;; Transfer fee to contract owner
      (try! (as-contract (stx-transfer? withdrawal-fee tx-sender CONTRACT_OWNER)))
      
      (ok {
        withdrawn: net-amount,
        fee: withdrawal-fee,
        remaining-balance: (- user-balance amount)
      })
    )
  )
)

;; Owner-only functions

;; Set withdrawal fee percentage (only contract owner)
(define-public (set-withdrawal-fee-percentage (new-fee-percentage uint))
  (begin
    ;; Check if caller is contract owner
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
    ;; Fee percentage should not exceed 100% (10000 basis points)
    (asserts! (<= new-fee-percentage u10000) ERR_INVALID_AMOUNT)
    ;; Update fee percentage
    (var-set withdrawal-fee-percentage new-fee-percentage)
    (ok new-fee-percentage)
  )
)

;; Emergency withdraw function for contract owner
(define-public (emergency-withdraw (amount uint))
  (begin
    ;; Check if caller is contract owner
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
    ;; Transfer STX from contract to owner
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (ok amount)
  )
)