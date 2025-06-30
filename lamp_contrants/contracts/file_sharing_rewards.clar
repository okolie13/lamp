;; FileSharingRewards Smart Contract
;; Rewards users for sharing popular files with fraud prevention

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_FILE_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_COOLDOWN_ACTIVE (err u104))
(define-constant ERR_ALREADY_DOWNLOADED (err u105))
(define-constant ERR_SELF_DOWNLOAD (err u106))

;; Data Variables
(define-data-var reward-pool uint u0)
(define-data-var base-reward uint u1000000) ;; 1 STX in microSTX
(define-data-var popularity-multiplier uint u10)
(define-data-var cooldown-period uint u144) ;; ~24 hours in blocks
(define-data-var contract-paused bool false)

;; Data Maps
(define-map files
  { file-id: (string-ascii 64) }
  {
    owner: principal,
    file-hash: (string-ascii 128),
    upload-block: uint,
    total-downloads: uint,
    total-rewards-earned: uint,
    is-active: bool
  }
)

(define-map file-downloads
  { file-id: (string-ascii 64), downloader: principal }
  {
    download-block: uint,
    reward-given: bool
  }
)

(define-map user-stats
  { user: principal }
  {
    files-uploaded: uint,
    total-downloads: uint,
    total-rewards-earned: uint,
    last-activity: uint
  }
)

(define-map download-cooldowns
  { user: principal, file-id: (string-ascii 64) }
  { last-download: uint }
)

;; Read-only functions
(define-read-only (get-file-info (file-id (string-ascii 64)))
  (map-get? files { file-id: file-id })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

(define-read-only (get-reward-pool)
  (var-get reward-pool)
)

(define-read-only (calculate-reward (file-id (string-ascii 64)))
  (let (
    (file-data (unwrap! (get-file-info file-id) u0))
    (downloads (get total-downloads file-data))
    (popularity-bonus (* (/ downloads u10) (var-get popularity-multiplier)))
  )
    (+ (var-get base-reward) popularity-bonus)
  )
)

(define-read-only (can-download (user principal) (file-id (string-ascii 64)))
  (let (
    (cooldown-data (map-get? download-cooldowns { user: user, file-id: file-id }))
    (current-block block-height)
  )
    (match cooldown-data
      cooldown (>= (- current-block (get last-download cooldown)) (var-get cooldown-period))
      true
    )
  )
)

(define-read-only (has-downloaded (user principal) (file-id (string-ascii 64)))
  (is-some (map-get? file-downloads { file-id: file-id, downloader: user }))
)

;; Private functions
(define-private (update-user-stats (user principal) (downloads-delta uint) (rewards-delta uint))
  (let (
    (current-stats (default-to 
      { files-uploaded: u0, total-downloads: u0, total-rewards-earned: u0, last-activity: u0 }
      (get-user-stats user)
    ))
  )
    (map-set user-stats
      { user: user }
      {
        files-uploaded: (get files-uploaded current-stats),
        total-downloads: (+ (get total-downloads current-stats) downloads-delta),
        total-rewards-earned: (+ (get total-rewards-earned current-stats) rewards-delta),
        last-activity: block-height
      }
    )
  )
)

;; Public functions
(define-public (upload-file (file-id (string-ascii 64)) (file-hash (string-ascii 128)))
  (let (
    (current-stats (default-to 
      { files-uploaded: u0, total-downloads: u0, total-rewards-earned: u0, last-activity: u0 }
      (get-user-stats tx-sender)
    ))
  )
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (is-none (get-file-info file-id)) ERR_UNAUTHORIZED)
    
    (map-set files
      { file-id: file-id }
      {
        owner: tx-sender,
        file-hash: file-hash,
        upload-block: block-height,
        total-downloads: u0,
        total-rewards-earned: u0,
        is-active: true
      }
    )
    
    (map-set user-stats
      { user: tx-sender }
      {
        files-uploaded: (+ (get files-uploaded current-stats) u1),
        total-downloads: (get total-downloads current-stats),
        total-rewards-earned: (get total-rewards-earned current-stats),
        last-activity: block-height
      }
    )
    
    (ok file-id)
  )
)

(define-public (download-file (file-id (string-ascii 64)))
  (let (
    (file-data (unwrap! (get-file-info file-id) ERR_FILE_NOT_FOUND))
    (file-owner (get owner file-data))
    (reward-amount (calculate-reward file-id))
  )
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (get is-active file-data) ERR_FILE_NOT_FOUND)
    (asserts! (not (is-eq tx-sender file-owner)) ERR_SELF_DOWNLOAD)
    (asserts! (not (has-downloaded tx-sender file-id)) ERR_ALREADY_DOWNLOADED)
    (asserts! (can-download tx-sender file-id) ERR_COOLDOWN_ACTIVE)
    (asserts! (>= (var-get reward-pool) reward-amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Update file stats
    (map-set files
      { file-id: file-id }
      (merge file-data {
        total-downloads: (+ (get total-downloads file-data) u1),
        total-rewards-earned: (+ (get total-rewards-earned file-data) reward-amount)
      })
    )
    
    ;; Record download
    (map-set file-downloads
      { file-id: file-id, downloader: tx-sender }
      {
        download-block: block-height,
        reward-given: true
      }
    )
    
    ;; Set cooldown
    (map-set download-cooldowns
      { user: tx-sender, file-id: file-id }
      { last-download: block-height }
    )
    
    ;; Update user stats
    (update-user-stats tx-sender u1 u0)
    (update-user-stats file-owner u0 reward-amount)
    
    ;; Transfer reward to file owner
    (var-set reward-pool (- (var-get reward-pool) reward-amount))
    (try! (stx-transfer? reward-amount (as-contract tx-sender) file-owner))
    
    (ok reward-amount)
  )
)

(define-public (deactivate-file (file-id (string-ascii 64)))
  (let (
    (file-data (unwrap! (get-file-info file-id) ERR_FILE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_UNAUTHORIZED)
    
    (map-set files
      { file-id: file-id }
      (merge file-data { is-active: false })
    )
    
    (ok true)
  )
)

;; Admin functions
(define-public (fund-reward-pool (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set reward-pool (+ (var-get reward-pool) amount))
    (ok (var-get reward-pool))
  )
)

(define-public (set-base-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-reward u0) ERR_INVALID_AMOUNT)
    (var-set base-reward new-reward)
    (ok new-reward)
  )
)

(define-public (set-popularity-multiplier (new-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set popularity-multiplier new-multiplier)
    (ok new-multiplier)
  )
)

(define-public (set-cooldown-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set cooldown-period new-period)
    (ok new-period)
  )
)

(define-public (toggle-contract (pause bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused pause)
    (ok pause)
  )
)

(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= amount (var-get reward-pool)) ERR_INSUFFICIENT_BALANCE)
    (var-set reward-pool (- (var-get reward-pool) amount))
    (try! (stx-transfer? amount (as-contract tx-sender) CONTRACT_OWNER))
    (ok amount)
  )
)