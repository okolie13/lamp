;; Voting with Reward Distribution Smart Contract
;; Complete working version with enhanced features

(define-fungible-token vote-reward-token)
(define-non-fungible-token voting-badge uint)

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ELECTION_NOT_FOUND (err u101))
(define-constant ERR_ELECTION_NOT_ACTIVE (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_CANDIDATE_NOT_FOUND (err u104))
(define-constant ERR_ELECTION_NOT_ENDED (err u105))
(define-constant ERR_REWARDS_ALREADY_DISTRIBUTED (err u106))
(define-constant ERR_INSUFFICIENT_FUNDS (err u107))
(define-constant ERR_INVALID_AMOUNT (err u108))
(define-constant ERR_DELEGATION_NOT_ALLOWED (err u109))
(define-constant ERR_INVALID_DELEGATE (err u110))
(define-constant ERR_VOTING_POWER_INSUFFICIENT (err u111))
(define-constant ERR_NOT_WHITELISTED (err u112))
(define-constant ERR_STAKING_REQUIRED (err u113))
(define-constant ERR_EMERGENCY_STOP (err u114))

;; Data Variables
(define-data-var election-counter uint u0)
(define-data-var badge-counter uint u0)
(define-data-var total-reward-pool uint u0)
(define-data-var contract-fee-rate uint u250)
(define-data-var min-stake-amount uint u1000)
(define-data-var emergency-stop bool false)
(define-data-var delegation-enabled bool true)

;; Data Maps
(define-map elections
  { election-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    start-block: uint,
    end-block: uint,
    total-votes: uint,
    total-voting-power: uint,
    voter-reward-per-vote: uint,
    winner-reward: uint,
    runner-up-reward: uint,
    rewards-distributed: bool,
    is-active: bool,
    is-private: bool,
    requires-staking: bool,
    min-stake: uint,
    quadratic-enabled: bool,
    max-votes-per-user: uint,
    winner-candidate-id: uint,
    runner-up-candidate-id: uint
  }
)

(define-map candidates
  { election-id: uint, candidate-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    vote-count: uint,
    voting-power: uint,
    wallet: principal,
    is-active: bool,
    campaign-fund: uint,
    endorsements: uint
  }
)

(define-map votes
  { election-id: uint, voter: principal }
  { candidate-id: uint, block-height: uint, voting-power: uint }
)

(define-map election-candidates
  { election-id: uint }
  { candidate-count: uint }
)

(define-map voter-rewards
  { election-id: uint, voter: principal }
  { 
    base-reward: uint,
    bonus-reward: uint,
    early-voter-bonus: uint,
    streak-bonus: uint,
    claimed: bool,
    total-amount: uint
  }
)

(define-map user-stakes
  { user: principal }
  {
    staked-amount: uint,
    stake-block: uint,
    unlock-block: uint,
    voting-power-bonus: uint
  }
)

(define-map delegations
  { delegator: principal, election-id: uint }
  { 
    delegate: principal,
    voting-power: uint,
    is-active: bool,
    delegation-block: uint
  }
)

(define-map election-whitelist
  { election-id: uint, user: principal }
  { is-whitelisted: bool, added-by: principal }
)

(define-map user-stats
  { user: principal }
  {
    total-votes-cast: uint,
    elections-participated: uint,
    rewards-earned: uint,
    voting-streak: uint,
    last-vote-block: uint,
    reputation-score: uint
  }
)

(define-map time-locked-rewards
  { user: principal, lock-id: uint }
  {
    amount: uint,
    unlock-block: uint,
    reward-type: (string-ascii 50),
    multiplier: uint
  }
)

(define-map governance-proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    target-parameter: (string-ascii 50),
    new-value: uint,
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool
  }
)

(define-map fraud-detection
  { user: principal }
  {
    suspicious-activity-count: uint,
    last-flag-block: uint,
    is-banned: bool,
    ban-end-block: uint
  }
)

;; Read-only functions
(define-read-only (get-election (election-id uint))
  (map-get? elections { election-id: election-id })
)

(define-read-only (get-candidate (election-id uint) (candidate-id uint))
  (map-get? candidates { election-id: election-id, candidate-id: candidate-id })
)

(define-read-only (get-vote (election-id uint) (voter principal))
  (map-get? votes { election-id: election-id, voter: voter })
)

(define-read-only (get-voter-reward-info (election-id uint) (voter principal))
  (map-get? voter-rewards { election-id: election-id, voter: voter })
)

(define-read-only (get-user-stake (user principal))
  (map-get? user-stakes { user: user })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

(define-read-only (get-delegation (delegator principal) (election-id uint))
  (map-get? delegations { delegator: delegator, election-id: election-id })
)

(define-read-only (is-whitelisted (election-id uint) (user principal))
  (default-to false (get is-whitelisted (map-get? election-whitelist { election-id: election-id, user: user })))
)

(define-read-only (get-token-balance (account principal))
  (ft-get-balance vote-reward-token account)
)

(define-read-only (get-contract-balance)
  (ft-get-balance vote-reward-token (as-contract tx-sender))
)

(define-read-only (calculate-voting-power (user principal) (election-id uint))
  (let (
    (base-power u1)
    (stake-data (get-user-stake user))
    (delegation-data (get-delegation user election-id))
    (user-stat (get-user-stats user))
  )
    (+ base-power
       (default-to u0 (get voting-power-bonus stake-data))
       (default-to u0 (get voting-power delegation-data))
       (/ (default-to u0 (get reputation-score user-stat)) u100)
    )
  )
)

(define-read-only (get-election-results (election-id uint))
  (let (
    (election-data (unwrap! (get-election election-id) (err u404)))
    (candidate-count (default-to u0 (get candidate-count (map-get? election-candidates { election-id: election-id }))))
  )
    (ok {
      election: election-data,
      candidate-count: candidate-count,
      winner-id: (get winner-candidate-id election-data),
      runner-up-id: (get runner-up-candidate-id election-data)
    })
  )
)

(define-read-only (get-locked-reward (user principal) (lock-id uint))
  (map-get? time-locked-rewards { user: user, lock-id: lock-id })
)

(define-read-only (get-governance-proposal (proposal-id uint))
  (map-get? governance-proposals { proposal-id: proposal-id })
)

;; Private helper functions
(define-private (is-election-active (election-id uint))
  (and
    (not (var-get emergency-stop))
    (match (get-election election-id)
      election-data
        (and 
          (get is-active election-data)
          (>= block-height (get start-block election-data))
          (<= block-height (get end-block election-data))
        )
      false
    )
  )
)

(define-private (is-election-ended (election-id uint))
  (match (get-election election-id)
    election-data
      (> block-height (get end-block election-data))
    false
  )
)

(define-private (calculate-quadratic-cost (vote-amount uint))
  (* vote-amount vote-amount)
)

(define-private (update-user-stats (user principal) (election-id uint))
  (let (
    (current-stats (default-to 
      { total-votes-cast: u0, elections-participated: u0, rewards-earned: u0, 
        voting-streak: u0, last-vote-block: u0, reputation-score: u0 }
      (get-user-stats user)
    ))
    (new-streak (if (< (- block-height (get last-vote-block current-stats)) u1000) 
                   (+ (get voting-streak current-stats) u1) 
                   u1))
  )
    (map-set user-stats
      { user: user }
      (merge current-stats {
        total-votes-cast: (+ (get total-votes-cast current-stats) u1),
        elections-participated: (+ (get elections-participated current-stats) u1),
        voting-streak: new-streak,
        last-vote-block: block-height,
        reputation-score: (+ (get reputation-score current-stats) u10)
      })
    )
  )
)

(define-private (update-election-winner (election-id uint) (candidate-id uint) (voting-power uint))
  (let (
    (election-data (unwrap! (get-election election-id) ERR_ELECTION_NOT_FOUND))
    (current-winner-id (get winner-candidate-id election-data))
    (current-runner-up-id (get runner-up-candidate-id election-data))
  )
    (if (is-eq current-winner-id u0)
      ;; First candidate becomes winner
      (begin
        (map-set elections
          { election-id: election-id }
          (merge election-data { winner-candidate-id: candidate-id })
        )
        (ok true)
      )
      ;; Compare with existing winner
      (let (
        (winner-data (unwrap! (get-candidate election-id current-winner-id) ERR_CANDIDATE_NOT_FOUND))
        (winner-votes (get voting-power winner-data))
      )
        (if (> voting-power winner-votes)
          ;; New winner found
          (begin
            (map-set elections
              { election-id: election-id }
              (merge election-data { 
                winner-candidate-id: candidate-id,
                runner-up-candidate-id: current-winner-id
              })
            )
            (ok true)
          )
          ;; Check if new runner-up
          (if (or (is-eq current-runner-up-id u0) 
                  (> voting-power (get voting-power (unwrap! (get-candidate election-id current-runner-up-id) ERR_CANDIDATE_NOT_FOUND))))
            (begin
              (map-set elections
                { election-id: election-id }
                (merge election-data { runner-up-candidate-id: candidate-id })
              )
              (ok true)
            )
            (ok true)
          )
        )
      )
    )
  )
)

;; Public functions

;; Emergency controls
(define-public (emergency-stop-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set emergency-stop true)
    (ok true)
  )
)

(define-public (resume-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set emergency-stop false)
    (ok true)
  )
)

;; Token management
(define-public (mint-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (ft-mint? vote-reward-token amount recipient)
  )
)

(define-public (fund-contract (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (ft-transfer? vote-reward-token amount tx-sender (as-contract tx-sender)))
    (var-set total-reward-pool (+ (var-get total-reward-pool) amount))
    (ok amount)
  )
)

;; Staking system
(define-public (stake-tokens (amount uint) (lock-duration uint))
  (let (
    (current-balance (ft-get-balance vote-reward-token tx-sender))
    (voting-power-bonus (/ (* amount lock-duration) u1000))
  )
    (asserts! (not (var-get emergency-stop)) ERR_EMERGENCY_STOP)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (>= amount (var-get min-stake-amount)) ERR_STAKING_REQUIRED)
    
    (try! (ft-transfer? vote-reward-token amount tx-sender (as-contract tx-sender)))
    
    (map-set user-stakes
      { user: tx-sender }
      {
        staked-amount: amount,
        stake-block: block-height,
        unlock-block: (+ block-height lock-duration),
        voting-power-bonus: voting-power-bonus
      }
    )
    
    (ok voting-power-bonus)
  )
)

(define-public (unstake-tokens)
  (let (
    (stake-data (unwrap! (get-user-stake tx-sender) ERR_CANDIDATE_NOT_FOUND))
  )
    (asserts! (>= block-height (get unlock-block stake-data)) ERR_STAKING_REQUIRED)
    
    (try! (as-contract (ft-transfer? vote-reward-token (get staked-amount stake-data) tx-sender tx-sender)))
    
    (map-delete user-stakes { user: tx-sender })
    (ok (get staked-amount stake-data))
  )
)

;; Election creation
(define-public (create-election 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (duration-blocks uint)
  (voter-reward-per-vote uint)
  (winner-reward uint)
)
  (let (
    (election-id (+ (var-get election-counter) u1))
    (start-block (+ block-height u1))
    (end-block (+ start-block duration-blocks))
  )
    (asserts! (not (var-get emergency-stop)) ERR_EMERGENCY_STOP)
    (asserts! (> duration-blocks u0) ERR_INVALID_AMOUNT)
    (asserts! (> voter-reward-per-vote u0) ERR_INVALID_AMOUNT)
    (asserts! (> winner-reward u0) ERR_INVALID_AMOUNT)
    
    (map-set elections
      { election-id: election-id }
      {
        title: title,
        description: description,
        creator: tx-sender,
        start-block: start-block,
        end-block: end-block,
        total-votes: u0,
        total-voting-power: u0,
        voter-reward-per-vote: voter-reward-per-vote,
        winner-reward: winner-reward,
        runner-up-reward: u0,
        rewards-distributed: false,
        is-active: true,
        is-private: false,
        requires-staking: false,
        min-stake: u0,
        quadratic-enabled: false,
        max-votes-per-user: u1,
        winner-candidate-id: u0,
        runner-up-candidate-id: u0
      }
    )
    
    (map-set election-candidates
      { election-id: election-id }
      { candidate-count: u0 }
    )
    
    (var-set election-counter election-id)
    (ok election-id)
  )
)

;; Add candidate
(define-public (add-candidate 
  (election-id uint)
  (name (string-ascii 50))
  (description (string-ascii 200))
  (candidate-wallet principal)
)
  (let (
    (election-data (unwrap! (get-election election-id) ERR_ELECTION_NOT_FOUND))
    (current-count (default-to u0 (get candidate-count (map-get? election-candidates { election-id: election-id }))))
    (candidate-id (+ current-count u1))
  )
    (asserts! (is-eq tx-sender (get creator election-data)) ERR_UNAUTHORIZED)
    (asserts! (< block-height (get start-block election-data)) ERR_ELECTION_NOT_ACTIVE)
    
    (map-set candidates
      { election-id: election-id, candidate-id: candidate-id }
      {
        name: name,
        description: description,
        vote-count: u0,
        voting-power: u0,
        wallet: candidate-wallet,
        is-active: true,
        campaign-fund: u0,
        endorsements: u0
      }
    )
    
    (map-set election-candidates
      { election-id: election-id }
      { candidate-count: candidate-id }
    )
    
    (ok candidate-id)
  )
)

;; Voting functions
(define-public (cast-vote (election-id uint) (candidate-id uint))
  (let (
    (election-data (unwrap! (get-election election-id) ERR_ELECTION_NOT_FOUND))
    (candidate-data (unwrap! (get-candidate election-id candidate-id) ERR_CANDIDATE_NOT_FOUND))
    (voting-power (calculate-voting-power tx-sender election-id))
  )
    (asserts! (is-election-active election-id) ERR_ELECTION_NOT_ACTIVE)
    (asserts! (is-none (get-vote election-id tx-sender)) ERR_ALREADY_VOTED)
    
    ;; Check private election whitelist
    (if (get is-private election-data)
      (asserts! (is-whitelisted election-id tx-sender) ERR_NOT_WHITELISTED)
      true
    )
    
    ;; Check staking requirement
    (if (get requires-staking election-data)
      (match (get-user-stake tx-sender)
        stake-data (asserts! (>= (get staked-amount stake-data) (get min-stake election-data)) ERR_STAKING_REQUIRED)
        (asserts! false ERR_STAKING_REQUIRED)
      )
      true
    )
    
    ;; Record the vote
    (map-set votes
      { election-id: election-id, voter: tx-sender }
      { candidate-id: candidate-id, block-height: block-height, voting-power: voting-power }
    )
    
    ;; Update candidate vote count
    (map-set candidates
      { election-id: election-id, candidate-id: candidate-id }
      (merge candidate-data { 
        vote-count: (+ (get vote-count candidate-data) u1),
        voting-power: (+ (get voting-power candidate-data) voting-power)
      })
    )
    
    ;; Update election totals
    (map-set elections
      { election-id: election-id }
      (merge election-data { 
        total-votes: (+ (get total-votes election-data) u1),
        total-voting-power: (+ (get total-voting-power election-data) voting-power)
      })
    )
    
    ;; Update winner tracking
    (try! (update-election-winner election-id candidate-id (+ (get voting-power candidate-data) voting-power)))
    
    ;; Set up voter reward
    (let (
      (base-reward (get voter-reward-per-vote election-data))
      (early-bonus (if (< block-height (+ (get start-block election-data) u100)) u50 u0))
      (user-stat (get-user-stats tx-sender))
      (streak-bonus (* (default-to u0 (get voting-streak user-stat)) u5))
    )
      (map-set voter-rewards
        { election-id: election-id, voter: tx-sender }
        {
          base-reward: base-reward,
          bonus-reward: u0,
          early-voter-bonus: early-bonus,
          streak-bonus: streak-bonus,
          claimed: false,
          total-amount: (+ base-reward early-bonus streak-bonus)
        }
      )
    )
    
    ;; Update user stats
    (update-user-stats tx-sender election-id)
    
    ;; Mint voting badge NFT
    (let ((badge-id (+ (var-get badge-counter) u1)))
      (try! (nft-mint? voting-badge badge-id tx-sender))
      (var-set badge-counter badge-id)
    )
    
    (ok voting-power)
  )
)

;; Reward distribution
(define-public (distribute-rewards (election-id uint))
  (let (
    (election-data (unwrap! (get-election election-id) ERR_ELECTION_NOT_FOUND))
    (winner-id (get winner-candidate-id election-data))
    (runner-up-id (get runner-up-candidate-id election-data))
    (contract-fee (/ (* (+ (get winner-reward election-data) (get runner-up-reward election-data)) (var-get contract-fee-rate)) u10000))
  )
    (asserts! (is-election-ended election-id) ERR_ELECTION_NOT_ENDED)
    (asserts! (not (get rewards-distributed election-data)) ERR_REWARDS_ALREADY_DISTRIBUTED)
    
    ;; Distribute winner reward
    (if (> winner-id u0)
      (match (get-candidate election-id winner-id)
        winner-data
          (try! (as-contract (ft-transfer? vote-reward-token (get winner-reward election-data) tx-sender (get wallet winner-data))))
        true
      )
      true
    )
    
    ;; Distribute runner-up reward
    (if (and (> runner-up-id u0) (> (get runner-up-reward election-data) u0))
      (match (get-candidate election-id runner-up-id)
        runner-up-data
          (try! (as-contract (ft-transfer? vote-reward-token (get runner-up-reward election-data) tx-sender (get wallet runner-up-data))))
        true
      )
      true
    )
    
    ;; Collect contract fee
    (if (> contract-fee u0)
      (try! (as-contract (ft-transfer? vote-reward-token contract-fee tx-sender CONTRACT_OWNER)))
      true
    )
    
    ;; Mark rewards as distributed
    (map-set elections
      { election-id: election-id }
      (merge election-data { rewards-distributed: true })
    )
    
    (ok true)
  )
)

;; Claim voter reward
(define-public (claim-voter-reward (election-id uint))
  (let (
    (election-data (unwrap! (get-election election-id) ERR_ELECTION_NOT_FOUND))
    (reward-info (unwrap! (get-voter-reward-info election-id tx-sender) ERR_CANDIDATE_NOT_FOUND))
  )
    (asserts! (is-election-ended election-id) ERR_ELECTION_NOT_ENDED)
    (asserts! (not (get claimed reward-info)) ERR_REWARDS_ALREADY_DISTRIBUTED)
    (asserts! (is-some (get-vote election-id tx-sender)) ERR_CANDIDATE_NOT_FOUND)
    
    ;; Transfer reward to voter
    (try! (as-contract (ft-transfer? vote-reward-token (get total-amount reward-info) tx-sender tx-sender)))
    
    ;; Mark as claimed
    (map-set voter-rewards
      { election-id: election-id, voter: tx-sender }
      (merge reward-info { claimed: true })
    )
    
    (ok (get total-amount reward-info))
  )
)