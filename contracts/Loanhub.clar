(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_POOL_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_LOAN_NOT_FOUND (err u103))
(define-constant ERR_LOAN_ALREADY_REPAID (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_POOL_INACTIVE (err u106))
(define-constant ERR_LOAN_OVERDUE (err u107))
(define-constant ERR_ALREADY_MEMBER (err u108))
(define-constant ERR_NOT_MEMBER (err u109))

(define-data-var next-pool-id uint u1)
(define-data-var next-loan-id uint u1)

(define-map lending-pools
  { pool-id: uint }
  {
    name: (string-ascii 50),
    creator: principal,
    total-funds: uint,
    available-funds: uint,
    interest-rate: uint,
    max-loan-amount: uint,
    loan-duration: uint,
    is-active: bool,
    member-count: uint
  }
)

(define-map pool-members
  { pool-id: uint, member: principal }
  { contribution: uint, joined-at: uint }
)

(define-map loans
  { loan-id: uint }
  {
    pool-id: uint,
    borrower: principal,
    amount: uint,
    interest-rate: uint,
    issued-at: uint,
    due-date: uint,
    repaid-amount: uint,
    is-repaid: bool,
    collateral: uint
  }
)

(define-map user-pool-memberships
  { user: principal, pool-id: uint }
  { is-member: bool }
)

(define-public (create-lending-pool 
  (name (string-ascii 50))
  (interest-rate uint)
  (max-loan-amount uint)
  (loan-duration uint)
  (initial-contribution uint))
  (let ((pool-id (var-get next-pool-id)))
    (if (> initial-contribution u0)
      (try! (stx-transfer? initial-contribution tx-sender (as-contract tx-sender)))
      true)
    (map-set lending-pools
      { pool-id: pool-id }
      {
        name: name,
        creator: tx-sender,
        total-funds: initial-contribution,
        available-funds: initial-contribution,
        interest-rate: interest-rate,
        max-loan-amount: max-loan-amount,
        loan-duration: loan-duration,
        is-active: true,
        member-count: u1
      }
    )
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      { contribution: initial-contribution, joined-at: stacks-block-height }
    )
    (map-set user-pool-memberships
      { user: tx-sender, pool-id: pool-id }
      { is-member: true }
    )
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

(define-public (join-pool (pool-id uint) (contribution uint))
  (let ((pool (unwrap! (map-get? lending-pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
        (existing-membership (map-get? user-pool-memberships { user: tx-sender, pool-id: pool-id })))
    (asserts! (get is-active pool) ERR_POOL_INACTIVE)
    (asserts! (is-none existing-membership) ERR_ALREADY_MEMBER)
    (asserts! (> contribution u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? contribution tx-sender (as-contract tx-sender)))
    (map-set lending-pools
      { pool-id: pool-id }
      (merge pool {
        total-funds: (+ (get total-funds pool) contribution),
        available-funds: (+ (get available-funds pool) contribution),
        member-count: (+ (get member-count pool) u1)
      })
    )
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      { contribution: contribution, joined-at: stacks-block-height }
    )
    (map-set user-pool-memberships
      { user: tx-sender, pool-id: pool-id }
      { is-member: true }
    )
    (ok true)
  )
)

(define-public (request-loan (pool-id uint) (amount uint) (collateral uint))
  (let ((pool (unwrap! (map-get? lending-pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
        (loan-id (var-get next-loan-id))
        (membership (map-get? user-pool-memberships { user: tx-sender, pool-id: pool-id })))
    (asserts! (get is-active pool) ERR_POOL_INACTIVE)
    (asserts! (is-some membership) ERR_NOT_MEMBER)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (get max-loan-amount pool)) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (get available-funds pool)) ERR_INSUFFICIENT_FUNDS)
    (if (> collateral u0)
      (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
      true)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set loans
      { loan-id: loan-id }
      {
        pool-id: pool-id,
        borrower: tx-sender,
        amount: amount,
        interest-rate: (get interest-rate pool),
        issued-at: stacks-block-height,
        due-date: (+ stacks-block-height (get loan-duration pool)),
        repaid-amount: u0,
        is-repaid: false,
        collateral: collateral
      }
    )
    (map-set lending-pools
      { pool-id: pool-id }
      (merge pool {
        available-funds: (- (get available-funds pool) amount)
      })
    )
    (var-set next-loan-id (+ loan-id u1))
    (ok loan-id)
  )
)

(define-public (repay-loan (loan-id uint))
  (let ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
        (pool (unwrap! (map-get? lending-pools { pool-id: (get pool-id loan) }) ERR_POOL_NOT_FOUND))
        (total-repayment (+ (get amount loan) (/ (* (get amount loan) (get interest-rate loan)) u100))))
    (asserts! (is-eq tx-sender (get borrower loan)) ERR_UNAUTHORIZED)
    (asserts! (not (get is-repaid loan)) ERR_LOAN_ALREADY_REPAID)
    (try! (stx-transfer? total-repayment tx-sender (as-contract tx-sender)))
    (if (> (get collateral loan) u0)
      (try! (as-contract (stx-transfer? (get collateral loan) tx-sender (get borrower loan))))
      true)
    (map-set loans
      { loan-id: loan-id }
      (merge loan {
        repaid-amount: total-repayment,
        is-repaid: true
      })
    )
    (map-set lending-pools
      { pool-id: (get pool-id loan) }
      (merge pool {
        available-funds: (+ (get available-funds pool) total-repayment),
        total-funds: (+ (get total-funds pool) (/ (* (get amount loan) (get interest-rate loan)) u100))
      })
    )
    (ok true)
  )
)

(define-public (withdraw-contribution (pool-id uint) (amount uint))
  (let ((pool (unwrap! (map-get? lending-pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
        (member-info (unwrap! (map-get? pool-members { pool-id: pool-id, member: tx-sender }) ERR_NOT_MEMBER)))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (get contribution member-info)) ERR_INSUFFICIENT_FUNDS)
    (asserts! (<= amount (get available-funds pool)) ERR_INSUFFICIENT_FUNDS)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      (merge member-info {
        contribution: (- (get contribution member-info) amount)
      })
    )
    (map-set lending-pools
      { pool-id: pool-id }
      (merge pool {
        total-funds: (- (get total-funds pool) amount),
        available-funds: (- (get available-funds pool) amount)
      })
    )
    (ok true)
  )
)

(define-read-only (get-pool-info (pool-id uint))
  (map-get? lending-pools { pool-id: pool-id })
)

(define-read-only (get-loan-info (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-member-info (pool-id uint) (member principal))
  (map-get? pool-members { pool-id: pool-id, member: member })
)

(define-read-only (is-pool-member (pool-id uint) (user principal))
  (is-some (map-get? user-pool-memberships { user: user, pool-id: pool-id }))
)

(define-read-only (get-total-pools)
  (- (var-get next-pool-id) u1)
)

(define-read-only (get-total-loans)
  (- (var-get next-loan-id) u1)
)

(define-read-only (calculate-loan-repayment (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan (+ (get amount loan) (/ (* (get amount loan) (get interest-rate loan)) u100))
    u0
  )
)

(define-read-only (is-loan-overdue (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan (and (not (get is-repaid loan)) (> stacks-block-height (get due-date loan)))
    false
  )
)