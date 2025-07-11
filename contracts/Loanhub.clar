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
(define-constant ERR_INVALID_RATING (err u110))
(define-constant ERR_ALREADY_RATED (err u111))
(define-constant ERR_CANNOT_RATE_OWN_POOL (err u112))
(define-constant ERR_MUST_HAVE_INTERACTED (err u113))

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

(define-map pool-ratings
  { pool-id: uint, rater: principal }
  { 
    rating: uint,
    review: (string-ascii 200),
    rated-at: uint,
    interaction-type: (string-ascii 20)
  }
)

(define-map pool-rating-summary
  { pool-id: uint }
  {
    total-ratings: uint,
    total-score: uint,
    average-rating: uint,
    five-star: uint,
    four-star: uint,
    three-star: uint,
    two-star: uint,
    one-star: uint
  }
)

(define-map creator-reputation
  { creator: principal }
  {
    total-pools: uint,
    total-ratings: uint,
    total-score: uint,
    average-rating: uint,
    successful-loans: uint,
    total-volume: uint,
    reputation-score: uint
  }
)

(define-map user-interactions
  { user: principal, pool-id: uint }
  {
    has-borrowed: bool,
    has-contributed: bool,
    loans-count: uint,
    total-borrowed: uint,
    total-contributed: uint,
    last-interaction: uint
  }
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
    (map-set user-interactions
      { user: tx-sender, pool-id: pool-id }
      { 
        has-borrowed: false,
        has-contributed: true,
        loans-count: u0,
        total-borrowed: u0,
        total-contributed: contribution,
        last-interaction: stacks-block-height
      }
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
    (let ((existing-interaction (map-get? user-interactions { user: tx-sender, pool-id: pool-id })))
      (match existing-interaction
        interaction (map-set user-interactions
          { user: tx-sender, pool-id: pool-id }
          (merge interaction {
            has-borrowed: true,
            loans-count: (+ (get loans-count interaction) u1),
            total-borrowed: (+ (get total-borrowed interaction) amount),
            last-interaction: stacks-block-height
          })
        )
        (map-set user-interactions
          { user: tx-sender, pool-id: pool-id }
          {
            has-borrowed: true,
            has-contributed: false,
            loans-count: u1,
            total-borrowed: amount,
            total-contributed: u0,
            last-interaction: stacks-block-height
          }
        )
      )
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

(define-public (rate-pool (pool-id uint) (rating uint) (review (string-ascii 200)))
  (let ((pool (unwrap! (map-get? lending-pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
        (interaction (map-get? user-interactions { user: tx-sender, pool-id: pool-id }))
        (existing-rating (map-get? pool-ratings { pool-id: pool-id, rater: tx-sender })))
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (not (is-eq tx-sender (get creator pool))) ERR_CANNOT_RATE_OWN_POOL)
    (asserts! (is-none existing-rating) ERR_ALREADY_RATED)
    (asserts! (is-some interaction) ERR_MUST_HAVE_INTERACTED)
    (let ((user-interaction (unwrap! interaction ERR_MUST_HAVE_INTERACTED)))
      (asserts! (or (get has-borrowed user-interaction) (get has-contributed user-interaction)) ERR_MUST_HAVE_INTERACTED)
      (map-set pool-ratings
        { pool-id: pool-id, rater: tx-sender }
        {
          rating: rating,
          review: review,
          rated-at: stacks-block-height,
          interaction-type: (if (get has-borrowed user-interaction) "borrower" "contributor")
        }
      )
      (begin
        (unwrap! (update-pool-rating-summary pool-id rating) ERR_UNAUTHORIZED)
        (unwrap! (update-creator-reputation (get creator pool) rating) ERR_UNAUTHORIZED)
        (ok true)
      )
    )
  )
)

(define-private (update-pool-rating-summary (pool-id uint) (new-rating uint))
  (let ((current-summary (default-to 
          { total-ratings: u0, total-score: u0, average-rating: u0, 
            five-star: u0, four-star: u0, three-star: u0, two-star: u0, one-star: u0 }
          (map-get? pool-rating-summary { pool-id: pool-id })))
        (new-total-ratings (+ (get total-ratings current-summary) u1))
        (new-total-score (+ (get total-score current-summary) new-rating))
        (new-average (/ new-total-score new-total-ratings)))
    (map-set pool-rating-summary
      { pool-id: pool-id }
      (merge current-summary {
        total-ratings: new-total-ratings,
        total-score: new-total-score,
        average-rating: new-average,
        five-star: (+ (get five-star current-summary) (if (is-eq new-rating u5) u1 u0)),
        four-star: (+ (get four-star current-summary) (if (is-eq new-rating u4) u1 u0)),
        three-star: (+ (get three-star current-summary) (if (is-eq new-rating u3) u1 u0)),
        two-star: (+ (get two-star current-summary) (if (is-eq new-rating u2) u1 u0)),
        one-star: (+ (get one-star current-summary) (if (is-eq new-rating u1) u1 u0))
      })
    )
    (ok true)
  )
)

(define-private (update-creator-reputation (creator principal) (new-rating uint))
  (let ((current-rep (default-to 
          { total-pools: u0, total-ratings: u0, total-score: u0, average-rating: u0,
            successful-loans: u0, total-volume: u0, reputation-score: u0 }
          (map-get? creator-reputation { creator: creator })))
        (new-total-ratings (+ (get total-ratings current-rep) u1))
        (new-total-score (+ (get total-score current-rep) new-rating))
        (new-average (/ new-total-score new-total-ratings))
        (base-score (/ (* new-average u20) u1))
        (volume-bonus (/ (get total-volume current-rep) u1000000))
        (loan-bonus (/ (get successful-loans current-rep) u10))
        (new-reputation-score (+ base-score volume-bonus loan-bonus)))
    (map-set creator-reputation
      { creator: creator }
      (merge current-rep {
        total-ratings: new-total-ratings,
        total-score: new-total-score,
        average-rating: new-average,
        reputation-score: new-reputation-score
      })
    )
    (ok true)
  )
)

(define-public (update-creator-pool-count (creator principal))
  (let ((current-rep (default-to 
          { total-pools: u0, total-ratings: u0, total-score: u0, average-rating: u0,
            successful-loans: u0, total-volume: u0, reputation-score: u0 }
          (map-get? creator-reputation { creator: creator }))))
    (map-set creator-reputation
      { creator: creator }
      (merge current-rep {
        total-pools: (+ (get total-pools current-rep) u1)
      })
    )
    (ok true)
  )
)

(define-public (update-successful-loan (creator principal) (loan-amount uint))
  (let ((current-rep (default-to 
          { total-pools: u0, total-ratings: u0, total-score: u0, average-rating: u0,
            successful-loans: u0, total-volume: u0, reputation-score: u0 }
          (map-get? creator-reputation { creator: creator }))))
    (map-set creator-reputation
      { creator: creator }
      (merge current-rep {
        successful-loans: (+ (get successful-loans current-rep) u1),
        total-volume: (+ (get total-volume current-rep) loan-amount)
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

(define-read-only (get-pool-rating-summary (pool-id uint))
  (map-get? pool-rating-summary { pool-id: pool-id })
)

(define-read-only (get-user-pool-rating (pool-id uint) (user principal))
  (map-get? pool-ratings { pool-id: pool-id, rater: user })
)

(define-read-only (get-creator-reputation (creator principal))
  (map-get? creator-reputation { creator: creator })
)

(define-read-only (get-user-interaction (user principal) (pool-id uint))
  (map-get? user-interactions { user: user, pool-id: pool-id })
)

(define-read-only (can-rate-pool (pool-id uint) (user principal))
  (let ((pool (map-get? lending-pools { pool-id: pool-id }))
        (interaction (map-get? user-interactions { user: user, pool-id: pool-id }))
        (existing-rating (map-get? pool-ratings { pool-id: pool-id, rater: user })))
    (match pool
      pool-data (and 
        (not (is-eq user (get creator pool-data)))
        (is-some interaction)
        (is-none existing-rating)
        (match interaction
          user-interaction (or (get has-borrowed user-interaction) (get has-contributed user-interaction))
          false
        )
      )
      false
    )
  )
)

(define-read-only (get-pool-ratings-list (pool-id uint) (limit uint) (offset uint))
  (let ((max-limit (if (> limit u50) u50 limit)))
    {
      pool-id: pool-id,
      limit: max-limit,
      offset: offset,
      summary: (map-get? pool-rating-summary { pool-id: pool-id })
    }
  )
)

(define-read-only (get-top-rated-pools (limit uint))
  (let ((max-limit (if (> limit u20) u20 limit)))
    {
      limit: max-limit,
      total-pools: (- (var-get next-pool-id) u1)
    }
  )
)

(define-read-only (get-creator-stats (creator principal))
  (let ((reputation (map-get? creator-reputation { creator: creator })))
    (match reputation
      rep-data {
        creator: creator,
        total-pools: (get total-pools rep-data),
        average-rating: (get average-rating rep-data),
        total-ratings: (get total-ratings rep-data),
        successful-loans: (get successful-loans rep-data),
        total-volume: (get total-volume rep-data),
        reputation-score: (get reputation-score rep-data),
        reputation-level: (get-reputation-level (get reputation-score rep-data))
      }
      {
        creator: creator,
        total-pools: u0,
        average-rating: u0,
        total-ratings: u0,
        successful-loans: u0,
        total-volume: u0,
        reputation-score: u0,
        reputation-level: "newcomer"
      }
    )
  )
)

(define-read-only (get-reputation-level (score uint))
  (if (>= score u80) "legendary"
    (if (>= score u60) "expert"
      (if (>= score u40) "trusted"
        (if (>= score u20) "established"
          "newcomer"
        )
      )
    )
  )
)