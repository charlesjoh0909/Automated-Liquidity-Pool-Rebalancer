(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-POOL-NOT-EXISTS (err u103))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-ZERO-AMOUNT (err u106))
(define-constant ERR-NO-REWARDS (err u107))
(define-constant ERR-REWARDS-NOT-ACTIVE (err u108))
(define-constant ERR-FLASH-LOAN-NOT-REPAID (err u109))
(define-constant ERR-FLASH-LOAN-ACTIVE (err u110))
(define-constant ERR-INVALID-CALLBACK (err u111))

(define-data-var next-pool-id uint u0)
(define-data-var total-pools uint u0)
(define-data-var protocol-fee-rate uint u30)
(define-data-var rebalance-threshold uint u500)
(define-data-var flash-loan-fee-rate uint u9)
(define-data-var flash-loan-in-progress bool false)

(define-map pools 
  { pool-id: uint }
  {
    token-x: principal,
    token-y: principal,
    reserve-x: uint,
    reserve-y: uint,
    lp-supply: uint,
    fee-rate: uint,
    last-price: uint,
    target-ratio: uint,
    created-by: principal,
    active: bool
  }
)

(define-map user-positions
  { user: principal, pool-id: uint }
  {
    lp-tokens: uint,
    last-interaction: uint
  }
)

(define-map pool-fees
  { pool-id: uint }
  {
    accumulated-fees-x: uint,
    accumulated-fees-y: uint
  }
)

(define-data-var total-protocol-fees uint u0)
(define-data-var reward-token principal .automated-liquidity-pool-rebalancer)
(define-data-var global-reward-rate uint u1000)

(define-map pool-rewards
  { pool-id: uint }
  {
    reward-rate: uint,
    total-staked: uint,
    reward-per-token: uint,
    last-update-block: uint,
    active: bool
  }
)

(define-map user-stakes
  { user: principal, pool-id: uint }
  {
    staked-amount: uint,
    stake-start-block: uint,
    reward-debt: uint,
    unclaimed-rewards: uint
  }
)

(define-map flash-loan-state
  { pool-id: uint }
  {
    borrowed-x: uint,
    borrowed-y: uint,
    borrower: principal
  }
)

(define-private (simple-sqrt (x uint))
  (if (<= x u1)
      x
      (let ((guess (/ x u2)))
        (let ((new-guess (/ (+ guess (/ x guess)) u2)))
          (let ((refined-guess (/ (+ new-guess (/ x new-guess)) u2)))
            refined-guess))))
)

(define-private (get-amount-out (amount-in uint) (reserve-in uint) (reserve-out uint) (fee-rate uint))
  (let ((amount-in-with-fee (* amount-in (- u1000 fee-rate)))
        (numerator (* amount-in-with-fee reserve-out))
        (denominator (+ (* reserve-in u1000) amount-in-with-fee)))
    (/ numerator denominator))
)

(define-private (calculate-price (reserve-x uint) (reserve-y uint))
  (if (is-eq reserve-y u0)
      u0
      (/ (* reserve-x u1000000) reserve-y))
)

(define-private (calculate-lp-tokens (reserve-x uint) (reserve-y uint) (amount-x uint) (amount-y uint) (lp-supply uint))
  (if (is-eq lp-supply u0)
      (simple-sqrt (* amount-x amount-y))
      (let ((lp-x (/ (* amount-x lp-supply) reserve-x))
            (lp-y (/ (* amount-y lp-supply) reserve-y)))
        (if (<= lp-x lp-y) lp-x lp-y)))
)

(define-private (calculate-reward-per-token (pool-id uint))
  (let ((pool-reward-data (default-to 
    { reward-rate: u0, total-staked: u0, reward-per-token: u0, last-update-block: u0, active: false }
    (map-get? pool-rewards { pool-id: pool-id }))))
    (if (is-eq (get total-staked pool-reward-data) u0)
        (get reward-per-token pool-reward-data)
        (let ((blocks-elapsed (- stacks-block-height (get last-update-block pool-reward-data)))
              (reward-increment (/ (* (get reward-rate pool-reward-data) blocks-elapsed) (get total-staked pool-reward-data))))
          (+ (get reward-per-token pool-reward-data) reward-increment))))
)

(define-private (calculate-user-rewards (user principal) (pool-id uint))
  (let ((stake-data (default-to 
    { staked-amount: u0, stake-start-block: u0, reward-debt: u0, unclaimed-rewards: u0 }
    (map-get? user-stakes { user: user, pool-id: pool-id })))
        (current-reward-per-token (calculate-reward-per-token pool-id)))
    (+ (get unclaimed-rewards stake-data)
       (* (get staked-amount stake-data) 
          (- current-reward-per-token (get reward-debt stake-data)))))
)

(define-private (update-pool-rewards (pool-id uint))
  (let ((current-reward-per-token (calculate-reward-per-token pool-id))
        (pool-reward-data (default-to 
          { reward-rate: u0, total-staked: u0, reward-per-token: u0, last-update-block: u0, active: false }
          (map-get? pool-rewards { pool-id: pool-id }))))
    (map-set pool-rewards 
      { pool-id: pool-id }
      (merge pool-reward-data {
        reward-per-token: current-reward-per-token,
        last-update-block: stacks-block-height
      }))
    true)
)

(define-private (needs-rebalancing (pool-id uint))
  (match (map-get? pools { pool-id: pool-id })
    pool-data
    (let ((current-price (calculate-price (get reserve-x pool-data) (get reserve-y pool-data)))
          (target-price (get target-ratio pool-data))
          (threshold (var-get rebalance-threshold)))
      (let ((price-diff (if (>= current-price target-price)
                           (- current-price target-price)
                           (- target-price current-price))))
        (> price-diff (/ (* target-price threshold) u10000))))
    false)
)

(define-public (create-pool (token-x principal) (token-y principal) (initial-x uint) (initial-y uint) (target-ratio uint))
  (let ((pool-id (var-get next-pool-id))
        (lp-tokens (simple-sqrt (* initial-x initial-y))))
    (asserts! (> initial-x u0) ERR-ZERO-AMOUNT)
    (asserts! (> initial-y u0) ERR-ZERO-AMOUNT)
    (asserts! (> target-ratio u0) ERR-INVALID-AMOUNT)
    
    (map-set pools
      { pool-id: pool-id }
      {
        token-x: token-x,
        token-y: token-y,
        reserve-x: initial-x,
        reserve-y: initial-y,
        lp-supply: lp-tokens,
        fee-rate: u30,
        last-price: (calculate-price initial-x initial-y),
        target-ratio: target-ratio,
        created-by: tx-sender,
        active: true
      })
    
    (map-set user-positions
      { user: tx-sender, pool-id: pool-id }
      {
        lp-tokens: lp-tokens,
        last-interaction: stacks-block-height
      })
    
    (map-set pool-fees
      { pool-id: pool-id }
      {
        accumulated-fees-x: u0,
        accumulated-fees-y: u0
      })
    
    (var-set next-pool-id (+ pool-id u1))
    (var-set total-pools (+ (var-get total-pools) u1))
    (ok pool-id))
)

(define-public (add-liquidity (pool-id uint) (amount-x uint) (amount-y uint) (min-lp-tokens uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (lp-tokens (calculate-lp-tokens 
          (get reserve-x pool-data) 
          (get reserve-y pool-data) 
          amount-x 
          amount-y 
          (get lp-supply pool-data)))
        (user-position (default-to 
          { lp-tokens: u0, last-interaction: u0 }
          (map-get? user-positions { user: tx-sender, pool-id: pool-id }))))
    
    (asserts! (get active pool-data) ERR-POOL-NOT-EXISTS)
    (asserts! (>= lp-tokens min-lp-tokens) ERR-SLIPPAGE-TOO-HIGH)
    (asserts! (> amount-x u0) ERR-ZERO-AMOUNT)
    (asserts! (> amount-y u0) ERR-ZERO-AMOUNT)
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        reserve-x: (+ (get reserve-x pool-data) amount-x),
        reserve-y: (+ (get reserve-y pool-data) amount-y),
        lp-supply: (+ (get lp-supply pool-data) lp-tokens),
        last-price: (calculate-price 
          (+ (get reserve-x pool-data) amount-x)
          (+ (get reserve-y pool-data) amount-y))
      }))
    
    (map-set user-positions
      { user: tx-sender, pool-id: pool-id }
      {
        lp-tokens: (+ (get lp-tokens user-position) lp-tokens),
        last-interaction: stacks-block-height
      })
    
    (ok lp-tokens))
)

(define-public (remove-liquidity (pool-id uint) (lp-tokens-to-burn uint) (min-amount-x uint) (min-amount-y uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (user-position (unwrap! (map-get? user-positions { user: tx-sender, pool-id: pool-id }) ERR-INSUFFICIENT-LIQUIDITY))
        (amount-x (/ (* lp-tokens-to-burn (get reserve-x pool-data)) (get lp-supply pool-data)))
        (amount-y (/ (* lp-tokens-to-burn (get reserve-y pool-data)) (get lp-supply pool-data))))
    
    (asserts! (get active pool-data) ERR-POOL-NOT-EXISTS)
    (asserts! (>= (get lp-tokens user-position) lp-tokens-to-burn) ERR-INSUFFICIENT-LIQUIDITY)
    (asserts! (>= amount-x min-amount-x) ERR-SLIPPAGE-TOO-HIGH)
    (asserts! (>= amount-y min-amount-y) ERR-SLIPPAGE-TOO-HIGH)
    (asserts! (> lp-tokens-to-burn u0) ERR-ZERO-AMOUNT)
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        reserve-x: (- (get reserve-x pool-data) amount-x),
        reserve-y: (- (get reserve-y pool-data) amount-y),
        lp-supply: (- (get lp-supply pool-data) lp-tokens-to-burn),
        last-price: (calculate-price 
          (- (get reserve-x pool-data) amount-x)
          (- (get reserve-y pool-data) amount-y))
      }))
    
    (map-set user-positions
      { user: tx-sender, pool-id: pool-id }
      {
        lp-tokens: (- (get lp-tokens user-position) lp-tokens-to-burn),
        last-interaction: stacks-block-height
      })
    
    (ok { amount-x: amount-x, amount-y: amount-y }))
)

(define-public (swap-x-for-y (pool-id uint) (amount-x uint) (min-amount-y uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (amount-y (get-amount-out amount-x (get reserve-x pool-data) (get reserve-y pool-data) (get fee-rate pool-data)))
        (fee-amount (/ (* amount-x (get fee-rate pool-data)) u1000))
        (pool-fees-data (default-to { accumulated-fees-x: u0, accumulated-fees-y: u0 } 
                                   (map-get? pool-fees { pool-id: pool-id }))))
    
    (asserts! (get active pool-data) ERR-POOL-NOT-EXISTS)
    (asserts! (>= amount-y min-amount-y) ERR-SLIPPAGE-TOO-HIGH)
    (asserts! (> amount-x u0) ERR-ZERO-AMOUNT)
    (asserts! (> amount-y u0) ERR-INSUFFICIENT-LIQUIDITY)
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        reserve-x: (+ (get reserve-x pool-data) amount-x),
        reserve-y: (- (get reserve-y pool-data) amount-y),
        last-price: (calculate-price 
          (+ (get reserve-x pool-data) amount-x)
          (- (get reserve-y pool-data) amount-y))
      }))
    
    (map-set pool-fees
      { pool-id: pool-id }
      (merge pool-fees-data {
        accumulated-fees-x: (+ (get accumulated-fees-x pool-fees-data) fee-amount)
      }))
    
    (begin
      (if (needs-rebalancing pool-id)
          (unwrap-panic (auto-rebalance pool-id))
          true)
      (ok amount-y)))
)

(define-public (swap-y-for-x (pool-id uint) (amount-y uint) (min-amount-x uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (amount-x (get-amount-out amount-y (get reserve-y pool-data) (get reserve-x pool-data) (get fee-rate pool-data)))
        (fee-amount (/ (* amount-y (get fee-rate pool-data)) u1000))
        (pool-fees-data (default-to { accumulated-fees-x: u0, accumulated-fees-y: u0 } 
                                   (map-get? pool-fees { pool-id: pool-id }))))
    
    (asserts! (get active pool-data) ERR-POOL-NOT-EXISTS)
    (asserts! (>= amount-x min-amount-x) ERR-SLIPPAGE-TOO-HIGH)
    (asserts! (> amount-y u0) ERR-ZERO-AMOUNT)
    (asserts! (> amount-x u0) ERR-INSUFFICIENT-LIQUIDITY)
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        reserve-x: (- (get reserve-x pool-data) amount-x),
        reserve-y: (+ (get reserve-y pool-data) amount-y),
        last-price: (calculate-price 
          (- (get reserve-x pool-data) amount-x)
          (+ (get reserve-y pool-data) amount-y))
      }))
    
    (map-set pool-fees
      { pool-id: pool-id }
      (merge pool-fees-data {
        accumulated-fees-y: (+ (get accumulated-fees-y pool-fees-data) fee-amount)
      }))
    
    (begin
      (if (needs-rebalancing pool-id)
          (unwrap-panic (auto-rebalance pool-id))
          true)
      (ok amount-x)))
)

(define-public (auto-rebalance (pool-id uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (current-price (calculate-price (get reserve-x pool-data) (get reserve-y pool-data)))
        (target-price (get target-ratio pool-data)))
    
    (asserts! (get active pool-data) ERR-POOL-NOT-EXISTS)
    (asserts! (needs-rebalancing pool-id) (ok false))
    
    (if (> current-price target-price)
        (let ((excess-x (/ (* (get reserve-x pool-data) (- current-price target-price)) (* u2 current-price)))
              (amount-y-out (get-amount-out excess-x (get reserve-x pool-data) (get reserve-y pool-data) u0)))
          (map-set pools
            { pool-id: pool-id }
            (merge pool-data {
              reserve-x: (- (get reserve-x pool-data) excess-x),
              reserve-y: (+ (get reserve-y pool-data) amount-y-out),
              last-price: target-price
            })))
        (let ((excess-y (/ (* (get reserve-y pool-data) (- target-price current-price)) (* u2 target-price)))
              (amount-x-out (get-amount-out excess-y (get reserve-y pool-data) (get reserve-x pool-data) u0)))
          (map-set pools
            { pool-id: pool-id }
            (merge pool-data {
              reserve-x: (+ (get reserve-x pool-data) amount-x-out),
              reserve-y: (- (get reserve-y pool-data) excess-y),
              last-price: target-price
            }))))
    
    (ok true))
)

(define-public (set-rebalance-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-threshold u1000) ERR-INVALID-AMOUNT)
    (var-set rebalance-threshold new-threshold)
    (ok true))
)

(define-public (set-protocol-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-rate u100) ERR-INVALID-AMOUNT)
    (var-set protocol-fee-rate new-rate)
    (ok true))
)

(define-public (deactivate-pool (pool-id uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set pools { pool-id: pool-id } (merge pool-data { active: false }))
    (ok true))
)

(define-public (reactivate-pool (pool-id uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set pools { pool-id: pool-id } (merge pool-data { active: true }))
    (ok true))
)

(define-public (activate-pool-rewards (pool-id uint) (reward-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? pools { pool-id: pool-id })) ERR-POOL-NOT-EXISTS)
    (asserts! (> reward-rate u0) ERR-INVALID-AMOUNT)
    
    (map-set pool-rewards
      { pool-id: pool-id }
      {
        reward-rate: reward-rate,
        total-staked: u0,
        reward-per-token: u0,
        last-update-block: stacks-block-height,
        active: true
      })
    (ok true))
)

(define-public (stake-lp-tokens (pool-id uint) (amount uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (user-position (unwrap! (map-get? user-positions { user: tx-sender, pool-id: pool-id }) ERR-INSUFFICIENT-LIQUIDITY))
        (pool-reward-data (unwrap! (map-get? pool-rewards { pool-id: pool-id }) ERR-REWARDS-NOT-ACTIVE))
        (current-stake (default-to 
          { staked-amount: u0, stake-start-block: u0, reward-debt: u0, unclaimed-rewards: u0 }
          (map-get? user-stakes { user: tx-sender, pool-id: pool-id }))))
    
    (asserts! (get active pool-reward-data) ERR-REWARDS-NOT-ACTIVE)
    (asserts! (>= (get lp-tokens user-position) amount) ERR-INSUFFICIENT-LIQUIDITY)
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    
    (update-pool-rewards pool-id)
    
    (let ((current-reward-per-token (calculate-reward-per-token pool-id))
          (pending-rewards (calculate-user-rewards tx-sender pool-id)))
      
      (map-set user-stakes
        { user: tx-sender, pool-id: pool-id }
        {
          staked-amount: (+ (get staked-amount current-stake) amount),
          stake-start-block: stacks-block-height,
          reward-debt: current-reward-per-token,
          unclaimed-rewards: pending-rewards
        })
      
      (map-set pool-rewards
        { pool-id: pool-id }
        (merge pool-reward-data {
          total-staked: (+ (get total-staked pool-reward-data) amount)
        }))
      
      (ok amount)))
)

(define-public (unstake-lp-tokens (pool-id uint) (amount uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (pool-reward-data (unwrap! (map-get? pool-rewards { pool-id: pool-id }) ERR-REWARDS-NOT-ACTIVE))
        (current-stake (unwrap! (map-get? user-stakes { user: tx-sender, pool-id: pool-id }) ERR-INSUFFICIENT-LIQUIDITY)))
    
    (asserts! (>= (get staked-amount current-stake) amount) ERR-INSUFFICIENT-LIQUIDITY)
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    
    (update-pool-rewards pool-id)
    
    (let ((current-reward-per-token (calculate-reward-per-token pool-id))
          (pending-rewards (calculate-user-rewards tx-sender pool-id)))
      
      (map-set user-stakes
        { user: tx-sender, pool-id: pool-id }
        {
          staked-amount: (- (get staked-amount current-stake) amount),
          stake-start-block: (get stake-start-block current-stake),
          reward-debt: current-reward-per-token,
          unclaimed-rewards: pending-rewards
        })
      
      (map-set pool-rewards
        { pool-id: pool-id }
        (merge pool-reward-data {
          total-staked: (- (get total-staked pool-reward-data) amount)
        }))
      
      (ok amount)))
)

(define-public (claim-rewards (pool-id uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (pool-reward-data (unwrap! (map-get? pool-rewards { pool-id: pool-id }) ERR-REWARDS-NOT-ACTIVE))
        (current-stake (unwrap! (map-get? user-stakes { user: tx-sender, pool-id: pool-id }) ERR-INSUFFICIENT-LIQUIDITY)))
    
    (asserts! (get active pool-reward-data) ERR-REWARDS-NOT-ACTIVE)
    
    (update-pool-rewards pool-id)
    
    (let ((rewards-to-claim (calculate-user-rewards tx-sender pool-id))
          (current-reward-per-token (calculate-reward-per-token pool-id)))
      
      (asserts! (> rewards-to-claim u0) ERR-NO-REWARDS)
      
      (map-set user-stakes
        { user: tx-sender, pool-id: pool-id }
        {
          staked-amount: (get staked-amount current-stake),
          stake-start-block: (get stake-start-block current-stake),
          reward-debt: current-reward-per-token,
          unclaimed-rewards: u0
        })
      
      (ok rewards-to-claim)))
)

(define-public (flash-loan (pool-id uint) (amount-x uint) (amount-y uint) (recipient principal))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (fee-x (/ (* amount-x (var-get flash-loan-fee-rate)) u10000))
        (fee-y (/ (* amount-y (var-get flash-loan-fee-rate)) u10000))
        (pool-fees-data (default-to { accumulated-fees-x: u0, accumulated-fees-y: u0 } 
                                   (map-get? pool-fees { pool-id: pool-id }))))
    
    (asserts! (get active pool-data) ERR-POOL-NOT-EXISTS)
    (asserts! (not (var-get flash-loan-in-progress)) ERR-FLASH-LOAN-ACTIVE)
    (asserts! (>= (get reserve-x pool-data) amount-x) ERR-INSUFFICIENT-LIQUIDITY)
    (asserts! (>= (get reserve-y pool-data) amount-y) ERR-INSUFFICIENT-LIQUIDITY)
    
    (var-set flash-loan-in-progress true)
    
    (map-set flash-loan-state
      { pool-id: pool-id }
      {
        borrowed-x: amount-x,
        borrowed-y: amount-y,
        borrower: tx-sender
      })
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        reserve-x: (- (get reserve-x pool-data) amount-x),
        reserve-y: (- (get reserve-y pool-data) amount-y)
      }))
    
    (ok { 
      amount-x: amount-x, 
      amount-y: amount-y, 
      fee-x: fee-x, 
      fee-y: fee-y 
    }))
)

(define-public (repay-flash-loan (pool-id uint))
  (let ((pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
        (loan-state (unwrap! (map-get? flash-loan-state { pool-id: pool-id }) ERR-INVALID-CALLBACK))
        (fee-x (/ (* (get borrowed-x loan-state) (var-get flash-loan-fee-rate)) u10000))
        (fee-y (/ (* (get borrowed-y loan-state) (var-get flash-loan-fee-rate)) u10000))
        (repay-amount-x (+ (get borrowed-x loan-state) fee-x))
        (repay-amount-y (+ (get borrowed-y loan-state) fee-y))
        (pool-fees-data (default-to { accumulated-fees-x: u0, accumulated-fees-y: u0 } 
                                   (map-get? pool-fees { pool-id: pool-id }))))
    
    (asserts! (var-get flash-loan-in-progress) ERR-INVALID-CALLBACK)
    (asserts! (is-eq (get borrower loan-state) tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        reserve-x: (+ (get reserve-x pool-data) repay-amount-x),
        reserve-y: (+ (get reserve-y pool-data) repay-amount-y)
      }))
    
    (map-set pool-fees
      { pool-id: pool-id }
      (merge pool-fees-data {
        accumulated-fees-x: (+ (get accumulated-fees-x pool-fees-data) fee-x),
        accumulated-fees-y: (+ (get accumulated-fees-y pool-fees-data) fee-y)
      }))
    
    (map-delete flash-loan-state { pool-id: pool-id })
    (var-set flash-loan-in-progress false)
    
    (ok true))
)

(define-public (set-flash-loan-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee-rate u100) ERR-INVALID-AMOUNT)
    (var-set flash-loan-fee-rate new-fee-rate)
    (ok true))
)

(define-read-only (get-pool (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

(define-read-only (get-user-position (user principal) (pool-id uint))
  (map-get? user-positions { user: user, pool-id: pool-id })
)

(define-read-only (get-pool-fees (pool-id uint))
  (map-get? pool-fees { pool-id: pool-id })
)

(define-read-only (get-quote-x-for-y (pool-id uint) (amount-x uint))
  (match (map-get? pools { pool-id: pool-id })
    pool-data (ok (get-amount-out amount-x (get reserve-x pool-data) (get reserve-y pool-data) (get fee-rate pool-data)))
    ERR-POOL-NOT-EXISTS)
)

(define-read-only (get-quote-y-for-x (pool-id uint) (amount-y uint))
  (match (map-get? pools { pool-id: pool-id })
    pool-data (ok (get-amount-out amount-y (get reserve-y pool-data) (get reserve-x pool-data) (get fee-rate pool-data)))
    ERR-POOL-NOT-EXISTS)
)

(define-read-only (get-total-pools)
  (var-get total-pools)
)

(define-read-only (get-rebalance-threshold)
  (var-get rebalance-threshold)
)

(define-read-only (check-rebalance-needed (pool-id uint))
  (needs-rebalancing pool-id)
)

(define-read-only (get-pool-rewards (pool-id uint))
  (map-get? pool-rewards { pool-id: pool-id })
)

(define-read-only (get-user-stake (user principal) (pool-id uint))
  (map-get? user-stakes { user: user, pool-id: pool-id })
)

(define-read-only (get-pending-rewards (user principal) (pool-id uint))
  (ok (calculate-user-rewards user pool-id))
)

(define-read-only (get-reward-rate (pool-id uint))
  (match (map-get? pool-rewards { pool-id: pool-id })
    reward-data (ok (get reward-rate reward-data))
    ERR-POOL-NOT-EXISTS)
)

(define-read-only (get-flash-loan-fee-rate)
  (var-get flash-loan-fee-rate)
)

(define-read-only (get-flash-loan-state (pool-id uint))
  (map-get? flash-loan-state { pool-id: pool-id })
)

(define-read-only (is-flash-loan-in-progress)
  (var-get flash-loan-in-progress)
)

(define-read-only (calculate-flash-loan-fee (amount uint))
  (/ (* amount (var-get flash-loan-fee-rate)) u10000)
)
