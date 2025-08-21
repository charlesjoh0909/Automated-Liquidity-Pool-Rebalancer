(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-POOL-NOT-EXISTS (err u103))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-ZERO-AMOUNT (err u106))

(define-data-var next-pool-id uint u0)
(define-data-var total-pools uint u0)
(define-data-var protocol-fee-rate uint u30)
(define-data-var rebalance-threshold uint u500)

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
