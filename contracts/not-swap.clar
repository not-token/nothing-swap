(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)

;; lp token definition
(define-fungible-token not-lp-tokens)

;; error constants
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-SLIPPAGE (err u501))
(define-constant ERR-YOU-POOR (err u600))

;; constants
(define-constant liq-creator-address 'SP3QZF1M1294FX796Q3QD4JR2AV0YX9XW2PMWBA45)
(define-constant liq-providers-fee u20)
(define-constant burn-fee u10)

;; data vars liquidity
(define-data-var is-liq-created bool false)
(define-data-var liquidity-stx-reserve uint u0)
(define-data-var liquidity-not-reserve uint u0)


;; read only functions 
(define-read-only (get-balance (user principal))
  (ok (ft-get-balance not-lp-tokens user)))

(define-read-only (get-decimals)
  (ok u6))

(define-read-only (get-name)
  (ok "not-lp"))

(define-read-only (get-symbol)
  (ok "NOTLP"))

(define-read-only (get-total-supply)
  (ok (ft-get-supply not-lp-tokens)))

(define-read-only (get-token-uri)
  (ok none))

(define-read-only (get-liq-reserves)
  (ok {
    liq-stx-reserve: (var-get liquidity-stx-reserve),
    liq-not-reserve: (var-get liquidity-not-reserve),
    liq-token-supply: (get-total-supply)
  })
)

;; public functions
(define-public (transfer (amount uint) (from principal) (to principal) (memo (optional (buff 34))))
    (begin
        (asserts! (or (is-eq from tx-sender) (is-eq from contract-caller)) ERR-UNAUTHORIZED)
        (ft-transfer? not-lp-tokens amount from to)
    )
)

(define-public (send-many (recipients (list 2000 { to: principal, amount: uint, memo: (optional (buff 34)) })))
  (fold check-err (map send-token recipients) (ok true))
)

(define-public (add-liquidity (stx-deposit uint) (not-deposit uint) (min-lp-out uint))
  (begin
    (asserts! (> stx-deposit u0) ERR-YOU-POOR)
    (asserts! (> not-deposit u0) ERR-YOU-POOR)
    (let (
      (current-stx-reserve (var-get liquidity-stx-reserve))
      (current-not-reserve (var-get liquidity-not-reserve))
      (tot-liquidity (ft-get-supply not-lp-tokens))
      (lp-out-stx (/ (* stx-deposit tot-liquidity) current-stx-reserve))
      (lp-out-not (/ (* not-deposit tot-liquidity) current-not-reserve))
      (liquidity-to-mint (if (>= lp-out-stx lp-out-not) lp-out-not lp-out-stx))
    )
      ;; Check slippage
      (asserts! (>= liquidity-to-mint min-lp-out) ERR-SLIPPAGE)
      
      ;; Transfer STX and NOT to the contract
      (try! (stx-transfer? stx-deposit tx-sender (as-contract tx-sender)))
      (try! (contract-call? 'SP32AEEF6WW5Y0NMJ1S8SBSZDAY8R5J32NBZFPKKZ.nope transfer not-deposit tx-sender (as-contract tx-sender) none))
      
      ;; Update reserves and mint LP tokens
      (var-set liquidity-stx-reserve (+ current-stx-reserve stx-deposit))
      (var-set liquidity-not-reserve (+ current-not-reserve not-deposit))
      
      ;; Mint LP tokens to the sender
      (try! (ft-mint? not-lp-tokens liquidity-to-mint tx-sender))
      (ok true)
    )
  )
)

(define-public (remove-liquidity (lp-token-amount uint) (min-stx-out uint) (min-not-out uint) (recipient principal))
    (let (
      (current-stx-reserve (var-get liquidity-stx-reserve))
      (current-not-reserve (var-get liquidity-not-reserve))
      (lp-tokens lp-token-amount)
      (tot-liquidity (ft-get-supply not-lp-tokens))
      (stx-amount (/ (* lp-tokens current-stx-reserve) tot-liquidity))
      (not-amount (/ (* lp-tokens current-not-reserve) tot-liquidity))
    )
      (asserts! (or (is-eq tx-sender recipient) (is-eq contract-caller recipient)) ERR-UNAUTHORIZED)
      (asserts! (> lp-token-amount u0) ERR-YOU-POOR)
      ;; Ensure withdrawals are ok with slippage
      (asserts! (and
                 (>= stx-amount min-stx-out)
                 (>= not-amount min-not-out))
               ERR-SLIPPAGE)
      
      ;; Burn LP tokens from the sender
      (try! (ft-burn? not-lp-tokens lp-tokens tx-sender))
      
      ;; Transfer STX and NOT to the sender
      (try! (as-contract (stx-transfer? stx-amount tx-sender recipient)))
      (try! (as-contract (contract-call? 'SP32AEEF6WW5Y0NMJ1S8SBSZDAY8R5J32NBZFPKKZ.nope transfer not-amount tx-sender recipient none)))

      
      ;; Update reserves
      (var-set liquidity-stx-reserve (- current-stx-reserve stx-amount))
      (var-set liquidity-not-reserve (- current-not-reserve not-amount))
      
      (ok {stx-amount: stx-amount, not-amount: not-amount})
    )
  )

(define-public (swap-stx-for-not (stx-in uint) (min-not-out uint))
    (let (
      (current-stx-reserve (var-get liquidity-stx-reserve))
      (current-not-reserve (var-get liquidity-not-reserve))
      (new-stx-reserve (+ current-stx-reserve stx-in))
      (k (* current-stx-reserve current-not-reserve))
      (new-not-reserve (/ k new-stx-reserve))
      (not-out (- current-not-reserve new-not-reserve))
      (not-liq-fee (/ (* not-out liq-providers-fee) u10000))
      (not-burn-fee (/ (* not-out burn-fee) u10000))
      (not-out-after-fee (- not-out not-liq-fee not-burn-fee))
      (user tx-sender))
      (asserts! (> stx-in u0) ERR-YOU-POOR)
      ;; Check slippage
      (asserts! (>= not-out-after-fee min-not-out) ERR-SLIPPAGE)
      
      ;; Transfer STX to the contract
      (try! (stx-transfer? stx-in tx-sender (as-contract tx-sender)))

      ;; Transfer NOT to the sender and retain not-liq-fee in the contract (liq providers rewards)
      (try! (as-contract (contract-call? 'SP32AEEF6WW5Y0NMJ1S8SBSZDAY8R5J32NBZFPKKZ.nope transfer not-out-after-fee tx-sender user none)))

      ;; Burn NOT fee
      (try! (contract-call? 'SPEMB0KQRD7PWKY2W2J2Y1Y6Q9YBJ702DWQADE7V.not-incinerator-v3 burn-nothing not-burn-fee))
      
      ;; Update reserves
      (var-set liquidity-stx-reserve new-stx-reserve)
      (var-set liquidity-not-reserve (+ new-not-reserve not-liq-fee))
    
      (ok not-out-after-fee)
    )
)

(define-public (swap-not-for-stx (not-in uint) (min-stx-out uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender recipient) ERR-UNAUTHORIZED)
    (let (
      (current-stx-reserve (var-get liquidity-stx-reserve))
      (current-not-reserve (var-get liquidity-not-reserve))
      (not-liq-fee (/ (* not-in liq-providers-fee) u10000))
      (not-burn-fee (/ (* not-in burn-fee) u10000))
      (not-in-after-fee (- not-in (+ not-liq-fee not-burn-fee)))
      (new-not-reserve (+ current-not-reserve not-in-after-fee))
      (k (* current-stx-reserve current-not-reserve))
      (new-stx-reserve (/ k new-not-reserve))
      (stx-out (- current-stx-reserve new-stx-reserve))
    )
      ;; Check slippage
      (asserts! (>= stx-out min-stx-out) ERR-SLIPPAGE)
      
      ;; Transfer NOT to the contract
      (try! (contract-call? 'SP32AEEF6WW5Y0NMJ1S8SBSZDAY8R5J32NBZFPKKZ.nope transfer not-in tx-sender (as-contract tx-sender) none))

      ;; Burn NOT fee
      (try! (contract-call? 'SPEMB0KQRD7PWKY2W2J2Y1Y6Q9YBJ702DWQADE7V.not-incinerator-v3 burn-nothing not-burn-fee))
      
      
      ;; Update reserves
      (var-set liquidity-not-reserve (+ new-not-reserve not-liq-fee))
      (var-set liquidity-stx-reserve new-stx-reserve)
      
      ;; Transfer STX to the sender (calculated after removing fees in NOT swapped in)
      (try! (as-contract (stx-transfer? stx-out tx-sender recipient)))
      (ok stx-out)
    )
  )
)

(define-public (create-initial-liquidity (stx-amount uint) (not-amount uint))
  (begin
    (asserts! (is-eq tx-sender liq-creator-address) ERR-UNAUTHORIZED)
    (asserts! (not (var-get is-liq-created)) ERR-UNAUTHORIZED)
    (asserts! (> not-amount u0) ERR-YOU-POOR)
    (asserts! (> stx-amount u0) ERR-YOU-POOR)
    (let (
      (initial-stx-reserve stx-amount)
      (initial-not-reserve not-amount)
    )
      ;; Set initial liquidity reserves
      (var-set liquidity-stx-reserve initial-stx-reserve)
      (var-set liquidity-not-reserve initial-not-reserve)
      
      ;; Mint initial LP tokens equal to the geometric mean of the reserves
      (let ((initial-liquidity (sqrti (* initial-stx-reserve initial-not-reserve))))
        
        ;; Mint LP tokens to burn address
        (try! (ft-mint? not-lp-tokens initial-liquidity 'SP000000000000000000002Q6VF78))
        (var-set is-liq-created true)
        (ok initial-liquidity)
      )
    )
  )
)


;; private functions

(define-private (check-err (result (response bool uint)) (prior (response bool uint)))
  (match prior ok-value result err-value (err err-value))
)

(define-private (send-token (recipient { to: principal, amount: uint, memo: (optional (buff 34)) }))
  (transfer (get amount recipient) tx-sender (get to recipient) (get memo recipient))
)
