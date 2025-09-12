(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-price-invalid (err u200))
(define-constant err-margin-too-high (err u201))
(define-constant err-price-locked (err u202))

(define-constant PRICE-PRODUCER u1)
(define-constant PRICE-DISTRIBUTOR u2)
(define-constant PRICE-WHOLESALE u3)
(define-constant PRICE-RETAIL u4)

(define-constant MAX-MARGIN-PERCENT u50)

(define-map food-batches
    uint 
    {
        producer: principal,
        product-name: (string-ascii 50),
        origin: (string-ascii 50),
        production-date: uint,
        expiry-date: uint,
        status: uint
    }
)

(define-map batch-prices
    { batch-id: uint, price-level: uint }
    {
        price: uint,
        currency: (string-ascii 10),
        set-by: principal,
        timestamp: uint,
        locked: bool
    }
)

(define-map price-history
    { batch-id: uint, price-level: uint, history-id: uint }
    {
        old-price: uint,
        new-price: uint,
        changed-by: principal,
        change-timestamp: uint,
        reason: (string-ascii 100)
    }
)

(define-data-var next-history-id uint u1)

(define-map market-data
    (string-ascii 50)
    {
        average-price: uint,
        min-price: uint,
        max-price: uint,
        last-updated: uint,
        sample-count: uint
    }
)

(define-map authorized-price-setters
    principal
    {
        authorized-levels: (list 4 uint),
        active: bool
    }
)

(define-public (authorize-price-setter 
    (setter principal) 
    (levels (list 4 uint)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-price-setters setter {
            authorized-levels: levels,
            active: true
        })
        (ok true)
    )
)

(define-public (set-batch-price
    (batch-id uint)
    (price-level uint)
    (price uint)
    (currency (string-ascii 10)))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found))
          (setter-info (unwrap! (map-get? authorized-price-setters tx-sender) err-not-found))
          (existing-price (map-get? batch-prices { batch-id: batch-id, price-level: price-level })))
        (asserts! (get active setter-info) (err u203))
        (asserts! (is-some (index-of (get authorized-levels setter-info) price-level)) (err u204))
        (asserts! (> price u0) err-price-invalid)
        (match existing-price
            current-price (asserts! (not (get locked current-price)) err-price-locked)
            true)
        (if (is-some existing-price)
            (begin
                (unwrap! (record-price-change batch-id price-level 
                         (get price (unwrap-panic existing-price)) price "Price update") 
                         (err u205))
                (map-set batch-prices { batch-id: batch-id, price-level: price-level }
                    (merge (unwrap-panic existing-price) {
                        price: price,
                        set-by: tx-sender,
                        timestamp: stacks-block-height
                    })))
            (map-set batch-prices { batch-id: batch-id, price-level: price-level }
                {
                    price: price,
                    currency: currency,
                    set-by: tx-sender,
                    timestamp: stacks-block-height,
                    locked: false
                }))
        (unwrap! (update-market-data (get product-name batch) price) (err u206))
        (ok true)
    )
)

(define-public (verify-price-margin
    (batch-id uint)
    (lower-level uint)
    (upper-level uint))
    (let ((lower-price-data (unwrap! (map-get? batch-prices { batch-id: batch-id, price-level: lower-level }) err-not-found))
          (upper-price-data (unwrap! (map-get? batch-prices { batch-id: batch-id, price-level: upper-level }) err-not-found)))
        (let ((lower-price (get price lower-price-data))
              (upper-price (get price upper-price-data))
              (margin-percent (/ (* (- upper-price lower-price) u100) lower-price)))
            (asserts! (<= margin-percent MAX-MARGIN-PERCENT) err-margin-too-high)
            (ok margin-percent)
        )
    )
)

(define-public (lock-price
    (batch-id uint)
    (price-level uint))
    (let ((price-data (unwrap! (map-get? batch-prices { batch-id: batch-id, price-level: price-level }) err-not-found)))
        (asserts! (is-eq tx-sender (get set-by price-data)) err-owner-only)
        (map-set batch-prices { batch-id: batch-id, price-level: price-level }
            (merge price-data { locked: true }))
        (ok true)
    )
)

(define-private (record-price-change
    (batch-id uint)
    (price-level uint)
    (old-price uint)
    (new-price uint)
    (reason (string-ascii 100)))
    (let ((history-id (var-get next-history-id)))
        (map-set price-history 
            { batch-id: batch-id, price-level: price-level, history-id: history-id }
            {
                old-price: old-price,
                new-price: new-price,
                changed-by: tx-sender,
                change-timestamp: stacks-block-height,
                reason: reason
            })
        (var-set next-history-id (+ history-id u1))
        (ok true)
    )
)

(define-private (update-market-data (product-name (string-ascii 50)) (new-price uint))
    (let ((current-data (default-to
                        { average-price: new-price, min-price: new-price, 
                          max-price: new-price, last-updated: stacks-block-height, sample-count: u0 }
                        (map-get? market-data product-name))))
        (let ((sample-count (get sample-count current-data))
              (current-avg (get average-price current-data))
              (new-avg (/ (+ (* current-avg sample-count) new-price) (+ sample-count u1))))
            (map-set market-data product-name {
                average-price: new-avg,
                min-price: (if (< new-price (get min-price current-data)) new-price (get min-price current-data)),
                max-price: (if (> new-price (get max-price current-data)) new-price (get max-price current-data)),
                last-updated: stacks-block-height,
                sample-count: (+ sample-count u1)
            })
            (ok true)
        )
    )
)

(define-read-only (get-batch-price (batch-id uint) (price-level uint))
    (map-get? batch-prices { batch-id: batch-id, price-level: price-level })
)

(define-read-only (get-price-history (batch-id uint) (price-level uint) (history-id uint))
    (map-get? price-history { batch-id: batch-id, price-level: price-level, history-id: history-id })
)

(define-read-only (get-market-data (product-name (string-ascii 50)))
    (map-get? market-data product-name)
)

(define-read-only (calculate-total-margin (batch-id uint))
    (let ((producer-price (map-get? batch-prices { batch-id: batch-id, price-level: PRICE-PRODUCER }))
          (retail-price (map-get? batch-prices { batch-id: batch-id, price-level: PRICE-RETAIL })))
        (match producer-price
            p-price (match retail-price
                        r-price (let ((producer-cost (get price p-price))
                                     (retail-cost (get price r-price)))
                                    (ok (/ (* (- retail-cost producer-cost) u100) producer-cost)))
                        (err u207))
            (err u208))
    )
)

(define-read-only (get-price-transparency-report (batch-id uint))
    (let ((producer-price (map-get? batch-prices { batch-id: batch-id, price-level: PRICE-PRODUCER }))
          (distributor-price (map-get? batch-prices { batch-id: batch-id, price-level: PRICE-DISTRIBUTOR }))
          (wholesale-price (map-get? batch-prices { batch-id: batch-id, price-level: PRICE-WHOLESALE }))
          (retail-price (map-get? batch-prices { batch-id: batch-id, price-level: PRICE-RETAIL })))
        (ok {
            producer-price: producer-price,
            distributor-price: distributor-price,
            wholesale-price: wholesale-price,
            retail-price: retail-price
        })
    )
)

(define-read-only (is-price-competitive (batch-id uint) (price-level uint))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found))
          (price-data (unwrap! (map-get? batch-prices { batch-id: batch-id, price-level: price-level }) err-not-found))
          (market-info (unwrap! (map-get? market-data (get product-name batch)) err-not-found)))
        (let ((batch-price (get price price-data))
              (market-avg (get average-price market-info)))
            (ok (<= batch-price (+ market-avg (/ market-avg u10))))
        )
    )
)
