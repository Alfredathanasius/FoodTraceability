;; FoodTraceability Smart Contract
;; Enables food supply chain verification, origin tracking and quality certification

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u103))

;; Data Variables
(define-data-var next-batch-id uint u1)

;; Define status values for food batches
(define-constant STATUS-REGISTERED u1)
(define-constant STATUS-CERTIFIED u2)
(define-constant STATUS-RECALLED u3)

;; Data Maps
(define-map food-batches
    uint 
    {
        producer: principal,
        product-name: (string-ascii 50),
        origin: (string-ascii 50),
        production-date: uint,
        expiry-date: uint,
        status: uint,
        certification: (string-ascii 100)
    }
)

(define-map restaurant-subscriptions
    principal 
    {
        is-active: bool,
        subscription-date: uint,
        verification-count: uint
    }
)

;; Public Functions

;; Register new food batch
(define-public (register-food-batch 
    (product-name (string-ascii 50))
    (origin (string-ascii 50))
    (production-date uint)
    (expiry-date uint))
    (let
        ((batch-id (var-get next-batch-id)))
        (asserts! (> expiry-date production-date) (err u104))
        (map-set food-batches batch-id {
            producer: tx-sender,
            product-name: product-name,
            origin: origin,
            production-date: production-date,
            expiry-date: expiry-date,
            status: STATUS-REGISTERED,
            certification: ""
        })
        (var-set next-batch-id (+ batch-id u1))
        (ok batch-id)
    )
)

;; Certify food batch
(define-public (certify-batch 
    (batch-id uint)
    (certification-details (string-ascii 100)))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-eq (get status batch) STATUS-REGISTERED) err-invalid-status)
        (map-set food-batches batch-id (merge batch {
            status: STATUS-CERTIFIED,
            certification: certification-details
        }))
        (ok true)
    )
)

;; Recall food batch
(define-public (recall-batch (batch-id uint))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (get producer batch))) err-owner-only)
        (map-set food-batches batch-id (merge batch {
            status: STATUS-RECALLED
        }))
        (ok true)
    )
)

;; Restaurant subscription management
(define-public (subscribe-restaurant)
    (let ((existing-subscription (map-get? restaurant-subscriptions tx-sender)))
        (asserts! (is-none existing-subscription) err-already-exists)
        (map-set restaurant-subscriptions tx-sender {
            is-active: true,
            subscription-date: stacks-block-height,
            verification-count: u0
        })
        (ok true)
    )
)

;; Verify food batch
(define-public (verify-batch (batch-id uint))
    (let 
        ((subscription (unwrap! (map-get? restaurant-subscriptions tx-sender) err-not-found))
         (batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (asserts! (get is-active subscription) (err u105))
        (map-set restaurant-subscriptions tx-sender (merge subscription {
            verification-count: (+ (get verification-count subscription) u1)
        }))
        (ok {
            producer: (get producer batch),
            product-name: (get product-name batch),
            origin: (get origin batch),
            status: (get status batch),
            certification: (get certification batch)
        })
    )
)

;; Read-only Functions

;; Get batch details
(define-read-only (get-batch-details (batch-id uint))
    (map-get? food-batches batch-id)
)

;; Get restaurant subscription details
(define-read-only (get-restaurant-subscription (restaurant principal))
    (map-get? restaurant-subscriptions restaurant)
)

;; Get total registered batches
(define-read-only (get-total-batches)
    (- (var-get next-batch-id) u1)
)

;; Check if batch is certified
(define-read-only (is-batch-certified (batch-id uint))
    (match (map-get? food-batches batch-id)
        batch (is-eq (get status batch) STATUS-CERTIFIED)
        false
    )
)

(define-constant RATING-MIN u1)
(define-constant RATING-MAX u5)

(define-map batch-ratings
    { batch-id: uint, rater: principal }
    { rating: uint, timestamp: uint }
)

(define-public (rate-batch (batch-id uint) (rating uint))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (asserts! (>= rating RATING-MIN) (err u106))
        (asserts! (<= rating RATING-MAX) (err u107))
        (map-set batch-ratings { batch-id: batch-id, rater: tx-sender }
            { rating: rating, timestamp: stacks-block-height })
        (ok true)
    )
)

(define-read-only (get-batch-rating (batch-id uint) (rater principal))
    (map-get? batch-ratings { batch-id: batch-id, rater: rater })
)


(define-map temperature-logs
    { batch-id: uint, timestamp: uint }
    { temperature: int, location: (string-ascii 50) }
)

(define-public (log-temperature (batch-id uint) (temperature int) (location (string-ascii 50)))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (map-set temperature-logs 
            { batch-id: batch-id, timestamp: stacks-block-height }
            { temperature: temperature, location: location }
        )
        (ok true)
    )
)

(define-read-only (get-temperature-log (batch-id uint) (timestamp uint))
    (map-get? temperature-logs { batch-id: batch-id, timestamp: timestamp })
)


(define-constant STATUS-TRANSFERRED u4)

(define-map batch-transfers
    uint
    { from: principal, to: principal, transfer-date: uint }
)

(define-public (transfer-batch (batch-id uint) (recipient principal))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (asserts! (is-eq (get producer batch) tx-sender) err-owner-only)
        (map-set batch-transfers batch-id
            { from: tx-sender, to: recipient, transfer-date: stacks-block-height })
        (map-set food-batches batch-id (merge batch 
            { producer: recipient, status: STATUS-TRANSFERRED }))
        (ok true)
    )
)


(define-map storage-conditions
    uint
    { humidity: uint, light-exposure: uint, verified-at: uint }
)

(define-public (verify-storage-conditions (batch-id uint) (humidity uint) (light-exposure uint))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set storage-conditions batch-id
            { humidity: humidity, light-exposure: light-exposure, verified-at: stacks-block-height })
        (ok true)
    )
)


(define-map expiry-alerts
    uint
    { alert-threshold: uint, notified: bool }
)

(define-public (set-expiry-alert (batch-id uint) (alert-threshold uint))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (asserts! (is-eq (get producer batch) tx-sender) err-owner-only)
        (map-set expiry-alerts batch-id
            { alert-threshold: alert-threshold, notified: false })
        (ok true)
    )
)

(define-read-only (check-expiry-alert (batch-id uint))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found))
          (alert (unwrap! (map-get? expiry-alerts batch-id) (err u109))))
        (ok (>= stacks-block-height (- (get expiry-date batch) (get alert-threshold alert))))
    )
)


(define-map batch-photos
    { batch-id: uint, photo-id: uint }
    { photo-hash: (string-ascii 64), timestamp: uint, description: (string-ascii 100) }
)

(define-data-var next-photo-id uint u1)

(define-public (add-batch-photo 
    (batch-id uint) 
    (photo-hash (string-ascii 64)) 
    (description (string-ascii 100)))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found))
          (photo-id (var-get next-photo-id)))
        (map-set batch-photos 
            { batch-id: batch-id, photo-id: photo-id }
            { photo-hash: photo-hash, 
              timestamp: stacks-block-height, 
              description: description })
        (var-set next-photo-id (+ photo-id u1))
        (ok photo-id)
    )
)


(define-map product-categories
    (string-ascii 20)
    { description: (string-ascii 100), requirements: (string-ascii 200) }
)

(define-map batch-categories
    uint
    { category: (string-ascii 20), verified: bool }
)

(define-public (create-category 
    (category-name (string-ascii 20)) 
    (description (string-ascii 100))
    (requirements (string-ascii 200)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set product-categories category-name
            { description: description, requirements: requirements })
        (ok true)
    )
)

(define-public (assign-batch-category 
    (batch-id uint) 
    (category (string-ascii 20)))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (asserts! (is-some (map-get? product-categories category)) (err u111))
        (map-set batch-categories batch-id
            { category: category, verified: false })
        (ok true)
    )
)

(define-map quality-checkpoints
    { batch-id: uint, checkpoint-id: uint }
    { checkpoint-name: (string-ascii 50),
      passed: bool,
      inspector: principal,
      notes: (string-ascii 200) }
)

(define-data-var next-checkpoint-id uint u1)

(define-public (record-quality-checkpoint
    (batch-id uint)
    (checkpoint-name (string-ascii 50))
    (passed bool)
    (notes (string-ascii 200)))
    (let ((checkpoint-id (var-get next-checkpoint-id)))
        (map-set quality-checkpoints
            { batch-id: batch-id, checkpoint-id: checkpoint-id }
            { checkpoint-name: checkpoint-name,
              passed: passed,
              inspector: tx-sender,
              notes: notes })
        (var-set next-checkpoint-id (+ checkpoint-id u1))
        (ok checkpoint-id)
    )
)


(define-map weight-records
    uint
    { initial-weight: uint,
      current-weight: uint,
      last-updated: uint }
)

(define-public (record-batch-weight
    (batch-id uint)
    (weight uint))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found))
          (existing-record (map-get? weight-records batch-id)))
        (match existing-record
            prev-record (map-set weight-records batch-id
                { initial-weight: (get initial-weight prev-record),
                  current-weight: weight,
                  last-updated: stacks-block-height })
            (map-set weight-records batch-id
                { initial-weight: weight,
                  current-weight: weight,
                  last-updated: stacks-block-height }))
        (ok true)
    )
)


(define-map packaging-info
    uint
    { package-type: (string-ascii 50),
      materials: (list 5 (string-ascii 20)),
      recyclable: bool,
      package-date: uint }
)

(define-public (add-packaging-info
    (batch-id uint)
    (package-type (string-ascii 50))
    (materials (list 5 (string-ascii 20)))
    (recyclable bool))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (map-set packaging-info batch-id
            { package-type: package-type,
              materials: materials,
              recyclable: recyclable,
              package-date: stacks-block-height })
        (ok true)
    )
)