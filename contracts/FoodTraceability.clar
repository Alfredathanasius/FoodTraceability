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

