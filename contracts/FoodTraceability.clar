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


(define-constant INSPECTION-PASS u1)
(define-constant INSPECTION-FAIL u2)
(define-constant INSPECTION-PENDING u3)

(define-map authorized-inspectors 
    principal 
    { active: bool, certification-id: (string-ascii 50) }
)

(define-map inspection-reports
    { batch-id: uint, report-id: uint }
    { 
        inspector: principal,
        timestamp: uint,
        temperature: int,
        humidity: uint,
        contamination-check: bool,
        status: uint,
        notes: (string-ascii 200)
    }
)

(define-data-var next-report-id uint u1)

(define-public (register-inspector (certification-id (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-inspectors tx-sender {
            active: true,
            certification-id: certification-id
        })
        (ok true)
    )
)

(define-public (submit-inspection-report
    (batch-id uint)
    (temperature int)
    (humidity uint)
    (contamination-check bool)
    (status uint)
    (notes (string-ascii 200)))
    (let ((report-id (var-get next-report-id))
          (inspector-info (unwrap! (map-get? authorized-inspectors tx-sender) err-not-found)))
        (asserts! (get active inspector-info) (err u110))
        (map-set inspection-reports
            { batch-id: batch-id, report-id: report-id }
            {
                inspector: tx-sender,
                timestamp: stacks-block-height,
                temperature: temperature,
                humidity: humidity,
                contamination-check: contamination-check,
                status: status,
                notes: notes
            })
        (var-set next-report-id (+ report-id u1))
        (ok report-id)
    )
)


(define-constant DISTRIBUTOR-ROLE u1)
(define-constant WHOLESALER-ROLE u2)
(define-constant RETAILER-ROLE u3)

(define-map distribution-chain
    { batch-id: uint, step-id: uint }
    {
        handler: principal,
        role: uint,
        location: (string-ascii 50),
        timestamp: uint,
        temperature: int,
        handling-notes: (string-ascii 100)
    }
)

(define-data-var next-step-id uint u1)

(define-map authorized-handlers
    principal
    { role: uint, active: bool }
)

(define-public (register-handler (handler principal) (role uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-handlers handler {
            role: role,
            active: true
        })
        (ok true)
    )
)

(define-public (record-distribution-step
    (batch-id uint)
    (location (string-ascii 50))
    (temperature int)
    (handling-notes (string-ascii 100)))
    (let ((step-id (var-get next-step-id))
          (handler-info (unwrap! (map-get? authorized-handlers tx-sender) err-not-found)))
        (asserts! (get active handler-info) (err u112))
        (map-set distribution-chain
            { batch-id: batch-id, step-id: step-id }
            {
                handler: tx-sender,
                role: (get role handler-info),
                location: location,
                timestamp: stacks-block-height,
                temperature: temperature,
                handling-notes: handling-notes
            })
        (var-set next-step-id (+ step-id u1))
        (ok step-id)
    )
)


(define-public (get-distribution-history (batch-id uint))
    (let ((history (map-get? distribution-chain { batch-id: batch-id, step-id: u0 })))
        (ok history)
    )
)

(define-constant COMPLAINT-OPEN u1)
(define-constant COMPLAINT-INVESTIGATING u2)
(define-constant COMPLAINT-RESOLVED u3)
(define-constant COMPLAINT-REJECTED u4)

(define-constant SEVERITY-LOW u1)
(define-constant SEVERITY-MEDIUM u2)
(define-constant SEVERITY-HIGH u3)
(define-constant SEVERITY-CRITICAL u4)

(define-data-var next-complaint-id uint u1)

(define-map consumer-complaints
    uint
    {
        batch-id: uint,
        consumer: principal,
        complaint-type: (string-ascii 50),
        description: (string-ascii 300),
        severity: uint,
        status: uint,
        filed-date: uint,
        resolution-date: (optional uint),
        resolution-notes: (string-ascii 200),
        resolver: (optional principal)
    }
)

(define-map producer-reputation
    principal
    {
        total-complaints: uint,
        resolved-complaints: uint,
        average-resolution-time: uint,
        reputation-score: uint
    }
)

(define-map complaint-evidence
    { complaint-id: uint, evidence-id: uint }
    {
        evidence-type: (string-ascii 30),
        evidence-hash: (string-ascii 64),
        description: (string-ascii 100),
        uploaded-date: uint
    }
)

(define-data-var next-evidence-id uint u1)

(define-public (file-complaint
    (batch-id uint)
    (complaint-type (string-ascii 50))
    (description (string-ascii 300))
    (severity uint))
    (let ((complaint-id (var-get next-complaint-id))
          (batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (asserts! (>= severity SEVERITY-LOW) (err u113))
        (asserts! (<= severity SEVERITY-CRITICAL) (err u114))
        (map-set consumer-complaints complaint-id
            {
                batch-id: batch-id,
                consumer: tx-sender,
                complaint-type: complaint-type,
                description: description,
                severity: severity,
                status: COMPLAINT-OPEN,
                filed-date: stacks-block-height,
                resolution-date: none,
                resolution-notes: "",
                resolver: none
            })
        (unwrap! (update-producer-complaint-count (get producer batch)) (err u117))
        (var-set next-complaint-id (+ complaint-id u1))
        (ok complaint-id)
    )
)

(define-public (update-complaint-status
    (complaint-id uint)
    (new-status uint)
    (resolution-notes (string-ascii 200)))
    (let ((complaint (unwrap! (map-get? consumer-complaints complaint-id) err-not-found))
          (batch (unwrap! (map-get? food-batches (get batch-id complaint)) err-not-found)))
        (asserts! (or (is-eq tx-sender contract-owner) 
                     (is-eq tx-sender (get producer batch))) err-owner-only)
        (asserts! (>= new-status COMPLAINT-OPEN) (err u115))
        (asserts! (<= new-status COMPLAINT-REJECTED) (err u116))
        (map-set consumer-complaints complaint-id
            (merge complaint {
                status: new-status,
                resolution-date: (if (or (is-eq new-status COMPLAINT-RESOLVED)
                                        (is-eq new-status COMPLAINT-REJECTED))
                                    (some stacks-block-height)
                                    none),
                resolution-notes: resolution-notes,
                resolver: (some tx-sender)
            }))
        (if (is-eq new-status COMPLAINT-RESOLVED)
            (update-producer-resolved-count (get producer batch))
            (ok true))
    )
)

(define-public (add-complaint-evidence
    (complaint-id uint)
    (evidence-type (string-ascii 30))
    (evidence-hash (string-ascii 64))
    (description (string-ascii 100)))
    (let ((complaint (unwrap! (map-get? consumer-complaints complaint-id) err-not-found))
          (evidence-id (var-get next-evidence-id)))
        (asserts! (is-eq tx-sender (get consumer complaint)) err-owner-only)
        (map-set complaint-evidence
            { complaint-id: complaint-id, evidence-id: evidence-id }
            {
                evidence-type: evidence-type,
                evidence-hash: evidence-hash,
                description: description,
                uploaded-date: stacks-block-height
            })
        (var-set next-evidence-id (+ evidence-id u1))
        (ok evidence-id)
    )
)

(define-private (update-producer-complaint-count (producer principal))
    (let ((current-rep (default-to 
                        { total-complaints: u0, resolved-complaints: u0, 
                          average-resolution-time: u0, reputation-score: u100 }
                        (map-get? producer-reputation producer))))
        (map-set producer-reputation producer
            (merge current-rep {
                total-complaints: (+ (get total-complaints current-rep) u1),
                reputation-score: (calculate-reputation-score 
                                  (+ (get total-complaints current-rep) u1)
                                  (get resolved-complaints current-rep))
            }))
        (ok true)
    )
)

(define-private (update-producer-resolved-count (producer principal))
    (let ((current-rep (unwrap! (map-get? producer-reputation producer) err-not-found)))
        (map-set producer-reputation producer
            (merge current-rep {
                resolved-complaints: (+ (get resolved-complaints current-rep) u1),
                reputation-score: (calculate-reputation-score 
                                  (get total-complaints current-rep)
                                  (+ (get resolved-complaints current-rep) u1))
            }))
        (ok true)
    )
)

(define-private (calculate-reputation-score (total uint) (resolved uint))
    (if (is-eq total u0)
        u100
        (/ (* resolved u100) total)
    )
)

(define-read-only (get-complaint-details (complaint-id uint))
    (map-get? consumer-complaints complaint-id)
)

(define-read-only (get-producer-reputation (producer principal))
    (map-get? producer-reputation producer)
)

(define-read-only (get-complaint-evidence (complaint-id uint) (evidence-id uint))
    (map-get? complaint-evidence { complaint-id: complaint-id, evidence-id: evidence-id })
)

(define-read-only (get-batch-complaints-count (batch-id uint))
    (let ((batch (unwrap! (map-get? food-batches batch-id) err-not-found)))
        (match (map-get? producer-reputation (get producer batch))
            rep (ok (get total-complaints rep))
            (ok u0)
        )
    )
)

(define-read-only (is-producer-reliable (producer principal))
    (match (map-get? producer-reputation producer)
        rep (>= (get reputation-score rep) u80)
        true
    )
)