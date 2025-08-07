;; Sustainability & Carbon Footprint Tracking Contract
;; Tracks environmental impact and sustainability metrics across food supply chain

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-invalid-data (err u302))
(define-constant err-unauthorized (err u303))

;; Activity types for carbon tracking
(define-constant ACTIVITY-PRODUCTION u1)
(define-constant ACTIVITY-TRANSPORT u2)
(define-constant ACTIVITY-STORAGE u3)
(define-constant ACTIVITY-PACKAGING u4)
(define-constant ACTIVITY-PROCESSING u5)

;; Sustainability rating levels
(define-constant RATING-EXCELLENT u5)
(define-constant RATING-GOOD u4)
(define-constant RATING-AVERAGE u3)
(define-constant RATING-POOR u2)
(define-constant RATING-CRITICAL u1)

;; Carbon footprint tracking per batch and activity
(define-map carbon-emissions
    { batch-id: uint, activity-type: uint, emission-id: uint }
    {
        co2-equivalent: uint, ;; in grams
        energy-consumed: uint, ;; in kWh
        fuel-type: (string-ascii 30),
        distance-km: uint,
        recorded-by: principal,
        timestamp: uint,
        verification-status: bool
    }
)

(define-data-var next-emission-id uint u1)

;; Water usage tracking
(define-map water-usage
    { batch-id: uint, stage: uint }
    {
        liters-used: uint,
        water-source: (string-ascii 50),
        recycled-percentage: uint,
        recorded-date: uint
    }
)

;; Renewable energy usage
(define-map renewable-energy
    { batch-id: uint, facility: (string-ascii 50) }
    {
        renewable-percentage: uint,
        energy-type: (string-ascii 30), ;; solar, wind, hydro, etc
        total-kwh: uint,
        cost-savings: uint,
        verification-date: uint
    }
)

;; Waste management tracking
(define-map waste-metrics
    { batch-id: uint, waste-type: (string-ascii 30) }
    {
        waste-generated: uint, ;; in kg
        waste-recycled: uint, ;; in kg
        waste-composted: uint, ;; in kg
        landfill-waste: uint, ;; in kg
        tracking-date: uint
    }
)

;; Sustainability certificates
(define-map sustainability-certificates
    { batch-id: uint, cert-type: (string-ascii 40) }
    {
        issuer: (string-ascii 60),
        certificate-id: (string-ascii 50),
        issue-date: uint,
        expiry-date: uint,
        verified: bool,
        scope: (string-ascii 100)
    }
)

;; Carbon offset programs
(define-map carbon-offsets
    { batch-id: uint, offset-id: uint }
    {
        offset-amount: uint, ;; in grams CO2
        offset-type: (string-ascii 40), ;; reforestation, renewable energy, etc
        cost-per-gram: uint,
        provider: (string-ascii 60),
        purchase-date: uint,
        verification-standard: (string-ascii 30)
    }
)

(define-data-var next-offset-id uint u1)

;; Sustainability scores per batch
(define-map sustainability-scores
    uint
    {
        carbon-score: uint, ;; 1-100
        water-efficiency-score: uint, ;; 1-100
        waste-management-score: uint, ;; 1-100
        renewable-energy-score: uint, ;; 1-100
        overall-rating: uint, ;; 1-5
        last-calculated: uint
    }
)

;; Authorized environmental auditors
(define-map authorized-auditors
    principal
    {
        certification-body: (string-ascii 60),
        audit-scope: (list 5 uint),
        active: bool,
        registration-date: uint
    }
)

;; Record carbon emissions for specific activities
(define-public (record-carbon-emission
    (batch-id uint)
    (activity-type uint)
    (co2-equivalent uint)
    (energy-consumed uint)
    (fuel-type (string-ascii 30))
    (distance-km uint))
    (let ((emission-id (var-get next-emission-id)))
        (asserts! (and (>= activity-type ACTIVITY-PRODUCTION) (<= activity-type ACTIVITY-PROCESSING)) err-invalid-data)
        (asserts! (> co2-equivalent u0) err-invalid-data)
        (map-set carbon-emissions 
            { batch-id: batch-id, activity-type: activity-type, emission-id: emission-id }
            {
                co2-equivalent: co2-equivalent,
                energy-consumed: energy-consumed,
                fuel-type: fuel-type,
                distance-km: distance-km,
                recorded-by: tx-sender,
                timestamp: stacks-block-height,
                verification-status: false
            })
        (var-set next-emission-id (+ emission-id u1))
        (unwrap! (update-sustainability-score batch-id) (err u304))
        (ok emission-id)
    )
)

;; Verify emission data by authorized auditor
(define-public (verify-emission
    (batch-id uint)
    (activity-type uint)
    (emission-id uint))
    (let ((auditor (unwrap! (map-get? authorized-auditors tx-sender) err-unauthorized))
          (emission (unwrap! (map-get? carbon-emissions { batch-id: batch-id, activity-type: activity-type, emission-id: emission-id }) err-not-found)))
        (asserts! (get active auditor) err-unauthorized)
        (asserts! (is-some (index-of (get audit-scope auditor) activity-type)) err-unauthorized)
        (map-set carbon-emissions { batch-id: batch-id, activity-type: activity-type, emission-id: emission-id }
            (merge emission { verification-status: true }))
        (ok true)
    )
)

;; Track water usage for batch
(define-public (record-water-usage
    (batch-id uint)
    (stage uint)
    (liters-used uint)
    (water-source (string-ascii 50))
    (recycled-percentage uint))
    (begin
        (asserts! (> liters-used u0) err-invalid-data)
        (asserts! (<= recycled-percentage u100) err-invalid-data)
        (map-set water-usage { batch-id: batch-id, stage: stage }
            {
                liters-used: liters-used,
                water-source: water-source,
                recycled-percentage: recycled-percentage,
                recorded-date: stacks-block-height
            })
        (unwrap! (update-sustainability-score batch-id) (err u304))
        (ok true)
    )
)

;; Record renewable energy usage
(define-public (record-renewable-energy
    (batch-id uint)
    (facility (string-ascii 50))
    (renewable-percentage uint)
    (energy-type (string-ascii 30))
    (total-kwh uint)
    (cost-savings uint))
    (begin
        (asserts! (<= renewable-percentage u100) err-invalid-data)
        (asserts! (> total-kwh u0) err-invalid-data)
        (map-set renewable-energy { batch-id: batch-id, facility: facility }
            {
                renewable-percentage: renewable-percentage,
                energy-type: energy-type,
                total-kwh: total-kwh,
                cost-savings: cost-savings,
                verification-date: stacks-block-height
            })
        (unwrap! (update-sustainability-score batch-id) (err u304))
        (ok true)
    )
)

;; Track waste management metrics
(define-public (record-waste-metrics
    (batch-id uint)
    (waste-type (string-ascii 30))
    (waste-generated uint)
    (waste-recycled uint)
    (waste-composted uint)
    (landfill-waste uint))
    (let ((total-managed (+ waste-recycled waste-composted)))
        (asserts! (>= waste-generated (+ total-managed landfill-waste)) err-invalid-data)
        (map-set waste-metrics { batch-id: batch-id, waste-type: waste-type }
            {
                waste-generated: waste-generated,
                waste-recycled: waste-recycled,
                waste-composted: waste-composted,
                landfill-waste: landfill-waste,
                tracking-date: stacks-block-height
            })
        (unwrap! (update-sustainability-score batch-id) (err u304))
        (ok true)
    )
)

;; Add sustainability certificate
(define-public (add-sustainability-certificate
    (batch-id uint)
    (cert-type (string-ascii 40))
    (issuer (string-ascii 60))
    (certificate-id (string-ascii 50))
    (expiry-date uint)
    (scope (string-ascii 100)))
    (begin
        (asserts! (> expiry-date stacks-block-height) err-invalid-data)
        (map-set sustainability-certificates { batch-id: batch-id, cert-type: cert-type }
            {
                issuer: issuer,
                certificate-id: certificate-id,
                issue-date: stacks-block-height,
                expiry-date: expiry-date,
                verified: false,
                scope: scope
            })
        (ok true)
    )
)

;; Purchase carbon offsets
(define-public (purchase-carbon-offset
    (batch-id uint)
    (offset-amount uint)
    (offset-type (string-ascii 40))
    (cost-per-gram uint)
    (provider (string-ascii 60))
    (verification-standard (string-ascii 30)))
    (let ((offset-id (var-get next-offset-id)))
        (asserts! (> offset-amount u0) err-invalid-data)
        (asserts! (> cost-per-gram u0) err-invalid-data)
        (map-set carbon-offsets { batch-id: batch-id, offset-id: offset-id }
            {
                offset-amount: offset-amount,
                offset-type: offset-type,
                cost-per-gram: cost-per-gram,
                provider: provider,
                purchase-date: stacks-block-height,
                verification-standard: verification-standard
            })
        (var-set next-offset-id (+ offset-id u1))
        (unwrap! (update-sustainability-score batch-id) (err u304))
        (ok offset-id)
    )
)

;; Register environmental auditor
(define-public (register-auditor
    (auditor principal)
    (certification-body (string-ascii 60))
    (audit-scope (list 5 uint)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-auditors auditor
            {
                certification-body: certification-body,
                audit-scope: audit-scope,
                active: true,
                registration-date: stacks-block-height
            })
        (ok true)
    )
)

;; Calculate and update sustainability score
(define-private (update-sustainability-score (batch-id uint))
    (let ((carbon-score (calculate-carbon-score batch-id))
          (water-score (calculate-water-score batch-id))
          (waste-score (calculate-waste-score batch-id))
          (energy-score (calculate-renewable-energy-score batch-id)))
        (let ((overall-score (/ (+ carbon-score water-score waste-score energy-score) u4))
              (rating (calculate-overall-rating overall-score)))
            (map-set sustainability-scores batch-id
                {
                    carbon-score: carbon-score,
                    water-efficiency-score: water-score,
                    waste-management-score: waste-score,
                    renewable-energy-score: energy-score,
                    overall-rating: rating,
                    last-calculated: stacks-block-height
                })
            (ok true)
        )
    )
)

;; Calculate carbon efficiency score (simplified scoring logic)
(define-private (calculate-carbon-score (batch-id uint))
    (let ((base-score u80)) ;; Default score, would need actual emission calculations
        (if (is-some (map-get? carbon-offsets { batch-id: batch-id, offset-id: u1 }))
            (+ base-score u15) ;; Bonus for offsets
            base-score)
    )
)

;; Calculate water efficiency score
(define-private (calculate-water-score (batch-id uint))
    (match (map-get? water-usage { batch-id: batch-id, stage: u1 })
        usage (if (>= (get recycled-percentage usage) u50) u90 u70)
        u60 ;; Default if no data
    )
)

;; Calculate waste management score
(define-private (calculate-waste-score (batch-id uint))
    (match (map-get? waste-metrics { batch-id: batch-id, waste-type: "general" })
        metrics (let ((total-waste (get waste-generated metrics))
                     (managed-waste (+ (get waste-recycled metrics) (get waste-composted metrics))))
                   (if (> total-waste u0)
                       (/ (* managed-waste u100) total-waste)
                       u50))
        u50 ;; Default if no data
    )
)

;; Calculate renewable energy score
(define-private (calculate-renewable-energy-score (batch-id uint))
    (match (map-get? renewable-energy { batch-id: batch-id, facility: "main" })
        energy (get renewable-percentage energy)
        u30 ;; Default if no renewable energy data
    )
)

;; Calculate overall sustainability rating
(define-private (calculate-overall-rating (score uint))
    (if (>= score u90) RATING-EXCELLENT
        (if (>= score u75) RATING-GOOD
            (if (>= score u60) RATING-AVERAGE
                (if (>= score u40) RATING-POOR
                    RATING-CRITICAL))))
)

;; Read-only functions

(define-read-only (get-carbon-emission (batch-id uint) (activity-type uint) (emission-id uint))
    (map-get? carbon-emissions { batch-id: batch-id, activity-type: activity-type, emission-id: emission-id })
)

(define-read-only (get-sustainability-score (batch-id uint))
    (map-get? sustainability-scores batch-id)
)

(define-read-only (get-carbon-footprint-total (batch-id uint))
    (let ((production-data (map-get? carbon-emissions { batch-id: batch-id, activity-type: ACTIVITY-PRODUCTION, emission-id: u1 }))
          (transport-data (map-get? carbon-emissions { batch-id: batch-id, activity-type: ACTIVITY-TRANSPORT, emission-id: u1 })))
        (let ((production-emission (match production-data emission (get co2-equivalent emission) u0))
              (transport-emission (match transport-data emission (get co2-equivalent emission) u0)))
            (ok (+ production-emission transport-emission))
        )
    )
)

(define-read-only (get-water-usage-data (batch-id uint) (stage uint))
    (map-get? water-usage { batch-id: batch-id, stage: stage })
)

(define-read-only (get-sustainability-certificate (batch-id uint) (cert-type (string-ascii 40)))
    (map-get? sustainability-certificates { batch-id: batch-id, cert-type: cert-type })
)

(define-read-only (get-carbon-offset (batch-id uint) (offset-id uint))
    (map-get? carbon-offsets { batch-id: batch-id, offset-id: offset-id })
)

(define-read-only (is-auditor-authorized (auditor principal))
    (match (map-get? authorized-auditors auditor)
        auth-data (get active auth-data)
        false
    )
)

