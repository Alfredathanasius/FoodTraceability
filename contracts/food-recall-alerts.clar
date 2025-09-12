;; Food Recall Alert System Contract
;; Automated monitoring and alert system for food safety incidents

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_ALERT_NOT_FOUND (err u401))
(define-constant ERR_BATCH_NOT_FOUND (err u402))
(define-constant ERR_INVALID_SEVERITY (err u403))
(define-constant ERR_STAKEHOLDER_NOT_FOUND (err u404))
(define-constant ERR_ALERT_ALREADY_RESOLVED (err u405))
(define-constant ERR_INVALID_ALERT_TYPE (err u406))
(define-constant ERR_NOTIFICATION_FAILED (err u407))

;; Alert severity levels
(define-constant SEVERITY_LOW u1)
(define-constant SEVERITY_MEDIUM u2)
(define-constant SEVERITY_HIGH u3)
(define-constant SEVERITY_CRITICAL u4)

;; Alert types
(define-constant ALERT_TEMPERATURE u1)
(define-constant ALERT_QUALITY_FAILURE u2)
(define-constant ALERT_COMPLAINT_PATTERN u3)
(define-constant ALERT_EXPIRY_WARNING u4)
(define-constant ALERT_CONTAMINATION u5)

;; Alert status
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_INVESTIGATING u2)
(define-constant STATUS_RESOLVED u3)
(define-constant STATUS_FALSE_ALARM u4)

;; Data variables
(define-data-var alert-counter uint u0)
(define-data-var total-stakeholders uint u0)
(define-data-var critical-alerts-count uint u0)

;; Alert registry
(define-map food-alerts
  uint
  {
    batch-id: uint,
    alert-type: uint,
    severity: uint,
    status: uint,
    triggered-by: principal,
    trigger-timestamp: uint,
    description: (string-ascii 200),
    affected-quantity: uint,
    resolution-deadline: uint,
    resolved-by: (optional principal),
    resolution-timestamp: (optional uint),
    resolution-notes: (string-ascii 300)
  }
)

;; Stakeholder registry for notifications
(define-map stakeholders
  principal
  {
    stakeholder-type: uint, ;; 1=Producer, 2=Distributor, 3=Retailer, 4=Authority
    contact-priority: uint, ;; 1=Immediate, 2=Urgent, 3=Standard
    notification-enabled: bool,
    last-notified: uint,
    total-alerts-received: uint,
    registration-date: uint
  }
)

;; Alert notifications log
(define-map alert-notifications
  { alert-id: uint, stakeholder: principal }
  {
    notification-timestamp: uint,
    delivery-method: (string-ascii 20),
    acknowledgment-required: bool,
    acknowledged: bool,
    acknowledgment-timestamp: (optional uint)
  }
)

;; Batch alert history for pattern analysis
(define-map batch-alert-history
  uint
  {
    total-alerts: uint,
    critical-alerts: uint,
    last-alert-date: uint,
    risk-score: uint,
    monitoring-active: bool
  }
)

;; Temperature violation tracking
(define-map temperature-violations
  { batch-id: uint, violation-id: uint }
  {
    recorded-temp: int,
    safe-range-min: int,
    safe-range-max: int,
    location: (string-ascii 50),
    duration-blocks: uint,
    violation-timestamp: uint,
    auto-alert-triggered: bool
  }
)

(define-data-var violation-counter uint u0)

;; Register stakeholder for alert notifications
(define-public (register-stakeholder 
  (stakeholder-type uint)
  (contact-priority uint))
  (begin
    (asserts! (and (>= stakeholder-type u1) (<= stakeholder-type u4)) ERR_UNAUTHORIZED)
    (asserts! (and (>= contact-priority u1) (<= contact-priority u3)) ERR_UNAUTHORIZED)
    
    (map-set stakeholders tx-sender
      {
        stakeholder-type: stakeholder-type,
        contact-priority: contact-priority,
        notification-enabled: true,
        last-notified: u0,
        total-alerts-received: u0,
        registration-date: stacks-block-height
      }
    )
    
    (var-set total-stakeholders (+ (var-get total-stakeholders) u1))
    (ok true)
  )
)

;; Trigger food safety alert
(define-public (trigger-alert 
  (batch-id uint)
  (alert-type uint)
  (severity uint)
  (description (string-ascii 200))
  (affected-quantity uint))
  (let
    (
      (alert-id (+ (var-get alert-counter) u1))
      (deadline-blocks (if (is-eq severity SEVERITY_CRITICAL) u72   ;; 12 hours for critical
                        (if (is-eq severity SEVERITY_HIGH) u288     ;; 2 days for high
                          (if (is-eq severity SEVERITY_MEDIUM) u720 ;; 5 days for medium
                            u1440))))                              ;; 10 days for low
      (resolution-deadline (+ stacks-block-height deadline-blocks))
    )
    (asserts! (and (>= alert-type ALERT_TEMPERATURE) (<= alert-type ALERT_CONTAMINATION)) ERR_INVALID_ALERT_TYPE)
    (asserts! (and (>= severity SEVERITY_LOW) (<= severity SEVERITY_CRITICAL)) ERR_INVALID_SEVERITY)
    (asserts! (> affected-quantity u0) ERR_INVALID_SEVERITY)
    
    (map-set food-alerts alert-id
      {
        batch-id: batch-id,
        alert-type: alert-type,
        severity: severity,
        status: STATUS_ACTIVE,
        triggered-by: tx-sender,
        trigger-timestamp: stacks-block-height,
        description: description,
        affected-quantity: affected-quantity,
        resolution-deadline: resolution-deadline,
        resolved-by: none,
        resolution-timestamp: none,
        resolution-notes: ""
      }
    )
    
    ;; Update batch alert history
    (let
      (
        (current-history (default-to 
          { total-alerts: u0, critical-alerts: u0, last-alert-date: u0, risk-score: u0, monitoring-active: true }
          (map-get? batch-alert-history batch-id)))
      )
      (map-set batch-alert-history batch-id
        {
          total-alerts: (+ (get total-alerts current-history) u1),
          critical-alerts: (+ (get critical-alerts current-history) (if (is-eq severity SEVERITY_CRITICAL) u1 u0)),
          last-alert-date: stacks-block-height,
          risk-score: (calculate-risk-score (+ (get total-alerts current-history) u1) severity),
          monitoring-active: true
        }
      )
    )
    
    ;; Update global critical alert count
    (if (is-eq severity SEVERITY_CRITICAL)
      (var-set critical-alerts-count (+ (var-get critical-alerts-count) u1))
      true
    )
    
    (var-set alert-counter alert-id)
    (ok alert-id)
  )
)

;; Log temperature violation and auto-trigger alert if critical
(define-public (log-temperature-violation
  (batch-id uint)
  (recorded-temp int)
  (safe-min int)
  (safe-max int)
  (location (string-ascii 50))
  (duration-blocks uint))
  (let
    (
      (violation-id (+ (var-get violation-counter) u1))
      (temp-outside-range (or (< recorded-temp safe-min) (> recorded-temp safe-max)))
      (is-critical (> duration-blocks u24)) ;; Critical if violation lasts more than 4 hours
    )
    (asserts! temp-outside-range ERR_INVALID_SEVERITY)
    
    (map-set temperature-violations { batch-id: batch-id, violation-id: violation-id }
      {
        recorded-temp: recorded-temp,
        safe-range-min: safe-min,
        safe-range-max: safe-max,
        location: location,
        duration-blocks: duration-blocks,
        violation-timestamp: stacks-block-height,
        auto-alert-triggered: false
      }
    )
    
    ;; Auto-trigger alert if critical temperature violation
    (if is-critical
      (begin
        (try! (trigger-alert batch-id ALERT_TEMPERATURE SEVERITY_HIGH 
               "Critical temperature violation detected" u1))
        (map-set temperature-violations { batch-id: batch-id, violation-id: violation-id }
          (merge (unwrap-panic (map-get? temperature-violations { batch-id: batch-id, violation-id: violation-id }))
                 { auto-alert-triggered: true }))
      )
      true
    )
    
    (var-set violation-counter violation-id)
    (ok violation-id)
  )
)

;; Resolve alert with resolution details
(define-public (resolve-alert 
  (alert-id uint)
  (resolution-notes (string-ascii 300)))
  (let
    (
      (alert (unwrap! (map-get? food-alerts alert-id) ERR_ALERT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq (get status alert) STATUS_RESOLVED)) ERR_ALERT_ALREADY_RESOLVED)
    
    (map-set food-alerts alert-id
      (merge alert {
        status: STATUS_RESOLVED,
        resolved-by: (some tx-sender),
        resolution-timestamp: (some stacks-block-height),
        resolution-notes: resolution-notes
      })
    )
    
    ;; Update critical alert count if was critical
    (if (is-eq (get severity alert) SEVERITY_CRITICAL)
      (var-set critical-alerts-count (- (var-get critical-alerts-count) u1))
      true
    )
    
    (ok true)
  )
)

;; Send alert notification to stakeholder
(define-public (send-alert-notification 
  (alert-id uint)
  (stakeholder principal)
  (delivery-method (string-ascii 20))
  (acknowledgment-required bool))
  (let
    (
      (alert (unwrap! (map-get? food-alerts alert-id) ERR_ALERT_NOT_FOUND))
      (stakeholder-data (unwrap! (map-get? stakeholders stakeholder) ERR_STAKEHOLDER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (get notification-enabled stakeholder-data) ERR_NOTIFICATION_FAILED)
    
    (map-set alert-notifications { alert-id: alert-id, stakeholder: stakeholder }
      {
        notification-timestamp: stacks-block-height,
        delivery-method: delivery-method,
        acknowledgment-required: acknowledgment-required,
        acknowledged: false,
        acknowledgment-timestamp: none
      }
    )
    
    ;; Update stakeholder notification count
    (map-set stakeholders stakeholder
      (merge stakeholder-data {
        last-notified: stacks-block-height,
        total-alerts-received: (+ (get total-alerts-received stakeholder-data) u1)
      })
    )
    
    (ok true)
  )
)

;; Acknowledge alert notification
(define-public (acknowledge-alert (alert-id uint))
  (let
    (
      (notification (unwrap! (map-get? alert-notifications { alert-id: alert-id, stakeholder: tx-sender }) ERR_ALERT_NOT_FOUND))
    )
    (asserts! (get acknowledgment-required notification) ERR_NOTIFICATION_FAILED)
    (asserts! (not (get acknowledged notification)) ERR_ALERT_ALREADY_RESOLVED)
    
    (map-set alert-notifications { alert-id: alert-id, stakeholder: tx-sender }
      (merge notification {
        acknowledged: true,
        acknowledgment-timestamp: (some stacks-block-height)
      })
    )
    
    (ok true)
  )
)

;; Calculate risk score based on alert history
(define-private (calculate-risk-score (total-alerts uint) (severity uint))
  (let
    (
      (base-score (* total-alerts u10))
      (severity-multiplier (if (is-eq severity SEVERITY_CRITICAL) u4
                           (if (is-eq severity SEVERITY_HIGH) u3
                             (if (is-eq severity SEVERITY_MEDIUM) u2 u1))))
      (risk-score (+ base-score (* severity u10)))
    )
    (if (> risk-score u100) u100 risk-score)
  )
)

;; Read-only functions

(define-read-only (get-alert-details (alert-id uint))
  (map-get? food-alerts alert-id)
)

(define-read-only (get-stakeholder-info (stakeholder principal))
  (map-get? stakeholders stakeholder)
)

(define-read-only (get-batch-alert-history (batch-id uint))
  (map-get? batch-alert-history batch-id)
)

(define-read-only (get-temperature-violation (batch-id uint) (violation-id uint))
  (map-get? temperature-violations { batch-id: batch-id, violation-id: violation-id })
)

(define-read-only (get-alert-notification (alert-id uint) (stakeholder principal))
  (map-get? alert-notifications { alert-id: alert-id, stakeholder: stakeholder })
)

(define-read-only (get-total-alerts)
  (var-get alert-counter)
)

(define-read-only (get-critical-alerts-count)
  (var-get critical-alerts-count)
)

(define-read-only (get-total-stakeholders)
  (var-get total-stakeholders)
)

(define-read-only (is-batch-high-risk (batch-id uint))
  (match (map-get? batch-alert-history batch-id)
    history (>= (get risk-score history) u75)
    false)
)

(define-read-only (get-active-alerts-by-severity (severity uint))
  (let
    (
      (total-alerts (var-get alert-counter))
    )
    ;; Simplified - would normally scan through all alerts
    (if (is-eq severity SEVERITY_CRITICAL)
      (var-get critical-alerts-count)
      u0)
  )
)

(define-read-only (calculate-batch-risk-level (batch-id uint))
  (let
    (
      (history (map-get? batch-alert-history batch-id))
    )
    (match history
      data (let
        (
          (risk-score (get risk-score data))
        )
        (if (>= risk-score u80) "HIGH"
          (if (>= risk-score u50) "MEDIUM"
            (if (>= risk-score u20) "LOW"
              "MINIMAL"))))
      "NO_DATA")
  )
)

(define-read-only (get-pending-acknowledgments (stakeholder principal))
  (let
    (
      (stakeholder-data (map-get? stakeholders stakeholder))
    )
    (match stakeholder-data
      data (get total-alerts-received data)
      u0)
  )
)
