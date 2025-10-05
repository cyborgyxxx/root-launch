;; RootLaunch - Decentralized Carbon Credit Marketplace
;; A smart contract for time-locked carbon credit NFTs with multi-signature verification

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-verified (err u103))
(define-constant err-insufficient-verifications (err u104))
(define-constant err-project-not-active (err u105))
(define-constant err-milestone-locked (err u106))
(define-constant err-invalid-amount (err u107))

;; Data Variables
(define-data-var project-nonce uint u0)
(define-data-var credit-nonce uint u0)
(define-data-var required-verifications uint u3)

;; Project Status
(define-constant STATUS-PENDING u1)
(define-constant STATUS-ACTIVE u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-SUSPENDED u4)

;; Verification Types
(define-constant VERIFICATION-SATELLITE u1)
(define-constant VERIFICATION-IOT u2)
(define-constant VERIFICATION-COMMUNITY u3)

;; Data Maps
(define-map projects
  { project-id: uint }
  {
    owner: principal,
    name: (string-ascii 100),
    project-type: (string-ascii 50),
    total-credits: uint,
    issued-credits: uint,
    status: uint,
    confidence-score: uint,
    created-at: uint
  }
)

(define-map carbon-credits
  { credit-id: uint }
  {
    project-id: uint,
    owner: principal,
    amount: uint,
    milestone: uint,
    locked-until: uint,
    is-unlocked: bool,
    created-at: uint
  }
)

(define-map project-verifications
  { project-id: uint, verification-type: uint }
  {
    verified: bool,
    verifier: principal,
    verified-at: uint
  }
)

(define-map community-validators
  { validator: principal }
  {
    reputation-score: uint,
    verifications-count: uint,
    governance-tokens: uint,
    is-active: bool
  }
)

(define-map validator-project-verifications
  { validator: principal, project-id: uint }
  { verified: bool }
)

(define-map project-milestones
  { project-id: uint, milestone: uint }
  {
    description: (string-ascii 200),
    credits-to-unlock: uint,
    required-data: (string-ascii 100),
    achieved: bool,
    achieved-at: uint
  }
)

;; Read-only functions
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-carbon-credit (credit-id uint))
  (map-get? carbon-credits { credit-id: credit-id })
)

(define-read-only (get-validator (validator principal))
  (map-get? community-validators { validator: validator })
)

(define-read-only (get-project-verification (project-id uint) (verification-type uint))
  (map-get? project-verifications { project-id: project-id, verification-type: verification-type })
)

(define-read-only (get-milestone (project-id uint) (milestone uint))
  (map-get? project-milestones { project-id: project-id, milestone: milestone })
)

(define-read-only (count-project-verifications (project-id uint))
  (let
    (
      (satellite-verified (default-to false (get verified (map-get? project-verifications { project-id: project-id, verification-type: VERIFICATION-SATELLITE }))))
      (iot-verified (default-to false (get verified (map-get? project-verifications { project-id: project-id, verification-type: VERIFICATION-IOT }))))
      (community-verified (default-to false (get verified (map-get? project-verifications { project-id: project-id, verification-type: VERIFICATION-COMMUNITY }))))
    )
    (+ (+ (if satellite-verified u1 u0) (if iot-verified u1 u0)) (if community-verified u1 u0))
  )
)

(define-read-only (is-credit-unlockable (credit-id uint))
  (match (map-get? carbon-credits { credit-id: credit-id })
    credit
    (and
      (not (get is-unlocked credit))
      (>= block-height (get locked-until credit))
      (default-to false (get achieved (map-get? project-milestones 
        { project-id: (get project-id credit), milestone: (get milestone credit) })))
    )
    false
  )
)

;; Public functions

;; Register a new environmental project
(define-public (create-project (name (string-ascii 100)) (project-type (string-ascii 50)) (total-credits uint))
  (let
    (
      (new-project-id (+ (var-get project-nonce) u1))
    )
    (asserts! (> total-credits u0) err-invalid-amount)
    (map-set projects
      { project-id: new-project-id }
      {
        owner: tx-sender,
        name: name,
        project-type: project-type,
        total-credits: total-credits,
        issued-credits: u0,
        status: STATUS-PENDING,
        confidence-score: u0,
        created-at: block-height
      }
    )
    (var-set project-nonce new-project-id)
    (ok new-project-id)
  )
)

;; Register as a community validator
(define-public (register-validator)
  (begin
    (map-set community-validators
      { validator: tx-sender }
      {
        reputation-score: u100,
        verifications-count: u0,
        governance-tokens: u0,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Submit verification for a project
(define-public (submit-verification (project-id uint) (verification-type uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (validator (unwrap! (map-get? community-validators { validator: tx-sender }) err-unauthorized))
    )
    (asserts! (get is-active validator) err-unauthorized)
    (asserts! (or (is-eq verification-type VERIFICATION-SATELLITE)
                  (is-eq verification-type VERIFICATION-IOT)
                  (is-eq verification-type VERIFICATION-COMMUNITY)) err-unauthorized)
    
    ;; Record verification
    (map-set project-verifications
      { project-id: project-id, verification-type: verification-type }
      {
        verified: true,
        verifier: tx-sender,
        verified-at: block-height
      }
    )
    
    ;; Update validator stats
    (map-set community-validators
      { validator: tx-sender }
      (merge validator {
        verifications-count: (+ (get verifications-count validator) u1),
        governance-tokens: (+ (get governance-tokens validator) u10)
      })
    )
    
    ;; Check if project should be activated
    (if (>= (count-project-verifications project-id) (var-get required-verifications))
      (map-set projects
        { project-id: project-id }
        (merge project {
          status: STATUS-ACTIVE,
          confidence-score: u100
        })
      )
      true
    )
    
    (ok true)
  )
)

;; Create milestone for a project
(define-public (create-milestone 
  (project-id uint) 
  (milestone uint) 
  (description (string-ascii 200)) 
  (credits-to-unlock uint) 
  (required-data (string-ascii 100)))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    )
    (asserts! (is-eq (get owner project) tx-sender) err-unauthorized)
    (map-set project-milestones
      { project-id: project-id, milestone: milestone }
      {
        description: description,
        credits-to-unlock: credits-to-unlock,
        required-data: required-data,
        achieved: false,
        achieved-at: u0
      }
    )
    (ok true)
  )
)

;; Mark milestone as achieved (requires verification consensus)
(define-public (achieve-milestone (project-id uint) (milestone uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (milestone-data (unwrap! (map-get? project-milestones { project-id: project-id, milestone: milestone }) err-not-found))
    )
    (asserts! (is-eq (get status project) STATUS-ACTIVE) err-project-not-active)
    (asserts! (>= (count-project-verifications project-id) (var-get required-verifications)) err-insufficient-verifications)
    (asserts! (not (get achieved milestone-data)) err-already-verified)
    
    (map-set project-milestones
      { project-id: project-id, milestone: milestone }
      (merge milestone-data {
        achieved: true,
        achieved-at: block-height
      })
    )
    (ok true)
  )
)

;; Issue time-locked carbon credit NFTs
(define-public (issue-carbon-credit 
  (project-id uint) 
  (recipient principal) 
  (amount uint) 
  (milestone uint) 
  (lock-duration uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (new-credit-id (+ (var-get credit-nonce) u1))
      (new-issued-total (+ (get issued-credits project) amount))
    )
    (asserts! (is-eq (get owner project) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status project) STATUS-ACTIVE) err-project-not-active)
    (asserts! (<= new-issued-total (get total-credits project)) err-invalid-amount)
    
    ;; Create carbon credit NFT
    (map-set carbon-credits
      { credit-id: new-credit-id }
      {
        project-id: project-id,
        owner: recipient,
        amount: amount,
        milestone: milestone,
        locked-until: (+ block-height lock-duration),
        is-unlocked: false,
        created-at: block-height
      }
    )
    
    ;; Update project issued credits
    (map-set projects
      { project-id: project-id }
      (merge project {
        issued-credits: new-issued-total
      })
    )
    
    (var-set credit-nonce new-credit-id)
    (ok new-credit-id)
  )
)

;; Unlock carbon credits when milestone is achieved and time-lock expires
(define-public (unlock-carbon-credit (credit-id uint))
  (let
    (
      (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
      (milestone-achieved (default-to false (get achieved (map-get? project-milestones 
        { project-id: (get project-id credit), milestone: (get milestone credit) }))))
    )
    (asserts! (is-eq (get owner credit) tx-sender) err-unauthorized)
    (asserts! (not (get is-unlocked credit)) err-already-verified)
    (asserts! (>= block-height (get locked-until credit)) err-milestone-locked)
    (asserts! milestone-achieved err-insufficient-verifications)
    
    (map-set carbon-credits
      { credit-id: credit-id }
      (merge credit {
        is-unlocked: true
      })
    )
    (ok true)
  )
)

;; Transfer carbon credit ownership
(define-public (transfer-carbon-credit (credit-id uint) (new-owner principal))
  (let
    (
      (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
    )
    (asserts! (is-eq (get owner credit) tx-sender) err-unauthorized)
    (asserts! (get is-unlocked credit) err-milestone-locked)
    
    (map-set carbon-credits
      { credit-id: credit-id }
      (merge credit {
        owner: new-owner
      })
    )
    (ok true)
  )
)

;; Administrative function to update required verifications
(define-public (set-required-verifications (new-requirement uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set required-verifications new-requirement)
    (ok true)
  )
)

;; Suspend a project (admin only)
(define-public (suspend-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set projects
      { project-id: project-id }
      (merge project {
        status: STATUS-SUSPENDED
      })
    )
    (ok true)
  )
)