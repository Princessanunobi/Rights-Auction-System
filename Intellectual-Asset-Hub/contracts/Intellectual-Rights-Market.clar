;; Digital IP Rights Marketplace Smart Contract
;; A comprehensive marketplace for auctioning and trading intellectual property rights
;; including patents, trademarks, copyrights, and other digital assets with automated
;; bidding, secure transfers, and transparent ownership tracking

;; ERROR CONSTANTS
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-AUCTION-NOT-EXISTS (err u101))
(define-constant ERR-AUCTION-ALREADY-ENDED (err u102))
(define-constant ERR-AUCTION-STILL-ACTIVE (err u103))
(define-constant ERR-INVALID-BID-AMOUNT (err u104))
(define-constant ERR-INTELLECTUAL-ASSET-NOT-FOUND (err u105))
(define-constant ERR-ALREADY-PROCESSED (err u106))
(define-constant ERR-NO-VALID-BIDS (err u107))
(define-constant ERR-INVALID-AUCTION-DURATION (err u108))
(define-constant ERR-INVALID-RESERVE-PRICE (err u109))
(define-constant ERR-PAYMENT-TRANSFER-FAILED (err u110))
(define-constant ERR-EMPTY-TITLE-PROVIDED (err u111))
(define-constant ERR-EMPTY-DESCRIPTION-PROVIDED (err u112))
(define-constant ERR-FEE-RATE-TOO-HIGH (err u113))
(define-constant ERR-INVALID-IP-TYPE (err u114))
(define-constant ERR-INVALID-ASSET-ID (err u115))

;; CONTRACT CONFIGURATION
(define-constant contract-deployer tx-sender)
(define-constant maximum-fee-rate u1000) ;; 10% maximum platform fee
(define-constant minimum-auction-duration u144) ;; ~24 hours in blocks
(define-constant basis-points-divisor u10000)
(define-constant max-asset-id u1000000) ;; Maximum allowed asset ID

;; Valid IP types - using string-ascii 50 to match function parameter
(define-constant valid-ip-types 
    (list 
        "patent"
        "trademark"
        "copyright"
        "trade-secret"
        "design"
        "software"
        "music"
        "art"
        "other"
    )
)

;; DATA STORAGE VARIABLES
(define-data-var auction-counter uint u1)
(define-data-var intellectual-asset-counter uint u1)
(define-data-var marketplace-fee-percentage uint u250) ;; 2.5% in basis points
(define-data-var total-assets-registered uint u0)
(define-data-var total-auctions-created uint u0)

;; DATA STRUCTURES
;; Intellectual Property Asset Registry
(define-map intellectual-property-registry
    { intellectual-asset-identifier: uint }
    {
        current-owner: principal,
        asset-title: (string-ascii 100),
        detailed-description: (string-utf8 500),
        intellectual-property-type: (string-ascii 50),
        metadata-location: (optional (string-ascii 200)),
        registration-block-height: uint,
        asset-status-active: bool,
        transfer-count: uint
    }
)

;; Auction Marketplace Listings
(define-map auction-marketplace-listings
    { auction-identifier: uint }
    {
        intellectual-asset-identifier: uint,
        asset-seller: principal,
        minimum-reserve-price: uint,
        highest-bid-amount: uint,
        leading-bidder: (optional principal),
        auction-start-block: uint,
        auction-end-block: uint,
        listing-status-active: bool,
        settlement-completed: bool,
        total-bid-count: uint
    }
)

;; Bidding Activity Records
(define-map bidding-activity-log
    { auction-identifier: uint, bidder-address: principal }
    {
        total-bid-amount: uint,
        bid-placement-block: uint,
        refund-processed: bool,
        bid-sequence-number: uint
    }
)

;; User Bid Tracking for Refunds
(define-map user-active-bids
    { auction-identifier: uint, participant-address: principal }
    { committed-bid-amount: uint }
)

;; Asset Ownership History
(define-map ownership-transfer-history
    { intellectual-asset-identifier: uint, transfer-sequence: uint }
    {
        previous-owner: principal,
        new-owner: principal,
        associated-auction: (optional uint),
        transfer-block-height: uint,
        transfer-method: (string-ascii 20)
    }
)

;; Marketplace Statistics
(define-map marketplace-statistics
    { statistic-type: (string-ascii 20) }
    {
        total-value: uint,
        last-updated: uint
    }
)

;; VALIDATION HELPER FUNCTIONS
(define-private (validate-ip-type (ip-type (string-ascii 50)))
    (or
        (is-eq ip-type "patent")
        (is-eq ip-type "trademark")
        (is-eq ip-type "copyright")
        (is-eq ip-type "trade-secret")
        (is-eq ip-type "design")
        (is-eq ip-type "software")
        (is-eq ip-type "music")
        (is-eq ip-type "art")
        (is-eq ip-type "other")
    )
)

(define-private (validate-asset-id (asset-id uint))
    (and (> asset-id u0) (<= asset-id max-asset-id))
)

(define-private (validate-auction-id (auction-id uint))
    (and (> auction-id u0) (< auction-id (var-get auction-counter)))
)

(define-private (validate-metadata-location (metadata (optional (string-ascii 200))))
    (match metadata
        location (> (len location) u0)
        true ;; None is valid
    )
)

(define-private (validate-principal (addr principal))
    (not (is-eq addr 'ST000000000000000000002AMW42H)) ;; Not burn address
)

;; READ-ONLY QUERY FUNCTIONS
(define-read-only (get-intellectual-asset-details (intellectual-asset-identifier uint))
    (map-get? intellectual-property-registry { intellectual-asset-identifier: intellectual-asset-identifier })
)

(define-read-only (get-auction-listing-details (auction-identifier uint))
    (map-get? auction-marketplace-listings { auction-identifier: auction-identifier })
)

(define-read-only (get-current-block-height)
    stacks-block-height
)

(define-read-only (check-auction-active-status (auction-identifier uint))
    (match (map-get? auction-marketplace-listings { auction-identifier: auction-identifier })
        auction-listing-data 
            (and 
                (get listing-status-active auction-listing-data)
                (<= stacks-block-height (get auction-end-block auction-listing-data))
                (>= stacks-block-height (get auction-start-block auction-listing-data))
            )
        false
    )
)

(define-read-only (get-user-bid-information (auction-identifier uint) (participant-address principal))
    (map-get? user-active-bids { auction-identifier: auction-identifier, participant-address: participant-address })
)

(define-read-only (get-bidding-history-record (auction-identifier uint) (participant-address principal))
    (map-get? bidding-activity-log { auction-identifier: auction-identifier, bidder-address: participant-address })
)

(define-read-only (get-marketplace-fee-rate)
    (var-get marketplace-fee-percentage)
)

(define-read-only (calculate-marketplace-fee (transaction-amount uint))
    (/ (* transaction-amount (var-get marketplace-fee-percentage)) basis-points-divisor)
)

(define-read-only (get-marketplace-statistics)
    {
        total-registered-assets: (var-get total-assets-registered),
        total-created-auctions: (var-get total-auctions-created),
        current-fee-rate: (var-get marketplace-fee-percentage)
    }
)

(define-read-only (get-asset-ownership-history (intellectual-asset-identifier uint))
    (map-get? ownership-transfer-history { intellectual-asset-identifier: intellectual-asset-identifier, transfer-sequence: u1 })
)

;; ASSET MANAGEMENT FUNCTIONS
(define-public (register-intellectual-property 
    (asset-title (string-ascii 100))
    (detailed-description (string-utf8 500))
    (intellectual-property-type (string-ascii 50))
    (metadata-location (optional (string-ascii 200)))
)
    (let 
        (
            (new-asset-identifier (var-get intellectual-asset-counter))
            ;; Create validated local variables
            (validated-ip-type intellectual-property-type)
            (validated-metadata metadata-location)
        )
        ;; Input validation
        (asserts! (> (len asset-title) u0) ERR-EMPTY-TITLE-PROVIDED)
        (asserts! (> (len detailed-description) u0) ERR-EMPTY-DESCRIPTION-PROVIDED)
        (asserts! (validate-ip-type validated-ip-type) ERR-INVALID-IP-TYPE)
        (asserts! (validate-metadata-location validated-metadata) ERR-INVALID-IP-TYPE)
        
        (map-set intellectual-property-registry
            { intellectual-asset-identifier: new-asset-identifier }
            {
                current-owner: tx-sender,
                asset-title: asset-title,
                detailed-description: detailed-description,
                intellectual-property-type: validated-ip-type,
                metadata-location: validated-metadata,
                registration-block-height: stacks-block-height,
                asset-status-active: true,
                transfer-count: u0
            }
        )
        
        (var-set intellectual-asset-counter (+ new-asset-identifier u1))
        (var-set total-assets-registered (+ (var-get total-assets-registered) u1))
        
        (ok new-asset-identifier)
    )
)

(define-public (transfer-asset-ownership (intellectual-asset-identifier uint) (recipient-address principal))
    (let 
        (
            ;; Validate and create local variables for all inputs
            (validated-asset-id intellectual-asset-identifier)
            (validated-recipient recipient-address)
            (asset-information (unwrap! (map-get? intellectual-property-registry { intellectual-asset-identifier: validated-asset-id }) ERR-INTELLECTUAL-ASSET-NOT-FOUND))
            (current-transfer-count (get transfer-count asset-information))
        )
        ;; Input validation
        (asserts! (validate-asset-id validated-asset-id) ERR-INVALID-ASSET-ID)
        (asserts! (validate-principal validated-recipient) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (is-eq tx-sender (get current-owner asset-information)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (get asset-status-active asset-information) ERR-INTELLECTUAL-ASSET-NOT-FOUND)
        
        (map-set intellectual-property-registry
            { intellectual-asset-identifier: validated-asset-id }
            (merge asset-information { 
                current-owner: validated-recipient,
                transfer-count: (+ current-transfer-count u1)
            })
        )
        
        (map-set ownership-transfer-history
            { intellectual-asset-identifier: validated-asset-id, transfer-sequence: (+ current-transfer-count u1) }
            {
                previous-owner: tx-sender,
                new-owner: validated-recipient,
                associated-auction: none,
                transfer-block-height: stacks-block-height,
                transfer-method: "direct-transfer"
            }
        )
        
        (ok true)
    )
)

;; AUCTION MANAGEMENT FUNCTIONS
(define-public (create-auction-listing 
    (intellectual-asset-identifier uint)
    (minimum-reserve-price uint)
    (auction-duration-blocks uint)
)
    (let 
        (
            (new-auction-identifier (var-get auction-counter))
            (validated-asset-id intellectual-asset-identifier)
            (asset-information (unwrap! (map-get? intellectual-property-registry { intellectual-asset-identifier: validated-asset-id }) ERR-INTELLECTUAL-ASSET-NOT-FOUND))
        )
        ;; Input validation
        (asserts! (validate-asset-id validated-asset-id) ERR-INVALID-ASSET-ID)
        (asserts! (is-eq tx-sender (get current-owner asset-information)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (get asset-status-active asset-information) ERR-INTELLECTUAL-ASSET-NOT-FOUND)
        (asserts! (> minimum-reserve-price u0) ERR-INVALID-RESERVE-PRICE)
        (asserts! (>= auction-duration-blocks minimum-auction-duration) ERR-INVALID-AUCTION-DURATION)
        
        (map-set auction-marketplace-listings
            { auction-identifier: new-auction-identifier }
            {
                intellectual-asset-identifier: validated-asset-id,
                asset-seller: tx-sender,
                minimum-reserve-price: minimum-reserve-price,
                highest-bid-amount: u0,
                leading-bidder: none,
                auction-start-block: stacks-block-height,
                auction-end-block: (+ stacks-block-height auction-duration-blocks),
                listing-status-active: true,
                settlement-completed: false,
                total-bid-count: u0
            }
        )
        
        (var-set auction-counter (+ new-auction-identifier u1))
        (var-set total-auctions-created (+ (var-get total-auctions-created) u1))
        
        (ok new-auction-identifier)
    )
)

(define-public (cancel-auction-listing (auction-identifier uint))
    (let 
        (
            (validated-auction-id auction-identifier)
            (auction-information (unwrap! (map-get? auction-marketplace-listings { auction-identifier: validated-auction-id }) ERR-AUCTION-NOT-EXISTS))
        )
        ;; Input validation
        (asserts! (validate-auction-id validated-auction-id) ERR-AUCTION-NOT-EXISTS)
        (asserts! (is-eq tx-sender (get asset-seller auction-information)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (get listing-status-active auction-information) ERR-AUCTION-NOT-EXISTS)
        (asserts! (is-eq (get highest-bid-amount auction-information) u0) ERR-INVALID-BID-AMOUNT)
        
        (map-set auction-marketplace-listings
            { auction-identifier: validated-auction-id }
            (merge auction-information { listing-status-active: false })
        )
        
        (ok true)
    )
)

;; BIDDING SYSTEM FUNCTIONS
(define-public (submit-auction-bid (auction-identifier uint) (bid-increment-amount uint))
    (let 
        (
            (auction-information (unwrap! (map-get? auction-marketplace-listings { auction-identifier: auction-identifier }) ERR-AUCTION-NOT-EXISTS))
            (existing-user-bid (default-to { committed-bid-amount: u0 } 
                (map-get? user-active-bids { auction-identifier: auction-identifier, participant-address: tx-sender })))
            (updated-total-bid (+ bid-increment-amount (get committed-bid-amount existing-user-bid)))
            (current-bid-count (get total-bid-count auction-information))
        )
        (asserts! (check-auction-active-status auction-identifier) ERR-AUCTION-ALREADY-ENDED)
        (asserts! (> bid-increment-amount u0) ERR-INVALID-BID-AMOUNT)
        (asserts! (> updated-total-bid (get highest-bid-amount auction-information)) ERR-INVALID-BID-AMOUNT)
        (asserts! (>= updated-total-bid (get minimum-reserve-price auction-information)) ERR-INVALID-BID-AMOUNT)
        
        (try! (stx-transfer? bid-increment-amount tx-sender (as-contract tx-sender)))
        
        (map-set user-active-bids
            { auction-identifier: auction-identifier, participant-address: tx-sender }
            { committed-bid-amount: updated-total-bid }
        )
        
        (map-set bidding-activity-log
            { auction-identifier: auction-identifier, bidder-address: tx-sender }
            {
                total-bid-amount: updated-total-bid,
                bid-placement-block: stacks-block-height,
                refund-processed: false,
                bid-sequence-number: (+ current-bid-count u1)
            }
        )
        
        (map-set auction-marketplace-listings
            { auction-identifier: auction-identifier }
            (merge auction-information {
                highest-bid-amount: updated-total-bid,
                leading-bidder: (some tx-sender),
                total-bid-count: (+ current-bid-count u1)
            })
        )
        
        (ok true)
    )
)

(define-public (process-bid-refund (auction-identifier uint))
    (let 
        (
            (validated-auction-id auction-identifier)
            (auction-information (unwrap! (map-get? auction-marketplace-listings { auction-identifier: validated-auction-id }) ERR-AUCTION-NOT-EXISTS))
            (user-bid-information (unwrap! (map-get? user-active-bids { auction-identifier: validated-auction-id, participant-address: tx-sender }) ERR-INVALID-BID-AMOUNT))
            (bidding-history-record (unwrap! (map-get? bidding-activity-log { auction-identifier: validated-auction-id, bidder-address: tx-sender }) ERR-INVALID-BID-AMOUNT))
            (refund-amount (get committed-bid-amount user-bid-information))
        )
        ;; Input validation
        (asserts! (validate-auction-id validated-auction-id) ERR-AUCTION-NOT-EXISTS)
        (asserts! (> stacks-block-height (get auction-end-block auction-information)) ERR-AUCTION-STILL-ACTIVE)
        (asserts! (not (get refund-processed bidding-history-record)) ERR-ALREADY-PROCESSED)
        (asserts! (> refund-amount u0) ERR-INVALID-BID-AMOUNT)
        (asserts! (not (is-eq (some tx-sender) (get leading-bidder auction-information))) ERR-INVALID-BID-AMOUNT)
        
        (map-set bidding-activity-log
            { auction-identifier: validated-auction-id, bidder-address: tx-sender }
            (merge bidding-history-record { refund-processed: true })
        )
        
        (map-delete user-active-bids { auction-identifier: validated-auction-id, participant-address: tx-sender })
        
        (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
        
        (ok refund-amount)
    )
)

;; AUCTION SETTLEMENT FUNCTIONS
(define-public (finalize-auction-settlement (auction-identifier uint))
    (let 
        (
            (validated-auction-id auction-identifier)
            (auction-information (unwrap! (map-get? auction-marketplace-listings { auction-identifier: validated-auction-id }) ERR-AUCTION-NOT-EXISTS))
            (asset-information (unwrap! (map-get? intellectual-property-registry { intellectual-asset-identifier: (get intellectual-asset-identifier auction-information) }) ERR-INTELLECTUAL-ASSET-NOT-FOUND))
            (winning-bidder (get leading-bidder auction-information))
            (final-sale-price (get highest-bid-amount auction-information))
            (asset-seller (get asset-seller auction-information))
            (calculated-platform-fee (calculate-marketplace-fee final-sale-price))
            (seller-net-proceeds (- final-sale-price calculated-platform-fee))
            (current-transfer-count (get transfer-count asset-information))
        )
        ;; Input validation
        (asserts! (validate-auction-id validated-auction-id) ERR-AUCTION-NOT-EXISTS)
        (asserts! (get listing-status-active auction-information) ERR-AUCTION-NOT-EXISTS)
        (asserts! (> stacks-block-height (get auction-end-block auction-information)) ERR-AUCTION-STILL-ACTIVE)
        (asserts! (not (get settlement-completed auction-information)) ERR-ALREADY-PROCESSED)
        
        (map-set auction-marketplace-listings
            { auction-identifier: validated-auction-id }
            (merge auction-information {
                listing-status-active: false,
                settlement-completed: true
            })
        )
        
        (match winning-bidder
            successful-bidder
            (begin
                (map-set intellectual-property-registry
                    { intellectual-asset-identifier: (get intellectual-asset-identifier auction-information) }
                    (merge asset-information { 
                        current-owner: successful-bidder,
                        transfer-count: (+ current-transfer-count u1)
                    })
                )
                
                (try! (as-contract (stx-transfer? seller-net-proceeds tx-sender asset-seller)))
                (try! (as-contract (stx-transfer? calculated-platform-fee tx-sender contract-deployer)))
                
                (map-set ownership-transfer-history
                    { intellectual-asset-identifier: (get intellectual-asset-identifier auction-information), transfer-sequence: (+ current-transfer-count u1) }
                    {
                        previous-owner: asset-seller,
                        new-owner: successful-bidder,
                        associated-auction: (some validated-auction-id),
                        transfer-block-height: stacks-block-height,
                        transfer-method: "auction-settlement"
                    }
                )
                
                (ok true)
            )
            (ok true)
        )
    )
)
;; ADMINISTRATIVE FUNCTIONS
(define-public (update-marketplace-fee-rate (new-fee-percentage uint))
    (begin
        (asserts! (is-eq tx-sender contract-deployer) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (<= new-fee-percentage maximum-fee-rate) ERR-FEE-RATE-TOO-HIGH)
        (var-set marketplace-fee-percentage new-fee-percentage)
        (ok true)
    )
)

(define-public (update-marketplace-statistics)
    (begin
        (map-set marketplace-statistics
            { statistic-type: "total-volume" }
            {
                total-value: (var-get total-auctions-created),
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)