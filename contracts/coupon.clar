(define-constant coupon-creator 'SP3GHS3JVCBPW4K0HJ95VCKZ6EDWV08YMZ85XQGN0)

(define-non-fungible-token coupon uint)

;; Error
(define-constant err-only-creator u1)
(define-constant err-mint-failed u2)
(define-constant err-invalid-code u3)
(define-constant err-transfer-failed u4)
(define-constant err-transfer-not-allowed u5)
(define-constant err-code-used u6)
(define-constant err-cannot-use u6)

;; Variables
(define-data-var next-code uint u1)
(define-data-var total-code uint u0)

(define-map coupons
  ((coupon-code uint))
  ((discount uint) (used bool))
)

;; get owner of a coupon
(define-read-only (owner-of? (coupon-code uint))
  (nft-get-owner? coupon coupon-code)
)

;; The coupon-creator create a coupon to receiver
;; By default, the created coupons belong to the coupon-creator
(define-public (create-coupon (discount uint))
  ;; (if (is-eq tx-sender coupon-creator)
    (let ((coupon-code (var-get next-code)))
      (if (is-ok (nft-mint? coupon coupon-code coupon-creator))
        (begin
          (map-set coupons {coupon-code: coupon-code}
            {
              discount: discount,
              used: false
            }
          )
          (var-set next-code (+ coupon-code u1))
          (var-set total-code (+ (var-get total-code) u1))
          (ok coupon-code)
        )
        (err err-mint-failed)
      )
    )
    ;; (err err-only-creator)
  ;; )
)

(define-private (is-valid (coupon-code uint))
  (is-some (map-get? coupons {coupon-code: coupon-code}))
)

;; get total code
(define-public (get-total-code)
  (ok (var-get total-code))
)

;; check coupon code is valid or not
(define-public (check-coupon-valid (coupon-code uint))
  (if (is-valid coupon-code)
    (ok true)
    (ok false)
  )
)

;; get owner of a valid coupon code
(define-public (get-owner-of (coupon-code uint))
  (if (is-valid coupon-code)
    (ok (default-to coupon-creator (owner-of? coupon-code)))
    (err err-invalid-code)
  )
)

;; check coupon discount rate
(define-public (check-coupon-discount (coupon-code uint))
  (if (is-valid coupon-code)
    (ok (default-to u0 (get discount (map-get? coupons {coupon-code: coupon-code}))))
    (err err-invalid-code)
  )
)

;; check coupon was used or not
(define-public (check-coupon-used (coupon-code uint))
  (if (is-valid coupon-code)
    (ok (default-to true (get used (map-get? coupons {coupon-code: coupon-code}))))
    (err err-invalid-code)
  )
)

;; use coupon
(define-public (use-coupon (coupon-code uint))
(let ((coupon-owner (unwrap! (owner-of? coupon-code) (err err-invalid-code))))
    (if (is-eq coupon-owner tx-sender)
      (match (map-get? coupons {coupon-code: coupon-code})
        code (if (get used code)
          (err err-code-used)
            (begin
              (map-set coupons
                {coupon-code: coupon-code}
                {discount: (get discount code), used: true})
              (ok (get discount code))
            )
          )
        (err err-invalid-code)
      )
      (err err-cannot-use)
    )
  )
)

;; Only coupon owner can transfer it to other
(define-public (transfer (coupon-code uint) (receiver principal))
  (let ((coupon-owner (unwrap! (owner-of? coupon-code) (err err-invalid-code))))
    (if (is-eq coupon-owner tx-sender)
      (match (nft-transfer? coupon coupon-code coupon-owner receiver)
        success (ok 1)
        error (err err-transfer-failed)
      )
      (err err-transfer-not-allowed)
    )
  )
)