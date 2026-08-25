(defun auth-service (user)
  (format nil "auth(~A)" user))

(defun inventory-service (sku)
  (format nil "reserve(~A)" sku))

(defun billing-service (amount)
  (format nil "charge(~D)" amount))

(defun checkout-facade (user sku amount)
  (format nil "~A>~A>~A"
          (auth-service user)
          (inventory-service sku)
          (billing-service amount)))

(format t "checkout=~A~%" (checkout-facade "alice" "SKU-42" 499))
