(herald unilateral)

(defprotocol unilateral basic
  (defrole init
     (vars (n text) (k akey))
     (trace
      (send (enc n k))
      (recv n))
     (uniq-orig n))
  (defrole resp
     (vars (n text) (k akey))
     (trace
      (recv (enc n k))
      (send n))))

(defskeleton unilateral
   (vars (k akey))
   (defstrand init 2 (k k))
   (non-orig (invk k)))
