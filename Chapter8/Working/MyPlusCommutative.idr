
myPlusCommutative : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutative Z m = rewrite plusZeroRightNeutral m in Refl -- {x = m}
myPlusCommutative (S k) m = hmmm k m $ Refl {x = m + (S k)}
  where
    hmmm : (k : Nat) -> (m : Nat) -> ((m + (S k)) = (m + (S k))) -> S (plus k m) = plus m (S k)
    hmmm k m prf = rewrite myPlusCommutative k m in (rewrite plusSuccRightSucc m k in prf)
