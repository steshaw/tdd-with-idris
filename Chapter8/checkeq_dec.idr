
zero_not_suc : (0 = S k) -> Void
zero_not_suc Refl impossible

suc_not_zero : (S k = 0) -> Void
suc_not_zero Refl impossible

no_rec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
no_rec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zero_not_suc
checkEqNat (S k) Z = No suc_not_zero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              No contra => No (no_rec contra)
                              Yes prf => Yes (cong prf)
