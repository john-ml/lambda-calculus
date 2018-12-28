# lambda-calculus

Typechecker/interpreter for a dependently typed lambda calculus.

Example session:
```
> :use std
Defined id : λ A : Type n, A -> A
Defined compose : λ A : Type n0, λ B : Type n1, λ C : Type n2, (B -> C) -> (A -> B) -> A -> C
Defined const : λ A : Type n3, λ B : Type n4, A -> B -> A
-- etc --
std> zero
(zero) : (nat)
std> # 2^3 = 8
std> exp (succ (succ zero)) (succ (succ (succ zero)))
(λ A : Type n_nat. λ f : A -> A. λ a : A. f (f (f (f (f (f (f (f (a))))))))) : (nat)
std> # let swap (a, b) := (b, a)
std> swap := λ A : Type n30, λ B : Type n31, λ p : and A B. λ C : Type n32, λ elim : B -> A -> C. p C (λ a : A. λ b : B. elim b a)
Defined swap : λ A : Type n30, λ B : Type n31, ((and) (A) (B)) -> λ C : Type n32, (B -> A -> C) -> C
std> # fst (swap (true, 1)) = 1
std> fst nat bool (swap bool nat (pair bool nat true (succ zero)))
(λ A : Type n_nat. λ f : A -> A. λ a : A. f (a)) : (nat)
std> # snd (swap (true, 1)) = true
std> snd nat bool (swap bool nat (pair bool nat true (succ zero)))
(true) : (bool)
```
