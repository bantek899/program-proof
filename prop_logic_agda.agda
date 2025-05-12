open import Data.Product renaming (_×_ to _∧_)

-- Propositional logic proofs

-- IMPLICATION

-- First projection (K Combinator)
-- A ⇒ B ⇒ A
K : {A B : Set } → A → B → A
K {A} {B} a b = a


-- Application
-- (A ⇒ B) ⇒ A ⇒ B                 

  -- 1. A → B  (assumption)
  -- 2. A       (assumption)
  -- 3. B       (by → elimination / derived from 1 and 2 using modus ponens)

-- f : A -> B    a : A
-- -------------------- (→ elimination / application)
--       f a : B
app : {A B : Set} → (A → B) → A → B
app f a = (f a) 


-- Flip
-- (A ⇒ B ⇒ C) ⇒ B ⇒ A ⇒ C
flip : {A B C : Set} → (A → B → C) → B → A → C
flip f b a = f a b

-- Transitivity / composition
-- (A ⇒ B) ⇒ (B ⇒ C) ⇒ (A ⇒ C) 
comp : {A B C : Set} → (A → B) → (B → C) → (A → C)
comp f g = λ x → g (f x)


-- S Combinator
-- (A ⇒ B ⇒ C) ⇒ (A ⇒ B) ⇒ A ⇒ C
S : {A B C : Set} → (A → B → C) → (A → B) → A → C
S f g a = f a (g a)



-- CONJUNCTION

-- Projections
-- (A ∧ B) ⇒ A
proj1 : {A B : Set} → (A ∧ B) → A
proj1 (fst , snd) = fst

-- (A ∧ B) ⇒ B
proj2 : {A B : Set} → (A ∧ B) → B
proj2 (fst , snd) = snd


-- Diagonal
-- A ⇒ A ∧ A
diag : {A : Set} → A → (A ∧ A)
diag a = a , a


-- Commutativity
-- (A ∧ B) ⇒ (B ∧ A)
∧-comm : {A B : Set} → (A ∧ B ) → (B ∧ A)
∧-comm (fst , snd) = snd , fst


-- Currying
-- (A ∧ B ⇒ C) ⇒ (A ⇒ B ⇒ C)
curry1 : {A B C : Set} → (A ∧ B → C) → (A → B → C)
curry1 f = λ a b → f (a , b)

-- (A ⇒ B ⇒ C) ⇒ (A ∧ B ⇒ C)
curry2 : {A B C : Set} → (A → B → C) → (A ∧ B → C)
curry2 f =  λ a  → f (proj1 a) (proj2 a)
