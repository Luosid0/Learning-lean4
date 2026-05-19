-------The Monad Type Class--------
--q1
inductive BinTree (α : Type) where
  | leaf : BinTree α
  | branch : BinTree α → α → BinTree α → BinTree α
deriving Repr

def BinTree.mapM [Monad m] (f : α → m β) : BinTree α → m (BinTree β)
  | BinTree.leaf => pure BinTree.leaf
  | BinTree.branch l x r =>
    f x >>= fun minddle =>
    BinTree.mapM f l >>= fun left =>
    BinTree.mapM f r >>= fun right =>
    pure (BinTree.branch left minddle right)

------- Example: Arithmetic in Monads --------
def Reader (ρ : Type) (α : Type) : Type := ρ → α

def Reader.pure (x : α) : Reader ρ α := fun _ => x

def read : Reader ρ ρ := fun env => env

def Reader.bind
    (result : Reader ρ α)
    (next : α → Reader ρ β) : Reader ρ β :=
  fun env => next (result env) env

instance : Monad (Reader ρ) where
  pure := Reader.pure
  bind := Reader.bind

--def ReaderOption (ρ : Type) (α : Type) : Type := ρ → Option α
-- TODO: when I learn monad transformer or distributive law.

------- do-nation for monads --------
def firstThirdFifthSeventh [Monad m] (lookup : List α → Nat → m α)
    (xs : List α) : m (α × α × α × α) := do
  pure (<- lookup xs 0, <- lookup xs 2, <- lookup xs 4, <- lookup xs 6)
