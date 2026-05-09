-------Positive Numbers--------
-- Q1
structure Pos where
  succ ::
  pred : Nat
deriving Repr

def Pos.Add (a:Pos)(b:Pos) : Pos:=
  {pred:=a.pred+b.pred+1}

instance : Add Pos where
  add := Pos.Add
#eval ({pred:=3}:Pos)+({pred:=4}:Pos) --{pred:=8}

def Pos.Mul (a:Pos)(b:Pos) : Pos:=
  {pred:=(a.pred+1)*(b.pred+1)-1}
instance : Mul Pos where
  mul := Pos.Mul
#eval ({pred:=3}:Pos)*({pred:=4}:Pos) --{pred:=19}

def Pos.ToString (a:Pos) : String :=
  toString (a.pred+1)
instance : ToString Pos where
  toString := Pos.ToString
#eval s!"there is {({pred:=3}:Pos)}" --"there is 4"

instance : OfNat Pos (n+1) where
  ofNat := {pred:=n}
#eval (3:Pos)+(4:Pos) -- { pred := 6 }

-- Q2
structure Even where
  double ::
  base : Nat
deriving Repr

def Even.Add (a:Even)(b:Even) : Even:=
  {base:=a.base+b.base}
instance : Add Even where
  add := Even.Add
#eval ({base:=3}:Even)+({base:=4}:Even) --{base:=7}

def Even.Mul (a:Even)(b:Even) : Even:=
  {base:=a.base*b.base*2}
instance : Mul Even where
  mul := Even.Mul
#eval ({base:=3}:Even)*({base:=4}:Even) --{base:=24}

def Even.ToString (a:Even) : String :=
  toString (a.base*2)
instance : ToString Even where
  toString := Even.ToString
#eval toString (Even.double 3) -- "6"
#eval s!"there is {(Even.double 3)}"

instance:OfNat Even 0 where
  ofNat :={base:=0}
instance [OfNat Even n] : OfNat Even (n + 2) where
  ofNat := { base := (OfNat.ofNat n : Even).base + 1 }
#eval (4:Even) --{ base := 2 }

---------Type Classes and Polymorphism------------------
-- maybe 32; set_option synthInstance.maxDepth 100
-- halting problem...

----------------Controlling Instance Search-------------------
structure PPoint (α : Type) where
  x : α
  y : α

@[default_instance]
instance[Mul α] : HMul (PPoint α) α (PPoint α) where
  hMul p a := {x:=p.x*a,y:=p.y*a}
#eval {x := 2.5, y := 3.7 : PPoint Float} * 2.0

-------------Standard Classes-------------
--q1
structure NonEmptyList (α : Type) : Type where
  head : α
  tail : List α

instance : HAppend (List α) (NonEmptyList α) (NonEmptyList α) where
  hAppend xs ys := match xs with
    | [] => ys
    | x::xs => {head := x, tail := xs ++ [ys.head] ++ ys.tail}

example : [1,2] ++ (⟨3, [4]⟩ : NonEmptyList Nat) = ⟨1, [2, 3, 4]⟩ := rfl

--q2 Binary Tree => Arrowed Graph
inductive BinTree (α : Type) where
  | leaf : BinTree α
  | branch : BinTree α → α → BinTree α → BinTree α
deriving Repr

-- (BinTree : Type_1}) : Type_0 -> Type_0
-- (Type : Type_1)
-- Functor is kind of endofunctor of category Type
-- ps : type is term, type class is predicate, aka, term in the next hierarchy
def BinTree.map {α β : Type} (f : α → β) : BinTree α → BinTree β
| BinTree.leaf => BinTree.leaf
| BinTree.branch l x r => BinTree.branch (BinTree.map f l) (f x) (BinTree.map f r)

instance : Functor BinTree where
  map := @BinTree.map
