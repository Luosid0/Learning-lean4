--——————————————————函数与定义———————————————————————
def joinStringsWith(a b c : String):String:=
String.append (String.append b a) c
#eval joinStringsWith "," "one" "and another"


def volume(a b c:Nat):Nat:= a*b*c
#eval volume 1 2 3


--——————————————————结构体———————————————————————
structure RectangularPrism where
  x : Float
  y : Float
  z : Float
deriving Repr
def RectangularPrism.volume(r:RectangularPrism):Float:=r.x * r.y * r.z
#eval {x:=3.0,y:=4.0,z:=5.0:RectangularPrism}.volume


structure Segment where
  start : Float
  terminal : Float
deriving Repr
def Segment.length (l :Segment):Float:=l.terminal-l.start
#eval Segment.length {start:=1.2,terminal:=3.1}


--————————————————————多态————————————————————-
def mytail?{α:Type}(xs : List α):Option α:=
match xs with
| []=>none
| [x] => some x
| _::y => mytail? y
#eval [1,2,3].head?
#eval mytail? [1,2,3]


def le0(x:Int):Bool := if x<0 then true else false
def List.findFirst?{α:Type}(xs:List α)(pedicate:α->Bool): Option α:=
match xs with
| []=>none
| x::y => if pedicate x then x else y.findFirst? pedicate
#eval [1,2,3,-1,4].findFirst? le0


def myProd.swap {α β : Type} (pair : α × β) : β × α :=
{fst:=pair.snd,snd:=pair.fst: β×α}
#eval myProd.swap (1,2)



structure PetName where
  zhongzhu:String
  name:String
deriving Repr
def animals : List PetName :=
  [{zhongzhu:="dog",name:="Spot" : PetName},
   {zhongzhu:="cat",name:="Tiger" : PetName},
   {zhongzhu:="dog",name:="Fifi" : PetName},
   {zhongzhu:="dog",name:="Rex" : PetName},
   {zhongzhu:="cat",name:="Floof" : PetName}
    ]
def howManyDogs (pets : List PetName) : Nat :=
  match pets with
  | [] => 0
  | x :: morePets => if x.zhongzhu="dog" then howManyDogs morePets + 1 else howManyDogs morePets
#eval howManyDogs animals


def zip {α β:Type}(xs:List α)(ys :List β):List (α × β):=
match xs,ys with
|[],_=>[]
|_,[]=>[]
|x:: morex, y::morey => List.append [(x,y)] (zip morex morey)
def xlist:List Nat :=[3,4,5]
def ylist:List Float:= [2.0, 3.7]
#eval zip xlist ylist


def take{α :Type}(n:Nat)(l:List α):List α:=
  match n,l with
  |0,_=>[]
  |_,[]=>[]
  |Nat.succ k, x::morex => [x].append (take k morex)
#eval take 3 ["bolete","Oyster"]
#eval take 1 ["bolete","Oyster"]


def distribution{α β γ:Type}(expre : α×(β ⊕ γ)):(α×β)⊕(α×γ):=
  match expre.snd with
  | Sum.inl b =>Sum.inl (expre.fst,b)
  | Sum.inr c =>Sum.inr (expre.fst,c)


def Time2toAddition{α:Type}(expre:Bool×α):α⊕α:=
  match expre with
  |(false,x)=>Sum.inl x
  |(true,x)=>Sum.inr x
