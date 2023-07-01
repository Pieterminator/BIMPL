concrete PropGGC of Prop = open Prelude, Formal in {

lincat
  Prop = TermPrec ;
  Atom = Bool => Str ;
  Pred1 = Bool => Str ;
  Pred2 = Bool => Str ;
  Var,
  Conj
    = Str ;
  Ind = TermPrec ;
  Fun1 = TermPrec ;
  Fun2 = TermPrec ;

lin
  PAtom a = constant (a ! True) ;
  PNeg = prefix 4 "~" ;
  PConj = infixl 1 ; --2
  PImpl = infixr 0 "$" ;
  PBimpl = infixr 0 "%" ;                                   -- Pieter
  PUniv v = prefix 3 ("@" ++ v) ;
  PExist v = prefix 3 ("/" ++ v) ;

  APred1 p x = \\b => appGGC (p ! b) (top x) ;
  APred2 p x y = \\b => appGGC (p ! b) (top x) (top y) ;

  IVar x = constant x ;
  IFun1 f x = constant (appGGC (top f) (top x)) ;
  IFun2 f x y = constant (appGGC (top f) (top x) (top y)) ;

  VString s = s.s ;

  CAnd = "&" ;
  COr = "|" ;

  PTaut = constant "T";
  PContra = constant "F";

-- supplementary

lincat
  Kind = Str ;
  [Prop],
  [Pred1], 
  [Ind], 
  [Var]
    = Str ;

lin
  AKind k x = table {True => appGGC k (top x) ; False => "~" ++ (appGGC k (top x)) } ;

-- test lexicon
lin
  IInt i = constant i.s ;

  Dodec = "Dodec" ;
  Student = "Student" ;
  Cube = "Cube" ;
  Prime = "Prime" ;
  Person = "Person" ;
  Tet = "Tet" ;
  Pet = "Pet" ;
  Small = slashGGC "Small" ;
  Medium = slashGGC "Medium" ;
  Large = slashGGC "Large" ;
  Even = slashGGC "Even" ;
  Adjoins = slashGGC "Adjoins" ;
  SameCol = slashGGC "SameCol" ;
  LeftOf = slashGGC "LeftOf" ;
  RightOf = slashGGC "RightOf" ;
  Smaller = slashGGC "Smaller" ;
  FrontOf = slashGGC "FrontOf" ;
  Larger = slashGGC "Larger" ;
  SameRow = slashGGC "SameRow" ;
  SameShape = slashGGC "SameShape" ;
  SameSize = slashGGC "SameSize" ;
  BackOf = slashGGC "BackOf" ;

oper
  appGGC = overload {
    appGGC : Str -> Str -> Str = \f,x -> f ++ curly x ;
    appGGC : Str -> Str -> Str -> Str = \f,x,y -> f ++ curly (x ++ "," ++ y);
    } ;
  slash = overload {
    slash : Str -> Str = \f -> "\\" + f ;
    slash : Str -> TermPrec = \f -> constant ("\\" + f) ;
    slash : Str -> Bool => Str = \f -> table {True => "\\" + f ; False => top (prefix 3 "\\sim" (constant ("\\" + f)))} ; -- Elze
    slash : Str -> Str -> Bool => Str = \f,g -> table {True => "\\" + f ; False => "\\" + g} ; 
    } ;
  slashGGC : Str -> Bool => Str = \f -> table {True => f ; False => top (prefix 3 "~" (constant (f)))} ;
  

  curly : Str -> Str = \s -> "(" ++ s ++ ")" ;
  bracket : Str -> Str = \s -> "[" ++ s ++ "]" ;

}
