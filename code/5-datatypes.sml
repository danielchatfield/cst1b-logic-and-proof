datatype prop
  = Atom of Atom
  | Neg of prop
  | Conj of prop * prop
  | Disj of prop * prop

infix 3 Disj
infix 4 Conj

fun nnf(Neg(p1 And p2)) = nnf(Neg p1) Or nnf(Neg p2)
  | nnf(Neg(p1 Or p2))  = nnf(Neg p1) And nnf(Neg p2)
  | nnf(Neg (Neg p1))   = nnf p1
  | nnf(p1 And p2)      = nnf p1 Conj nnf p2
  | nnf(p1 Or p2)       = nnf p1 Disj nnf P2
  | nnf p               = p

type Atom = string
type interpretation = (Atom * bool) list
