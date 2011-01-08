class Mu { }
class Any is Mu { }
class Num { }
class Nil { }
class Str { }
class ClassHOW { }

sub infix:<+> ($x,$y) { $x + $y }

Q:CgOp { (prog [say (obj_getstr {2 + 2})] [null var]) }
