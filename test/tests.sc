val s = "01/31 17:24:44  rcvi addr unit       1 : hu A1  (_no_alias_)"
val items = s.split(' ')
val foo =items(2)

var lines = List("      * = On  x = Dimmed\n",
  "    Unit: 1..4...8\n",
  "  Housecode A (*...*...........)",
  "")

val p2 = "  Housecode ([A-P]) \\(([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])\\)".r
val infop2 = "  Housecode ([A-P]) \\(([.|*]+)\\)".r

val stuff = lines.map {
  case p2(h, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) => h
  case _ => "foo"
}


val p1 = "\\(([.|*])\\)".r
val p1(m) = "(.)"

val p3 = "  Housecode ([A-P])".r

val p2(h,u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16) = "  Housecode A (.*.*............)"

val infop2(house,units) = "  Housecode A (.*.*............)"
u12






