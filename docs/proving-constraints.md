```.haskell
p1 :: forall n5 n6 n7.Dict (((n5 + n6) + n7) ~ (n5 + (n6 + n7))) -> forall n4.Dict ((n4 + ((n5 + n6) + n7)) ~ (n4 + (n5 + (n6 + n7))))
p1 Dict = Dict

ev :: forall n4 n5 n6 n7.Dict (((n4 + (n5 + n6)) + n7) ~ ((n4 + n5) + (n6 + n7)))
ev =  Dict \\ (plusAssociates @n4 @n5 @(n6 + n7)) \\ (p1 @n5 @n6 @n7 (plusAssociates @n5 @n6 @n7) @n4) \\ plusAssociates @n4 @(n5 + n6) @n7 

p3 :: forall n1 n4 n5 n2 n6 n7.(n1 ~ (n4 + n5), n2 ~ (n6 + n7)) => Dict (((n4 + (n5 + n6)) + n7) ~ (n1 + n2))
p3 = Dict \\ ev @n4 @n5 @n6 @n7

p :: forall n1 n4 n5 n2 n6 n7.(n1 ~ (n4 + n5), n2 ~ (n6 + n7))
  => ITemplate n4
  -> ITemplate n5
  -> ITemplate n6
  -> ITemplate n7
  -> ITemplate (n1+n2)
p t1 t2 t3 t4 = (t1 >+> (t2 >+> t3) >+> t4) \\ p3 @n1 @n4 @n5 @n2 @n6 @n7
```

p1 :: forall n1 n2.Dict ((1 + (n1 + n2)) ~ ((1 + n1) + n2))
p1 = Dict \\ plusAssociates @1 @n1 @n2 

p2 :: forall n1 n2.Dict ((1 + n1) + n2 > 0)
p2 = Dict \\ plusAssociates @1 @n1 @n2 \\ p11 @1 @(n1+n2)

-- TODO: Figure out how to prove this.
p11 :: forall n1 n2.(n1 > 0) => Dict ((n1 + n2) > 0)
p11 = unsafeAxiom

-- TODO: Figure out how to prove this.
p12 :: forall n1 n2.(n2 > 0) => Dict ((n1 + n2) > 0)
p12 = unsafeAxiom

p10 :: Dict (1 > 0)
p10 = Dict

p3 :: forall m n1 n2.Dict ((m + (n1 + n2)) ~ ((m + n1) + n2))
p3 = Dict \\ plusAssociates @m @n1 @n2

p4 :: forall m n1 n2.n1 + n2 > 0 => Dict(m + (n1 + n2) > 0)
p4 = Dict \\ p12 @m @(n1 + n2)

p5 :: forall m n4 n5 n2 n1.n1 ~ (n4 + n5) => Dict (((m + n4) + (n5 + n2)) ~ ((m + n1) + n2))
p5 = Dict \\ plusAssociates @m @n4 @(n5 + n2) \\ plusAssociates @n4 @n5 @n2 \\ plusAssociates @m @n1 @n2

p13 :: forall n4 n5 n2.Dict (n4 + (n5 + n2) ~ (n4 + n5) + n2)
p13 = Dict \\ plusAssociates @n4 @n5 @n2

p6 :: forall n4 n5 n2.n4 + n5 > 0 => Dict (n4 + (n5 + n2) > 0)
p6 = Dict \\ p13 @n4 @n5 @n2 \\ p11 @(n4 + n5) @n2

p7 :: forall m n1 n2.(n1 + n2) > 0 => Dict ((m + n1) + n2 > 0)
p7 = Dict \\ plusAssociates @m @n1 @n2 \\ p12 @m @(n1 + n2)

--((m + n1) + n2) ~ (m + n)
p8 :: forall m n1 n4 n5.n1 ~ n4 + n5 => Dict (((m + n4) + n5) ~ (m + n1))
p8 = Dict \\ plusAssociates @m @n4 @n5 

p9 :: forall m n1 n2 n4 n5.(m ~ (n1 + n2), n2 ~ (n4 + n5)) => Dict (((n1 + n4) + n5) ~ m)
p9 = Dict \\ plusAssociates @n1 @n4 @n5

p14 :: forall n1 n2 n4 n5 m n.(m ~ (n1 + n2),n2 ~ (n4 + n5)) => Dict((n1 + n4) + (n5 + n) ~ (m + n))
p14 = Dict \\ plusAssociates @n1 @n4 @(n5+n) \\ plusAssociates @n4 @n5 @n \\
plusAssociates @n1 @n2 @n

p3 :: forall m n.Dict (S m ~ S n) -> Dict (m ~ n)
p3 p = mapDict (plusIsCancellative @1 @m @n) (p \\ plusCommutes @1 @m \\ plusCommutes @1 @n)

p2 :: forall m n n1 . (m ~ (n + 1),m ~ (n1 + 1)) => Dict (n1 ~ n)
p2 = p3 Dict
