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