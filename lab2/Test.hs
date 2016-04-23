-- @Author: JONATHAN STEVEN PRIETO CUBIDES
-- @Date: 2016-04-17 13:53:04
-- @Last Modified time: 2016-04-23 00:21:34

module Test
    where
import Test.HUnit

import FOL
import CNF
import PCNF
import Utils

x,y,z :: Term
x   = Var 0
y   = Var 1
z   = Var 2

px  = Pred 0 [x]
py  = Pred 1 [y]
pz  = Pred 2 [z]

qx  = Pred 3 [x]
qy  = Pred 4 [y]
qz  = Pred 5 [z]

rx  = Pred 6 [x]
ry  = Pred 7 [y]
rz  = Pred 8 [z]

f0      = (Forall x px)
rf0     = (Forall y (Pred 0 [y]))
test0   = TestCase (assertEqual "f0" rf0 (pcnf f0))

f1      = (Not
            (Exists x
                (Imp
                    px
                    (Forall x px))))
rf1     = Forall (Var 2)
                (Exists (Var 1)
                        (And
                            (Pred 0 [Var 2])
                            (Not (Pred 0 [Var 1]))))
test1   = TestCase (assertEqual "f1" (pcnf f1) rf1)

f2      = (Forall x
            (Or px (Exists y qy)))
rf2     = Forall (Var 3)
                (Exists (Var 2)
                    (Or
                        (Pred 0 [Var 3])
                        (Pred 4 [Var 2])))
test2   = TestCase (assertEqual "f2" (pcnf f2) rf2)

f3      = (Or (Forall x px) (Exists y qy))
rf3     = Forall (Var 3)
                (Exists (Var 2)
                    (Or
                        (Pred 0 [Var 3])
                        (Pred 4 [Var 2])))
test3   = TestCase (assertEqual "f3" (pcnf f3) rf3)

f4      = (And (Forall x px) (Exists y qy))
rf4     = Forall (Var 3)
                (Exists (Var 2)
                    (And
                        (Pred 0 [Var 3])
                        (Pred 4 [Var 2])))
test4   = TestCase (assertEqual "f4" (pcnf f4) rf4)

f5 = (Not
        (Imp
            (And
                (Forall x (Imp px qx))
                (Forall x (Imp qx rx))
            ) 
            (Forall x (Imp px rx))
        )
    )

rf5 = Exists (Var 1)
            (And
                (And
                    (Forall (Var 3)
                        (Or
                            (Not
                                (Pred 0 [Var 3]))
                            (Pred 3 [Var 3]) ))
                    (Forall (Var 2)
                        (Or
                            (Not (Pred 3 [Var 2]))
                            (Pred 6 [Var 2])) ))
                (And
                    (Pred 0 [Var 1])
                    (Not (Pred 6 [Var 1]))))
test5 = TestCase (assertEqual "f5" (pcnf f5) rf5)

tests = TestList [test0, test1, test2, test3, test4, test5]