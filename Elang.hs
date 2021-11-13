data E = IntLit Int
       | BoolLit Bool
       | Plus E E
       | Minus E E
       | Multiplies E E
       | Exponentiate E E
       | Equals E E
         deriving (Eq, Show)

eval :: E -> E
eval (Equals x y) = BoolLit (eval x == eval y )
--eval (Plus x y) = 

plus:: E -> E -> Int
plus (IntLit x) (IntLit y) = x + y

multiply:: E -> E -> Int
multiply (IntLit x) (IntLit y) = x * y

minus:: E -> E -> Int
minus (IntLit x) (IntLit y) = x - y

exponet :: E-> E-> Int
exponet (IntLit x) (IntLit y) = x^y