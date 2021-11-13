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
eval (Plus x y) = plus x y
eval (Minus x y) = minus x y
eval (Multiplies x y) = multiply x y
eval (Exponentiate x y) = exponet x y

plus:: E -> E -> E
plus (IntLit x) (IntLit y) = IntLit (x + y)
plus (IntLit x) y = eval y 

multiply:: E -> E -> E
multiply (IntLit x) (IntLit y) = IntLit (x * y)
multiply (IntLit x) y = eval y 

minus:: E -> E -> E
minus (IntLit x) (IntLit y) = IntLit (x - y)
minus (IntLit x) y = eval y 
   
exponet :: E-> E-> E
exponet (IntLit x) (IntLit y) = IntLit (x^y)
exponet (IntLit x) y = eval y 