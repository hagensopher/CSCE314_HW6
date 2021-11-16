

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

eval (Plus (IntLit x) (IntLit y)) = IntLit(x+y)
eval (Plus x y) = eval(Plus (eval(x)) (eval(y)))

eval (Minus (IntLit x) (IntLit y)) = IntLit (x - y)
eval (Minus x y) = eval(Minus (eval(x)) (eval(y)))

eval (Multiplies (IntLit x) (IntLit y)) = IntLit (x * y)
eval (Multiplies x y) = eval(Multiplies (eval(x)) (eval(y)))

eval (Exponentiate (IntLit x) (IntLit y)) = IntLit (x^y)
eval (Exponentiate x y) = eval(Exponentiate (eval(x)) (eval(y)))

eval (BoolLit x) = BoolLit x
eval (IntLit x) = IntLit x



log2Sim :: E -> E
log2Sim (IntLit 1) = IntLit 0
log2Sim (IntLit x) = IntLit(round (logBase 2 (fromIntegral x)))

log2Sim (Multiplies (IntLit x) (IntLit y)) = Plus (IntLit x) (IntLit y) 
log2Sim (Multiplies x y) = Plus (log2Sim(x)) (log2Sim(y)) 

log2Sim (Exponentiate (IntLit x)(IntLit y)) = Multiplies (log2Sim(IntLit x)) (IntLit y) -- have to reduce to keep equality
log2Sim (Exponentiate x y) = Multiplies (log2Sim x) (log2Sim y) -- have to reduce to keep equality

log2Sim (Equals x y) = Equals (log2Sim x) (log2Sim y)

log2Sim (BoolLit x) = (BoolLit x)
