module Stack (Stack(..),pop,push,peek,isEmpty,stackValues) where



newtype Stack a = Stack [a] deriving (Eq,Show,Ord)


pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

push :: a -> Stack a -> Stack a
push v (Stack xs) = Stack (v:xs)


peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x


isEmpty :: Stack a  -> Bool
isEmpty (Stack []) =  True
isEmpty (Stack _) = False


stackValues :: Stack a -> [a]
stackValues (Stack v) = v 




