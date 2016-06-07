module Z3.Logic where

data Pred t a = PTrue
              | PFalse
              | PConj (Pred t a) (Pred t a)
              | PDisj (Pred t a) (Pred t a)
              | PNeg (Pred t a)
              | PForAll String (Pred t a)
              | PExists String (Pred t a)
              | PImpli (Pred t a) (Pred t a)
              | PAssert a
