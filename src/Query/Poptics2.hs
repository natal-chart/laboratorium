
{-#  LANGUAGE RankNTypes  #-}
{-#  LANGUAGE TupleSections  #-}

module Query.Poptics2 where


{- 

Haskell code for the paper "Profunctor Optics: Modular Data Accessors"
by Matthew Pickering, Jeremy Gibbons, and Nicolas Wu

to appear in "The Art, Science, and Engineering of Programming" 1(2), 2017

This code has been extracted from the literate source of the paper,
so apologies for the terse formatting. The explanations of the code
are all in the paper itself.

 -}


import Prelude hiding (Traversable, traverse)
import Control.Applicative hiding (empty)
data Lens a b s t = Lens { view :: s -> a, update ::  ( b, s) -> t }
pi1 :: Lens a b ( a, c) ( b, c)
pi1 = Lens viewFst updateFst where
  viewFst (x,y)         = x
  updateFst (x',(x,y))  = (x',y)
sign :: Lens Bool Bool Integer Integer
sign = Lens view update where
  view x        = (x >= 0)
  update (b,x)  = if b then abs x else - (abs x)
data Prism a b s t = Prism { match :: s -> Either t a, build :: b -> t }
the :: Prism a b (Maybe a) (Maybe b)
the = Prism match build where
  match (Just x)  = Right x
  match Nothing   = Left Nothing
  build x         = Just x
whole :: Prism Integer Integer Double Double
whole = Prism match build where
  match x
    |  f == 0     = Right n
    |  otherwise  = Left x
    where (n, f) = properFraction x
  build = fromIntegral
pi11 :: Lens a b ( ( a, c), d) ( ( b, c), d)
pi11 = Lens view update where
  Lens v u         =  pi1
  view             =  v . v
  update (x',xyz)  =  u (xy', xyz) where
                             xy   = v xyz
                             xy'  = u (x',xy) 
data Adapter a b s t = Adapter { from :: s -> a, to :: b -> t }
flatten :: Adapter ( a, b, c) ( a', b', c') ( ( a, b), c) ( ( a', b'), c')
flatten = Adapter from to where
  from ((x,y),z)  = (x,y,z)
  to (x,y,z)      = ((x,y),z)
data State s a = State { run :: s -> ( a, s) }
inc :: Bool -> State Integer Bool
inc b = State (\ n -> (b, n+1))
instance Functor (State s) where
  fmap f m  = State (\ s -> let (x,s') = run m s in (f x, s'))

instance Applicative (State s) where
  pure x    = State (\ s ->  (x,s))
  m <*> n   = State (\ s ->  let  (f,s')   = run m s
                                  (x,s'')  = run n s'
                             in (f x, s''))
data Tree a = Empty | Node (Tree a) a (Tree a)
inorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
inorder m Empty         = pure Empty
inorder m (Node t x u)  = ((pure Node <*> inorder m t) <*> m x) <*> inorder m u
countOdd :: Integer -> State Integer Bool
countOdd n = if even n then pure False else inc True
data FunList a b t = Done t | More a (FunList a b (b -> t))
out :: FunList a b t -> Either t (a, FunList a b (b->t))
out (Done t)    = Left t
out (More x l)  = Right (x,l)

inn :: Either t (a, FunList a b (b->t)) -> FunList a b t
inn (Left t)       = Done t
inn (Right (x,l))  = More x l
instance Functor (FunList a b) where
  fmap f (Done t)    = Done (f t)
  fmap f (More x l)  = More x (fmap (f .) l)

instance Applicative (FunList a b) where
  pure                 = Done
  Done f <*> l'        = fmap f l'
  More x l <*> l'      = More x (fmap flip l <*> l')
single :: a -> FunList a b b
single x = More x (Done id)
fuse :: FunList b b t -> t
fuse (Done t)    = t
fuse (More x l)  = fuse l x
data Traversal a b s t = Traversal { extract :: s -> FunList a b t }
inorderC :: Traversal a b (Tree a) (Tree b)
inorderC = Traversal (inorder single)
class Profunctor p where
  dimap :: (a'->a) -> (b->b') -> p a b -> p a' b'
instance Profunctor (->) where
  dimap f g h = g . h . f
data UpStar f a b = UpStar { unUpStar :: a -> f b }
instance Functor f => Profunctor (UpStar f) where
  dimap f g (UpStar h) = UpStar (fmap g . h . f)
class Profunctor p => Cartesian p where
  first   :: p a b -> p ( a, c) ( b, c)
  second  :: p a b -> p ( c, a) ( c, b)
instance Cartesian (->) where
  first h   = cross h id
  second h  = cross id h
instance Functor f => Cartesian (UpStar f) where
  first (UpStar unUpStar)   = UpStar (rstrength . cross unUpStar id)
  second (UpStar unUpStar)  = UpStar (lstrength . cross id unUpStar)
rstrength :: Functor f => ( f a, b) -> f ( a, b)
rstrength (fx,y) = fmap (,y) fx
lstrength :: Functor f => ( a, f b)  -> f ( a, b)
lstrength (x,fy) = fmap (x,) fy
twist :: ( a, b) -> ( b, a)
twist (a,b) = (b,a)
class Profunctor p => Cocartesian p where
  left   :: p a b -> p (Either a c) (Either b c)
  right  :: p a b -> p (Either c a) (Either c b)
data Zero

absurd :: Zero -> a
absurd = absurd     --  a total function
instance Cocartesian (->) where
  left h   = plus h id
  right h  = plus id h
instance Applicative f => Cocartesian (UpStar f) where
  left (UpStar unUpStar)   = UpStar (either (fmap Left . unUpStar) (pure . Right))
  right (UpStar unUpStar)  = UpStar (either (pure . Left) (fmap Right . unUpStar))
class Profunctor p => Monoidal p where
  par    :: p a b -> p c d -> p ( a, c) ( b, d)
  empty  :: p () ()
instance Monoidal (->) where
  par    = cross
  empty  = id
instance Applicative f => Monoidal (UpStar f) where
  empty    = UpStar pure
  par h k  = UpStar (pair (unUpStar h) (unUpStar k))
pair :: Applicative f => (a -> f b) -> (c -> f d) -> (a,c) -> f (b,d)
pair h k (x,y) = pure (,) <*> h x <*> k y
type Optic p a b s t = p a b -> p s t
type AdapterP a b s t = forall p . Profunctor p => Optic p a b s t
adapterC2P :: Adapter a b s t -> AdapterP a b s t
adapterC2P (Adapter o i) = dimap o i
instance Profunctor (Adapter a b) where
  dimap f g (Adapter o i) = Adapter (o . f) (g . i)
adapterP2C :: AdapterP a b s t -> Adapter a b s t
adapterP2C l = l (Adapter id id)
type LensP a b s t = forall p . Cartesian p => Optic p a b s t
instance Profunctor (Lens a b) where
  dimap f g (Lens v u) = Lens (v . f) (g . u . cross id f)

instance Cartesian (Lens a b) where
  first (Lens v u)   = Lens (v . fst) (fork (u . cross id fst) (snd . snd))
  second (Lens v u)  = Lens (v . snd) (fork (fst . snd) (u . cross id snd))
lensC2P :: Lens a b s t -> LensP a b s t
lensC2P (Lens v u) = dimap (fork v id) u . first
lensP2C :: LensP a b s t -> Lens a b s t
lensP2C l = l (Lens id fst)
type PrismP a b s t = forall p . Cocartesian p => Optic p a b s t
instance Profunctor (Prism a b) where
  dimap f g (Prism m b) = Prism (plus g id . m . f) (g . b)

instance Cocartesian (Prism a b) where
  left (Prism m b)   = Prism (either (plus Left id . m) (Left . Right)) (Left . b)
  right (Prism m b)  = Prism (either (Left . Left) (plus Right id . m))  (Right . b)
prismC2P :: Prism a b s t -> PrismP a b s t
prismC2P (Prism m b) = dimap m (either id b) . right
prismP2C :: PrismP a b s t -> Prism a b s t
prismP2C l = l (Prism Right id)
traverse ::  (Cocartesian p, Monoidal p) => p a b -> p (FunList a c t) (FunList b c t)
traverse k = dimap out inn (right (par k (traverse k)))
type TraversalP a b s t =  forall p . (Cartesian p, Cocartesian p, Monoidal p) => Optic p a b s t
traversalC2P :: Traversal a b s t -> TraversalP a b s t
traversalC2P (Traversal h) k = dimap h fuse (traverse k)
instance Profunctor (Traversal a b) where
  dimap f g (Traversal h) = Traversal (fmap g . h . f)

instance Cartesian (Traversal a b) where
  first (Traversal h)   = Traversal (\ (s,c) -> fmap (,c) (h s))
  second (Traversal h)  = Traversal (\ (c,s) -> fmap (c,) (h s))

instance Cocartesian (Traversal a b) where
  left (Traversal h)   = Traversal (either (fmap Left . h) (Done . Right))
  right (Traversal h)  = Traversal (either (Done . Left) (fmap Right . h))

instance Monoidal (Traversal a b) where
  par (Traversal h) (Traversal k) = Traversal (pair h k)
  empty = Traversal pure
traversalP2C :: TraversalP a b s t -> Traversal a b s t
traversalP2C l = l (Traversal single)
traverseOf :: TraversalP a b s t -> (forall f . Applicative f => (a -> f b) -> s -> f t)
traverseOf p f = unUpStar (p (UpStar f))
pi1P :: Cartesian p => p a b -> p ( a, c) ( b, c)
pi1P = dimap (fork fst id) (cross id snd) . first
pi11P :: LensP a b ( ( a, c), d) ( ( b, c), d)
pi11P = pi1P . pi1P
theP :: PrismP a b (Maybe a) (Maybe b)
theP = prismC2P the
inorderP :: TraversalP a b (Tree a) (Tree b)
inorderP = traversalC2P inorderC
type  Number    = String
type  ID        = String
type  Name      = String
data  Contact   = Phone Number | Skype ID
data  Entry     = Entry Name Contact
type  Book      = Tree Entry
phone :: PrismP Number Number Contact Contact
phone = prismC2P (Prism m Phone) where
  m (Phone n)  = Right n
  m (Skype s)  = Left (Skype s)

contact :: LensP Contact Contact Entry Entry
contact = lensC2P (Lens v u) where
  v (Entry n c)      = c
  u (c', Entry n c)  = Entry n c'
contactPhone :: TraversalP Number Number Entry Entry
contactPhone = contact . phone
bookPhones ::  TraversalP Number Number Book Book
bookPhones = inorderP . contact . phone
tidyBook :: Book -> Book
tidyBook = bookPhones tidyNumber
printBook :: Book -> IO Book
printBook = traverseOf bookPhones output
listBookPhones :: Book -> [Number]
listBookPhones = getConst . traverseOf bookPhones (Const . (\ x -> [x]))
tidyNumber :: Number -> Number
tidyNumber = id

output :: Show a => a -> IO a
output x = do { print x ; return x }
fork :: (a->b) -> (a->c) -> a -> (b,c)
fork f g x = (f x, g x)
cross :: (a->a') -> (b->b') -> (a,b) -> (a',b')
cross f g (x,y) = (f x, g y)
plus :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
plus f g = either (Left . f) (Right . g)
data List a = Nil | Cons a (List a)
travFunList :: Applicative f => (a -> f b) -> FunList a c t -> f (FunList b c t)
travFunList f (Done t)    = pure (Done t)
travFunList f (More x l)  = pure More <*> f x <*> travFunList f l
identity :: (Cartesian p, Monoidal p) => p a a
identity = dimap lunit' lunit (first empty)
lunit :: ((),a) -> a
lunit ((),a) = a

lunit' :: a -> ((),a)
lunit' a = ((),a)
