{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Query.Poptics where

import Prelude hiding (traverse)
import Control.Category ((>>>))
import Control.Applicative (liftA2, Const (..))
import Control.Arrow ((&&&), (|||), (***))
newtype State s a = State { run :: s -> (a,s)}

instance Functor (State s) where
  fmap f m = State (\s -> let (x, s') = run m s in (f x, s'))

instance Applicative (State s) where
  pure x = State (\s -> (x,s))
  m <*> n =
    State (\s -> let (f, s') = run m s
                     (x, s'') = run n s'
                  in (f x, s''))

data Tree a
  = Empty | Node (Tree a) a (Tree a)
  deriving Show

inorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
inorder m Empty = pure Empty
inorder m (Node l x r) = Node <$> inorder m l <*> m x <*> inorder m r

inc :: Bool -> State Integer Bool
inc b = State $ \n -> (b, n + 1)

countOdd :: Integer -> State Integer Bool
countOdd n = if even n then pure False else inc True

ct :: Tree Integer -> State Integer (Tree Bool)
ct = inorder countOdd

-------------

-- | equivalent to a traversable S represented as
-- exists n. A^n x (B^n -> T)
-- where exponentials don't stand for functions, but for
-- size of containers A and B
data FunList a b t = Done t | More a (FunList a b (b -> t))

-- | these two fns witness the isomorphism between
-- FunList A B T and T + (A x (FunList A B (B -> T)))

out :: FunList a b t -> Either t (a, FunList a b (b -> t))
out (Done t) = Left t
out (More x l) = Right (x, l)

inn :: Either t (a, FunList a b (b -> t)) -> FunList a b t
inn (Left t) = Done t
inn (Right (x, l)) = More x l

-- a traversal takes an effectful computation A -> F B onto the
-- elements of a container, lifting this to an effectful computation
-- S -> F T over the whole container. A container S with elements of type A
-- is traversable when B, T and (A -> F B) -> (S -> F T) exist forall Functor F
-- the above function yields an isomorphism S <-> FunList A B T

instance Functor (FunList a b) where
  fmap f (Done t) = Done (f t)
  fmap f (More x l) = More x (fmap (f.) l)

instance Applicative (FunList a b) where
  pure = Done
  Done f <*> l' = fmap f l'
  More x l <*> l' = More x (fmap flip l <*> l')

singleton :: a -> FunList a b b
singleton x = More x (Done id)

-- the above can be seen as a "monoid" for FunList: empty, map, concat

-- | retrieve the traversable container from the FunList:
fuse :: FunList b b t -> t
fuse (Done t) = t
fuse (More x l) = fuse l x

newtype Traversal a b s t = Traversal { extract :: s -> FunList a b t}

-- extract the in-order sequence of elements from the tree,
-- and also allow to refill with a new sequence of elements

inorderC :: Traversal a b (Tree a) (Tree b)
inorderC = Traversal (inorder singleton)

---------
-- do these mean anything?
y :: Tree b -> Tree b
y = fuse . extract inorderC

x :: Traversal (State Integer Bool) b (Tree Integer) (Tree b)
x = Traversal $ inorder (singleton . countOdd)
----------------

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

-- | f can be understood as a preprocessor (which is why it's contravariant:
-- has to output whatever h expects,) and g is a post-processor
-- (which is why it's covariant, it takes what h produces)
instance Profunctor (->) where
  dimap f g h = f >>> h >>> g

-- Any functor can lift to a profunctor via @UpStar@ 

-- | The `profunctors` lib calls this @Star@
newtype UpStar f a b = UpStar {unUpStar :: a -> f b}

instance Functor f => Profunctor (UpStar f) where
  dimap f g (UpStar h) = UpStar (fmap g . h . f)

--- lil' goblin example:
isEvenF :: Integer -> (Bool, Integer)
isEvenF n = (even n, n)

upstarEven :: UpStar ((,) Bool) Integer Integer
upstarEven = UpStar isEvenF

z :: (Num b, Integral a) => UpStar ((,) Bool) a b
z =
  dimap a' b' upstarEven
  where
    a' = (3+) . toInteger
    b' = fromIntegral . (+2)

--- >>> z' 3
-- (True,8)
z' :: (Integral a, Num b) => a -> (Bool, b)
z'= unUpStar z


-- dual of UpStar:

newtype CoStar f a b = CoStar {unCoStar :: f a -> b}

instance Functor f => Profunctor (CoStar f) where
  dimap f g (CoStar h) = CoStar (g . h . fmap f)

-- lil goblin example:

costarHead :: CoStar [] b b
costarHead = CoStar head

-- >>> (unCoStar toHead) ["Luis", "Tina"]
-- "Hello Luis, and goodbye"
toHead :: CoStar [] String String
toHead =
  dimap a' b' costarHead
  where
    a' = ("Hello "<>)
    b' = (<>", and goodbye")

----------------------------------

-- | Strength with respect to product types.
-- @profunctors@ calls this @Strong@,
-- also says it's the "generalizing Star of a strong Functor"
class Profunctor p => Cartesian p where
  first :: p a b -> p (a,c) (b,c)
  second :: p a b -> p (c,a) (c,b)

instance Cartesian (->) where
  first h = cross h id
  second h = cross id h

cross :: (t1 -> a) -> (t2 -> b) -> (t1, t2) -> (a, b)
cross f g (a,b) = (f a, g b)

-- >>> feven (2, "hello")
-- (True,"hello")
feven :: (Integer, c) -> (Bool, c)
feven = first even

instance Functor f => Cartesian (UpStar f) where
  first (UpStar u) = UpStar (rstrength . cross u id)
  second (UpStar u) = UpStar (lstrength . cross id u)

rstrength :: Functor f => (f a, b) -> f (a, b)
rstrength (fx, y) = fmap (,y) fx

lstrength :: Functor f => (a, f b) -> f (a, b)
lstrength (x, fy) = fmap (x,) fy

-- lil goblin example:

-- >>> (unUpStar zStar) (3, "ignored")
-- (False,(3,"ignored"))
zStar :: UpStar ((,) Bool) (Integer, c) (Integer, c)
zStar = first upstarEven

--------

-- | Known in @profunctors@ as @Choice@,
-- "generalizing CoStar of a Functor that's strong in Either"
class Profunctor p => CoCartesian p where
  left :: p a b -> p (Either a c) (Either b c)
  right :: p a b -> p (Either c a) (Either c b)

instance CoCartesian (->) where
  left h = plus h id
  right h = plus id h

plus :: (t1 -> a) -> (t2 -> b) -> Either t1 t2 -> Either a b
plus f g (Left x) = Left (f x)
plus f g (Right x) = Right (g x)

-- -- NOTE(luis), I have no idea how to implement CoCartesian
-- for CoStar... how the heck is a functor "strong with respect to
-- either"???????
-- sumLStrength :: Applicative f => Either (f a) b -> f (Either a b)
-- sumLStrength (Left fx) = fmap Left fx
-- sumLStrength (Right y) = pure (Right y) 

-- sumRStrength :: Applicative f => Either a (f b) -> f (Either a b)
-- sumRStrength (Left x) = pure (Left x)
-- sumRStrength (Right fy) = fmap Right fy

-- --strengthl :: Functor f => f (Either a b) -> Either (f a) b
-- --strengthl fa = undefined

-- instance Applicative f => CoCartesian (CoStar f) where
--   left (CoStar c) = CoStar (strengthl . c) --(strengthl . plus c id)

-- even though @Profunctors@ claims that Choice (CoCartesian)
-- is "the Costar for Functors strong in Either," the paper says that
-- "there's no dual construction for functions with structured arguments"

instance Applicative f => CoCartesian (UpStar f) where
  left (UpStar u) = UpStar (either (fmap Left . u) (pure . Right))
  right (UpStar u) = UpStar (either (pure . Left) (fmap Right . u))

---------

class Profunctor p => Monoidal p where
  par :: p a b -> p c d -> p (a, c) (b, d)
  empty :: p () ()

instance Monoidal (->) where
  par = cross
  empty = id

instance Applicative f => Monoidal (UpStar f) where
  empty = UpStar pure
  par h k = UpStar $ pair (unUpStar h) (unUpStar k)

pair :: Applicative f => (t1 -> f a1) -> (t2 -> f a2) -> (t1, t2) -> f (a1, a2)
pair h k (x,y) = (,) <$> h x <*> k y

--instance Applicative f => (a -> f b) -> (c -> f d) -> (a, c) -> f (b, d)

-----------
-- OPTICS
-----------

type Optic p a b s t = p a b -> p s t

data Adapter a b s t = Adapter {from :: s -> a, to :: b -> t}

type AdapterP a b s t = forall p. Profunctor p => Optic p a b s t

adapterC2P :: Adapter a b s t -> AdapterP a b s t
adapterC2P (Adapter o i) = dimap o i

instance Profunctor (Adapter a b) where
  dimap f g (Adapter o i)  = Adapter (o .f) (g . i)

adapterP2C :: AdapterP a b s t -> Adapter a b s t
adapterP2C l = l (Adapter id id)

-- goblin:

c' :: Adapter Bool Bool Integer Integer
c' = Adapter even (\b -> if b then 1 else 0)
--c2p :: Optic AdapterP Bool Bool Integer Integer

c2p :: Profunctor p => Optic p Bool Bool Integer Integer
c2p = adapterC2P c'

p2c :: Adapter Bool Bool Integer Integer
p2c = adapterP2C c2p

--------------

data Lens a b s t = Lens {view :: s -> a, update :: (b,s) -> t}

type LensP a b s t = forall p. Cartesian p => Optic p a b s t

instance Profunctor (Lens a b) where
  dimap f g (Lens v u) = Lens (f >>> v) (cross id f >>> u >>> g)

-- NOTE(luis) the fanout operator (&&&) is used here, but the paper calls for
-- a fork function that is equivalent: fork f g x = (f x, g x)
instance Cartesian (Lens a b) where
  first (Lens v u)  = Lens (fst >>> v) ((cross id fst >>> u) &&& (snd >>> snd))
  second (Lens v u) = Lens (snd >>> v) ((snd >>> fst) &&& (cross id snd >>> u))

lensC2P :: Lens a b s t -> LensP a b s t
lensC2P (Lens v u) = first >>> dimap (v &&& id) u

lensP2C :: LensP a b s t -> Lens a b s t
lensP2C l = l (Lens id fst)

-- goblin

data Goblin = Goblin {name :: String, age :: Integer}

nameL :: Lens String String Goblin Goblin
nameL = Lens name (\(newName, g) -> g{name = newName})


nameLP :: Cartesian p => Optic p String String Goblin Goblin
nameLP = lensC2P nameL

-- >>> (view nameL) aGoblin
-- "timby"
-- but how to use the profunctor version??
aGoblin :: Goblin
aGoblin = Goblin "timby" 42

---

data Prism a b s t = Prism {match :: s -> Either t a, build :: b -> t}

type PrismP a b s t = forall p. CoCartesian p => Optic p a b s t

instance Profunctor (Prism a b) where
  dimap f g (Prism m b) = Prism (f >>> m >>> plus g id) (b >>> g)

instance CoCartesian (Prism a b) where
  left (Prism m b) = Prism (either (m >>> plus Left id) (Right >>> Left)) (b >>> Left)
  right (Prism m b) = Prism (either (Left >>> Left) (m >>> plus Right id)) (b >>> Right)

prismC2P :: Prism a b s t -> PrismP a b s t
prismC2P (Prism m b) = right >>> dimap m (either id b)

prismP2C :: PrismP a b s t -> Prism a b s t
prismP2C l = l (Prism Right id)


-----

--data Traversal a b s t = Traversal {extract :: s -> FunList a b t}

traverse :: (CoCartesian p, Monoidal p) => p a b -> p (FunList a c t) (FunList b c t)
traverse k = dimap out inn (right $ par k (traverse k))

type TraversalP a b s t = forall p. (Cartesian p, CoCartesian p, Monoidal p) => Optic p a b s t

traversalC2P :: Traversal a b s t -> TraversalP a b s t
traversalC2P (Traversal h) k = dimap h fuse (traverse k)

instance Profunctor (Traversal a b) where
  dimap f g (Traversal h) = Traversal (f >>> h >>> fmap g)

instance Cartesian (Traversal a b) where
  first (Traversal h)  = Traversal (\(s,c) -> fmap (,c) (h s))
  second (Traversal h) = Traversal (\(c,s) -> fmap (c,) (h s))

instance CoCartesian (Traversal a b) where
  left (Traversal h)  = Traversal $ either (h >>> fmap Left) (Right >>> Done)
  right (Traversal h) = Traversal $ either (Left >>> Done) (h >>> fmap Right)

instance Monoidal (Traversal a b) where
  par (Traversal h) (Traversal k) = Traversal (pair h k)
  empty = Traversal pure

traversalP2C :: TraversalP a b s t -> Traversal a b s t
traversalP2C l = l (Traversal singleton)

traverseOf :: TraversalP a b s t -> (forall f. Applicative f => (a -> f b) -> s -> f t)
traverseOf p f = unUpStar (p (UpStar f))

------------

pair1 :: Lens a b (a,c) (b,c)
pair1 =
  Lens v u
  where
    v (a, c)= a
    u (b, (a,c)) = (b,c)

pair1P' ::LensP a b (a, c) (b, c)
pair1P' = lensC2P pair1

--- | NOTE(luis) the paper of course doesn't use any Arrow operators here,
--- I just found it interesting how fanout and split seem to fit right at
-- home here.
pair1P :: LensP a b (a,c) (b,c)
pair1P = first >>> dimap (fst &&& id) (second snd)--- same as: (id *** snd) or (cross id snd)

-- trivial lens composition (the record/concrete representation is much harder to compose)

pairP11 :: LensP a b ((a,c),d) ((b,c),d)
pairP11 = pair1P . pair1P

the :: Prism a b (Maybe a) (Maybe b)
the =
  Prism m b
  where
    m Nothing = Left Nothing
    m (Just a) = Right a
    b b' = Just b'

theP' :: PrismP a b (Maybe a) (Maybe b)
theP' = prismC2P the

theP :: PrismP a b (Maybe a) (Maybe b)
theP = right >>> dimap (maybe (Left Nothing) Right) (either id Just)

-- combination of optics: neither of these are purely a lens or a prism!

-- optic onto the first component of an optional pair:
-- using the profunctor instance of functions to act onto a lens:
-- >>> thePP1 (^2) (Just (3, True))
-- Just (9,True)

thePP1 :: (Cartesian p, CoCartesian p) => Optic p a b (Maybe (a, c)) (Maybe (b, c))
thePP1 = pair1P >>> theP

-- optic onto the optional first component of a pair:
p1The :: (Cartesian p, CoCartesian p) => Optic p a b (Maybe a, c) (Maybe b, c)
p1The = theP >>> pair1P

inorderP :: TraversalP a b (Tree a) (Tree b)
inorderP = traversalC2P inorderC

--- the above can be composed with other optics:
firstInLeafPairs :: TraversalP a b (Tree (a,c)) (Tree (b,c))
firstInLeafPairs = pair1P >>> inorderP

---optionalLeaf :: TraversalP a b (Tree (Maybe a)) (Tree (Maybe b))
---optionalLeaf :: Prism a b (Maybe a) c
optionalLeaf :: TraversalP a b (Tree (Maybe a)) (Tree (Maybe b))
optionalLeaf = theP >>> inorderP

--- can turn an optic into a traversal
--- >>> run (traverseOf firstInLeafPairs countOdd tree1) 0
-- (Node (Node Empty (True,"hello") Empty) (False,"goodbye") (Node Empty (True,"hi again") Empty),2)
--- >>> run (traverseOf optionalLeaf countOdd tree2) 0
-- (Node (Node Empty (Just True) Empty) (Just False) (Node Empty Nothing Empty),1)
tree1 :: Tree (Integer, [Char])
tree1 = Node (Node Empty (1,"hello") Empty) (2, "goodbye") (Node Empty (3,"hi again") Empty)
tree2 :: Tree (Maybe Integer)
tree2 = Node (Node Empty (Just 1) Empty) (Just 2) (Node Empty (Nothing) Empty)

-- another example

type Number = String
type ID = String
type Name = String
data Contact = Phone Number | Skype ID
data Entry = Entry Name Contact
type Book = Tree Entry

phone :: PrismP Number Number Contact Contact
phone = prismC2P (Prism m Phone) where
  m (Phone n) = Right n
  m (Skype s) = Left (Skype s)

contact :: LensP Contact Contact Entry Entry
contact = lensC2P (Lens v u) where
  v (Entry n c) = c
  u (c', Entry n c) = Entry n c'

-- | interesting that this is chosen as a traversal vs "just an optic?" in the paper?
contactPhone :: (Cartesian p, CoCartesian p) => Optic p Number Number Entry Entry--TraversalP Number Number Entry Entry
contactPhone = contact.phone --phone >>> contact

bookPhones :: TraversalP Number Number Book Book
bookPhones = inorderP.contactPhone --phone >>> contact >>> inorderP 

tidyNumber :: Number -> Number
tidyNumber = id

tidyBook :: Book -> Book
tidyBook = bookPhones tidyNumber

output :: Number -> IO Number
output n = print n >> pure n

printBook :: Book -> IO Book
printBook = traverseOf bookPhones output

listBookPhones :: Book -> [Number]
listBookPhones = getConst . traverseOf bookPhones (Const . (: []))
