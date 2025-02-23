...

-- Internal utilities
escapeSpecialChar :: T.Text -> T.Text
escapeSpecialChar = T.concatMap handleChar
  where
    -- FIXME: probably more cases to handle
    handleChar '\n' = "\n"
    handleChar '\t' = "\t"
    handleChar '\r' = "\r"
    handleChar '"' = "\""
    handleChar '\\' = "\\"
    handleChar c = T.singleton c

-- | Alternative version of 'unwords' that avoid adding spaces on empty strings.
{-# NOINLINE [1] unwords' #-}
unwords' :: [String] -> String
unwords' [] = ""
unwords' ("" : ws) = unwords' ws
unwords' (w : ws) = w ++ go ws
  where
    go [] = ""
    go ("" : vs) = go vs
    go (v : vs) = ' ' : (v ++ go vs)

-- | Append a element to a 'NonEmpty' list.
(|>) :: NonEmpty a -> a -> NonEmpty a
xs |> x = NE.fromList $ NE.toList xs ++ [x]

-- Property paths

-- | Permit different type of property path expressions to be seemlessly be put
--  into arguments for '(.//.)' and similar functions.
class PropertyPathExprLike a where
  propertyPathExpr :: a -> PropertyPathExpr

-- | Possible expressions in a SPARQL property path.
data PropertyPathExpr
  = -- | Property path of length one expression. Ex. @rdf:type@
    PathLengthOneExpr PathLengthOne
  | -- | Ex. @foaf:knows/foaf:knows@
    SequencePathExpr PropertyPathExpr PropertyPathExpr
  | -- | Ex. @^foaf:knows@
    InversePathExpr PropertyPathExpr
  | -- | Ex. @rdfs:label|foaf:name@
    AlternativePathExpr PropertyPathExpr PropertyPathExpr
  | -- | Ex. @rdf:type/rdfs:subClassOf*@
    ZeroOrMorePathExpr PropertyPathExpr
  | -- | Ex. @foaf:knows+@
    OneOrMorePathExpr PropertyPathExpr
  | -- | Ex. @rdf:type/rdfs:subClassOf?@
    ZeroOrOnePathExpr PropertyPathExpr
  | -- | Ex. @!(rdf:type|^rdf:type)@
    NegativePropertySetExpr NegativePropertySet
  deriving (Show)

-- instance PredicateTermLike PropertyPathExpr

-- | Possible expressions in a negated property set.
data NegativePropertySet
  = NegativePropertySetPath PathLengthOne NegativePropertySet
  | NegativePropertySetPathOne PathLengthOne
  deriving (Show)

instance QueryShow NegativePropertySet where
  qshow (NegativePropertySetPath p ps) = qshow p ++ "|" ++ qshow ps
  qshow (NegativePropertySetPathOne p) = qshow p

instance Semigroup NegativePropertySet where
  (NegativePropertySetPathOne p1) <> p2 = NegativePropertySetPath p1 p2
  (NegativePropertySetPath p1 ps) <> p2 = NegativePropertySetPath p1 (ps <> p2)

instance PropertyPathExprLike PropertyPathExpr where
  propertyPathExpr = id

-- newtype IRIRef = IRIRef Text

-- instance Show IRIRef where
--   show (IRIRef iri) = unpack iri

instance PropertyPathExprLike IRIRef where
  propertyPathExpr = PathLengthOneExpr . PathLengthOneIRI

data PathLengthOne
  = PathLengthOneIRI IRIRef
  | PathLengthOneA
  | PathLengthOneInverse PathLengthOne
  deriving (Show)

instance PropertyPathExprLike PathLengthOne where
  propertyPathExpr p = PathLengthOneExpr p

instance NegativePropertySetLike PathLengthOne where
  negativePropertySet p = NegativePropertySetPathOne p

instance QueryShow PropertyPathExpr where
  qshow (PathLengthOneExpr p) = qshow p
  qshow (SequencePathExpr pexpr1 pexpr2) =
    "(" ++ qshow pexpr1 ++ "/" ++ qshow pexpr2 ++ ")"
  qshow (InversePathExpr pexpr) = "(^" ++ qshow pexpr ++ ")"
  qshow (AlternativePathExpr pexpr1 pexpr2) =
    "(" ++ qshow pexpr1 ++ "|" ++ qshow pexpr2 ++ ")"
  qshow (ZeroOrMorePathExpr pexpr) = "(" ++ qshow pexpr ++ "*)"
  qshow (OneOrMorePathExpr pexpr) = "(" ++ qshow pexpr ++ "+)"
  qshow (ZeroOrOnePathExpr pexpr) = "(" ++ qshow pexpr ++ "?)"
  qshow (NegativePropertySetExpr p) = "!(" ++ qshow p ++ ")"

instance QueryShow PathLengthOne where
  qshow (PathLengthOneIRI iri) = qshow iri
  qshow PathLengthOneA = "a"
  qshow (PathLengthOneInverse p) = "^" ++ qshow p

class NegativePropertySetLike a where
  negativePropertySet :: a -> NegativePropertySet

instance NegativePropertySetLike IRIRef where
  negativePropertySet iri = NegativePropertySetPathOne $ PathLengthOneIRI iri

instance NegativePropertySetLike NegativePropertySet where
  negativePropertySet ps = ps

-- | Creating a property path sequence.
--
--  >>> IRIRef "rdf:type" ./. IRIRef "rdfs:subClassOf" ./. IRIRef "rdfs:subClassOf"
--  rdf:type/rdfs:subClassOf/rdfs:subClassOf
infixl 5 .//.

(.//.) ::
  (PropertyPathExprLike a, PropertyPathExprLike b) =>
  a ->
  b ->
  PropertyPathExpr
x .//. y = SequencePathExpr (propertyPathExpr x) (propertyPathExpr y)

-- | Creating an alternative property path.
--
--  >>> IRIRef "rdfs:label" .|. IRIRef "foaf:name" .|. IRIRef "foaf:givenName
--  rdfs:label|foaf:name|foaf:givenName
infixl 4 .|.

(.|.) ::
  (PropertyPathExprLike a, PropertyPathExprLike b) =>
  a ->
  b ->
  PropertyPathExpr
x .|. y = AlternativePathExpr (propertyPathExpr x) (propertyPathExpr y)

-- | Creating an alternative property path inside a negative property set.
--
--  >>> neg $ IRIRef "rdfs:label" ..|.. IRIRef "foaf:name" ..|.. IRIRef "foaf:givenName"
--  !(rdfs:label|foaf:name|foaf:givenName)
infixl 4 ..|..

(..|..) ::
  (NegativePropertySetLike a, NegativePropertySetLike b) =>
  a ->
  b ->
  NegativePropertySet
p1 ..|.. p2 = negativePropertySet p1 <> negativePropertySet p2

-- | Creating an inverse property path.
--
--  >>> inv (IRIRef "foaf:mbox")
--  ^foaf:mbox
inv :: PropertyPathExprLike a => a -> PropertyPathExpr
inv p = InversePathExpr (propertyPathExpr p)

-- | Creating an inverse path of length one inside a negative property set.
--
--  >>> neg $ IRIRef "rdfs:label" ..|.. IRIRef "foaf:name" ..|.. (inv' $ IRIRef "foaf:givenName")
--  !(rdfs:label|foaf:name|^foaf:givenName)
inv' :: NegativePropertySetLike a => a -> NegativePropertySet
inv' p = case negativePropertySet p of
  NegativePropertySetPathOne p1 ->
    NegativePropertySetPathOne (PathLengthOneInverse p1)
  NegativePropertySetPath p1 ps ->
    NegativePropertySetPath (PathLengthOneInverse p1) ps

-- | Creating a negative property set.
--
--  >>> inv foafMbox
--  ^foaf:mbox
neg :: NegativePropertySet -> PropertyPathExpr
neg = NegativePropertySetExpr

-- | Variable "a" representing the @rdf:type@ property.
a :: PathLengthOne
a = PathLengthOneA

-- | Creating a zero or more path.
--
--  >>> IRIRef "rdf:type" ./. ((IRIRef "rdfs:subClassOf") *.)
--  rdf:type/rdfs:subClassOf*
(*.) :: PropertyPathExprLike a => a -> PropertyPathExpr
(*.) p = ZeroOrMorePathExpr (propertyPathExpr p)

-- | Creating a one or more path.
--
--  >>> ((IRIRef "foaf:knows") +.) ./. IRIRef "foaf:name"
--  foaf:knows+/foaf:name
(+.) :: PropertyPathExprLike a => a -> PropertyPathExpr
(+.) p = OneOrMorePathExpr (propertyPathExpr p)

-- | Creating a zero or one path.
--
--  >>> rdfType ./. (rdfsSubClassOf ?.)
--  rdf:type/rdfs:subClassOf?
(?.) :: PropertyPathExprLike a => a -> PropertyPathExpr
(?.) p = ZeroOrOnePathExpr (propertyPathExpr p)
