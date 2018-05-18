{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

--
-- Grammar Level 4 (circular): ActualDesignator, ActualParameterPart, ActualPart, Aggregate, Allocator, AssociationElement, AssociationList, AttributeName, Choice, Choices, Constraint, DiscreteRange, ElementAssociation, Expression, Factor, FormalDesignator, FormalPart, FunctionCall, IndexConstraint, IndexedName, Literal, Name, NumericLiteral, PhysicalLiteral, Prefix, Primary, QualifiedExpression, Range, RangeConstraint, Relation, SelectedName, ShiftExpression, Signature, SimpleExpression, SliceName, SubtypeIndication, Term, TypeConversion, TypeMark
--

module Level4 where

import BasicTypes
import CharacterSets
import Classes
import Data.Data
import Data.Monoid
import Generics.Eot
import PrettyPrint
import qualified Data.Text.Lazy as T
import qualified Data.Text.Prettyprint.Doc as P
import Test.QuickCheck.Arbitrary
import Classes

type P_MaybeActualParameterPart = (Maybe (T, (NT ActualParameterPart), T))
maybeActualParameterPart :: Decorator f => f (Maybe (T, NT ActualParameterPart, T))
maybeActualParameterPart = o $ do
  po <- parenOpen
  app <- (n93 :: Decorator f => f (NT ActualParameterPart))
  pc <- parenClose
  return (po, app, pc)
-- helper function used with FormalPart and ActualPart
-- dcon '(' actual_designator ')'
mkNameOrTypeMark :: (Decorator m, Rule m a1, Rule m a) => (NT a -> T -> NT a1 -> T -> b) -> m b
mkNameOrTypeMark dcon = do
  name_typemark <- n93 -- either Name OR TypeMark depending on dcon !!
  po <- parenOpen
  fd  <- n93
  pc <- parenClose
  return $ dcon name_typemark po fd pc

-- actual_designator
--          ::= expression
--            | name
--            | 'OPEN'
data ActualDesignator = AD1 (NT Expression) | AD2 (NT Name) | AD3 T deriving (Eq, Show, Data, Generic)
instance (Rule f Expression, Rule f Name) => Rule f ActualDesignator where
  get = trace "ActualDesignator" $ {-# SCC "get_ActualDesignator" #-} c
    [ AD3 <$> (txt "open" [VHDL1993]) -- order matters here
    , AD2 <$> n93 -- try a simple name first
    , AD1 <$> n93
    -- [ AD3 <$> txt "open" [VHDL1993] -- order matters here
    -- , AD2 <$> (n93 :: f (NT Name)) -- try a simple name first
    -- , AD1 <$> (n93 :: f (NT Expression))
    ]

-- actual_parameter_part
--          ::= association_list
newtype ActualParameterPart = APP (NT AssociationList) deriving (Eq, Show, Data, Generic)
instance Rule f AssociationList => Rule f ActualParameterPart where
  get = APP <$> n93

-- actual_part
--          ::= actual_designator
--            | ( name | type_mark ) '(' actual_designator ')'
data ActualPart = AP1 (NT ActualDesignator) | APName (NT Name) T (NT ActualDesignator) T | APTypeMark (NT TypeMark) T (NT ActualDesignator) T deriving (Eq, Show, Data, Generic)
instance (Rule f ActualDesignator, Rule f Name, Rule f TypeMark) => Rule f ActualPart where
  get = trace "ActualPart" $ {-# SCC "get_ActualPart" #-} c
    [ AP1 <$> n93
    , mkNameOrTypeMark APName
    , mkNameOrTypeMark APTypeMark
    ]

-- aggregate
--          ::= '(' element_association ( ',' element_association )* ')'
data Aggregate = MkAggregate T (NT ElementAssociation) [(T, (NT ElementAssociation))] T deriving (Eq, Show, Data, Generic)
instance Rule f ElementAssociation => Rule f Aggregate where
  get = do
    po <- parenOpen
    ea <- n93
    rest <- moreComma
    pc <- parenClose
    return $ MkAggregate po ea rest pc

-- allocator
--          ::= 'NEW' ( subtype_indication | qualified_expression )
data Allocator = A1 T (NT SubtypeIndication) | A2 T (NT QualifiedExpression) deriving (Eq, Show, Data, Generic)
instance (Rule f SubtypeIndication, Rule f QualifiedExpression) => Rule f Allocator where
  get = c
    [ A1 <$> (txt "new" [VHDL1993]) <*> n93
    , A2 <$> (txt "new" [VHDL1993]) <*> n93
    ]

-- association_element
--          ::= ( formal_part '=>' )? actual_part
data AssociationElement = AE (Maybe (NT FormalPart, T)) (NT ActualPart) deriving (Eq, Show, Data, Generic)
instance (Rule f FormalPart, Rule f ActualPart) => Rule f AssociationElement where
  get = do
    fp <- o $ do
      f <- n93
      a <- txt "=>" [VHDL1993]
      return (f, a)
    ap <- n93
    return $ AE fp ap

-- association_list
--          ::= association_element ( ',' association_element )*
data AssociationList = AL (NT AssociationElement) [(T, NT AssociationElement)] deriving (Eq, Show, Data, Generic)
instance Rule f AssociationElement => Rule f AssociationList where
  get = do
    ae <- n93
    rest <- moreComma
    return $ AL ae rest

-- attribute_name
--          ::= prefix signature? "'" attribute_designator ( '(' expression ')' )?
data AttributeName = AN (NT Prefix) (Maybe (NT TypeMark)) T (Maybe (T, (NT Expression), T)) deriving (Eq, Show, Data, Generic)
instance (Rule f Prefix, Rule f TypeMark, Rule f Expression) => Rule f AttributeName where
  get = do
    pp <- n93
    ss <- o n93
    cc <- chr '\'' [VHDL1993]
    -- ad <- n93
    ee <- o $ do
      po <- parenOpen
      e <- n93
      pc <- parenClose
      return (po, e, pc)
    return $ AN pp ss cc ee

-- choice   ::= simple_expression
--            | discrete_range
--            | simple_name
--            | 'OTHERS'
data Choice =
  CSmimpleExpression (NT SimpleExpression)
  | CDiscreteRange (NT DiscreteRange)
  -- | CSimpleName (NT SimpleName)
  | COthers T
  deriving (Eq, Show, Data, Generic)
instance (Rule f SimpleExpression, Rule f DiscreteRange) => Rule f Choice where
  get = c
    [ CSmimpleExpression <$> n93
    , CDiscreteRange     <$> n93
    -- , CSimpleName        <$> n93
    , COthers            <$> txt "others" [VHDL1993]
    ]

-- constraint
--          ::= range_constraint
--            | index_constraint
data Constraint = CRange (NT RangeConstraint) | CIndex (NT DiscreteRange) deriving (Eq, Show, Data, Generic)
instance (Rule f RangeConstraint, Rule f DiscreteRange) => Rule f Constraint where
  get = c
    [ CRange <$> n93
    , CIndex <$> n93
    ]

-- discrete_range
--          ::= subtype_indication
--            | range
data DiscreteRange = DRSubtypeIndication (NT SubtypeIndication) | DRRange (NT Range) deriving (Eq, Show, Data, Generic)
instance (Rule f SubtypeIndication, Rule f Range) => Rule f DiscreteRange where
  get = c
    [ DRSubtypeIndication <$> n93
    , DRRange <$> n93
    ]

-- element_association
--          ::= ( choices '=>' )? expression
data ElementAssociation = EA (Maybe (NT Choice, T)) (NT Expression) deriving (Eq, Show, Data, Generic)
instance (Rule f Choice, Rule f Expression) => Rule f ElementAssociation where
  get = do
    c <- o $ do
      c <- n93
      a <- txt "=>" [VHDL1993]
      return (c, a)
    e <- n93
    return $ EA c e

-- expression
--          ::= relation ( ( 'AND' relation )* | ( 'OR' relation )* | ( 'XOR' relation )* | ( 'NAND' | 'NOR' ) relation | ( 'XNOR' relation )* )
data Expression =
  And (NT SimpleExpression) [(T, (NT SimpleExpression))]
  | Or (NT SimpleExpression) [(T, (NT SimpleExpression))]
  | Xor (NT SimpleExpression) [(T, (NT SimpleExpression))]
  | Nand (NT SimpleExpression) (T, (NT SimpleExpression))
  | Nor (NT SimpleExpression) (T, (NT SimpleExpression))
  | Xnor (NT SimpleExpression) [(T, (NT SimpleExpression))]
  deriving (Eq, Show, Data, Generic)
instance Rule f SimpleExpression => Rule f Expression where
  get = {-# SCC "get_IndexedName" #-} c
    [ And  <$> n93 <*> emore "and"
    , Or   <$> n93 <*> emore "or"
    , Xor  <$> n93 <*> emore "xor"
    , Nand <$> n93 <*> etwo "nand"
    , Nor  <$> n93 <*> etwo "nor"
    , Xnor <$> n93 <*> emore "xnor"
    ]
    where etwo tok = do
            n1 <- txt tok [VHDL1993]
            n2 <- n93
            return (n1, n2)
          emore tok = do
            m $ do
              n2 <- txt tok [VHDL1993]
              n3 <- n93
              return (n2, n3)

-- factor   ::= ( primary '**' | 'ABS' | 'NOT' )? primary
data Factor = FPower (NT Primary) (Maybe (T, (NT Primary))) | FAbs T (NT Primary) | FNot T (NT Primary) deriving (Eq, Show, Data, Generic)
instance Rule f Primary => Rule f Factor where
  get = trace "Factor" $ {-# SCC "get_Factor" #-} c -- c
    [ do
      p <- n93
      rest <- o $ do
        p <- txt "**" [VHDL1993]
        p2 <- n93
        return (p, p2)
      return $ FPower p rest
    , FAbs <$> (txt "abs" [VHDL1993]) <*> n93
    , FNot <$> (txt "not" [VHDL1993]) <*> n93
    ]

-- formal_designator
--          ::= name
newtype FormalDesignator = MkFormalDesignator (NT Name) deriving (Eq, Show, Data, Generic)
instance Rule f Name => Rule f FormalDesignator where
  get = trace "FormalDesignator" $ {-# SCC "get_FormalDesignator" #-} MkFormalDesignator <$> n93

-- formal_part
--          ::= formal_designator
--            | ( name | type_mark ) '(' formal_designator ')'
data FormalPart = FP1 (NT FormalDesignator) | FPName (NT Name) T (NT FormalDesignator) T | FPTypeMark (NT TypeMark) T (NT FormalDesignator) T deriving (Eq, Show, Data, Generic)
instance (Rule f FormalDesignator, Rule f Name, Rule f TypeMark) => Rule f FormalPart where
  get = trace "FormalPart" $ {-# SCC "get_FormalPart" #-} c
    [ FP1 <$> n93
    , mkNameOrTypeMark FPName
    , mkNameOrTypeMark FPTypeMark
    ]

-- function_call
--          ::= name ( '(' actual_parameter_part ')' )?
data FunctionCall = FC (NT Name) P_MaybeActualParameterPart deriving (Eq, Show, Data, Generic)
-- redundant: Rule f ActualParameterPart
instance Rule f Name => Rule f FunctionCall where
  get = trace "FunctionCall" $ {-# SCC "get_FunctionCall" #-} do
    nn <- n93
    app <- maybeActualParameterPart
    return $ FC nn app

-- index_constraint
--          ::= '(' discrete_range ( ',' discrete_range )* ')'
data IndexConstraint = IC T (NT DiscreteRange) [(T, NT DiscreteRange)] T deriving (Eq, Show, Data, Generic)
instance Rule f DiscreteRange => Rule f IndexConstraint where
  get = do
    po <- parenOpen
    dr <- n93
    rest <- moreComma
    pc <- parenClose
    return $ IC po dr rest pc

-- indexed_name
--          ::= prefix '(' expression ( ',' expression )* ')'
data IndexedName = IN (NT Prefix) T (NT Expression) [(T, NT Expression)] T deriving (Eq, Show, Data, Generic)
instance (Rule f Prefix, Rule f Expression) => Rule f IndexedName where
  get = {-# SCC "get_IndexedName" #-} do
    pp <- n93
    po <- parenOpen
    ee <- n93
    ee2 <- moreComma
    pc <- parenClose
    return $ IN pp po ee ee2 pc

-- literal  ::= numeric_literal
--            | enumeration_literal
--            | string_literal
--            | bit_string_literal
--            | 'NULL'
data Literal =
  LNumericLiteral (NT Name)
  -- | LEnumerationLiteral (NT EnumerationLiteral)
  -- | LStringLiteral (NT StringLiteral)
  -- | LBitStringLiteral (NT BitStringLiteral)
  | LNull T
  deriving (Eq, Show, Data, Generic)
instance (Rule f Name) => Rule f Literal where
  get = c
    [ LNumericLiteral     <$> n93
    -- , LEnumerationLiteral <$> n93
    -- , LStringLiteral      <$> n93
    -- , LBitStringLiteral   <$> n93
    , LNull               <$> txt "null" [VHDL1993]
    ]

-- name     ::= simple_name
--            | operator_symbol
--            | selected_name
--            | indexed_name
--            | slice_name
--            | attribute_name
data Name = N3 (NT Prefix) | N4 (NT IndexedName) | N6 (NT AttributeName) deriving (Eq, Show, Data, Generic)
instance (Rule f Prefix, Rule f IndexedName, Rule f AttributeName) => Rule f Name where
  get = trace "Name" $ {-# SCC "get_Name" #-} c
    [ N3 <$> n93
    , N4 <$> n93
    -- , N5 <$> n93
    , N6 <$> n93
    ]

-- prefix   ::= name
--            | function_call
data Prefix = PrefixName (NT Name) | PrefixFunctionCall (NT FunctionCall) deriving (Eq, Show, Data, Generic)
instance (Rule f Name, Rule f FunctionCall) => Rule f Prefix where
  get = trace "Prefix" $ {-# SCC "get_Prefix" #-} c
    [ PrefixName <$> n93
    , PrefixFunctionCall <$> n93
    ]

-- primary  ::= name
--            | literal
--            | aggregate
--            | function_call
--            | qualified_expression
--            | type_conversion
--            | allocator
--            | '(' expression ')'
data Primary =
  PName (NT Name)
  -- | PLiteral (NT Literal)
  | PAggregate (NT Aggregate)
  | PFunctionCall (NT FunctionCall)
  | PQualifiedExpression (NT QualifiedExpression)
  | PTypeConversion (NT TypeConversion)
  | PAllocator (NT Allocator)
  | PExpression T (NT Expression) T
  deriving (Eq, Show, Data, Generic)
--get_levels: instance (Rule f Name, Rule f Aggregate, Rule f FunctionCall, Rule f QualifiedExpression, Rule f TypeConversion, Rule f Allocator, Rule f Expression) => Rule f Primary where
instance (Rule f Name, Rule f Aggregate, Rule f FunctionCall, Rule f QualifiedExpression
         , Rule f TypeConversion, Rule f Allocator, Rule f Expression) => Rule f Primary where
  get = trace "Primary" $ {-# SCC "get_Primary" #-} c
    [ PName                <$> n93
    -- , PLiteral             <$> n93
    , PAggregate           <$> n93
    , PFunctionCall        <$> n93
    , PQualifiedExpression <$> n93
    , PTypeConversion      <$> n93
    , PAllocator           <$> n93
    ,                          exp
    -- [ PName <$> (n93 :: f (NT Name))
    -- , PLiteral <$> (n93 :: f (NT Literal))
    -- , PAggregate <$> (n93 :: f (NT Aggregate))
    -- , PFunctionCall <$> (n93 :: f (NT FunctionCall))
    -- , PQualifiedExpression <$> (n93 :: f (NT QualifiedExpression))
    -- , PTypeConversion <$> (n93 :: f (NT TypeConversion))
    -- , PAllocator <$> (n93 :: f (NT Allocator))
    -- , PExpression <$> parenOpen <*> (n93 :: f (NT Expression)) <*> parenClose
    -- , exp
    ]
    where exp = do
            po <- parenOpen
            ee  <- n93
            pc <- parenClose
            return $ PExpression po ee pc

-- qualified_expression
--          ::= type_mark "'" ( '(' expression ')' | aggregate )
data QualifiedExpression = QEExpression (NT TypeMark) T T (NT Expression) T | EQAggregate (NT TypeMark) T deriving (Eq, Show, Data, Generic)
instance (Rule f TypeMark, Rule f Expression) => Rule f QualifiedExpression where
  get = c [qexp, qagg]
    where qexp = do
            tm <- n93
            q  <- chr '\'' [VHDL1993]
            po <- parenOpen
            ee  <- n93
            pc <- parenClose
            return $ QEExpression tm q po ee pc
          qagg = do
            tm <- n93
            q  <- chr '\'' [VHDL1993]
            -- a  <- n93
            return $ EQAggregate tm q

-- range    ::= attribute_name
--            | simple_expression direction simple_expression
data Range = R1 (NT AttributeName) | R2 (NT SimpleExpression) (NT SimpleExpression) deriving (Eq, Show, Data, Generic)
instance (Rule f AttributeName, Rule f SimpleExpression) => Rule f Range where
  get = c
    [ R1 <$> n93
    , R2 <$> n93<*> n93
    ]

-- range_constraint
--          ::= 'range' range
data RangeConstraint = RC T (NT Range) deriving (Eq, Show, Data, Generic)
instance Rule f Range => Rule f RangeConstraint where
  get = do
    r1 <- txt "range" [VHDL1993]
    r2 <- n93
    return $ RC r1 r2

-- relation
--          ::= shift_expression ( relational_operator shift_expression )?
data Relation = R (NT SimpleExpression) (Maybe ((NT SimpleExpression))) deriving (Eq, Show, Data, Generic)
instance (Rule f SimpleExpression) => Rule f Relation where
  get = do
    se <- n93
    rest <- o $ do
      -- ro <- n93
      se <- n93
      return se
    return $ R se rest

-- shift_expression
--          ::= simple_expression ( shift_operator simple_expression )?
data ShiftExpression = ShiftE (NT SimpleExpression) (Maybe ((NT SimpleExpression))) deriving (Eq, Show, Data, Generic)
instance (Rule f SimpleExpression) => Rule f ShiftExpression where
  get = do
    se <- n93
    rest <- o $ do
      -- so <- n93
      se <- n93
      return se
    return $ ShiftE se rest

-- simple_expression
--          ::= sign? term ( adding_operator term )*
data SimpleExpression = SimpleE (NT Primary) [(NT Primary)] deriving (Eq, Show, Data, Generic)
-- data SimpleExpression = SimpleE T deriving (Eq, Show, Data, Generic)
instance (Rule f Primary) => Rule f SimpleExpression where
  -- get = SimpleE <$> txt "bla" [VHDL1993]
  get = do
    -- ss <- o n93
    tt <- n93
    rest <- m $ do
      -- ao <- n93
      tt2 <- n93
      return tt2
    return $ SimpleE tt rest

-- slice_name
--          ::= prefix '(' discrete_range ')'
data SliceName = SliceNPrefix (NT DiscreteRange) deriving (Eq, Show, Data, Generic)
instance Rule f DiscreteRange => Rule f SliceName where
  get = SliceNPrefix <$> n93

-- subtype_indication
--          ::= name? type_mark constraint?
data SubtypeIndication = SI (Maybe (NT Name)) (NT TypeMark) (Maybe (NT Constraint)) deriving (Eq, Show, Data, Generic)
instance (Rule f Name, Rule f TypeMark, Rule f Constraint) => Rule f SubtypeIndication where
  get = trace "SubtypeIndication" $ {-# SCC "get_SubtypeIndication" #-} do
    nn <- o n93
    tm <- n93
    cc <- o n93
    return $ SI nn tm cc

-- type_conversion
--          ::= type_mark '(' expression ')'
data TypeConversion = MkTypeConversion (NT TypeMark) T (NT Expression) T deriving (Eq, Show, Data, Generic)
instance (Rule f TypeMark, Rule f Expression) => Rule f TypeConversion where
  get = do
    tm <- n93
    po <- parenOpen
    e  <- n93
    pc <- parenClose
    return $ MkTypeConversion tm po e pc

-- type_mark
--          ::= type_name | subtype_name
data TypeMark = TM1 (NT Name) | TM2 (NT Name) deriving (Eq, Show, Data, Generic)
instance Rule f Name => Rule f TypeMark where
  get = trace "TypeMark" $ {-# SCC "get_TypeMark" #-} c
    [ TM1 <$> n93
    , TM2 <$> n93
    ]
  -- todo: figure out later IF and how to make the distinction with the symbol table


-- makeAll [t|Choices|]
-- makeAll [t|NumericLiteral|]
-- makeAll [t|PhysicalLiteral|]
-- makeAll [t|SelectedName|]
-- makeAll [t|Signature|]
-- makeAll [t|Term|]

-- instance Arbitrary ActualDesignator where
--   arbitrary = get
-- instance PrettyPrint ActualDesignator

-- instance Arbitrary ActualParameterPart where
--   arbitrary = get
-- instance PrettyPrint ActualParameterPart

-- instance Arbitrary ActualPart where
--   arbitrary = get
-- instance PrettyPrint ActualPart

-- instance Arbitrary Aggregate where
--   arbitrary = get
-- instance PrettyPrint Aggregate

-- instance Arbitrary Allocator where
--   arbitrary = get
-- instance PrettyPrint Allocator

-- instance Arbitrary AssociationElement where
--   arbitrary = get
-- instance PrettyPrint AssociationElement

-- instance Arbitrary AssociationList where
--   arbitrary = get
-- instance PrettyPrint AssociationList

-- instance Arbitrary AttributeName where
--   arbitrary = get
-- instance PrettyPrint AttributeName

-- instance Arbitrary Choice where
--   arbitrary = get
-- instance PrettyPrint Choice

-- instance Arbitrary Constraint where
--   arbitrary = get
-- instance PrettyPrint Constraint

-- instance Arbitrary DiscreteRange where
--   arbitrary = get
-- instance PrettyPrint DiscreteRange

-- instance Arbitrary ElementAssociation where
--   arbitrary = get
-- instance PrettyPrint ElementAssociation

-- instance Arbitrary Expression where
--   arbitrary = get
-- instance PrettyPrint Expression

-- instance Arbitrary Factor where
--   arbitrary = get
-- instance PrettyPrint Factor

-- instance Arbitrary FormalDesignator where
--   arbitrary = get
-- instance PrettyPrint FormalDesignator

-- instance Arbitrary FormalPart where
--   arbitrary = get
-- instance PrettyPrint FormalPart

-- instance Arbitrary FunctionCall where
--   arbitrary = get
-- instance PrettyPrint FunctionCall

-- instance Arbitrary IndexConstraint where
--   arbitrary = get
-- instance PrettyPrint IndexConstraint

-- instance Arbitrary IndexedName where
--   arbitrary = get
-- instance PrettyPrint IndexedName

-- instance Arbitrary Literal where
--   arbitrary = get
-- instance PrettyPrint Literal

-- instance Arbitrary Name where
--   arbitrary = get
-- instance PrettyPrint Name

-- instance Arbitrary Prefix where
--   arbitrary = get
-- instance PrettyPrint Prefix

-- instance Arbitrary Primary where
--   arbitrary = get
-- instance PrettyPrint Primary

-- instance Arbitrary QualifiedExpression where
--   arbitrary = get
-- instance PrettyPrint QualifiedExpression

-- instance Arbitrary RangeConstraint where
--   arbitrary = get
-- instance PrettyPrint RangeConstraint

-- instance Arbitrary Range where
--   arbitrary = get
-- instance PrettyPrint Range

-- instance Arbitrary Relation where
--   arbitrary = get
-- instance PrettyPrint Relation

-- instance Arbitrary ShiftExpression where
--   arbitrary = get
-- instance PrettyPrint ShiftExpression

-- instance Arbitrary SimpleExpression where
--   arbitrary = get
-- instance PrettyPrint SimpleExpression

-- instance Arbitrary SliceName where
--   arbitrary = get
-- instance PrettyPrint SliceName

-- instance Arbitrary SubtypeIndication where
--   arbitrary = get
-- instance PrettyPrint SubtypeIndication

-- instance Arbitrary TypeConversion where
--   arbitrary = get
-- instance PrettyPrint TypeConversion

-- instance Arbitrary TypeMark where
--   arbitrary = get
-- instance PrettyPrint TypeMark
