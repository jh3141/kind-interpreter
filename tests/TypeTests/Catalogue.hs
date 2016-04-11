module TypeTests.Catalogue(catalogueTypeTests) where

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST

catalogueTypeTests :: TestTree
catalogueTypeTests =
    testGroup "Catalogue" [
        testCase "Empty catalogue is empty" $
                 Map.size newCatalogue @?= 0,
        testCase "Add item to catalogue" $
                 (catAdd newCatalogue nqid def) |@| nqid @?=
                 (Right $ ClassDefinition []),
        testCase "Add qualified item to catalogue" $
                 (catAdd newCatalogue qid def) |@| qid @?=
                 (Right $ ClassDefinition []),
        testCase "Add qualified item to catalogue in existing namespace" $
                 (catAdd
                  (catAdd newCatalogue qid def)
                  qid2 (FunctionDefinition [])) |@| qid2 @?=
                 (Right $ FunctionDefinition []),
        testCase "Adding item to existing namespace doesn't affect other items" $
                 (catAdd
                  (catAdd newCatalogue qid def)
                  qid2 (FunctionDefinition [])) |@| qid @?=
                 (Right $ ClassDefinition []),
        testCase "Items with multiple levels of qualification" $
                 catFlatten (catAdd newCatalogue mqid def) @?=
                 [ (mqid, mqid, def) ],
        testCase "Flatten catalogue with only unqualified items" $
                 (catFlatten $
                  newCatalogue |+| (nqid, def)
                               |+| (nqid2, def)) @?=
                 [ (nqid, nqid, def), (nqid2, nqid2, def) ],
        testCase "Flatten catalogue with qualified items" $
                 (catFlatten $
                  newCatalogue |+| (qid, def)
                               |+| (qid2, def)) @?=
                 [ (qid2, qid2, def), (qid, qid, def) ] -- qid2 < qid
    ]

def :: Definition
def = ClassDefinition []
nqid :: ScopedID
nqid = UnqualifiedID "nqid"
nqid2 :: ScopedID
nqid2 = UnqualifiedID "nqid2"
qid :: ScopedID
qid = QualifiedID "qid_a" $ UnqualifiedID "qid_b"
qid2 :: ScopedID
qid2 = QualifiedID "qid_a" $ UnqualifiedID "qid2_b"
mqid :: ScopedID
mqid = QualifiedID "qid_a" $ QualifiedID "qid3_b" $ UnqualifiedID "qid3_c"
