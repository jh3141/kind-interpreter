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
                 (catAdd newCatalogue nqid (ClassDefinition [])) |@| nqid @?=
                 (Right $ ClassDefinition []),
        testCase "Add qualified item to catalogue" $
                 (catAdd newCatalogue qid (ClassDefinition [])) |@| qid @?=
                 (Right $ ClassDefinition []),
        testCase "Add qualified item to catalogue in existing namespace" $
                 (catAdd
                  (catAdd newCatalogue qid (ClassDefinition []))
                  qid2 (FunctionDefinition [])) |@| qid2 @?=
                 (Right $ FunctionDefinition []),
        testCase "Adding item to existing namespace doesn't affect other items" $
                 (catAdd
                  (catAdd newCatalogue qid (ClassDefinition []))
                  qid2 (FunctionDefinition [])) |@| qid @?=
                 (Right $ ClassDefinition [])
    ]


nqid :: ScopedID
nqid = UnqualifiedID "nqid"
nqid2 :: ScopedID
nqid2 = UnqualifiedID "nqid2"
qid :: ScopedID
qid = QualifiedID "qid_a" $ UnqualifiedID "qid_b"
qid2 :: ScopedID
qid2 = QualifiedID "qid_a" $ UnqualifiedID "qid2_b"
       
