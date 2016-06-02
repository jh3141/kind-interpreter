module TypeTests.Catalogue(catalogueTypeTests) where

import qualified Data.HashTable.ST.Basic as HT
import qualified Data.HashTable.Class as HTC
import Data.List
import Control.Monad.Except
import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
import KindLang.Util.Control
import KindLang.Data.KStat

catalogueTypeTests :: TestTree
catalogueTypeTests =
    testGroup "Catalogue" [
        testCase "Empty catalogue is empty" $
                 runToEither (newCatalogue >>= catFlatten) @?= Right [],
        testCase "Add item to catalogue" $
                 runToEither (newCatalogue >>= (|+~| (nqid, def))
                                           >>= (|@~| nqid)) @?=
                 (Right $ Left def),
        testCase "Add qualified item to catalogue" $
                 runToEither (newCatalogue >>= (|+~| (qid, def))
                                           >>= (|@~| qid)) @?=
                 (Right $ Left def),
        testCase "Add qualified item to catalogue in existing namespace" $
                 runToEither (newCatalogue >>= (|+~| (qid, def))
                                           >>= (|+~| (qid2, def2))
                                           >>= (|@~| qid2)) @?=
                 (Right $ Left def2),
        testCase "Adding item to existing namespace doesn't affect other items" $
                 runToEither (newCatalogue >>= (|+~| (qid, def))
                                           >>= (|+~| (qid2, def2))
                                           >>= (|@~| qid)) @?=
                 (Right $ Left def),
        testCase "Items with multiple levels of qualification" $
                 (sortOn flatCid $ expectNoErrors "unexpected error"
                  (newCatalogue >>=
                     (|+~| (mqid, def)) >>=
                     (|+~| (mmqid2, def)) >>=
                     (|+~| (mmqid, def)) >>= catFlatten)) @?=
                 [ (mmqid, mmqid, Left def),
                   (mmqid2, mmqid2, Left def),
                   (mqid, mqid, Left def) ],
        testCase "Flatten catalogue with only unqualified items" $
                 expectNoErrors "unexpected error"
                 (newCatalogue >>= (|+~| (nqid, def))
                               >>= (|+~| (nqid2, def))
                               >>= catFlatten) @?=
                 [ (nqid, nqid, Left def), (nqid2, nqid2, Left def) ],
        testCase "Flatten catalogue with qualified items" $
                 expectNoErrors "unexpected error"
                 (newCatalogue >>= (|+~| (qid, def))
                               >>= (|+~| (qid2, def))
                               >>= catFlatten) @?=
                 [ (qid2, qid2, Left def), (qid, qid, Left def) ], -- qid2 < qid
        testCase "Add item with canonical id different to resolvable id" $
                 expectNoErrors "unexpected error"
                 (newCatalogue >>= (|++~| (nqid, qid, def)) >>= catFlatten) @?=
                 [ (nqid, qid, Left def) ]
    ]

def :: Definition
def = ClassDefinition []
def2 :: Definition
def2 = FunctionDefinition []        
nqid :: NSID
nqid = UnqualifiedID "nqid"
nqid2 :: NSID
nqid2 = UnqualifiedID "nqid2"
qid :: NSID
qid = QualifiedID "qid_a" $ UnqualifiedID "qid_b"
qid2 :: NSID
qid2 = QualifiedID "qid_a" $ UnqualifiedID "qid2_b"
mqid :: NSID
mqid = QualifiedID "qid_a" $ QualifiedID "qid3_b" $ UnqualifiedID "qid3_c"
mmqid :: NSID
mmqid = foldrn QualifiedID UnqualifiedID ["a","b","c","d","e","f"]
mmqid2 :: NSID
mmqid2 = foldrn QualifiedID UnqualifiedID ["a","b","c","g","h","i"]
        
