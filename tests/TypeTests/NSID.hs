module TypeTests.NSID(scopedIDTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.BasicTypes


scopedIDTests :: TestTree
scopedIDTests =
    testGroup "NSID" [
        testCase "NQID qualified by NQID" $
                 nqid `qualifiedBy` nqid2 @?=
                      (QualifiedID "nqid2" $ UnqualifiedID "nqid"),
        testCase "NQID qualified by QID" $
                 nqid `qualifiedBy` qid @?=
                      (QualifiedID "qid_a" $ QualifiedID "qid_b" $
                       UnqualifiedID "nqid"),
        testCase "QID qualified by QID" $
                 qid2 `qualifiedBy` qid @?=
                      (QualifiedID "qid_a" $ QualifiedID "qid_b" $
                       QualifiedID "qid2_a" $ UnqualifiedID "qid2_b"),
        testCase "qualifiedByStrings" $
                 "nqid" `qualifiedByStrings` ["qid_a","qid_b"] @?=
                 nqid `qualifiedBy` qid,
        testCase "qualifierOf nqid" $ qualifierOf nqid @?= Nothing,
        testCase "qualifierOf qid" $ qualifierOf qid @?=
                     (Just $ UnqualifiedID "qid_a"),
        testCase "withoutNamespace nqid" $ withoutNamespace nqid @?= "nqid",
        testCase "withoutNamespace qid" $ withoutNamespace qid @?= "qid_b",
        testCase "long id produced from list" $
                 show ("z" `qualifiedByStrings` ["a","b","c","d"]) @?=
                          "(NSID \"a::b::c::d::z\")",
        testCase "listToNSID" $
                 show (listToNSID ["a","b","c","d"]) @?=
                          "(NSID \"a::b::c::d\")"
    ]


nqid :: NSID
nqid = UnqualifiedID "nqid"
nqid2 :: NSID
nqid2 = UnqualifiedID "nqid2"
qid :: NSID
qid = QualifiedID "qid_a" $ UnqualifiedID "qid_b"
qid2 :: NSID
qid2 = QualifiedID "qid2_a" $ UnqualifiedID "qid2_b"
       
