import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit ((@?=), testCase)
import Tokenizer


-- testSimplify :: TestTree
-- testSimplify = 
--     testGroup "Simplify" [plusTests, minusTests, sqrtTests, multTests, divTests]
--   where {
--     plusTests = testGroup "Plus" [
--         testCase "1 + 0 + 1 == 1 + 1" $ simplify (1 + 0 + 1) @?= 1 + 1 
--       , testCase "1 + 1 + 2 == 1 + 1 + 2" $ simplify (1 + 1 + 2) @?= 1 + 1 + 2
--     ];
--     minusTests = testGroup "Minus" [
--         testCase "1 - (1 - 0) == 0" $ simplify (1 - 1 - 0) @?= 0
--       , testCase "1 - (2 - 0) == 1 - 2" $ simplify (1 - (2 - 0)) @?= 1 - 2
--       , testCase "1 - (0 - 1)" $ simplify ((Bin (Const 1.0) Minus (Bin (Const 0.0) Minus (Const 1.0)))) @?= Bin (Const 1.0) Minus (Const (-1.0))
--       , testCase "x - x == 0" $ simplify (Bin (Var "x") Minus (Var "x")) @?= Const 0
--       ];
--     sqrtTests = testGroup "Sqrt" [
--         testCase "sqrt(1) == 1" $ simplify( Sqrt (Const 1.0)) @?= Const 1.0
--       , testCase "sqrt(0) == 0" $ simplify(Sqrt (Const 0.0)) @?= Const 0.0
--       , testCase "sqrt(2) == sqrt(2)" $ simplify(Sqrt (Const 2.0)) @?= Sqrt (Const 2.0)
--     ];
--     multTests = testGroup "Mult" [
--         testCase "3 * 0 == 0" $ simplify (3 * 0) @?= 0
--       , testCase "0 * sqrt(7) == 0" $ simplify (Bin (Const 0.0) Mult (Sqrt (Const 7.0))) @?= 0
--       , testCase "z * 1 == z" $ simplify (Bin (Var "z") Mult (Const 1)) @?= Var "z"
--       , testCase "(1 + 7) * 1 == 1 + 7" $ simplify ((1 + 7) * 1) @?= 1 + 7
--     ];
--     divTests = testGroup "Div" [
--         testCase "0 / x == 0 / x" $ simplify (Bin (Const 0) Div (Var "x")) @?= Bin (Const 0) Div (Var "x")
--       , testCase "y / 1 == y" $ simplify (Bin (Var "y") Div (Const 1.0)) @?= Var ("y")
--       , testCase "(1 + 7) / 1 == 1 + 7" $ simplify (Bin (Bin (Const 1.0) Plus (Const 7.0)) Div (Const 1)) @?= 1 + 7
--     ]
--     }

tests :: TestTree
tests = 
    testGroup "tokeinzer tests" [textToStateTests, tokenCreationTests, topTokensTest, topTokensInTextTest]
  where {
    textToStateTests = testGroup "text conversion tests" [
        testCase "wikipedia text to state" $ textToTokenizerState "aaabdaaabac" @?= (
            TokenizerState {text = "aaabdaaabac"
                            , encodedText = [97,97,97,98,100,97,97,97,98,97,99]
                            , decodeTable = [(97,"a"),(98,"b"),(100,"d"),(99,"c")]
                            }),

        testCase "natural text" $ textToTokenizerState "Thanks for all the support, and thanks for your sustained interest in the things I have made." @?= (
            TokenizerState {text = "Thanks for all the support, and thanks for your sustained interest in the things I have made."
                            , encodedText = [84,104,97,110,107,115,32,102,111,114,32,97,108,108,32,116,104,101,32,115,117,112,112,111,114,116,44,32,97,110,100,32,116,104,97,110,107,115,32,102,111,114,32,121,111,117,114,32,115,117,115,116,97,105,110,101,100,32,105,110,116,101,114,101,115,116,32,105,110,32,116,104,101,32,116,104,105,110,103,115,32,73,32,104,97,118,101,32,109,97,100,101,46]
                            , decodeTable = [(84,"T"),(104,"h"),(97,"a"),(110,"n"),(107,"k"),(115,"s"),(32," "),(102,"f"),(111,"o"),(114,"r"),(108,"l"),(116,"t"),(101,"e"),(117,"u"),(112,"p"),(44,","),(100,"d"),(121,"y"),(105,"i"),(103,"g"),(73,"I"),(118,"v"),(109,"m"),(46,".")]
                            })       
        ];
    
    tokenCreationTests = testGroup "token creation tests" [
        testCase "make one token from wikipedia text" $ makeOneToken (textToTokenizerState "aaabdaaabac") @?= (
                TokenizerState {text = "aaabdaaabac"
                                , encodedText = [101,97,98,100,101,97,98,97,99]
                                , decodeTable = [(101,"aa"),(97,"a"),(98,"b"),(100,"d"),(99,"c")]
                                }),
        testCase "make two tokens from wikipedia text" $ makeNTokens (textToTokenizerState "aaabdaaabac") 2 @?= (
                TokenizerState {text = "aaabdaaabac"
                                , encodedText = [102,98,100,102,98,97,99]
                                , decodeTable = [(102,"aaa"),(101,"aa"),(97,"a"),(98,"b"),(100,"d"),(99,"c")]
                                })
        ];
    
    topTokensTest = testGroup "top token tests" [
        testCase "top tokens from wikipedia example" $  topNTokens (makeNTokens (textToTokenizerState "aaabdaaabac") 2) 5 @?= (
                [(98,2),(102,2),(97,1),(99,1),(100,1)])
        ];

    topTokensInTextTest = testGroup "conversion back to text" [
        testCase "top tokens from wikipedia example" $  tokenizerStateToText (makeNTokens (textToTokenizerState "aaabdaaabac") 2) @?= (
                "aaa b d aaa b a c")
        ]
    
    
    }

main :: IO ()
main = defaultMain $ testGroup "Tests" [tests]