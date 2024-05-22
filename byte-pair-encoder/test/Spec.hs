import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit ((@?=), testCase)
import Tokenizer
-- aaabdaaabac

tests :: TestTree
tests = 
    testGroup "tokeinzer tests" [baseDefTests]
  where
    { 
    baseDefTests = testGroup "Base tests" [
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

    ]
    }

main :: IO ()
main = defaultMain $ testGroup "Tests" [tests]