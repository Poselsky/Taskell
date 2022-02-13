{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.HUnit 
import Parsing.Parser
import System.Environment (getArgs)

main:: IO () 
main = do 
    print <$> getArgs
    _ <- runTestTT $ "ParseTopLevelTests," ~: parseTopLevelTests 
    _ <- runTestTT $ "parsing test improvements," ~: testImprovements 
    return ()
    

parseTopLevel_testAssignIfExpr:: Test
parseTopLevel_testAssignIfExpr = "" ~: do 
    let a = parseToplevel "int a = 0; if( a; < 0.35) {a; }else{ a; }"
    assertEqual "Parse if expr" "Right [( BinaryOp = ( Var a Int Nothing ) Int Just 0 ),If( ( BinaryOp < ( Var a Int Nothing ) Float Just 0.35 )){ ( Var a Int Nothing ) } else { ( Var a Int Nothing ) }]" (show a)

parseTopLevel_testVarAssignExpr:: Test
parseTopLevel_testVarAssignExpr = "" ~: do
        let a = parseToplevel "int a = 0;"
        assertEqual "Parse varWithAssign" "Right [( BinaryOp = ( Var a Int Nothing ) Int Just 0 )]" (show a)

parseTopLevel_testVarExpr:: Test
parseTopLevel_testVarExpr = "" ~: do
        let a = parseToplevel "int a;"
        assertEqual "Parse varWithAssign" "Right [( Var a Int Nothing )]" (show a)

parseTopLevel_testVarExprAssignLater_shouldFail:: Test
parseTopLevel_testVarExprAssignLater_shouldFail = "Test with semicolon between var and assignment" ~: do
        let a = parseToplevel "int a; a; = 0"
        case a of
          Left pe -> assertBool "This case should pass" True
          Right exs -> assertFailure $ "Semicolon between variable and assign sign is not permited: " ++ show exs

parseTopLevel_testVarExprAssignLater_Improvement:: Test
parseTopLevel_testVarExprAssignLater_Improvement = "" ~: do
        let a = parseToplevel "int a; a = 0;"
        assertEqual "Parse varWithAssign improvement: " "Right [( Var a Int Nothing ),( BinaryOp = ( Var a Int Nothing ) Int Just 0 )]" (show a)

parseTopLevelTests= TestList 
    [
      parseTopLevel_testAssignIfExpr
    , parseTopLevel_testVarAssignExpr
    , parseTopLevel_testVarExpr
    , parseTopLevel_testVarExprAssignLater_shouldFail
    ]

testImprovements = TestList 
    [
        parseTopLevel_testVarExprAssignLater_Improvement
    ]