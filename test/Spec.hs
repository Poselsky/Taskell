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
    let a = parseToplevel "int b = 0; if ( 55 > b ) { b; } else { b; }"
    assertEqual "Parse if expr: " "Right [ [ ( BinaryOp = ( Var b Int Nothing ) Int Just 0 ) ][ If( ( BinaryOp > Int Just 55 ( Var b Int Nothing ) )){ [ ( Var b Int Nothing ) ] } else { [ ( Var b Int Nothing ) ] } ] ]" (show a)

parseTopLevel_testVarAssignExpr:: Test
parseTopLevel_testVarAssignExpr = "" ~: do
        let a = parseToplevel "int a = 0;"
        assertEqual "Parse varWithAssign: " "Right [ [ ( BinaryOp = ( Var a Int Nothing ) Int Just 0 ) ] ]" (show a)

parseTopLevel_testVarExpr:: Test
parseTopLevel_testVarExpr = "" ~: do
        let a = parseToplevel "int a;"
        assertEqual "Parse var simple expr: " "Right [ [ ( Var a Int Nothing ) ] ]" (show a)

parseTopLevel_testVarExprAssignLater_shouldFail:: Test
parseTopLevel_testVarExprAssignLater_shouldFail = "Test with semicolon between var and assignment" ~: do
        let a = parseToplevel "int a; a; = 0"
        case a of
          -- We expect this method to fail
          Left pe -> assertBool "This case should pass" True
          Right exs -> assertFailure $ "Semicolon between variable and assign sign is not permited: " ++ show exs

parseTopLevel_testVarExprAssignLater_Improvement:: Test
parseTopLevel_testVarExprAssignLater_Improvement = "" ~: do
        let a = parseToplevel "int a; a = 0;"
        assertEqual "Parse varWithAssign improvement: " "Right [ [ ( Var a Int Nothing ) ][ ( BinaryOp = ( Var a Int Nothing ) Int Just 0 ) ] ]" (show a)

parseTopLevel_longFunctionArgs:: Test
parseTopLevel_longFunctionArgs = "" ~: do
        let a = parseToplevel "fun(int a, int b, int c, int d) : int { }"
        assertEqual "Parse with many func args: " "Right [ Function \"int\" fun [ ( Var a Int Nothing )( Var b Int Nothing )( Var c Int Nothing )( Var d Int Nothing ) ] { [  ] }  ]" $ show a 

parseTopLevel_sumFunction:: Test
parseTopLevel_sumFunction = "" ~: do
        let a = parseToplevel "fun(int a, int b) : int { int c = a + b; }"
        case a of
          Left pe -> do 
              assertFailure $ show pe 
          Right exs -> assertEqual "" "[ Function \"int\" fun [ ( Var a Int Nothing )( Var b Int Nothing ) ] { [ ( BinaryOp = ( Var c Int Nothing ) ( BinaryOp + ( Var a Int Nothing ) ( Var b Int Nothing ) ) ) ] }  ]" $ show exs 

parseTopLevel_varDeclaration:: Test
parseTopLevel_varDeclaration = "" ~: do
        let a = parseToplevel "int a;"
        case a of
          Left pe -> do 
              assertFailure $ show pe 
          Right exs -> assertBool "Parse var declaration" True 

parseTopLevelTests= TestList 
    [
      parseTopLevel_testAssignIfExpr
    , parseTopLevel_testVarAssignExpr
    , parseTopLevel_testVarExpr
    , parseTopLevel_testVarExprAssignLater_shouldFail
    , parseTopLevel_longFunctionArgs
    , parseTopLevel_sumFunction
    , parseTopLevel_varDeclaration
    ]

testImprovements = TestList 
    [
        parseTopLevel_testVarExprAssignLater_Improvement
    ]