------------------------------------------------------------------------------
--- A library to represent JavaScript programs.
---
--- @author Michael Hanus
--- @version May 2017
--- @category general
------------------------------------------------------------------------------

module JavaScript.Types
  ( JSExp(..), JSStat(..), JSBranch(..), JSFDecl(..), jsConsTerm )
 where

------------------------------------------------------------------------------
--- Type of JavaScript expressions.
--- @cons JSString    - string constant
--- @cons JSInt       - integer constant
--- @cons JSBool      - Boolean constant
--- @cons JSIVar      - indexed variable
--- @cons JSIArrayIdx - array access to index array variable
--- @cons JSOp        - infix operator expression
--- @cons JSFCall     - function call
--- @cons JSApply     - function call where the function is an expression
--- @cons JSLambda    - (anonymous) function with indexed variables as arguments
data JSExp = JSString String
           | JSInt    Int
           | JSBool   Bool
           | JSIVar   Int
           | JSIArrayIdx Int Int
           | JSOp     String JSExp JSExp
           | JSFCall  String [JSExp]
           | JSApply  JSExp JSExp
           | JSLambda [Int] [JSStat]
 deriving Eq

--- Type of JavaScript statements.
--- @cons JSAssign  - assignment
--- @cons JSIf      - conditional
--- @cons JSSwitch  - switch statement
--- @cons JSPCall   - procedure call
--- @cons JSReturn  - return statement
--- @cons JSVarDecl - local variable declaration
data JSStat = JSAssign  JSExp JSExp
            | JSIf      JSExp [JSStat] [JSStat]
            | JSSwitch  JSExp [JSBranch]
            | JSPCall   String [JSExp]
            | JSReturn  JSExp
            | JSVarDecl Int
 deriving Eq

-- Type of branches in a switch statement.
--- @cons JSCase    - case branch
--- @cons JSDefault - default branch
data JSBranch = JSCase String [JSStat]
              | JSDefault [JSStat]
 deriving Eq

-- Type of JavaScript function declarations.
data JSFDecl = JSFDecl String [Int] [JSStat]
 deriving Eq


------------------------------------------------------------------------------
--- Representation of constructor terms in JavaScript as array structures.
--- @param cons - the name of the data constructor
--- @param args - the arguments of the constructor term
jsConsTerm :: String -> [JSExp] -> JSExp
jsConsTerm cons args = JSFCall "new Array" (JSString cons : args)

------------------------------------------------------------------------------
