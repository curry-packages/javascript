------------------------------------------------------------------------------
--- A library to represent JavaScript programs.
---
--- @author Michael Hanus
--- @version May 2017
--- @category general
------------------------------------------------------------------------------

module JavaScript.Show
  ( showJSExp, showJSStat, showJSFDecl )
 where

import Data.List ( intercalate )

import JavaScript.Types

------------------------------------------------------------------------------
--- Shows a JavaScript expression as a string in JavaScript syntax.
showJSExp :: JSExp -> String
showJSExp (JSString s) = "\""++s++"\""
showJSExp (JSInt i) = show i
showJSExp (JSBool b) = if b then "true" else "false"
showJSExp (JSIVar i) = "x"++show i
showJSExp (JSIArrayIdx ai i) = "x"++show ai++"["++show i++"]"
showJSExp (JSOp op e1 e2) =
  "(" ++ showJSExp e1 ++ " " ++ op ++ " " ++ showJSExp e2 ++ ")"
showJSExp (JSFCall f args) =
  f ++ "(" ++ intercalate "," (map showJSExp args) ++ ")"
showJSExp (JSApply f e) = showJSExp f ++ "(" ++ showJSExp e ++ ")"
showJSExp (JSLambda params body) =
  "function(" ++ intercalate "," (map (showJSExp . JSIVar) params) ++
  ") {" ++ concatMap (showJSStat 1) body ++ "} "

--- Shows a JavaScript statement as a string in JavaScript syntax
--- with indenting.
--- @param i - number of spaces to indent this statement
--- @param jstat - the JavaScript statement to print
showJSStat :: Int -> JSStat -> String
showJSStat i (JSAssign e1 e2) =
  blanks i ++ showJSExp e1 ++ " = " ++ showJSExp e2 ++";"
showJSStat i (JSIf e s1 s2) =
  blanks i ++ "if ("++showJSExp e++") {\n"++
  concatMap ((++"\n") . (showJSStat (i+2))) s1 ++
  if null s2
  then blanks i ++ "}"
  else blanks i ++ "} else {\n" ++
       concatMap ((++"\n") . (showJSStat (i+2))) s2 ++
       blanks i ++ "}"
showJSStat i (JSSwitch e bs) =
  blanks i ++ "switch ("++showJSExp e++") {\n"++
  concatMap showJSBranch bs ++
  blanks i ++ "}"
 where
  showJSBranch (JSCase cs bstats) =
     blanks (i+2) ++ "case \"" ++ cs ++ "\" :\n" ++
     concatMap ((++"\n") . (showJSStat (i+4))) bstats ++
     blanks (i+4) ++ "break;\n"
  showJSBranch (JSDefault bstats) =
     blanks (i+2) ++ "default :\n" ++
     concatMap ((++"\n") . (showJSStat (i+4))) bstats

showJSStat i (JSPCall p args) =
  blanks i ++ p ++ "(" ++ intercalate "," (map showJSExp args) ++ ")"
showJSStat i (JSReturn e) = blanks i ++ "return " ++ showJSExp e ++";"
showJSStat i (JSVarDecl vi) = blanks i ++ "var x" ++ show vi ++";"

blanks :: Int -> String
blanks n = replicate n ' '

--- Shows a JavaScript function declaration as a string in JavaScript syntax.
showJSFDecl :: JSFDecl -> String
showJSFDecl (JSFDecl f args body) =
  "function " ++ f ++ "(" ++
      intercalate "," (map showJSExp (map JSIVar args)) ++ ") {\n" ++
  concatMap ((++"\n") . (showJSStat 2)) body ++"}\n\n"

------------------------------------------------------------------------------
