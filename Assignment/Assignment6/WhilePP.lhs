> {-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 

This file contains the definition of the abstract syntax for the 
WhilePP programming language, as well as a pretty printer. You 
do not need to modify this file.

> module WhilePP where

> import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
> import qualified Text.PrettyPrint.HughesPJ as PP
> import Control.Monad.Error

As before, we have variables, and expressions.

> type Variable = String

> data Value =
>     IntVal Int
>   | BoolVal Bool
>   deriving (Show, Eq)

> data Expression =
>     Var Variable
>   | Val Value  
>   | Op  Bop Expression Expression
>   deriving (Show, Eq)

> data Bop = 
>     Plus     
>   | Minus    
>   | Times    
>   | Divide   
>   | Gt        
>   | Ge       
>   | Lt       
>   | Le       
>   deriving (Show, Eq)

For the interpreter exercise, it will be useful to make Value an
instance of the `Error` class:

> instance Error Value where
>   noMsg    = (IntVal 0)

Programs in the language are simply values of this type:

> data Statement =
>     Assign Variable Expression          
>   | If Expression Statement Statement
>   | While Expression Statement       
>   | Sequence Statement Statement        
>   | Skip
>   | Print String Expression
>   | Throw Expression
>   | Try Statement Variable Statement
>   deriving (Show, Eq)


--------------------------------------------------------------------
-- Pretty printing for the WHILE programming language


> class PP a where
>   pp :: a -> Doc

> instance PP Bop where
>   pp Plus   = PP.char '+'
>   pp Minus  = PP.char '-'
>   pp Times  = PP.char '*'
>   pp Divide = PP.char '/'
>   pp Gt     = PP.char '>'
>   pp Ge     = PP.text ">="
>   pp Lt     = PP.char '<'
>   pp Le     = PP.text "<="

> instance PP Value where
>   pp (IntVal i)  = PP.int i 
>   pp (BoolVal b) = if b then PP.text "true" else PP.text "false"

> instance PP Expression where
>   pp (Var x) = PP.text x
>   pp (Val x) = pp x
>   pp e@(Op _ _ _) = ppPrec 0 e  where
>      ppPrec n (Op bop e1 e2) =
>         parens (level bop < n) $
>            ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2 
>      ppPrec _ e' = pp e'
>      parens b = if b then PP.parens else id

> -- use the C++ precendence level table
> level :: Bop -> Int
> level Plus   = 3
> level Minus  = 3 
> level Times  = 5
> level _      = 8

> instance PP Statement where
>   pp (Assign x e) = PP.text x <+> PP.text ":=" <+> pp e
>   pp (If e s1 s2) = 
>     PP.vcat [PP.text "if" <+> pp e <+> PP.text "then",
>          PP.nest 2 (pp s1), 
>          PP.text "else",
>          PP.nest 2 (pp s2),
>          PP.text "endif"]
>   pp (While e s)  = 
>      PP.vcat [PP.text "while" <+> pp e <+> PP.text "do",
>               PP.nest 2 (pp s),
>               PP.text "endwhile"]            
>   pp (Sequence s1@(Sequence _ _) s2) = 
>        PP.parens (pp s1) <> PP.semi $$ pp s2     
>   pp (Sequence s1 s2) = pp s1 <> PP.semi $$ pp s2
>   pp Skip = PP.text "skip"
>   pp (Print s e) = PP.text "print" <+> PP.doubleQuotes (PP.text s) <+> pp e
>   pp (Throw e) = PP.text "throw" <+> pp e 
>   pp (Try s1 v s2) = PP.vcat [ PP.text "try", 
>                                PP.nest 2 (pp s1), 
>                                PP.text "catch" <+> PP.text v <+> PP.text "with",
>                                PP.nest 2 (pp s2),
>                                PP.text "endwith" ]

> display :: PP a => a -> String
> display = show . pp




