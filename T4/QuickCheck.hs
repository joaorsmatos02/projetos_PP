module QuickCheck where

import Test.QuickCheck 
import T4_Module
import Data.Char (ord, toLower)

------------------------------------------------------------
-- QuickCheck
------------------------------------------------------------
stringRd :: Gen String 
stringRd = listOf $ elements (',' : '.' : ' ' : ';' : ':' : '!' : ['A'..'Z'] ++ ['a'..'z'])

newtype MensagemValida = MensagemValida String
    deriving Show

instance Arbitrary MensagemValida where
    arbitrary = fmap MensagemValida stringRd

-- --------

newtype ChaveValidam = ChaveValidam String
    deriving Show

instance Arbitrary ChaveValidam where
    arbitrary = fmap ChaveValidam stringRdCvm

stringRdCvm :: Gen String 
stringRdCvm = listOf $ elements (['A'..'Z'] ++ ['a'..'z'])

-- --------

newtype ChaveValidaM = ChaveValidaM String
    deriving Show

instance Arbitrary ChaveValidaM where
    arbitrary = fmap ChaveValidaM stringRdCvM

stringRdCvM :: Gen String 
stringRdCvM = listOf $ elements ['A'..'Z'] 

------------------------------------------------------------
-- Properties

prop_cesar_mn :: MensagemValida -> Int -> Int -> Property
prop_cesar_mn (MensagemValida s) i i2 = i >= 0 && i2 >= 0 ==> cesar (cesar s i) i2 == cesar s (i + i2) 

prop_cesar :: MensagemValida -> Int -> Property
prop_cesar (MensagemValida s) i = i >= 0 ==> fromCesar (cesar s i) i == s

prop_vigenere :: MensagemValida -> ChaveValidam -> Property 
prop_vigenere (MensagemValida a) (ChaveValidam b) = not (null b) ==> getVigenere (vigenere a b 0) b 0 == a

prop_subst :: MensagemValida -> ChaveValidaM -> Bool
prop_subst (MensagemValida a) (ChaveValidaM b) = decipherSubtsBack(decipherSubts a b) b == a 

-- ----------------

prop_vigenere_subs :: MensagemValida -> Bool 
prop_vigenere_subs (MensagemValida a) = vigenere a "A" 0 == decipherSubts a []

prop_vigenere_cesar :: MensagemValida -> Bool 
prop_vigenere_cesar (MensagemValida a) =  vigenere a "Z" 0 == cesar a (-1)

prop_subs_cesar :: MensagemValida -> Bool 
prop_subs_cesar (MensagemValida a) = decipherSubts a (['B'..'Z']++['A']) == cesar a 1
