{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Monad
import Course.Applicative
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams :: Chars -> FilePath -> IO (List Chars)
fastAnagrams string dictFile =
  do dictContents <- readFile dictFile
     let dict  = buildDict dictContents
         perms = permutations string
     pure $ filter ((`S.member` dict) . (<$>) toLower) perms

buildDict :: Chars -> S.Set Chars
buildDict contents = foldRight S.insert S.empty lowerLines
  where
    lowerLines = lines $ toLower <$> contents

-- Not using these?

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
