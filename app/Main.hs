{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor.Contravariant
import           Data.Int                       ( Int64 )
import qualified Hasql.Connection              as Connection
import qualified Hasql.Decoders                as Decoders
import qualified Hasql.Encoders                as Encoders
import           Hasql.Session                  ( Session )
import           Hasql.Statement                ( Statement(..) )
import           Prelude
import qualified Hasql.Session                 as Session



main :: IO ()
main = do
  c <- Connection.acquire connectionSettings
  case c of
    Left  e          -> print e
    Right connection -> do
      result <- Session.run (sumAndDivModSession 3 8 3) connection
      print result
 where
  connectionSettings =
    Connection.settings "localhost" 5432 "postgres" "1" "postgres"

-- * Sessions

--
-- Session is an abstraction over the database connection and all possible errors.
-- It is used to execute statements.
-- It is composable and has a Monad instance.
--
-- It's recommended to define sessions in a dedicated 'Sessions'
-- submodule of your project.
-------------------------


sumAndDivModSession :: Int64 -> Int64 -> Int64 -> Session (Int64, Int64)
sumAndDivModSession a b c = do
  liftIO (putStrLn "Dirty IO has hapspened!")
-- Get the sum of a and b
  sumOfAAndB <- Session.statement (a, b) sumStatement
-- Divide the sum by c and get the modulo as well
  Session.statement (sumOfAAndB, c) divModStatement

-- * Statements

--
-- Statement is a definition of an individual SQL-statement,
-- accompanied by a specification of how to encode its parameters and
-- decode its result.
--
-- It's recommended to define statements in a dedicated 'Statements'
-- submodule of your project.
-------------------------


sumStatement :: Statement (Int64, Int64) Int64
sumStatement = Statement sql encoder decoder True
 where
  sql = "select $1 + $2"
  encoder =
    (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8))
      <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
  decoder =
    Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement = Statement sql encoder decoder True
 where
  sql = "select $1 / $2, $1 % $2"
  encoder =
    (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8))
      <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
  decoder = Decoders.singleRow row
   where
    row =
      (,)
        <$> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
