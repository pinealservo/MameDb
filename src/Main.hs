{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import Data.Maybe

import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T

import MameDb.Types
import MameDb.XmlParse
import MameDb.Database


main :: IO ()
main = do
  mc <- dbConnect
  case mc of
    Nothing -> print "Database not available!"
    Just c  -> do
      dbReset c
      runResourceT $
        parseMachinesFrom "info/mame.xml" $
          CL.mapM_ (lift . dbInsertMachine c)
      dbDisconnect c
      putStrLn "Finished reading mame.xml"
