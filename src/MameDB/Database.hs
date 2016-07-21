{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
module MameDb.Database
  ( dbConnect
  , dbDisconnect
  , dbInsertMachine
  , dbCreate
  , dbReset
  ) where

import Control.Arrow

import Data.Functor.Contravariant (contramap)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Int (Int32)

import Hasql.Connection
import Hasql.Session
import Hasql.Query
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import MameDb.Types

dbConnect :: IO (Maybe Connection)
dbConnect = do
  let conOpts = settings "localhost" 5432 "mamedb" "" "mamedb"
  res <- acquire conOpts
  case res of
    Left err   -> print err >> return Nothing
    Right conn -> return (Just conn)

dbDisconnect :: Connection -> IO ()
dbDisconnect = release

createTables :: Session ()
createTables =
  sql "CREATE TABLE machine ( \
      \  id serial, \
      \  name text NOT NULL, \
      \  description text NOT NULL, \
      \  year text, \
      \  manufacturer text, \
      \  PRIMARY KEY (id), \
      \  UNIQUE (name) \
      \); \
      \CREATE TABLE rom ( \
      \  id serial, \
      \  size integer NOT NULL, \
      \  crc text NOT NULL, \
      \  sha1 text, \
      \  PRIMARY KEY (id), \
      \  UNIQUE (crc), \
      \  UNIQUE (sha1) \
      \); \
      \CREATE TABLE machine_roms ( \
      \  id serial, \
      \  machine_id integer NOT NULL, \
      \  rom_id integer NOT NULL, \
      \  name text NOT NULL, \
      \  parent_name text, \
      \  status text NOT NULL, \
      \  optional boolean NOT NULL, \
      \  PRIMARY KEY (id), \
      \  FOREIGN KEY (machine_id) REFERENCES machine(id) \
      \    ON DELETE CASCADE ON UPDATE CASCADE, \
      \  FOREIGN KEY (rom_id) REFERENCES rom(id) \
      \    ON DELETE CASCADE ON UPDATE CASCADE \
      \);"

dropTables :: Session ()
dropTables =
  sql "DROP TABLE machine_roms; \
      \DROP TABLE rom; \
      \DROP TABLE machine;"

insertMachine :: Query Machine Int32
insertMachine = statement sqltext encoder decoder True
  where
    sqltext = "INSERT INTO machine \
              \  (name, description, year, manufacturer) \
              \VALUES \
              \  ($1, $2, $3, $4) \
              \RETURNING id;"
    encoder =
      contramap (name . attributes) (E.value E.text)  <>
      contramap description (E.value E.text)          <>
      contramap year (E.nullableValue E.text)         <>
      contramap manufacturer (E.nullableValue E.text)
    decoder =
      D.singleRow (D.value D.int4)

checkRom :: Query Text (Maybe Int32)
checkRom = statement sqltext encoder decoder True
  where
    sqltext = "SELECT id FROM rom WHERE crc = $1;"
    encoder = E.value E.text
    decoder = D.maybeRow (D.value D.int4)

insertRom :: Query Rom Int32
insertRom = statement sqltext encoder decoder True
  where
    sqltext = "INSERT INTO rom \
              \  (size, crc, sha1) \
              \VALUES \
              \  ($1, $2, $3) \
              \RETURNING id;"
    encoder =
      contramap (fromIntegral . romSize) (E.value E.int4) <>
      contramap (fromMaybe "" . romCrc) (E.value E.text)  <>
      contramap romSha1 (E.nullableValue E.text)
    decoder =
      D.singleRow (D.value D.int4)

insertMachineRomRow :: Query (Int32, Int32, Rom) ()
insertMachineRomRow = statement sqltext encoder D.unit True
  where
    sqltext = "INSERT INTO machine_roms \
              \  (machine_id, rom_id, name, parent_name, status, optional) \
              \VALUES \
              \  ($1, $2, $3, $4, $5, $6); "
    encoder =
      let
        getMId (x, _, _) = x
        getRId (_, x, _) = x
        getRom (_, _, x) = x
      in
        contramap getMId (E.value E.int4) <>
        contramap getRId (E.value E.int4) <>
        contramap (romName . getRom) (E.value E.text) <>
        contramap (romMerge . getRom) (E.nullableValue E.text) <>
        contramap (pack . show . romStatus . getRom) (E.value E.text) <>
        contramap (romOptional . getRom) (E.value E.bool)

insertMachineRom :: Query (Int32, Rom) ()
insertMachineRom =
  proc (mId, rom) -> do
    let mCrc = romCrc rom
    case mCrc of
      Just crc -> do
        mrId <- checkRom -< crc
        rId <- case mrId of
          Nothing  -> do
            rId <- insertRom -< rom
            returnA -< rId
          Just rId -> returnA -< rId
        insertMachineRomRow -< (mId, rId, rom)
      Nothing -> returnA -< ()

insertFullMachine :: Query Machine ()
insertFullMachine =
  proc machine -> do
    mId <- insertMachine -< machine
    insertRomList -< (mId, roms machine)
  where
    insertRomList :: Query (Int32, [Rom]) ()
    insertRomList =
      proc (mId, romList) ->
        case romList of
          []     -> returnA -< ()
          (r:rs) -> do
            insertMachineRom -< (mId, r)
            insertRomList -< (mId, rs)

dbInsertMachine :: Connection -> Machine -> IO ()
dbInsertMachine conn mach = do
  result <- run (query mach insertFullMachine) conn
  case result of
    Left err -> print err
    _        -> return ()

dbCreate :: Connection -> IO ()
dbCreate conn = do
  result <- run createTables conn
  case result of
    Left err -> print err
    _        -> return ()

dbReset :: Connection -> IO ()
dbReset conn = do
  dropResult <- run dropTables conn
  case dropResult of
    Left err -> print err
    _        -> do
      createResult <- run createTables conn
      case createResult of
        Left err -> print err
        _        -> return ()
