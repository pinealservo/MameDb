{-# LANGUAGE OverloadedStrings #-}
module MameDb.XmlParse
  ( parseMame
  , parseMachinesFrom
  ) where

import Control.Monad (void)
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Void (Void)
import Data.Text (unpack)
import Data.XML.Types
import Numeric (readHex)

import Text.XML.Stream.Parse

import MameDb.Types


--
-- Attribute Parse Helpers
--

boolAttr :: Name -> Bool -> AttrParser Bool
boolAttr attrName defaultVal = do
  res <- attr attrName
  return $ case res of
    Just "yes" -> True
    Just "no"  -> False
    _          -> defaultVal

intAttr :: Name -> AttrParser (Maybe Int)
intAttr attrName = do
  res <- attr attrName
  return $ (read . unpack) <$> res

doubleAttr :: Name -> AttrParser (Maybe Double)
doubleAttr attrName = do
  res <- attr attrName
  return $ (read . unpack) <$> res

hexAttr :: Name -> AttrParser (Maybe Int)
hexAttr attrName = do
  res <- attr attrName
  return $ (fst . head . readHex . unpack) <$> res

statusAttr :: AttrParser Status
statusAttr = do
  res <- attr "status"
  return $ case res of
    Just "baddump" -> BadDump
    Just "nodump"  -> NoDump
    _              -> GoodDump

chipTypeAttr :: AttrParser (Maybe ChipType)
chipTypeAttr = do
  res <- attr "type"
  return $ case res of
    Just "cpu"   -> Just Cpu
    Just "audio" -> Just Audio
    _            -> Nothing

displayTypeAttr :: AttrParser (Maybe DisplayType)
displayTypeAttr = do
  res <- attr "type"
  return $ case res of
    Just "raster"  -> Just Raster
    Just "vector"  -> Just Vector
    Just "lcd"     -> Just LCD
    Just "unknown" -> Just Unknown
    _              -> Nothing

rotateAttr :: AttrParser (Maybe DisplayRotate)
rotateAttr = do
  res <- attr "rotate"
  return $ case res of
    Just "0"   -> Just R0
    Just "90"  -> Just R90
    Just "180" -> Just R180
    Just "270" -> Just R270
    _          -> Nothing

driverStatusAttr :: Name -> AttrParser (Maybe DriverStatus)
driverStatusAttr attrName = do
  res <- attr attrName
  return $ case res of
    Just "good"        -> Just Good
    Just "imperfect"   -> Just Imperfect
    Just "preliminary" -> Just Preliminary
    _                  -> Nothing

requireDriverStatus :: Name -> AttrParser DriverStatus
requireDriverStatus attrName =
  let msg = "missing driver attribute " ++ unpack (nameLocalName attrName)
  in force msg (driverStatusAttr attrName)

saveStateAttr :: AttrParser Bool
saveStateAttr = do
  res <- attr "savestate"
  return $ case res of
    Just "supported"   -> True
    Just "unsupported" -> False
    _                  -> False

listStatusAttr :: AttrParser (Maybe ListStatus)
listStatusAttr = do
  res <- attr "status"
  return $ case res of
    Just "original"   -> Just Original
    Just "compatible" -> Just Compatible
    _                 -> Nothing

--
-- Tag Parsers
--

parseRamOption :: MonadThrow m => ConduitM Event o m (Maybe RamOption)
parseRamOption =
  tagName "ramoption" attrParser tagHandler
 where
  attrParser =
    RamOption <$> attr "default"
              <*> pure []
  tagHandler opt = do
    optionList <- many contentMaybe
    return opt{ ramOptionList = optionList }

parseSoftwareList :: MonadThrow m => ConduitM Event o m (Maybe SoftwareList)
parseSoftwareList =
  tagName "softwarelist" attrParser return
 where
  attrParser =
    SoftwareList <$> requireAttr "name"
                 <*> force "softwarelist attribute status missing"
                       listStatusAttr
                 <*> attr "filter"

parseSlotOption :: MonadThrow m => ConduitM Event o m (Maybe SlotOption)
parseSlotOption =
  tagName "slotoption" attrParser return
 where
  attrParser =
    SlotOption <$> requireAttr "name"
               <*> requireAttr "devname"
               <*> boolAttr "default" False

parseSlot :: MonadThrow m => ConduitM Event o m (Maybe Slot)
parseSlot =
  tagName "slot" attrParser tagHandler
 where
  attrParser =
    Slot <$> requireAttr "name"
         <*> pure []
  tagHandler slot = do
    opts <- many parseSlotOption
    return slot{ slotOptions = opts }

parseInstance :: MonadThrow m => ConduitM Event o m (Maybe Instance)
parseInstance =
  tagName "instance" attrParser return
 where
  attrParser =
    Instance <$> requireAttr "name"
             <*> requireAttr "briefname"

parseExtension :: MonadThrow m => ConduitM Event o m (Maybe Extension)
parseExtension =
  tagName "extension" (Extension <$> requireAttr "name") return

parseDevice :: MonadThrow m => ConduitM Event o m (Maybe Device)
parseDevice =
  tagName "device" attrParser tagHandler
 where
  attrParser =
    Device <$> requireAttr "type"
           <*> attr "tag"
           <*> attr "mandatory"
           <*> attr "interface"
           <*> pure []
           <*> pure []
  tagHandler device = do
    instances <- many parseInstance
    extensions <- many parseExtension
    return device{ deviceInstances = instances
                 , deviceExtensions = extensions }

parseDriver :: MonadThrow m => ConduitM Event o m (Maybe Driver)
parseDriver =
  tagName "driver" attrParser return
 where
  attrParser =
    Driver <$> requireDriverStatus "status"
           <*> requireDriverStatus "emulation"
           <*> requireDriverStatus "color"
           <*> requireDriverStatus "sound"
           <*> requireDriverStatus "graphic"
           <*> driverStatusAttr "cocktail"
           <*> driverStatusAttr "protection"
           <*> saveStateAttr

parseAdjuster :: MonadThrow m => ConduitM Event o m (Maybe Adjuster)
parseAdjuster =
  tagName "adjuster" attrParser return
 where
  attrParser =
    Adjuster <$> requireAttr "name"
             <*> requireAttr "default"

parseAnalog :: MonadThrow m => ConduitM Event o m (Maybe Analog)
parseAnalog =
  tagName "analog" attrParser return
 where
  attrParser = Analog <$> force "analog attribute mask missing"
                            (intAttr "mask")

parsePort :: MonadThrow m => ConduitM Event o m (Maybe Port)
parsePort =
  tagName "port" attrParser tagHandler
 where
  attrParser =
    Port <$> requireAttr "tag"
         <*> pure []
  tagHandler port = do
    analogs <- many parseAnalog
    return port{ portAnalog = analogs }

parseSetting :: MonadThrow m => ConduitM Event o m (Maybe Setting)
parseSetting =
  tagName "confsetting" attrParser return
 where
  attrParser =
    Setting <$> requireAttr "name"
            <*> requireAttr "value"
            <*> boolAttr "default" False

parseConfiguration :: MonadThrow m => ConduitM Event o m (Maybe Configuration)
parseConfiguration =
  tagName "configuration" attrParser tagHandler
 where
  attrParser =
    Configuration <$> requireAttr "name"
                  <*> requireAttr "tag"
                  <*> force "configuration attribute mask missing"
                        (intAttr "mask")
                  <*> pure []
  tagHandler config = do
    settingList <- many parseSetting
    return config{ configSetting = settingList }

parseDipValue :: MonadThrow m => ConduitM Event o m (Maybe DipValue)
parseDipValue =
  tagName "dipvalue" attrParser return
 where
  attrParser =
    DipValue <$> requireAttr "name"
             <*> requireAttr "value"
             <*> boolAttr "default" False

parseDipSwitch :: MonadThrow m => ConduitM Event o m (Maybe DipSwitch)
parseDipSwitch =
  tagName "dipswitch" attrParser tagHandler
 where
  attrParser =
    DipSwitch <$> requireAttr "name"
              <*> requireAttr "tag"
              <*> force "dipswitch mask attribute missing" (intAttr "mask")
              <*> pure []
  tagHandler dip = do
    valueList <- many parseDipValue
    return dip{ dipSwitchValues = valueList }

parseControl :: MonadThrow m => ConduitM Event o m (Maybe Control)
parseControl =
  tagName "control" attrParser return
 where
  attrParser =
    Control <$> requireAttr "type"
            <*> intAttr "minimum"
            <*> intAttr "maximum"
            <*> intAttr "sensitivity"
            <*> intAttr "keydelta"
            <*> boolAttr "reverse" False
            <*> attr "ways"
            <*> attr "ways2"
            <*> attr "ways3"

parseInput :: MonadThrow m => ConduitM Event o m (Maybe Input)
parseInput =
  tagName "input" attrParser tagHandler
 where
  attrParser =
    Input <$> boolAttr "service" False
          <*> boolAttr "tilt" False
          <*> force "input players attribute missing" (intAttr "players")
          <*> intAttr "buttons"
          <*> intAttr "coins"
          <*> pure []
  tagHandler inputVal = do
    controlList <- many parseControl
    return inputVal{ inputControls = controlList }

parseSound :: MonadThrow m => ConduitM Event o m (Maybe Sound)
parseSound =
  tagName "sound"
    (Sound <$> force "channel count attribute missing" (intAttr "channels"))
    return

parseDisplay :: MonadThrow m => ConduitM Event o m (Maybe Display)
parseDisplay =
  tagName "display" attrParser return
 where
  attrParser =
    Display <$> requireAttr "tag"
            <*> force "display type attribute missing" displayTypeAttr
            <*> force "display rotate attribute missing" rotateAttr
            <*> boolAttr "flipx" False
            <*> intAttr "width"
            <*> intAttr "height"
            <*> force "display refresh attribute missing"
                  (doubleAttr "refresh")
            <*> intAttr "pixclock"
            <*> intAttr "htotal"
            <*> intAttr "hbend"
            <*> intAttr "hbstart"
            <*> intAttr "vtotal"
            <*> intAttr "vbend"
            <*> intAttr "vbstart"

parseChip :: MonadThrow m => ConduitM Event o m (Maybe Chip)
parseChip =
  tagName "chip" attrParser return
 where
  attrParser =
    Chip <$> requireAttr "name"
         <*> attr "tag"
         <*> force "chip type attribute missing" chipTypeAttr
         <*> attr "clock"

parseDeviceRef :: MonadThrow m => ConduitM Event o m (Maybe DeviceRef)
parseDeviceRef =
  tagName "device_ref" (DeviceRef <$> requireAttr "name") return

parseSample :: MonadThrow m => ConduitM Event o m (Maybe Sample)
parseSample =
  tagName "sample" (Sample <$> requireAttr "name") return

parseDisk :: MonadThrow m => ConduitM Event o m (Maybe Disk)
parseDisk =
  tagName "disk" attrParser return
 where
  attrParser =
    Disk <$> requireAttr "name"
         <*> attr "sha1"
         <*> attr "merge"
         <*> attr "region"
         <*> attr "index"
         <*> boolAttr "writable" False
         <*> statusAttr
         <*> boolAttr "optional" False

parseRom :: MonadThrow m => ConduitM Event o m (Maybe Rom)
parseRom =
  tagName "rom" attrParser return
 where
  attrParser =
    Rom <$> requireAttr "name"
        <*> attr "bios"
        <*> force "rom size attribute missing" (intAttr "size")
        <*> attr "crc"
        <*> attr "sha1"
        <*> attr "merge"
        <*> attr "region"
        <*> hexAttr "offset"
        <*> statusAttr
        <*> boolAttr "optional" False

parseBiosSet :: MonadThrow m => ConduitM Event o m (Maybe BiosSet)
parseBiosSet =
  tagName "biosset" attrParser return
 where
  attrParser =
    BiosSet <$> requireAttr "name"
            <*> requireAttr "description"
            <*> boolAttr "default" False

parseMachine :: MonadThrow m => ConduitM Event o m (Maybe Machine)
parseMachine =
  tagName "machine" attrParser tagHandler
 where
  attrParser =
    MachineAttributes <$> requireAttr "name"
                      <*> attr "sourcefile"
                      <*> boolAttr "isbios" False
                      <*> boolAttr "isdevice" False
                      <*> boolAttr "ismechanical" False
                      <*> boolAttr "runnable" True
                      <*> attr "cloneof"
                      <*> attr "romof"
                      <*> attr "sampleof"
  tagHandler attrs =
     Machine <$> pure attrs
             <*> force "machine description missing"
                   (tagNoAttr "description" content)
             <*> tagNoAttr "year" content
             <*> tagNoAttr "manufacturer" content
             <*> many parseBiosSet
             <*> many parseRom
             <*> many parseDisk
             <*> many parseDeviceRef
             <*> many parseSample
             <*> many parseChip
             <*> many parseDisplay
             <*> parseSound
             <*> parseInput
             <*> many parseDipSwitch
             <*> many parseConfiguration
             <*> many parsePort
             <*> many parseAdjuster
             <*> parseDriver
             <*> many parseDevice
             <*> many parseSlot
             <*> many parseSoftwareList
             <*> many parseRamOption

parseMame :: MonadThrow m => ConduitM Event Machine m ()
parseMame = void $ tagIgnoreAttrs "mame" $ manyYield parseMachine

parseMachinesFrom :: MonadResource m
                  => FilePath -> ConduitM Machine Void m b -> m b
parseMachinesFrom path action =
  parseFile def path $$ parseMame $= action
