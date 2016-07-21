module MameDb.Types where

import Data.Text (Text)

data BiosSet = BiosSet
  { bsName :: Text
  , bsDescription :: Text
  , bsDefault :: Bool
  } deriving (Show)

data Status = BadDump
            | NoDump
            | GoodDump
            deriving (Eq, Show)

data Rom = Rom
  { romName :: Text
  , romBios :: Maybe Text
  , romSize :: Int
  , romCrc :: Maybe Text
  , romSha1 :: Maybe Text
  , romMerge :: Maybe Text
  , romRegion :: Maybe Text
  , romOffset :: Maybe Int
  , romStatus :: Status
  , romOptional :: Bool
  } deriving (Show)

data Disk = Disk
  { diskName :: Text
  , diskSha1 :: Maybe Text
  , diskMerge :: Maybe Text
  , diskRegion :: Maybe Text
  , diskIndex :: Maybe Text
  , diskWritable :: Bool
  , diskStatus :: Status
  , diskOptional :: Bool
  } deriving (Show)

data DeviceRef = DeviceRef
  { deviceRefName :: Text
  } deriving (Show)

data Sample = Sample
  { sampleName :: Text
  } deriving (Show)

data ChipType = Cpu
              | Audio
  deriving (Eq, Show)

data Chip = Chip
  { chipName :: Text
  , chipTag :: Maybe Text
  , chipType :: ChipType
  , chipClock :: Maybe Text
  } deriving (Show)

data DisplayType = Raster
                 | Vector
                 | LCD
                 | Unknown
  deriving (Eq, Show)

data DisplayRotate = R0
                   | R90
                   | R180
                   | R270
  deriving (Eq, Show)

data Display = Display
  { displayTag :: Text
  , displayType :: DisplayType
  , displayRotate :: DisplayRotate
  , displayFlipX :: Bool
  , displayWidth :: Maybe Int
  , displayHeight :: Maybe Int
  , displayRefresh :: Double
  , displayPixClock :: Maybe Int
  , displayHTotal :: Maybe Int
  , displayHBEnd :: Maybe Int
  , displayHBStart :: Maybe Int
  , displayVTotal :: Maybe Int
  , displayVBEnd :: Maybe Int
  , displayVBStart :: Maybe Int
  } deriving (Show)

data Sound = Sound
  { soundChannels :: Int
  } deriving (Show)

data Control = Control
  { controlType :: Text
  , controlMinimum :: Maybe Int
  , controlMaximum :: Maybe Int
  , controlSensitivity :: Maybe Int
  , controlKeyDelta :: Maybe Int
  , controlReverse :: Bool
  , controlWays :: Maybe Text
  , controlWays2 :: Maybe Text
  , controlWays3 :: Maybe Text
  } deriving (Show)

data Input = Input
  { inputService :: Bool
  , inputTilt :: Bool
  , inputPlayers :: Int
  , inputButtons :: Maybe Int
  , inputCoins :: Maybe Int
  , inputControls :: [Control]
  } deriving (Show)

data DipValue = DipValue
  { dipValueName :: Text
  , dipValueValue :: Text
  , dipValueDefault :: Bool
  } deriving (Show)

data DipSwitch = DipSwitch
  { dipSwitchName :: Text
  , dipSwitchTag :: Text
  , dipSwitchMask :: Int
  , dipSwitchValues :: [DipValue]
  } deriving (Show)

data Setting = Setting
  { settingName :: Text
  , settingValue :: Text
  , settingDefault :: Bool
  } deriving (Show)

data Configuration = Configuration
  { configName :: Text
  , configTag :: Text
  , configMask :: Int
  , configSetting :: [Setting]
  } deriving (Show)

data Analog = Analog
  { analogMask :: Int
  } deriving (Show)

data Port = Port
  { portTag :: Text
  , portAnalog :: [Analog]
  } deriving (Show)

data Adjuster = Adjuster
  { adjusterName :: Text
  , adjusterDefault :: Text
  } deriving (Show)

data DriverStatus = Good
                  | Imperfect
                  | Preliminary
  deriving (Eq, Show)

data Driver = Driver
  { driverStatus :: DriverStatus
  , driverEmulation :: DriverStatus
  , driverColor :: DriverStatus
  , driverSound :: DriverStatus
  , driverGraphic :: DriverStatus
  , driverCocktail :: Maybe DriverStatus
  , driverProtection :: Maybe DriverStatus
  , driverSaveState :: Bool
  } deriving (Show)

data Instance = Instance
  { instanceName :: Text
  , instanceBriefName :: Text
  } deriving (Show)

data Extension = Extension
  { extensionName :: Text
  } deriving (Show)

data Device = Device
  { deviceType :: Text
  , deviceTag :: Maybe Text
  , deviceMandatory :: Maybe Text
  , deviceInterface :: Maybe Text
  , deviceInstances :: [Instance]
  , deviceExtensions :: [Extension]
  } deriving (Show)

data SlotOption = SlotOption
  { optionName :: Text
  , optionDevName :: Text
  , optionDefault :: Bool
  } deriving (Show)

data Slot = Slot
  { slotName :: Text
  , slotOptions :: [SlotOption]
  } deriving (Show)

data ListStatus = Original
                | Compatible
  deriving (Eq, Show)

data SoftwareList = SoftwareList
  { listName :: Text
  , listStatus :: ListStatus
  , listFilter :: Maybe Text
  } deriving (Show)

data RamOption = RamOption
  { ramOptionDefault :: Maybe Text
  , ramOptionList :: [Text]
  } deriving (Show)

data MachineAttributes = MachineAttributes
  { name :: Text
  , sourceFile :: Maybe Text
  , isBios :: Bool
  , isDevice :: Bool
  , isMechanical :: Bool
  , runnable :: Bool
  , cloneOf :: Maybe Text
  , romOf :: Maybe Text
  , sampleOf :: Maybe Text
  } deriving (Show)

data Machine = Machine
  { attributes :: MachineAttributes
  , description :: Text
  , year :: Maybe Text
  , manufacturer :: Maybe Text
  , biosSets :: [BiosSet]
  , roms :: [Rom]
  , disks :: [Disk]
  , deviceRefs :: [DeviceRef]
  , samples :: [Sample]
  , chips :: [Chip]
  , displays :: [Display]
  , sound :: Maybe Sound
  , input :: Maybe Input
  , dipSwitches :: [DipSwitch]
  , configurations :: [Configuration]
  , ports :: [Port]
  , adjusters :: [Adjuster]
  , driver :: Maybe Driver
  , devices :: [Device]
  , slots :: [Slot]
  , softwareLists :: [SoftwareList]
  , ramOptions :: [RamOption]
  } deriving (Show)
