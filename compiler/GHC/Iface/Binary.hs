{-# LANGUAGE BinaryLiterals, CPP, ScopedTypeVariables, BangPatterns #-}

--
--  (c) The University of Glasgow 2002-2006
--

{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

-- | Binary interface file support.
module GHC.Iface.Binary (
        -- * Public API for interface file serialisation
        writeBinIface,
        readBinIface,
        readBinIface_,
        getSymtabName,
        getDictFastString,
        CheckHiWay(..),
        TraceBinIFace(..),
        getWithUserData,
        putWithUserData,

        -- * Internal serialisation functions
        getSymbolTable,
        putName,
        putDictionary,
        putFastString,
        putSymbolTable,
        BinSymbolTable(..),
        BinDictionary(..)

    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Tc.Utils.Monad
import GHC.Builtin.Utils   ( isKnownKeyName, lookupKnownKeyName )
import GHC.Iface.Env
import GHC.Unit
import GHC.Unit.Module.ModIface
import GHC.Types.Name
import GHC.Driver.Session
import GHC.Platform.Profile
import GHC.Types.Unique.FM
import GHC.Types.Unique.Supply
import GHC.Utils.Panic
import GHC.Utils.Binary as Binary
import GHC.Types.SrcLoc
import GHC.Data.FastMutInt
import GHC.Types.Unique
import GHC.Utils.Outputable
import GHC.Types.Name.Cache
import GHC.Platform
import GHC.Data.FastString
import GHC.Settings.Constants
import GHC.Utils.Misc

import Data.Array
import Data.Array.ST
import Data.Array.Unsafe
import Data.Bits
import Data.Char
import Data.Word
import Data.IORef
import Data.Foldable
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Strict as State

-- ---------------------------------------------------------------------------
-- Reading and writing binary interface files
--

data CheckHiWay = CheckHiWay | IgnoreHiWay
    deriving Eq

data TraceBinIFace
   = TraceBinIFace (SDoc -> IO ())
   | QuietBinIFace

-- | Read an interface file
readBinIface :: CheckHiWay -> TraceBinIFace -> FilePath
             -> TcRnIf a b ModIface
readBinIface checkHiWay traceBinIFaceReading hi_path = do
    ncu <- mkNameCacheUpdater
    dflags <- getDynFlags
    let profile = targetProfile dflags
    liftIO $ readBinIface_ profile checkHiWay traceBinIFaceReading hi_path ncu

-- | Read an interface file in 'IO'.
readBinIface_ :: Profile -> CheckHiWay -> TraceBinIFace -> FilePath
              -> NameCacheUpdater
              -> IO ModIface
readBinIface_ profile checkHiWay traceBinIFace hi_path ncu = do
    let platform = profilePlatform profile

        wantedGot :: String -> a -> a -> (a -> SDoc) -> IO ()
        wantedGot what wanted got ppr' =
            case traceBinIFace of
               QuietBinIFace         -> return ()
               TraceBinIFace printer -> printer $
                     text what <> text ": " <>
                     vcat [text "Wanted " <> ppr' wanted <> text ",",
                           text "got    " <> ppr' got]

        errorOnMismatch :: (Eq a, Show a) => String -> a -> a -> IO ()
        errorOnMismatch what wanted got =
            -- This will be caught by readIface which will emit an error
            -- msg containing the iface module name.
            when (wanted /= got) $ throwGhcExceptionIO $ ProgramError
                         (what ++ " (wanted " ++ show wanted
                               ++ ", got "    ++ show got ++ ")")
    bh <- Binary.readBinMem hi_path

    -- Read the magic number to check that this really is a GHC .hi file
    -- (This magic number does not change when we change
    --  GHC interface file format)
    magic <- get bh
    wantedGot "Magic" (binaryInterfaceMagic platform) magic (ppr . unFixedLength)
    errorOnMismatch "magic number mismatch: old/corrupt interface file?"
        (unFixedLength $ binaryInterfaceMagic platform) (unFixedLength magic)

    -- Check the interface file version and profile tag.
    check_ver  <- get bh
    let our_ver = show hiVersion
    wantedGot "Version" our_ver check_ver text
    errorOnMismatch "mismatched interface file versions" our_ver check_ver

    check_tag <- get bh
    let tag = profileBuildTag profile
    wantedGot "Way" tag check_tag ppr
    when (checkHiWay == CheckHiWay) $
        errorOnMismatch "mismatched interface file profile tag" tag check_tag

    extFields_p <- get bh

    mod_iface <- getWithUserData ncu bh

    seekBin bh extFields_p
    extFields <- get bh

    return mod_iface{mi_ext_fields = extFields}


-- | This performs a get action after reading the dictionary and symbol
-- table. It is necessary to run this before trying to deserialise any
-- Names or FastStrings.
getWithUserData :: Binary a => NameCacheUpdater -> BinHandle -> IO a
getWithUserData ncu bh = do
    -- Read the dictionary
    -- The next word in the file is a pointer to where the dictionary is
    -- (probably at the end of the file)
    dict_p <- Binary.get bh
    data_p <- tellBin bh          -- Remember where we are now
    seekBin bh dict_p
    dict   <- getDictionary bh
    seekBin bh data_p             -- Back to where we were before

    -- Initialise the user-data field of bh
    bh <- do
        bh <- return $ setUserData bh $ newReadState (error "getSymtabName")
                                                     (getDictFastString dict)
        symtab_p <- Binary.get bh     -- Get the symtab ptr
        data_p <- tellBin bh          -- Remember where we are now
        seekBin bh symtab_p
        symtab <- getSymbolTable bh ncu
        seekBin bh data_p             -- Back to where we were before

        -- It is only now that we know how to get a Name
        return $ setUserData bh $ newReadState (getSymtabName ncu dict symtab)
                                               (getDictFastString dict)

    -- Read the interface file
    get bh

-- | Write an interface file
writeBinIface :: Profile -> TraceBinIFace -> FilePath -> ModIface -> IO ()
writeBinIface profile traceBinIface hi_path mod_iface = do
    bh <- openBinMem initBinMemSize
    let platform = profilePlatform profile
    put_ bh (binaryInterfaceMagic platform)

    -- The version and profile tag go next
    put_ bh (show hiVersion)
    let tag = profileBuildTag profile
    put_  bh tag

    extFields_p_p <- tellBin bh
    put_ bh extFields_p_p

    putWithUserData traceBinIface bh mod_iface

    extFields_p <- tellBin bh
    putAt bh extFields_p_p extFields_p
    seekBin bh extFields_p
    put_ bh (mi_ext_fields mod_iface)

    -- And send the result to the file
    writeBinMem bh hi_path

-- | Put a piece of data with an initialised `UserData` field. This
-- is necessary if you want to serialise Names or FastStrings.
-- It also writes a symbol table and the dictionary.
-- This segment should be read using `getWithUserData`.
putWithUserData :: Binary a => TraceBinIFace -> BinHandle -> a -> IO ()
putWithUserData traceBinIface bh payload = do
    -- Remember where the dictionary pointer will go
    dict_p_p <- tellBin bh
    -- Placeholder for ptr to dictionary
    put_ bh dict_p_p

    -- Remember where the symbol table pointer will go
    symtab_p_p <- tellBin bh
    put_ bh symtab_p_p
    -- Make some initial state
    symtab_next <- newFastMutInt 0
    symtab_map <- newIORef emptyUFM
    let bin_symtab = BinSymbolTable {
                         bin_symtab_next = symtab_next,
                         bin_symtab_map  = symtab_map }
    dict_next_ref <- newFastMutInt 0
    dict_map_ref <- newIORef emptyUFM
    let bin_dict = BinDictionary {
                       bin_dict_next = dict_next_ref,
                       bin_dict_map  = dict_map_ref }

    -- Put the main thing,
    bh <- return $ setUserData bh $ newWriteState (putName bin_dict bin_symtab)
                                                  (putName bin_dict bin_symtab)
                                                  (putFastString bin_dict)
    put_ bh payload

    -- Write the symtab pointer at the front of the file
    symtab_p <- tellBin bh        -- This is where the symtab will start
    putAt bh symtab_p_p symtab_p  -- Fill in the placeholder
    seekBin bh symtab_p           -- Seek back to the end of the file

    -- Write the symbol table itself
    symtab_next <- readFastMutInt symtab_next
    symtab_map  <- readIORef symtab_map
    putSymbolTable bh symtab_next symtab_map
    case traceBinIface of
      QuietBinIFace         -> return ()
      TraceBinIFace printer ->
         printer (text "writeBinIface:" <+> int symtab_next
                                        <+> text "Names")

    -- NB. write the dictionary after the symbol table, because
    -- writing the symbol table may create more dictionary entries.

    -- Write the dictionary pointer at the front of the file
    dict_p <- tellBin bh          -- This is where the dictionary will start
    putAt bh dict_p_p dict_p      -- Fill in the placeholder
    seekBin bh dict_p             -- Seek back to the end of the file

    -- Write the dictionary itself
    dict_next <- readFastMutInt dict_next_ref
    dict_map  <- readIORef dict_map_ref
    putDictionary bh dict_next dict_map
    case traceBinIface of
      QuietBinIFace         -> return ()
      TraceBinIFace printer ->
         printer (text "writeBinIface:" <+> int dict_next
                                        <+> text "dict entries")



-- | Initial ram buffer to allocate for writing interface files
initBinMemSize :: Int
initBinMemSize = 1024 * 1024

binaryInterfaceMagic :: Platform -> FixedLengthEncoding Word32
binaryInterfaceMagic platform
 | target32Bit platform = FixedLengthEncoding 0x1face
 | otherwise            = FixedLengthEncoding 0x1face64


-- -----------------------------------------------------------------------------
-- The symbol table
--

putSymbolTable :: BinHandle -> Int -> UniqFM Name (Int,Name) -> IO ()
putSymbolTable bh next_off symtab = do
    put_ bh next_off
    let names = elems (array (0,next_off-1) (nonDetEltsUFM symtab))
      -- It's OK to use nonDetEltsUFM here because the elements have
      -- indices that array uses to create order
    mapM_ (\n -> serialiseName bh n symtab) names

getSymbolTable :: BinHandle -> NameCacheUpdater -> IO SymbolTable
getSymbolTable bh ncu = do
    sz <- get bh
    od_names <- sequence (replicate sz (get bh))
    updateNameCache ncu $ \namecache ->
        runST $ flip State.evalStateT namecache $ do
            mut_arr <- lift $ newSTArray_ (0, sz-1)
            for_ (zip [0..] od_names) $ \(i, odn) -> do
                (nc, !n) <- State.gets $ \nc -> fromOnDiskName nc odn
                lift $ writeArray mut_arr i n
                State.put nc
            arr <- lift $ unsafeFreeze mut_arr
            namecache' <- State.get
            return (namecache', arr)
  where
    -- This binding is required because the type of newArray_ cannot be inferred
    newSTArray_ :: forall s. (Int, Int) -> ST s (STArray s Int Name)
    newSTArray_ = newArray_

type OnDiskName = (Unit, ModuleName, OccName)

fromOnDiskName :: NameCache -> OnDiskName -> (NameCache, Name)
fromOnDiskName nc (pid, mod_name, occ) =
    let mod   = mkModule pid mod_name
        cache = nsNames nc
    in case lookupOrigNameCache cache  mod occ of
           Just name -> (nc, name)
           Nothing   ->
               let (uniq, us) = takeUniqFromSupply (nsUniqs nc)
                   name       = mkExternalName uniq mod occ noSrcSpan
                   new_cache  = extendNameCache cache mod occ name
               in ( nc{ nsUniqs = us, nsNames = new_cache }, name )

serialiseName :: BinHandle -> Name -> UniqFM key (Int,Name) -> IO ()
serialiseName bh name _ = do
    let mod = ASSERT2( isExternalName name, ppr name ) nameModule name
    put_ bh (moduleUnit mod, moduleName mod, nameOccName name)


-- Note [Symbol table representation of names]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- An occurrence of a name in an interface file is serialized as a single 32-bit
-- word. The format of this word is:
--  00xxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
--   A normal name. x is an index into the symbol table
--  10xxxxxx xxyyyyyy yyyyyyyy yyyyyyyy
--   A known-key name. x is the Unique's Char, y is the int part. We assume that
--   all known-key uniques fit in this space. This is asserted by
--   GHC.Builtin.Utils.knownKeyNamesOkay.
--
-- During serialization we check for known-key things using isKnownKeyName.
-- During deserialization we use lookupKnownKeyName to get from the unique back
-- to its corresponding Name.


-- See Note [Symbol table representation of names]
putName :: BinDictionary -> BinSymbolTable -> BinHandle -> Name -> IO ()
putName _dict BinSymbolTable{
               bin_symtab_map = symtab_map_ref,
               bin_symtab_next = symtab_next }
        bh name
  | isKnownKeyName name
  , let (c, u) = unpkUnique (nameUnique name) -- INVARIANT: (ord c) fits in 8 bits
  = -- ASSERT(u < 2^(22 :: Int))
    put_ bh (0x80000000
             .|. (fromIntegral (ord c) `shiftL` 22)
             .|. (fromIntegral u :: Word32))

  | otherwise
  = do symtab_map <- readIORef symtab_map_ref
       case lookupUFM symtab_map name of
         Just (off,_) -> put_ bh (fromIntegral off :: Word32)
         Nothing -> do
            off <- readFastMutInt symtab_next
            -- MASSERT(off < 2^(30 :: Int))
            writeFastMutInt symtab_next (off+1)
            writeIORef symtab_map_ref
                $! addToUFM symtab_map name (off,name)
            put_ bh (fromIntegral off :: Word32)

-- See Note [Symbol table representation of names]
getSymtabName :: NameCacheUpdater
              -> Dictionary -> SymbolTable
              -> BinHandle -> IO Name
getSymtabName _ncu _dict symtab bh = do
    i :: Word32 <- get bh
    case i .&. 0xC0000000 of
      0x00000000 -> return $! symtab ! fromIntegral i

      0x80000000 ->
        let
          tag = chr (fromIntegral ((i .&. 0x3FC00000) `shiftR` 22))
          ix  = fromIntegral i .&. 0x003FFFFF
          u   = mkUnique tag ix
        in
          return $! case lookupKnownKeyName u of
                      Nothing -> pprPanic "getSymtabName:unknown known-key unique"
                                          (ppr i $$ ppr (unpkUnique u))
                      Just n  -> n

      _ -> pprPanic "getSymtabName:unknown name tag" (ppr i)

data BinSymbolTable = BinSymbolTable {
        bin_symtab_next :: !FastMutInt, -- The next index to use
        bin_symtab_map  :: !(IORef (UniqFM Name (Int,Name)))
                                -- indexed by Name
  }

putFastString :: BinDictionary -> BinHandle -> FastString -> IO ()
putFastString dict bh fs = allocateFastString dict fs >>= put_ bh

allocateFastString :: BinDictionary -> FastString -> IO Word32
allocateFastString BinDictionary { bin_dict_next = j_r,
                                   bin_dict_map  = out_r} f = do
    out <- readIORef out_r
    let !uniq = getUnique f
    case lookupUFM_Directly out uniq of
        Just (j, _)  -> return (fromIntegral j :: Word32)
        Nothing -> do
           j <- readFastMutInt j_r
           writeFastMutInt j_r (j + 1)
           writeIORef out_r $! addToUFM_Directly out uniq (j, f)
           return (fromIntegral j :: Word32)

getDictFastString :: Dictionary -> BinHandle -> IO FastString
getDictFastString dict bh = do
    j <- get bh
    return $! (dict ! fromIntegral (j :: Word32))

data BinDictionary = BinDictionary {
        bin_dict_next :: !FastMutInt, -- The next index to use
        bin_dict_map  :: !(IORef (UniqFM FastString (Int,FastString)))
                                -- indexed by FastString
  }

