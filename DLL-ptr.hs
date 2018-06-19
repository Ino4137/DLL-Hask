{-# LANGUAGE TemplateHaskell, DeriveGeneric, ViewPatterns #-}

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable.Generic
import Foreign.Storable
import Control.Lens
import System.IO.Unsafe
import Generics.Deriving

data DLN a = DLN { 
    _backw :: Ptr (DLN a), 
    _this :: Ptr a, 
    _thisPtr :: Ptr (DLN a),
    _forw :: Ptr (DLN a)
  } deriving Generic

makeLenses ''DLN
instance Storable a => GStorable (DLN a)

firstElemDLN :: Storable a => DLN a -> DLN a  
firstElemDLN nd = if _backw nd /= nullPtr then 
    firstElemDLN $ unsafePerformIO (peek $ _backw nd) 
  else nd

lastElemDLN :: Storable a => DLN a -> DLN a
lastElemDLN nd = if _forw nd /= nullPtr then 
    lastElemDLN $ unsafePerformIO (peek $ _forw nd) 
  else nd
  
instance (Storable a, Show a) => Show (DLN a) where
  show l = 
    let accumEls :: Storable a => DLN a -> [a]
        accumEls e 
          |  _forw e == nullPtr = unsafePerformIO (peek $ _this e) : []
          | otherwise = unsafePerformIO (peek $ _this e) : accumEls (unsafePerformIO (peek $ _forw e))
    in show $ accumEls (firstElemDLN l)

data DLL a = DLL {
    _front :: (DLN a),
    _back :: (DLN a)
  } deriving Generic

makeLenses ''DLL
instance Storable a => GStorable (DLL a)

instance (Storable a, Show a) => Show (DLL a) where
  show (DLL fr _) = show fr

singletonDLN :: Storable a => a -> DLN a
singletonDLN a = 
  let ptr = unsafePerformIO malloc 
      pval = unsafePerformIO malloc
      val = unsafePerformIO (poke pval a)
  in  ptr `seq` val `seq` DLN nullPtr pval ptr nullPtr

singletonDLL :: Storable a => a -> DLL a
singletonDLL a =
  let node = singletonDLN a
  in DLL node node

consDLL :: Storable a => a -> DLL a -> DLL a
consDLL e dll@(DLL fs lt) =
  let newN = consDLN e fs
  in  newN `seq` (dll & front .~ newN & back .~ lastElemDLN newN)

snocDLL :: Storable a => DLL a -> a -> DLL a
snocDLL dll@(DLL fs lt) e =
  let newN = snocDLN lt e
  in  newN `seq` (dll & back .~ newN & front .~ firstElemDLN newN)

consDLN :: Storable a => a -> DLN a -> DLN a 
consDLN e (firstElemDLN -> l) = 
  let ptr = unsafePerformIO malloc
      pval = unsafePerformIO malloc
      val = unsafePerformIO (poke pval e)
      dln = DLN nullPtr pval ptr (_thisPtr l) 
      mem = unsafePerformIO (poke ptr dln)
      newNext = unsafePerformIO $ poke (_thisPtr l) (backw .~ ptr $ l)
  in  val `seq` mem `seq` newNext `seq` dln 

snocDLN :: Storable a => DLN a -> a -> DLN a
snocDLN (lastElemDLN -> nd) e =
  let ptr = unsafePerformIO malloc
      pval = unsafePerformIO malloc
      val = unsafePerformIO (poke pval e)
      dln = DLN (_thisPtr nd) pval ptr nullPtr
      mem = unsafePerformIO (poke ptr dln)
      newPrev = unsafePerformIO $ poke (_thisPtr nd) (forw .~ ptr $ nd)
  in  val `seq` mem `seq` newPrev `seq` dln

