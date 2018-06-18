{-# LANGUAGE TemplateHaskell, DeriveGeneric, ViewPatterns #-}

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable.Generic
import Foreign.Storable
import Control.Lens
import System.IO.Unsafe
import Generics.Deriving

data DLL a = DLL { 
    _backw :: Ptr (DLL a), 
    _this :: Ptr a, 
    _thisPtr :: Ptr (DLL a),
    _forw :: Ptr (DLL a)
  } deriving Generic

firstElem :: Storable a => DLL a -> DLL a  
firstElem nd = if _backw nd /= nullPtr then 
    firstElem $ unsafePerformIO (peek $ _backw nd) 
  else nd

lastElem :: Storable a => DLL a -> DLL a
lastElem nd = if _forw nd /= nullPtr then 
    lastElem $ unsafePerformIO (peek $ _forw nd) 
  else nd
  
instance (Storable a, Show a) => Show (DLL a) where
  show l = 
    let accumEls :: Storable a => DLL a -> [a]
        accumEls e 
          |  _forw e == nullPtr = unsafePerformIO (peek $ _this e) : []
          | otherwise = unsafePerformIO (peek $ _this e) : accumEls (unsafePerformIO (peek $ _forw e))
    in show $ accumEls (firstElem l)    

makeLenses ''DLL
instance Storable a => GStorable (DLL a)

singletonDLL :: Storable a => a -> DLL a
singletonDLL a = 
  let ptr = unsafePerformIO malloc 
      pval = unsafePerformIO malloc
      val = unsafePerformIO (poke pval a)
  in  ptr `seq` val `seq` DLL nullPtr pval ptr nullPtr

consDLL :: Storable a => a -> DLL a -> DLL a 
consDLL e (firstElem -> l) = 
  let ptr = unsafePerformIO malloc
      pval = unsafePerformIO malloc
      val = unsafePerformIO (poke pval e)
      dll = DLL nullPtr pval ptr (_thisPtr l) 
      mem = unsafePerformIO (poke ptr dll)
      newNext = unsafePerformIO $ poke (_thisPtr l) (backw .~ ptr $ l)
  in  val `seq` mem `seq` newNext `seq` dll 

snocDLL :: Storable a => DLL a -> a -> DLL a
snocDLL (lastElem -> nd) e =
  let ptr = unsafePerformIO malloc
      pval = unsafePerformIO malloc
      val = unsafePerformIO (poke pval e)
      dll = DLL (_thisPtr nd) pval ptr nullPtr
      mem = unsafePerformIO (poke ptr dll)
      newPrev = unsafePerformIO $ poke (_thisPtr nd) (forw .~ ptr $ nd)
  in  val `seq` mem `seq` newPrev `seq` dll
