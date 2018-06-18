{-# LANGUAGE TemplateHaskell, DeriveGeneric, FlexibleInstances, ViewPatterns #-}

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable.Generic
import Foreign.Storable
import Control.Lens
import System.IO.Unsafe
import Generics.Deriving

data DLL = DLL { 
    _backw :: Ptr DLL, 
    _this :: Int, 
    _thisPtr :: Ptr DLL,
    _forw :: Ptr DLL
  } deriving Generic

firstElem :: DLL -> DLL
firstElem nd = if _backw nd /= nullPtr then 
    firstElem $ unsafePerformIO (peek $ _backw nd) 
  else nd

lastElem :: DLL -> DLL
lastElem nd = if _forw nd /= nullPtr then 
    lastElem $ unsafePerformIO (peek $ _forw nd) 
  else nd
  
instance Show DLL where
  show l = 
    let accumEls :: DLL -> [Int]
        accumEls e 
          |  _forw e == nullPtr = _this e : []
          | otherwise = _this e : accumEls (unsafePerformIO (peek $ _forw e))
    in show $ accumEls (firstElem l)    

makeLenses ''DLL
instance GStorable DLL

singletonDLL :: Int -> DLL
singletonDLL a = 
  let ptr = unsafePerformIO malloc 
  in DLL nullPtr a ptr nullPtr

consDLL :: Int -> DLL -> DLL
consDLL e l = 
  let ptr :: Ptr DLL
      ptr = unsafePerformIO malloc
      dll = DLL nullPtr e ptr (_thisPtr l) 
      mem = unsafePerformIO (poke ptr dll)
      newNext = unsafePerformIO $ poke (_thisPtr l) (backw .~ ptr $ l)
  in  mem `seq` newNext `seq` dll 

snocDLL :: DLL -> Int -> DLL
snocDLL l e =
  let ptr :: Ptr DLL
      ptr = unsafePerformIO malloc
      nd = lastElem l
      dll = DLL (_thisPtr nd) e ptr nullPtr
      mem = unsafePerformIO (poke ptr dll)
      newPrev = unsafePerformIO $ poke (_thisPtr nd) (forw .~ ptr $ nd)
  in  mem `seq` newPrev `seq` dll