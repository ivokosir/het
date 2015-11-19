{-# LANGUAGE ForeignFunctionInterface #-}
module Sdl2 where
import Foreign.C

foreign import ccall "SDL.h SDL_Init" init :: CUInt -> IO CInt
foreign import ccall "SDL.h SDL_INIT_VIDEO" videoFlag :: CUInt

