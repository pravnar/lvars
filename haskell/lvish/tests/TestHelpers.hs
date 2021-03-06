{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}

-- | To make it easier to build (multithreaded) tests

module TestHelpers
 ( 
   -- * Testing parameters
   numElems, getNumAgents, producerRatio,

   -- * Utility for controlling the number of threads used by generated tests.
   setTestThreads,

   -- * Test initialization, reading common configs
   stdTestHarness,

   -- * Misc utilities
   nTimes, assertOr, timeOut,
   -- timeOutPure, 
   exceptionOrTimeOut, allowSomeExceptions, assertException
 )
 where 

import Control.Monad
import Control.Exception
--import Control.Concurrent
--import Control.Concurrent.MVar
import GHC.Conc
import Data.IORef
import Data.Time.Clock
import Data.List (isInfixOf, intersperse)
import qualified Data.Set as S
import Text.Printf
import Control.Concurrent (forkOS, forkIO, ThreadId)
-- import Control.Exception (catch, SomeException, fromException, bracket, AsyncException(ThreadKilled))
import Control.Exception (bracket)
import System.Environment (withArgs, getArgs, getEnvironment)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit  (hUnitTestToTests)
import Test.HUnit as HU

import Control.LVish.SchedIdempotent (liftIO, dbgLvl, forkWithExceptions)
import Debug.Trace (trace)

--------------------------------------------------------------------------------


#if __GLASGOW_HASKELL__ >= 704
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)
#else
import GHC.Conc (numCapabilities)
getNumCapabilities :: IO Int
getNumCapabilities = return numCapabilities

setNumCapabilities :: Int -> IO ()
setNumCapabilities = error "setNumCapabilities not supported in this older GHC!  Set NUMTHREADS and +RTS -N to match."

getNumProcessors :: IO Int
getNumProcessors = return 1 
#endif    

theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

----------------------------------------------------------------------------------------------------
-- TODO: In addition to setting these parameters from environment
-- variables, it would be nice to route all of this through a
-- configuration record, so that it can be changed programmatically.

-- How many elements should each of the tests pump through the queue(s)?
numElems :: Int
numElems = case lookup "NUMELEMS" theEnv of 
             Nothing  -> 100 * 1000 -- 500000
             Just str -> warnUsing ("NUMELEMS = "++str) $ 
                         read str

forkThread :: IO () -> IO ThreadId
forkThread = case lookup "OSTHREADS" theEnv of 
               Nothing -> forkIO
               Just x -> warnUsing ("OSTHREADS = "++x) $ 
                 case x of 
                   "0"     -> forkIO
                   "False" -> forkIO
                   "1"     -> forkOS
                   "True"  -> forkOS
                   oth -> error$"OSTHREAD environment variable set to unrecognized option: "++oth

-- | How many communicating agents are there?  By default one per
-- thread used by the RTS.
getNumAgents :: IO Int
getNumAgents = case lookup "NUMAGENTS" theEnv of 
                Nothing  -> getNumCapabilities
                Just str -> warnUsing ("NUMAGENTS = "++str) $ 
                            return (read str)

-- | It is possible to have imbalanced concurrency where there is more
-- contention on the producing or consuming side (which corresponds to
-- settings of this parameter less than or greater than 1).
producerRatio :: Double
producerRatio = case lookup "PRODUCERRATIO" theEnv of 
                 Nothing  -> 1.0
                 Just str -> warnUsing ("PRODUCERRATIO = "++str) $ 
                             read str

warnUsing :: String -> a -> a
warnUsing str a = trace ("  [Warning]: Using environment variable "++str) a


-- | Dig through the test constructors to find the leaf IO actions and bracket them
--   with a thread-setting action.
setTestThreads :: Int -> HU.Test -> HU.Test
setTestThreads nm tst = loop False tst
 where
   loop flg x = 
    case x of
      TestLabel lb t2 -> TestLabel (decor flg lb) (loop True t2)
      TestList ls -> TestList (map (loop flg) ls)
      TestCase io -> TestCase (bracketThreads nm io)

   -- We only need to insert the numcapabilities in the description string ONCE:
   decor False lb = "N"++show nm++"_"++ lb
   decor True  lb = lb

   bracketThreads :: Int -> IO a -> IO a
   bracketThreads n act =
     bracket (getNumCapabilities)
             setNumCapabilities
             (\_ -> do dbgPrint 1 ("\n   [Setting # capabilities to "++show n++" before test] \n")
                       setNumCapabilities n
                       act)

-- | Repeat a group of tests while varying the number of OS threads used.  Also,
-- read configuration info.
--
-- WARNING: uses setNumCapabilities.
stdTestHarness :: (IO Test) -> IO ()
stdTestHarness genTests = do 
  numAgents <- getNumAgents 
  putStrLn$ "Running with numElems "++show numElems++" and numAgents "++ show numAgents
  putStrLn "Use NUMELEMS, NUMAGENTS, NUMTHREADS to control the size of this benchmark."
  args <- getArgs

  np <- getNumProcessors
  putStrLn $"Running on a machine with "++show np++" hardware threads."

  -- We allow the user to set this directly, because the "-t" based regexp selection
  -- of benchmarks is quite limited.
  let all_threads = case lookup "NUMTHREADS" theEnv of
                      Just str -> [read str]
                      Nothing -> S.toList$ S.fromList$
                        [1, 2, np `quot` 2, np, 2*np ]
  putStrLn $"Running tests for these thread settings: "  ++show all_threads
  all_tests <- genTests 

  -- Don't allow concurent tests (the tests are concurrent!):
  withArgs (args ++ ["-j1","--jxml=test-results.xml"]) $ do 

    -- Hack, this shouldn't be necessary, but I'm having problems with -t:
    tests <- case all_threads of
              [one] -> do cap <- getNumCapabilities
                          unless (cap == one) $ setNumCapabilities one
                          return all_tests
              _ -> return$ TestList [ setTestThreads n all_tests | n <- all_threads ]
    TF.defaultMain$ hUnitTestToTests tests

----------------------------------------------------------------------------------------------------
-- DEBUGGING
----------------------------------------------------------------------------------------------------

-- | Debugging flag shared by all accelerate-backend-kit modules.
--   This is activated by setting the environment variable DEBUG=1..5
dbg :: Int
dbg = case lookup "DEBUG" theEnv of
       Nothing  -> defaultDbg
       Just ""  -> defaultDbg
       Just "0" -> defaultDbg
       Just s   ->
         trace (" ! Responding to env Var: DEBUG="++s)$
         case reads s of
           ((n,_):_) -> n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s

defaultDbg :: Int
defaultDbg = 0

-- | Print if the debug level is at or above a threshold.
dbgPrint :: Int -> String -> IO ()
dbgPrint lvl str = if dbg < lvl then return () else do
--    hPutStrLn stderr str
    -- hPrintf stderr str 
    -- hFlush stderr
    printf str
    hFlush stdout

dbgPrintLn :: Int -> String -> IO ()
dbgPrintLn lvl str = dbgPrint lvl (str++"\n")


------------------------------------------------------------------------------------------
-- Misc Helpers
------------------------------------------------------------------------------------------

-- | Ensure that executing an action returns an exception
-- containing one of the expected messages.
assertException  :: [String] -> IO a -> IO ()
assertException msgs action = do
 x <- catch (do action; return Nothing) 
            (\e -> do putStrLn $ "Good.  Caught exception: " ++ show (e :: SomeException)
                      return (Just $ show e))
 case x of 
  Nothing -> error "Failed to get an exception!"
  Just s -> 
   if  any (`isInfixOf` s) msgs
   then return () 
   else error $ "Got the wrong exception, expected one of the strings: "++ show msgs
        ++ "\nInstead got this exception:\n  " ++ show s

-- | For testing quasi-deterministic programs: programs that always
-- either raise a particular exception or produce a particular answer.
allowSomeExceptions :: [String] -> IO a -> IO (Either SomeException a)
allowSomeExceptions msgs action = do
 catch (do a <- action; evaluate a; return (Right a))
       (\e ->
         let estr = show e in
         if  any (`isInfixOf` estr) msgs
          then do when (dbgLvl>=1) $
                    putStrLn $ "Caught allowed exception: " ++ show (e :: SomeException)
                  return (Left e)
          else error $ "Got the wrong exception, expected one of the strings: "++ show msgs
               ++ "\nInstead got this exception:\n  " ++ show estr)

exceptionOrTimeOut :: Double -> [String] -> IO a -> IO ()
exceptionOrTimeOut time msgs action = do
  x <- timeOut time $
       allowSomeExceptions msgs action
  case x of
    Just (Right _val) -> error "exceptionOrTimeOut: action returned successfully!" 
    Just (Left _exn)  -> return () -- Error, yay!
    Nothing           -> return () -- Timeout.

-- | Time-out an IO action by running it on a separate thread, which is killed when
-- the timer expires.  This requires that the action do allocation, otherwise it will
-- be non-preemptable.
timeOut :: Double -> IO a -> IO (Maybe a)
timeOut interval act = do
  result <- newIORef Nothing
  tid <- forkIO (act >>= writeIORef result . Just)
  t0  <- getCurrentTime
  let loop = do
        stat <- threadStatus tid
        case stat of
          ThreadFinished  -> readIORef result
          ThreadBlocked _ -> do putStrLn " [lvish-tests] Test timed out -- thread blocked!"
                                return Nothing
          ThreadDied      -> do putStrLn " [lvish-tests] Test timed out -- thread died!"
                                return Nothing
          ThreadRunning   -> do 
            now <- getCurrentTime
            let delt :: Double
                delt = fromRational$ toRational$ diffUTCTime now t0
            if delt >= interval
              then do killThread tid -- TODO: should probably wait for it to show up as dead.
                      return Nothing
              else do threadDelay (10 * 1000)
                      loop   
  loop

{-# NOINLINE timeOutPure #-}
-- | Evaluate a pure value to weak-head normal form, with timeout.
--   This is NONDETERMINISTIC, so its type is sketchy:
--
-- WARNING: This doesn't seem to work properly yet!  I am seeing spurious failures.
-- -RRN [2013.10.24]
--
timeOutPure :: Double -> a -> Maybe a
timeOutPure tm thnk =
  unsafePerformIO (timeOut tm (evaluate thnk))

assertOr :: Assertion -> Assertion -> Assertion
assertOr act1 act2 = 
  catch act1 
        (\(e::SomeException) -> act2)


nTimes :: Int -> (Int -> IO a) -> IO ()
nTimes 0 _ = return ()
nTimes n c = c n >> nTimes (n-1) c

