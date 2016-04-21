import Control.Monad
import Control.Exception
import Control.Monad.Managed
import Control.Concurrent
import Control.Concurrent.Async
import Network.Socket
import Data.Traversable
import qualified Data.Set as Set
import Algebra.Lattice ((\/), bottom)

import Data.IORef

-- A user interaction starts a broadcast
receiver sock = forever $ do
  return ()

sender sock = forever $ do
  return ()

local = forever $ do
  return ()

newtype GrowOnlySet a = GrowOnlySet (Set.Set a) deriving (JoinSemiLattice, BoundedJoinSemiLattice)

data TwoPSet a = TwoPSet
  { added   :: (Set.Set a)
  , removed :: (Set.Set a)
  }

instance JoinSemiLattice TwoPSet where
  (TwoPSet a1 r1) \/ (TwoPSet a2 r2) = TwoPSet (a1 \/ a2) (r1 \/ r2)
  
instance BoundedJoinSemiLattice TwoPSet where
  bottom = TwoPSet (Set.empty) (Set.empty)


-- for the counters, we need to have a unique id per node, and know the network size
-- we assume the network is a /24 subnet. And that the IP address is unique.
-- thus, the network size is 255, and everybody got its own identifier already
data Experiment
  = IncOnlyCounterExp    -- maintain an increase-only counter accross the network with predefined size of the network
  | PNCounterExp         --
  | SetExp
  | TwoPSetExp

data Options = Options
  { optRecvAddr :: String
  , optRecvPort :: String
  , optBroadcastAddr :: String
  , optBroadcastPort :: String
  }

defaultOptions = Options
  { optRecvAddr = "0.0.0.0"
  , optRecvPort = "1337"
  , optBroadcastAddr = "255.255.255.255"import Algebra.Lattice ()
  , optBroadcastPort = "1337"
  }


main = runManaged $ do
  -- Because State based CRDTs are idempotent, associative, commutative Missing a message,
  -- or receiving a message twice is not a concern. This way, our gossip protocol
  -- is basically listening to packets from all IPs, and sending packets to all IPs. 
  -- It is super simple, but not very efficient.
  -- But it suffices to show the properties of CRDTs.
  -- Operation-based CRDTs are more efficient, as they send diffs of state, but they
  -- are hence not idempotent, and extra network guarentees (guarentee of delivery,
  -- guarentee of in-orderness, and guarentee of no duplicates) are needed.
  --
  recvAddrInfo <- head <$> liftIO (getAddrInfo (Just $ defaultHints {addrFlags = [AI_PASSIVE]})
                                       (Just "0.0.0.0")
                                       (Just "1234"))

  sendAddrInfo <- head <$> liftIO (getAddrInfo Nothing
                                       (Just "255.255.255.255")
                                       (Just "1234"))
  recvSock <- managed (bracket (socket (addrFamily recvAddrInfo) Datagram defaultProtocol) close)
  sendSock <- managed (bracket (socket (addrFamily sendAddrInfo) Datagram defaultProtocol) close)


  liftIO $ bind recvSock (addrAddress recvAddrInfo)

  liftIO . runConcurrently $
    Concurrently (receiver recvSock) *>
    Concurrently (sender   sendSock) *>
    Concurrently local
