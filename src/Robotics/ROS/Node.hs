-- |The primary entrypoint to the ROS client library portion of
-- roshask. This module defines the actions used to configure a ROS
-- Node.
module Robotics.ROS.Node
  ( Node
  , runNode
  , subscribe
  , advertise
  , getParam
  , getParamOpt
  , getName
  , getNamespace
  , module Robotics.ROS.Types
  , module Robotics.ROS.Time
  ) where

import System.Environment (getEnvironment, getArgs)
import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (get, execStateT)
import Network.XmlRpc.Internals (XmlRpcType)
import Control.Concurrent.STM (newTVarIO)
import Control.Concurrent (newEmptyMVar)
import Data.Conduit (Source, Sink)
import Control.Monad (when)
import Data.Binary (Binary)
import qualified Data.Map as M

import Robotics.ROS.Graph.Stats (recvMessageStat, sendMessageStat)
import qualified Robotics.ROS.Graph.ParameterServer as P
import Robotics.ROS.Node.ArgRemapping
import Robotics.ROS.Node.Slave
import Robotics.ROS.Node.Type
import Robotics.ROS.Msg.MsgInfo
import Robotics.ROS.Types
import Robotics.ROS.Time

-- |Subscrive to given topic name
subscribe :: (MonadIO m, Binary a, MsgInfo a) => TopicName -> Node (Source m a)
subscribe = undefined

-- |Advertise a topic publishing a stream of values.
advertise :: (MonadIO m, Binary a, MsgInfo a) => TopicName -> Node (Sink a m ())
advertise = undefined

-- |Apply any matching renames to a given name.
remapName :: String -> Node String
remapName name = asks (maybe name id . lookup name . nodeRemaps)

-- |Convert relative names to absolute names. Leaves absolute names
-- unchanged.
canonicalizeName :: String -> Node String
canonicalizeName n@('/':_) = return n
canonicalizeName ('~':n) = do state <- get
                              let node = name state
                              return $ node ++ "/" ++ n
canonicalizeName n = do (++n) . namespace <$> get

-- |Get a parameter value from the Parameter Server.
getServerParam :: XmlRpcType a => String -> Node (Maybe a)
getServerParam var = do state <- get
                        let masterUri = master state
                            myName = name state
                        -- Call hasParam first because getParam only returns
                        -- a partial result (just the return code) in failure.
                        hasParam <- liftIO $ P.hasParam masterUri myName var
                        case hasParam of
                          Right True -> liftIO $ P.getParam masterUri myName var
                          _ -> return Nothing

-- |Get the value associated with the given parameter name. If the
-- parameter is not set, then 'Nothing' is returned; if the parameter
-- is set to @x@, then @Just x@ is returned.
getParamOpt :: (XmlRpcType a, FromParam a) => String -> Node (Maybe a)
getParamOpt var = do var' <- remapName =<< canonicalizeName var
                     params <- nodeParams <$> ask
                     case lookup var' params of
                       Just val -> return . Just $ fromParam val
                       Nothing -> getServerParam var'

-- |Get the value associated with the given parameter name. If the
-- parameter is not set, return the second argument as the default
-- value.
getParam :: (XmlRpcType a, FromParam a) => String -> a -> Node a
getParam var def = maybe def id <$> getParamOpt var

-- |Get the current node's name.
getName :: Node String
getName = name <$> get

-- |Get the current namespace.
getNamespace :: Node String
getNamespace = namespace <$> get

-- |Run a ROS Node.
runNode :: NodeName -> Node a -> IO ()
runNode name (Node nConf) = do 
    (env, args) <- liftIO $ (,) <$> getEnvironment <*> getArgs

    let getConfig' var def = maybe def id $ lookup var env
        getConfig = flip lookup env
        masterConf = getConfig' "ROS_MASTER_URI" "http://localhost:11311"
        namespaceConf = let ns = getConfig' "ROS_NAMESPACE" "/"
                         in if last ns == '/' then ns else ns ++ "/"
        (nameMap, params) = parseRemappings args
        name' = case lookup "__name" params of
                    Just x -> fromParam x
                    Nothing -> case name of
                                   '/':_ -> name
                                   _ -> namespaceConf ++ name
        -- Name remappings apply to exact strings and resolved names.
        resolve p@(('/':_),_) = [p]
        resolve (('_':n),v) = [(name'++"/"++n, v)]
        resolve (('~':n),v) = [(name'++"/"++ n, v)] --, ('_':n,v)]
        resolve (n,v) = [(namespaceConf ++ n,v), (n,v)]
        nameMap' = concatMap resolve nameMap
        params' = concatMap resolve params

    when (not $ null nameMap') $
        putStrLn $ "Remapping name(s) " ++ show nameMap'
    when (not $ null params') $
        putStrLn $ "Setting parameter(s) " ++ show params'

    varURI   <- newEmptyMVar
    varStats <- newTVarIO M.empty

    let nodeConfig   = runReaderT nConf (NodeConfig params' nameMap')
        initialState = NodeState name' masterConf namespaceConf varURI varStats M.empty
        statefulNode = execStateT nodeConfig initialState
     in slave name' =<< statefulNode
