module DocSource
       ( runTDSource
       , TfIdfCollection
       ) where

import           Control.Monad            (mapM, unless)
import           Data.Map                 (keys)
import           Data.Text                (pack, unpack)
import           Formatting               (sformat, shown, (%))
import           NLP.Similarity.VectorSim (mkDocument, tf_idf)
import           NLP.Types                (Corpus (..), mkCorpus)
import           System.Directory         (doesDirectoryExist, listDirectory)
import           System.FilePath          ((</>))
import           Universum

import           Config                   (TMConfig (..))
import           Types

newtype TextDirectorySource a = TDSource
    { getTDSource :: Base TMError a
    } deriving (Functor, Applicative, Monad, MonadIO)

type DocCollection = [[Text]]
type TfIdfCollection = [[Double]]

preprocess :: Text -> [Text]
preprocess = words

getTDCollection :: FilePath -> TextDirectorySource DocCollection
getTDCollection = fmap (map preprocess) . (mapM readFile =<<) . liftIO . getDirRec
    where
    getDirRec fp = do
        isDir <- doesDirectoryExist fp
        if isDir
        then fmap concat $ map (fp </>) <$> listDirectory fp >>= mapM getDirRec
        else return [fp]

tfIdfVectorize :: DocCollection -> TfIdfCollection
tfIdfVectorize dic = map docVectorize dic where
    docVectorize doc = map (\w -> tf_idf w (mkDocument doc) corpus) terms
    terms = keys (corpTermCounts corpus)
    corpus = mkCorpus dic

runTDSource :: FilePath -> Base TMError TfIdfCollection
runTDSource = fmap tfIdfVectorize . getTDSource . getTDCollection
