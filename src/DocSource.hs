{-# LANGUAGE FlexibleContexts #-}

module DocSource
       ( runTDSource
       , TfIdfCollection
       ) where

import           Control.Lens             ((.=))
import           Control.Monad            (mapM, unless)
import qualified Data.Map                 as M
import           Data.Text                (pack, unpack)
import           Formatting               (sformat, shown, (%))
import           NLP.Similarity.VectorSim (mkDocument, tf_idf)
import           NLP.Types                (Corpus (..), mkCorpus, termCounts)
import           System.Directory         (doesDirectoryExist, listDirectory)
import           System.FilePath          (takeDirectory, takeFileName, (</>))
import           Universum

import           Config                   (TMConfig (..))
import           Types

newtype TextDirectorySource a = TDSource
    { getTDSource :: Base a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadState ProcessData)

type DocCollection = [[Text]]
type TfIdfCollection = [[Double]]

preprocess :: Text -> [Text]
preprocess = words

getTDCollection :: FilePath -> TextDirectorySource DocCollection
getTDCollection = fmap (map preprocess) . (mapM readFile =<<) . (lbls =<<) . liftIO . getDirRec
  where
    lbls w = do
        labels .= map (pack . takeFileName . takeDirectory) w
        return w
    getDirRec fp = do
        isDir <- doesDirectoryExist fp
        if isDir
        then fmap concat $ map (fp </>) <$> listDirectory fp >>= mapM getDirRec
        else return [fp]

tfIdfVectorize :: DocCollection -> TfIdfCollection
tfIdfVectorize dic = map docVectorize dic
  where
    docVectorize doc = map (\w -> tf_idf w (mkDocument doc) corpus) terms
    thresh cnt = cnt >= 10 && cnt <= 500
    terms = M.keys (corpTermCounts corpus)
    corpus = Corpus (corpLength cnf) (M.filter thresh (corpTermCounts cnf))
    cnf = mkCorpus dic

runTDSource :: FilePath -> Base TfIdfCollection
runTDSource = fmap tfIdfVectorize . getTDSource . getTDCollection
