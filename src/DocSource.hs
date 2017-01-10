{-# LANGUAGE FlexibleContexts #-}

module DocSource
       ( runTDSource
       , TfIdfCollection
       , DocumentsInfo (..)
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

data DocumentsInfo = DocumentsInfo
    { diNClusters  :: Int
    , diCollection :: TfIdfCollection
    }

preprocess :: Text -> [Text]
preprocess = words

getTDCollection :: FilePath -> TextDirectorySource (Int, DocCollection)
getTDCollection fp = do
    files <- liftIO $ getDirRec fp
    let lbs = ordNub $ map (pack . takeFileName . takeDirectory) files
    labels .= lbs
    (,) (length lbs) <$> mapM (fmap preprocess . readFile) files
  where
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

runTDSource :: FilePath -> Base DocumentsInfo
runTDSource fp = getTDSource $ do-- fmap tfIdfVectorize . getTDSource . getTDCollection
    (diNClusters, dc) <- getTDCollection fp
    let diCollection = tfIdfVectorize dc
    return DocumentsInfo {..}

