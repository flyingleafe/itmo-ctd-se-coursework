module NLP (loadDataset, tfIdfVectorize) where

import           Data.Map                 (keys)
import           Data.Text                (pack, unpack)
import           NLP.Similarity.VectorSim (mkDocument, tf_idf)
import           NLP.Types                (Corpus (..), mkCorpus)
import           System.Directory         (doesDirectoryExist, listDirectory)
import           System.FilePath          ((</>))
import           Universum

preprocess :: Text -> [Text]
preprocess = words

loadDataset :: FilePath -> IO [[Text]]
loadDataset = fmap (map preprocess) . (mapM readFile =<<) . getDirRec
    where
    getDirRec fp = do
        isDir <- doesDirectoryExist fp
        if isDir
        then fmap concat $ map (fp </>) <$> listDirectory fp >>= mapM getDirRec
        else return [fp]

tfIdfVectorize :: [[Text]] -> [[Double]]
tfIdfVectorize dic = map docVectorize dic where
    docVectorize doc = map (\w -> tf_idf w (mkDocument doc) corpus) terms
    terms = keys (corpTermCounts corpus)
    corpus = mkCorpus dic
