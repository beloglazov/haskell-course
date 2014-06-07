{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage x =
    let elems = drop 1 $ words x
        in case x of
    ('E':' ':_) -> LogMessage
        (Error $ read $ head elems)
        (read $ elems !! 1)
        (unwords $ drop 2 elems)
    ('W':' ':_) -> LogMessage
        Warning
        (read $ head elems)
        (unwords $ drop 1 elems)
    ('I':' ':_) -> LogMessage
        Info
        (read $ head elems)
        (unwords $ drop 1 elems)
    _           -> Unknown x

-- testParse parse 10 "error.log"
parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree              = tree
insert _ tree@(Node _ (Unknown _) _) = tree
insert m Leaf                        = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node l n@(LogMessage _ nt _) r)
    | t < nt    = Node (insert m l) n r
    | otherwise = Node l n (insert m r)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

isErrorSeverity50 :: LogMessage -> Bool
isErrorSeverity50 (LogMessage (Error s) _ _) = s >= 50
isErrorSeverity50 _                          = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m
getMessage (Unknown m)        = m

-- testWhatWentWrong parse whatWentWrong "sample.log"
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isErrorSeverity50
