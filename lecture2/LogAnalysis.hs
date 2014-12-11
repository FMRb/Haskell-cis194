{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
getFirstCharSentence :: String -> String
getFirstCharSentence [] = "X"
getFirstCharSentence sentence = take 1 sentence

getErrorCode :: String -> Int
getErrorCode logLine
            | getFirstCharSentence logLine == "E" = read (take 2)
            otherwise 0

parseMessageType :: String -> Int -> MessageType
parseMessageType "I" _ = Info
parseMessageType "W" _ = Warning
parseMessageType "E" n = Error n
                
parseMyString :: String -> MessageType  
parseMyString logLine |
            length (words logLine) < 2 = False
            otherwise parseMessageType (take 1 (words logLine)) (take 2 (words logLine)) 

-- parseMessage :: String -> LogMessage
