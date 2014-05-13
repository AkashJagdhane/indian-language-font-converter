module Font (
	convertToStr, convertToFile  
  ) where

import Graphics.UI.Gtk
import Data.Char
import HandleTranslation

--converting from marathi sentence in english font to marathi font 
convertToStr :: String -> String -> IO()
convertToStr lang "" = putStrLn ""
convertToStr lang str = if(checkValid lang) then (if(lang == "hindi") then (makeWindow "marathi" (checkForSymbols (map toLower str))) else (makeWindow lang (checkForSymbols (map toLower str)))) else putStrLn "first argument should always be from the list : \n[\"hindi\",\"marathi\",\"kannada\",\"gujarati\",\"bengali\",\"punjabi\",\"telugu\",\"malayalam\",\"oriya\"]"

--checking for validity of language
checkValid str = if(elem (map toLower str) ["hindi","marathi","kannada","gujarati","bengali","punjabi","telugu","malayalam","oriya"]) then True else False

convertToFile :: String -> FilePath -> FilePath -> IO()
convertToFile lang "" "" = putStrLn ""
convertToFile lang file1 file2 = if(checkValid lang) then (if(lang == "hindi") then (convertToFileTemp "marathi" file1 file2) else (convertToFileTemp lang file1 file2)) else putStrLn "first argument should always be from the list : \n[\"hindi\",\"marathi\",\"kannada\",\"gujarati\",\"bengali\",\"punjabi\",\"telugu\",\"malayalam\",\"oriya\"]"

--converting from any of the 9 Indian Language sentence(s) file in english font to Indian font (for marathi,hindi,kannada,gujarati,bengali,punjabi,telugu,malayalam,oriya) 
convertToFileTemp :: String -> FilePath -> FilePath -> IO()
convertToFileTemp lang "" "" = putStrLn ""
convertToFileTemp lang file1 file2 = do
  initGUI	
  textField1 <- textViewNew
  textBuffer1 <- textViewGetBuffer textField1
  str <- readFile file1
  marathiFont (finalMarathi $ map toLower (checkForSymbols str)) "Languages.db" textBuffer1 (lang ++ "Fonts")
  sI <- textBufferGetStartIter textBuffer1                    
  eI <- textBufferGetEndIter textBuffer1
  text1 <- textBufferGetText textBuffer1 sI eI True
  let ch = filter (/='a') $ unwords $ putNL $ words (leaveIt text1)
  writeFile file2 ch
         
