module HandleTranslation (
	checkForSymbols, checkForChar, makeWindow, onlyOneLang, putNL, putDot, leaveIt, marathiFont, tempVowels, actionVowels, forNM, forU, forI, forE, forO, forA, finalMarathi, mytemp, splitMarathi, tempConsonents, actionConsonents, forK, forKH, forKSH, forG, forJ, forT, forTT, forD, forDD, forDDH, forDH, forDNY, forC, forCH, forCHH 
  ) where


import Graphics.UI.Gtk
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import Data.Char

--checks for special symbols and arranges a string accordingly
checkForSymbols :: String -> String
checkForSymbols str = concat $ map checkForChar str
 
checkForChar :: Char -> String
checkForChar ch = if(elem ch "~`!@#$%^&*()-_+|,./?;:[{]}\"\'\\") then (" "++[ch]) else [ch] 


--making a window to display the output text
makeWindow :: String -> String -> IO()
makeWindow lang str = do
  initGUI
  window <- windowNew
  
  textField1 <- textViewNew
  textBuffer1 <- textViewGetBuffer textField1
  srctextfont <- fontDescriptionFromString "Courier Bold 20"  
  widgetModifyFont textField1 (Just srctextfont)
  
  scroll1 <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport scroll1 textField1
  
  containerAdd window scroll1
    
  onlyOneLang textBuffer1 (lang ++ "Fonts") str
  
  set window [windowTitle := "Indian Language Script Writer", containerBorderWidth := 10]
  windowSetGeometryHints window (Nothing :: Maybe Window)
    (Just (500,100)) (Just (500,100)) Nothing Nothing Nothing
  
  widgetModifyBg window StateNormal (Color 10000 20000 30000) --(Color 6851 6851 30000) 		        
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

--for calling functions which will convert the sentence into tokens and fetching appropriate letter from database
onlyOneLang :: TextBuffer -> String -> String -> IO()
onlyOneLang textBuffer2 fonts str = 
  do marathiFont (finalMarathi $ map toLower str) "Languages.db" textBuffer2 fonts
     sI <- textBufferGetStartIter textBuffer2                    
     eI <- textBufferGetEndIter textBuffer2
     text1 <- textBufferGetText textBuffer2 sI eI True
     let ch = filter (/='a') $ unwords $ putNL $ words (leaveIt text1)
     textBufferSetText textBuffer2 ch

--checking for dot or quetion mark and if not found , then adding it at the end of a sentence
putDot :: [String] -> [String]
putDot lstStr = if(elem (last lstStr)  [".","?"]) then lstStr else (lstStr ++ ["."])
putNL lstStr = if(elem (last lstStr) [".","?"]) then ([""] ++ tk ++ [(head dp) ++ "\n"] ++ (tail dp)) else (lstStr) 
  where 
    tk = takeWhile (temp) lstStr  -- /="."
    dp = dropWhile (temp) lstStr
    temp e = not $ elem e [".","?"]

--neglecting ~ symbol which was used while creating devanagari script
leaveIt :: [Char] -> [Char]
leaveIt "" = ""
leaveIt (e:str) = if(e=='~') then (leaveIt str) else (e:(leaveIt str))


--converting sentence into marathi font and putting data into text buffer
marathiFont :: [String] -> String -> TextBuffer -> String -> IO()
marathiFont [] dbname buffer langFonts = mapM_ putStrLn []
marathiFont lstStr dbname buffer langFonts = 
  do conn <- connectSqlite3 dbname
     temp <- quickQuery' conn
     	     ("select meaning from "++langFonts++" where letter = ?") [toSql (head lstStr)]

     -- let stringRows = [(unwords (map convRow temp)))]
     let stringRows = [unwords (map convRow temp)]	
     let vi = checkType stringRows (head lstStr)   
    
     
     e <- textBufferGetEndIter buffer 
     
     textBufferInsert buffer e vi
     textBufferInsert buffer e "~"	     
     --putStrLn $ show stringRows
     Database.HDBC.disconnect conn
     marathiFont (tail lstStr) dbname buffer langFonts 

  where convRow :: [SqlValue] -> String
        convRow [sqlMeaning] = meaning
          where meaning = case (fromSql sqlMeaning) of 
	          Just x -> x
	          Nothing -> "NULL"
                  
        convRow x = fail $ "Unexpected result: " ++ show x    
        checkType ([]) str = str
	checkType (e:lst) str = if(e == []) then (checkType lst str) else e


--for splitting according to vowels
tempVowels str = (length ss, ss)
  where
    tt = actionVowels str
    ss = forNM str (length tt)

actionVowels str = if((head (tail str)) == 'u') 
       	         then (forU str)
		 else (
		      if((head (tail str)) == 'i') 
		      then (forI str) 
		      else 
		      (
		      if((head (tail str)) == 'e')
		      then (forE str)
		      else
		      (
		      if((head (tail str)) ==  'o')
		      then (forO str)
		      else
		      (
		      if((head (tail str)) == 'a')
		      then (forA str)
		      else ((head str):"")
		      ))))

forNM str len = if(length str < 5) then (take len str) else (if((elem (head re) "nm") && (not $ elem (head (tail re)) "aeiou")) then (take (len+1) str) else (take len str)) 
  where
    re = drop len str

forU str = take 2 str
forI str = take 2 str

forE (e:e1:[]) = e:e1:[]
forE (e:e1:str) = if((head str)=='e') then (take 3 (e:e1:str)) else (take 2 (e:e1:str))

forO (e:e1:[]) = e:e1:[]
forO (e:e1:str) = if((head str)=='o') then (take 3 (e:e1:str)) else (take 2 (e:e1:str))

forA (e:e1:[]) = e:e1:[]
forA (e:e1:str) = if(elem (head str) "aiu") then (take 3 (e:e1:str)) else (take 2 (e:e1:str))


--main function for splitting the whole sentence into set of strings to take their appropriate symbol from a database
finalMarathi str = concat $ map mytemp $ map splitMarathi $ words str

mytemp str = str ++ [" "]

--splitting a sentence according to the possibilities
splitMarathi "" = []
splitMarathi (e:[]) = [[e]]
splitMarathi (e:str) = if(not (elem e ['a'..'z'])) then ([e] : (splitMarathi str)) else (if(elem e ",.") then [(e:str)] else (if(not $ elem (last str) "aeiou" ) then (splitMarathi ((e:str)++"a")) else (if(elem e "kgcjtdpbs") then ((snd ch ): (splitMarathi (drop (fst ch) (e:str)))) else ((snd ch1 ): (splitMarathi (drop (fst ch1) (e:str)))))))	
  where
    ch = tempConsonents (e:str)
    ch1 = tempVowels (e:str)


tempConsonents str = (length (actionConsonents str), (actionConsonents str))

actionConsonents str = if((head str) == 'k') 
       	         then (forK str)
		 else (
		      if((head str) == 'g') 
		      then (forG str) 
		      else 
		      (
		      if((head  str) == 'c')
		      then (forC str)
		      else
		      (
		      if((head str) ==  'j')
		      then (forJ str)
		      else
		      (
		      if((head str) == 't')
		      then (forT str)
		      else
		      (
		      if((head str) == 'd')
		      then (forD str)
		      else
		      (
		      if((head str) == 'p')
		      then (forP str)
		      else
		      (
		      if((head str) == 'b')
		      then (forB str)
		      else
		      (
		      if((head str) == 's')
		      then (forS str)
		      else (snd (tempVowels str))
		      ))))
		      ))))


--FOR K, KH, KSH
forK (e:str) = if(head str == 'h') then (forKH (e:str)) else (if(head str == 's') then (forKSH (e:str)) else (snd $ tempVowels (e:str)))


forKH (e:str) = ((e:[])++(snd $ tempVowels str))

forKSH (e:e1:[]) = e:e1:[]     
forKSH (e:e1:str) = if((head str)=='h') then ((e:e1:[])++(snd $ tempVowels str))  else  (snd $ tempVowels (e:e1:str))


--FOR G, GH
forG (e:str) = if(head str == 'h') then ((e:[])++(snd $ tempVowels str))  else (snd $ tempVowels (e:str))

--FOR J, JH
forJ (e:str) = if(head str == 'h') then ((e:[])++(snd $ tempVowels str))  else (snd $ tempVowels (e:str))

--FOR T, TH, TT, TTH
forT (e:str) = if(head str == 't') then (forTT (e:str)) else(if(head str == 'h') then ((e:[])++(snd $ tempVowels str))  else (snd $ tempVowels (e:str)))

forTT (e:e1:str) = if(head str == 'h') then ((e:e1:[])++(snd $ tempVowels str))  else ((e:[])++(snd $ tempVowels (e1:str)))

--FOR D, DH, DNY, DD DDH

forD (e:str) = if(head str == 'd') then (forDD (e:str)) else (if(head str == 'h') then (forDH (e:str)) else (if(head str == 'n') then (forDNY (e:str)) else (snd $ tempVowels (e:str))))

forDD (e:e1:str) = if(head str == 'h') then (forDDH (e:e1:str)) else ((e:[])++(snd $ tempVowels (e1:str)))

forDDH (e:e1:str) = ((e:e1:[])++(snd $ tempVowels str))  

forDH (e:str) = ((e:[])++(snd $ tempVowels str))

forDNY (e:e1:[]) = e:e1:[]     
forDNY (e:e1:str) = if((head str)=='y') then ((e:e1:[])++(snd $ tempVowels str))  else  (snd $ tempVowels (e:e1:str))


--FOR C, CH, CHH

forC (e:str) = if(head str == 'h') then (if(head (tail str) == 'h') then (forCHH (e:str))  else (forCH (e:str))) else (snd $ tempVowels (e:str))

forCH (e:str) = ((e:[])++(snd $ tempVowels str))

forCHH (e:e1:[]) = e:e1:[]     
forCHH (e:e1:str) = ((e:e1:[])++(snd $ tempVowels str))



--FOR P, PH
forP (e:str) = if(head str == 'h') then ((e:[])++(snd $ tempVowels str))  else (snd $ tempVowels (e:str))

--FOR B, BH
forB (e:str) = if(head str == 'h') then ((e:[])++(snd $ tempVowels str))  else (snd $ tempVowels (e:str))

--FOR S, SH, SSH
forS (e:str) = if(head str == 's') then (forSSH (e:str)) else (if(head str == 'h') then ((e:[])++(snd $ tempVowels str))  else (snd $ tempVowels (e:str)))

forSSH (e:e1:str) = (e:e1:[])++(snd $ tempVowels str)
