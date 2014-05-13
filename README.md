indian-language-font-converter
==============================

hackage link (link to cabal package) :

http://hackage.haskell.org/package/indian-language-font-converter

** How To install this cabal package :

1) go to the extracted directory "indian-language-font-converter-0.0"

2) on terminal , fire the following commands :
      $ sudo runhaskell Setup.hs configure
      $ sudo runhaskell Setup.hs build
      $ sudo runhaskell Setup.hs install

3) to check if the package is installed into ghci, do the following :
      $ ghci
      prelude> :module IndianLanguage

   If you will reach something like "prelude IndianLanguage>" in haskell then you can say that the package is properly installed.

4) whenever you need to use this package, just put "Languages.db" file into a current directory, else the functions will give an error(related to sql)

5) well, we can now use this package


** Testing the package :

1) we have three modules : 
		IndianLanguage.Font
		IndianLanguage.HandleTranslation

2) IndianLanguage.HandleTranslation is a supporting module for the remaining two modules. Our main aim is to use first module listed above.

3) Prelude> :module IndianLanguage.Font 
   Prelude IndianLanguage.Font> convertToStr "marathi" "maaze naav Aakaash aahe" 

   It will generate a window having a textview with "माझे नाव आकाश आहे" in it.
 
   Prelude IndianLanguage.Font> convertToFile "marathi" "inputFile.txt" "outputFile.txt"

   Here, inputFile.txt will have any number of sentences on marathi meaning but which are written in english alphabets, like :

   File Line No 1 : maaze naav aakaash aahe. 
   File Line No 2 : mee chinchawadd madhye raahato. 
   File Line No 3 : aakaash maazaa mitra aahe. 
   File Line No 4 : aaj guruvaar aahe. 
   File Line No 5 : shubh prabhaat. 

   And after calling "convertToFile" function, our "outputFile.txt" will contain :
   File Line No 1 : माझे नाव आकाश आहे .
   File Line No 2 : मी चिंचवड मध्ये राहतो . 
   File Line No 3 : आकाश माझा मित्र आहे . 
   File Line No 4 : आज गुरुवार आहे . 
   File Line No 5 : शुभ प्रभात .
