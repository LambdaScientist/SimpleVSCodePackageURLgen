{-# LANGUAGE ScopedTypeVariables #-}


-- https://stackoverflow.com/questions/37071388/how-to-install-vscode-extensions-offline
-- Installation

-- In order to install the extension

-- As of VSCode 1.7.1 dragging or opening the extension does not work any more. In order to install it manually you need to:

--     open the extensions sidebar
--     click on the ellipsis in the right upper corner
--     choose Install from VSIX


-- Format
-- https://marketplace.visualstudio.com/items?itemName=ms-vscode.csharp
-- https://marketplace.visualstudio.com/items?itemName=PUBLISHER.EXTENSTION
-- https://${publisher}.gallery.vsassets.io/_apis/public/gallery/publisher/${publisher}/extension/${extension name}/${version}/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage
type Publisher = String
type ExtenstionName = String
type Version = String
type URL = String

makeVsixURL :: Publisher -> ExtenstionName -> Version -> URL
makeVsixURL publisher extName version = http ++ gallery ++ pub ++ ext ++ ver ++ vsix
  where
    http = "https://"
    gallery = publisher ++ ".gallery.vsassets.io/_apis/public/gallery"
    pub = "/publisher/" ++ publisher
    ext = "/extension/" ++ extName
    ver = "/" ++ version 
    vsix = "/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage"

takeMarketURL :: URL -> Version -> URL
takeMarketURL url = makeVsixURL pub ext
  where
    takeAfterLast char = reverse . takeWhile (/= char ) . reverse
    ending = takeAfterLast '=' url
    (pub,ext) = splitAtFirstMatch (=='.') ending

splitAtFirstMatch :: forall a . Eq a => (a -> Bool) -> [a] -> ([a],[a])
splitAtFirstMatch f [] = ([],[])
splitAtFirstMatch f xs = splitter [] xs
  where
    splitter :: [a] -> [a] -> ([a],[a])
    splitter left [] = (left,[])
    splitter left (r:emainder)
            = if f r
              then (left, emainder)
              else splitter (left ++ [r]) emainder


-- Example
-- https://marketplace.visualstudio.com/items?itemName=mshr-h.VHDL

csharp :: URL
csharp = makeVsixURL "ms-vscode" "csharp" "1.3.0"
runCsharp :: IO ()
runCsharp = putStrLn csharp

vhdl :: Version -> URL
vhdl = takeMarketURL "https://marketplace.visualstudio.com/items?itemName=mshr-h.VHDL"
defaultVHDL :: URL
defaultVHDL = vhdl "0.0.2"
runDefVhdl :: IO ()
runDefVhdl = putStrLn defaultVHDL 


cobaltNeon :: Version -> URL
cobaltNeon = takeMarketURL "https://marketplace.visualstudio.com/items?itemName=audioj.theme-cobaltneon"
defaultCobaltNeon :: URL
defaultCobaltNeon = cobaltNeon "0.0.1"
runDefCobaltNeon :: IO ()
runDefCobaltNeon = putStrLn defaultCobaltNeon 





