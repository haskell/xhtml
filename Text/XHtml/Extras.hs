{-# language OverloadedStrings #-}

module Text.XHtml.Extras where

import qualified Data.Text.Lazy as LText

import Text.XHtml.Internals
import Text.XHtml.Strict.Elements
import Text.XHtml.Strict.Attributes

--
-- * Converting strings to HTML
--

-- | Convert a 'String' to 'Html', converting
--   characters that need to be escaped to HTML entities.
stringToHtml :: String -> Html
stringToHtml = primHtml . builderToString . stringToHtmlString

{-# INLINE stringToHtml #-}

-- | This converts a string, but keeps spaces as non-line-breakable.
lineToHtml :: String -> Html
lineToHtml =
    primHtmlNonEmptyBuilder . stringToHtmlString . foldMap htmlizeChar2
  where
    htmlizeChar2 ' ' = "&nbsp;"
    htmlizeChar2 c   = [c]

{-# INLINE lineToHtml #-}

-- | This converts a string, but keeps spaces as non-line-breakable,
--   and adds line breaks between each of the strings in the input list.
linesToHtml :: [String] -> Html
linesToHtml []     = noHtml
linesToHtml [x]    = lineToHtml x
linesToHtml (x:xs) = lineToHtml x +++ br +++ linesToHtml xs

{-# INLINE linesToHtml #-}

--
-- * Html abbreviations
--

primHtmlChar  :: String -> Html

-- | Copyright sign.
copyright     :: Html

-- | Non-breaking space.
spaceHtml     :: Html
bullet        :: Html


primHtmlChar x = primHtml ("&" ++ x ++ ";")
copyright      = primHtmlChar "copy"
spaceHtml      = primHtmlChar "nbsp"
bullet         = primHtmlChar "#149"

-- | Same as 'paragraph'.
p :: Html -> Html
p =  paragraph

--
-- * Hotlinks
--

type URL = LText.Text

data HotLink = HotLink {
      hotLinkURL        :: URL,
      hotLinkContents   :: Html,
      hotLinkAttributes :: [HtmlAttr]
      } deriving Show

instance HTML HotLink where
      toHtml hl = anchor ! (href (hotLinkURL hl) : hotLinkAttributes hl)
                      << hotLinkContents hl

instance ADDATTRS HotLink where
      hl ! attr = hl { hotLinkAttributes = hotLinkAttributes hl ++ attr }

hotlink :: URL -> Html -> HotLink
hotlink url h = HotLink {
      hotLinkURL = url,
      hotLinkContents = h,
      hotLinkAttributes = [] }


--
-- * Lists
--

-- (Abridged from Erik Meijer's Original Html library)

ordList   :: (HTML a) => [a] -> Html
ordList items = olist << map (li <<) items

unordList :: (HTML a) => [a] -> Html
unordList items = ulist << map (li <<) items

defList   :: (HTML a,HTML b) => [(a,b)] -> Html
defList items
 = dlist << [ [ dterm << dt, ddef << dd ] | (dt,dd) <- items ]

--
-- * Forms
--

widget :: LText.Text -> LText.Text -> [HtmlAttr] -> Html
widget w n attrs = input ! ([thetype w] ++ ns ++ attrs)
  where ns = if LText.null n then [] else [name n,identifier n]

checkbox :: LText.Text -> LText.Text -> Html
hidden   :: LText.Text -> LText.Text -> Html
radio    :: LText.Text -> LText.Text -> Html
reset    :: LText.Text -> LText.Text -> Html
submit   :: LText.Text -> LText.Text -> Html
password :: LText.Text           -> Html
textfield :: LText.Text          -> Html
afile    :: LText.Text           -> Html
clickmap :: LText.Text           -> Html

checkbox n v = widget "checkbox" n [value v]
hidden   n v = widget "hidden"   n [value v]
radio    n v = widget "radio"    n [value v]
reset    n v = widget "reset"    n [value v]
submit   n v = widget "submit"   n [value v]
password n   = widget "password" n []
textfield n  = widget "text"     n []
afile    n   = widget "file"     n []
clickmap n   = widget "image"    n []

{-# DEPRECATED menu "menu generates strange XHTML, and is not flexible enough. Roll your own that suits your needs." #-}
menu :: LText.Text -> [Html] -> Html
menu n choices
   = select ! [name n] << [ option << p << choice | choice <- choices ]

gui :: LText.Text -> Html -> Html
gui act = form ! [action act,method "post"]
