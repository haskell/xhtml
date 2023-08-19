{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings, FlexibleInstances, BangPatterns, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.XHtml.internals
-- Copyright   :  (c) Andy Gill, and the Oregon Graduate Institute of
--                Science and Technology, 1999-2001,
--                (c) Bjorn Bringert, 2004-2006
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Chris Dornan <chris@chrisdornan.com>
-- Stability   :  Stable
-- Portability :  Portable
--
-- Internals of the XHTML combinator library.
-----------------------------------------------------------------------------
module Text.XHtml.Internals
    ( module Text.XHtml.Internals
    , Builder
    ) where

import qualified Data.Text.Lazy as LText
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Builder hiding (char7)
import qualified Data.ByteString.Builder.Prim as P
import Data.ByteString.Builder.Prim hiding (intDec, charUtf8)
import Data.ByteString.Internal (c2w)
import qualified Data.Semigroup as Sem
import qualified Data.Monoid as Mon
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word8)

type LText = LText.Text

infixr 2 +++  -- combining Html
infixr 7 <<   -- nesting Html
infixl 8 !    -- adding optional arguments

--
-- * Data types
--

-- | A important property of Html is that all strings inside the
-- structure are already in Html friendly format.
data HtmlElement
      = HtmlString !Builder
        -- ^ ..just..plain..normal..text... but using &copy; and &amb;, etc.
      | HtmlTag {
              markupTag      :: !BSL.ByteString,
              markupAttrs    :: [HtmlAttr] -> [HtmlAttr],
              markupContent  :: !Html
              }
        -- ^ tag with internal markup

-- | Attributes with name and value.
data HtmlAttr = HtmlAttr !Builder !Builder


htmlAttrPair :: HtmlAttr -> (Builder,Builder)
htmlAttrPair (HtmlAttr n v) = (n,v)


newtype Html = Html { unHtml :: [HtmlElement] -> [HtmlElement] }

getHtmlElements :: Html -> [HtmlElement]
getHtmlElements html = unHtml html []

builderToString :: Builder -> String
builderToString =
    LText.unpack . LText.decodeUtf8 . toLazyByteString

lazyByteStringToString :: BSL.ByteString -> String
lazyByteStringToString =
    LText.unpack . LText.decodeUtf8

--
-- * Classes
--

instance Show Html where
      showsPrec _ html = showString (builderToString (renderHtmlFragment html))
      showList = foldr ((.) . shows) id

instance Show HtmlAttr where
      showsPrec _ (HtmlAttr str val) =
              showString (builderToString str) .
              showString "=" .
              shows (builderToString val)

-- | @since 3000.2.2
instance Sem.Semigroup Html where
    (<>) = (+++)
    {-# INLINE (<>) #-}

instance Mon.Monoid Html where
    mempty = noHtml
    mappend = (Sem.<>)
    {-# INLINE mappend #-}

-- | HTML is the class of things that can be validly put
-- inside an HTML tag. So this can be one or more 'Html' elements,
-- or a 'String', for example.
class HTML a where
      toHtml     :: a -> Html
      toHtmlFromList :: [a] -> Html

      toHtmlFromList xs = Html (foldr (\x acc -> unHtml (toHtml x) . acc) id xs)

instance HTML Html where
      toHtml a    = a
      {-# INLINE toHtml #-}
      toHtmlFromList htmls = Html (foldr (\x acc -> unHtml x . acc) id htmls)
      {-# INLINE toHtmlFromList #-}

instance HTML Char where
      toHtml       a = toHtml [a]
      {-# INLINE toHtml #-}
      toHtmlFromList []  = Html id
      toHtmlFromList str = Html (HtmlString (stringToHtmlString str) :)
      {-# INLINE toHtmlFromList #-}

instance (HTML a) => HTML [a] where
      toHtml = toHtmlFromList
      {-# INLINE toHtml #-}

instance HTML a => HTML (Maybe a) where
      toHtml = maybe noHtml toHtml
      {-# INLINE toHtml #-}

instance HTML Text where
    toHtml "" = Html id
    toHtml xs = Html (HtmlString (textToHtmlString xs) :)
    {-# INLINE toHtml #-}

instance HTML LText.Text where
    toHtml "" = Html id
    toHtml xs = Html (HtmlString (lazyTextToHtmlString xs) : )
    {-# INLINE toHtml #-}

mapDlist :: (a -> b) -> ([a] -> [a]) -> [b] -> [b]
mapDlist f as = (map f (as []) ++)
{-# INLINE mapDlist #-}

class ADDATTRS a where
      (!) :: a -> [HtmlAttr] -> a

-- | CHANGEATTRS is a more expressive alternative to ADDATTRS
class CHANGEATTRS a where
      changeAttrs :: a -> ([HtmlAttr] -> [HtmlAttr]) -> a

instance (ADDATTRS b) => ADDATTRS (a -> b) where
      fn ! attr        = \ arg -> fn arg ! attr
      {-# INLINE (!) #-}

instance (CHANGEATTRS b) => CHANGEATTRS (a -> b) where
      changeAttrs fn f arg = changeAttrs (fn arg) f

instance ADDATTRS Html where
    (Html htmls) ! attr = Html (mapDlist addAttrs htmls)
      where
        addAttrs html =
            case html of
                HtmlTag { markupAttrs = attrs, .. } ->
                    HtmlTag
                        { markupAttrs = attrs . (attr ++)
                        , ..
                        }
                _ ->
                    html
    {-# INLINE (!) #-}


instance CHANGEATTRS Html where
      changeAttrs (Html htmls) f = Html (mapDlist addAttrs htmls)
        where
              addAttrs html@(HtmlTag { markupAttrs = attrs })
                            = html { markupAttrs = f . attrs }
              addAttrs html = html


--
-- * Html primitives and basic combinators
--

-- | Put something inside an HTML element.
(<<) :: (HTML a) =>
        (Html -> b) -- ^ Parent
     -> a -- ^ Child
     -> b
fn << arg = fn (toHtml arg)

{-# SPECIALIZE (<<) :: (Html -> b) -> String -> b #-}
{-# SPECIALIZE (<<) :: (Html -> b) -> Text -> b #-}
{-# SPECIALIZE (<<) :: (Html -> b) -> LText -> b #-}
{-# SPECIALIZE (<<) :: (Html -> b) -> Html -> b #-}
{-# SPECIALIZE (<<) :: (Html -> b) -> [Html] -> b #-}
{-# INLINABLE (<<) #-}

concatHtml :: (HTML a) => [a] -> Html
concatHtml = Html . foldr ((.) . unHtml . toHtml) id

{-# SPECIALIZE concatHtml :: [Html] -> Html #-}
{-# INLINABLE concatHtml #-}

-- | Create a piece of HTML which is the concatenation
--   of two things which can be made into HTML.
(+++) :: (HTML a, HTML b) => a -> b -> Html
a +++ b = Html (unHtml (toHtml a) . unHtml (toHtml b))

{-# SPECIALIZE (+++) :: Html -> Html -> Html #-}
{-# INLINABLE (+++) #-}

-- | An empty piece of HTML.
noHtml :: Html
noHtml = Html id

{-# INLINE noHtml #-}

-- | Checks whether the given piece of HTML is empty. This materializes the
-- list, so it's not great to do this a bunch.
isNoHtml :: Html -> Bool
isNoHtml (Html xs) = null (xs [])

-- | Constructs an element with a custom name.
tag :: BSL.ByteString -- ^ Element name
    -> Html -- ^ Element contents
    -> Html
tag str htmls =
    Html
        (
        HtmlTag
            { markupTag = str
            , markupAttrs = id
            , markupContent = htmls
            }
        :
        )

-- | Constructs an element with a custom name, and
--   without any children.
itag :: BSL.ByteString -> Html
itag str = tag str noHtml

emptyAttr :: Builder -> HtmlAttr
emptyAttr s = HtmlAttr s s

intAttr :: Builder -> Int -> HtmlAttr
intAttr s = HtmlAttr s . intDec
{-# INLINE intAttr #-}

strAttr :: Builder -> LText.Text -> HtmlAttr
strAttr s = HtmlAttr s . lazyTextToHtmlString
{-# INLINE strAttr #-}

htmlAttr :: Builder -> Html -> HtmlAttr
htmlAttr s t = HtmlAttr s (renderHtmlFragment t)

{-
foldHtml :: (String -> [HtmlAttr] -> [a] -> a)
      -> (String -> a)
      -> Html
      -> a
foldHtml f g (HtmlTag str attr fmls)
      = f str attr (map (foldHtml f g) fmls)
foldHtml f g (HtmlString  str)
      = g str

-}

-- | Processing Strings into Html friendly things.
stringToHtmlString :: String -> Builder
stringToHtmlString = primMapListBounded charUtf8HtmlEscaped
{-# INLINE stringToHtmlString #-}

-- | Copied from @blaze-builder@
{-# INLINE charUtf8HtmlEscaped #-}
charUtf8HtmlEscaped :: BoundedPrim Char
charUtf8HtmlEscaped =
    condB (>  '>' ) P.charUtf8 $
    condB (== '<' ) (fixed4 ('&',('l',('t',';')))) $              -- &lt;
    condB (== '>' ) (fixed4 ('&',('g',('t',';')))) $              -- &gt;
    condB (== '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $        -- &amp;
    condB (== '"' ) (fixed6 ('&',('q',('u',('o',('t',';')))))) $  -- &quot;
    liftFixedToBounded P.char7 -- fallback for Chars smaller than '>'
  where
    {-# INLINE fixed4 #-}
    fixed4 x = liftFixedToBounded $ const x >$<
      char7 >*< char7 >*< char7 >*< char7

    {-# INLINE fixed5 #-}
    fixed5 x = liftFixedToBounded $ const x >$<
      char7 >*< char7 >*< char7 >*< char7 >*< char7

    {-# INLINE fixed6 #-}
    fixed6 x = liftFixedToBounded $ const x >$<
      char7 >*< char7 >*< char7 >*< char7 >*< char7 >*< char7

textToHtmlString :: Text -> Builder
textToHtmlString = Text.encodeUtf8BuilderEscaped wordHtmlEscaped
{-# INLINE textToHtmlString #-}

lazyTextToHtmlString :: LText.Text -> Builder
lazyTextToHtmlString = LText.encodeUtf8BuilderEscaped wordHtmlEscaped

-- | Copied from @blaze-builder@
{-# INLINE wordHtmlEscaped #-}
wordHtmlEscaped :: P.BoundedPrim Word8
wordHtmlEscaped =
  P.condB (>  c2w '>' ) (P.condB (== c2w '\DEL') P.emptyB $ P.liftFixedToBounded P.word8) $
  P.condB (== c2w '<' ) (fixed4 ('&',('l',('t',';')))) $                  -- &lt;
  P.condB (== c2w '>' ) (fixed4 ('&',('g',('t',';')))) $                  -- &gt;
  P.condB (== c2w '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $            -- &amp;
  P.condB (== c2w '"' ) (fixed6 ('&',('q',('u',('o',('t',';')))))) $      -- &quot;
  P.condB (== c2w '\'') (fixed5 ('&',('#',('3',('9',';'))))) $            -- &#39;
  P.condB (\c -> c >= c2w ' ' || c == c2w '\t' || c == c2w '\n' || c == c2w '\r')
        (P.liftFixedToBounded P.word8) P.emptyB
  where
  {-# INLINE fixed4 #-}
  fixed4 x = P.liftFixedToBounded $ const x P.>$<
    P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8
  {-# INLINE fixed5 #-}
  fixed5 x = P.liftFixedToBounded $ const x P.>$<
    P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8
  {-# INLINE fixed6 #-}
  fixed6 x = P.liftFixedToBounded $ const x P.>$<
    P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8

-- | This is not processed for special chars.
-- use stringToHtml or lineToHtml instead, for user strings,
-- because they understand special chars, like @'<'@.
primHtml :: String -> Html
primHtml x | null x    = Html id
           | otherwise = Html (HtmlString (stringUtf8 x) :)

{-# INLINE primHtml #-}

-- | Does not process special characters, or check to see if it is empty.
primHtmlNonEmptyBuilder :: Builder -> Html
primHtmlNonEmptyBuilder x = Html (HtmlString x :)

{-# INLINE primHtmlNonEmptyBuilder #-}

--
-- * Html Rendering
--

mkHtml :: HTML html => html -> Html
mkHtml = (tag "html" ! [strAttr "xmlns" "http://www.w3.org/1999/xhtml"] <<)

{-# SPECIALIZE mkHtml :: Html -> Html #-}
{-# INLINABLE mkHtml #-}

-- | Output the HTML without adding newlines or spaces within the markup.
--   This should be the most time and space efficient way to
--   render HTML, though the output is quite unreadable.
showHtmlInternal :: HTML html =>
                    Builder -- ^ DOCTYPE declaration
                 -> html -> Builder
showHtmlInternal docType theHtml =
    docType <> showHtmlFragment (mkHtml theHtml)

{-# SPECIALIZE showHtmlInternal :: Builder -> Html -> Builder #-}
{-# INLINABLE showHtmlInternal #-}


-- | Outputs indented HTML. Because space matters in
--   HTML, the output is quite messy.
renderHtmlInternal :: HTML html =>
                      Builder  -- ^ DOCTYPE declaration
                   -> html -> Builder
renderHtmlInternal docType theHtml =
      docType <> "\n" <> renderHtmlFragment (mkHtml theHtml) <> "\n"

{-# SPECIALIZE renderHtmlInternal :: Builder -> Html -> Builder #-}
{-# INLINABLE renderHtmlInternal #-}

-- | Outputs indented HTML, with indentation inside elements.
--   This can change the meaning of the HTML document, and
--   is mostly useful for debugging the HTML output.
--   The implementation is inefficient, and you are normally
--   better off using 'showHtml' or 'renderHtml'.
prettyHtmlInternal :: HTML html =>
                      String -- ^ DOCTYPE declaration
                   -> html -> String
prettyHtmlInternal docType theHtml =
    docType ++ "\n" ++ prettyHtmlFragment (mkHtml theHtml)

-- | Render a piece of HTML without adding a DOCTYPE declaration
--   or root element. Does not add any extra whitespace.
showHtmlFragment :: HTML html => html -> Builder
showHtmlFragment h =
    go $ getHtmlElements $ toHtml h
  where
    go [] = mempty
    go (x : xs) = showHtml' x <> go xs

{-# SPECIALIZE showHtmlFragment :: Html -> Builder #-}
{-# INLINABLE showHtmlFragment #-}

-- | Render a piece of indented HTML without adding a DOCTYPE declaration
--   or root element. Only adds whitespace where it does not change
--   the meaning of the document.
renderHtmlFragment :: HTML html => html -> Builder
renderHtmlFragment h =
    go $ getHtmlElements $ toHtml h
  where
    go [] = mempty
    go (x:xs) = renderHtml' 0 x <> go xs

{-# SPECIALIZE renderHtmlFragment :: Html -> Builder #-}
{-# INLINABLE renderHtmlFragment #-}

-- | Render a piece of indented HTML without adding a DOCTYPE declaration
--   or a root element.
--   The indentation is done inside elements.
--   This can change the meaning of the HTML document, and
--   is mostly useful for debugging the HTML output.
--   The implementation is inefficient, and you are normally
--   better off using 'showHtmlFragment' or 'renderHtmlFragment'.
prettyHtmlFragment :: HTML html => html -> String
prettyHtmlFragment =
    unlines . concatMap prettyHtml' . getHtmlElements . toHtml

-- | Show a single HTML element, without adding whitespace.
showHtml' :: HtmlElement -> Builder
showHtml' (HtmlString str) = str
showHtml'(HtmlTag { markupTag = name,
                    markupContent = html,
                    markupAttrs = attrs })
    = if isValidHtmlITag name && isNoHtml html
      then renderTag True nameBuilder (attrs []) ""
      else renderTag False nameBuilder (attrs []) ""
        <> go (getHtmlElements html)
        <> renderEndTag nameBuilder ""
  where
    go [] = mempty
    go (x:xs) = showHtml' x <> go xs

    nameBuilder :: Builder
    nameBuilder = lazyByteString name

renderHtml' :: Int -> HtmlElement -> Builder
renderHtml' _ (HtmlString str) = str
renderHtml' n (HtmlTag
              { markupTag = name,
                markupContent = html,
                markupAttrs = attrs })
      = if isValidHtmlITag name && isNoHtml html
        then renderTag True nameBuilder (attrs []) nl
        else renderTag False nameBuilder (attrs []) nl
          <> foldMap (renderHtml' (n+2)) (getHtmlElements html)
          <> renderEndTag nameBuilder nl
    where
      nl :: Builder
      nl = charUtf8 '\n' <> tabs <> spaces

      tabs :: Builder
      tabs =
        case n `div` 8 of
          m | m <= 0 -> mempty
          m          -> Sem.stimes m (charUtf8 '\t')

      spaces :: Builder
      spaces =
        case n `mod` 8 of
          m | m <= 0 -> mempty
          m          -> Sem.stimes m (charUtf8 ' ')

      nameBuilder :: Builder
      nameBuilder = lazyByteString name


prettyHtml' :: HtmlElement -> [String]
prettyHtml' (HtmlString str) = [builderToString str]
prettyHtml' (HtmlTag
              { markupTag = name,
                markupContent = html,
                markupAttrs = attrs })
      = if isValidHtmlITag name && isNoHtml html
        then
         [rmNL (renderTag True nameBuilder (attrs []) "")]
        else
         [rmNL (renderTag False nameBuilder (attrs []) "")] ++
          shift (concatMap prettyHtml' (getHtmlElements html)) ++
         [rmNL (renderEndTag nameBuilder "")]
  where
      shift = map ("   " ++)
      rmNL = filter (/= '\n') . builderToString

      nameBuilder :: Builder
      nameBuilder = lazyByteString name


-- | Show a start tag
renderTag :: Bool       -- ^ 'True' if the empty tag shorthand should be used
          -> Builder    -- ^ Tag name
          -> [HtmlAttr] -- ^ Attributes
          -> Builder    -- ^ Whitespace to add after attributes
          -> Builder
renderTag empty name attrs nl
      = "<" <> name <> shownAttrs <> nl <> close
  where
      close = if empty then " />" else ">"

      shownAttrs = foldr (\attr acc -> charUtf8 ' ' <> showPair attr <> acc) mempty attrs

      showPair :: HtmlAttr -> Builder
      showPair (HtmlAttr key val)
              = key <> "=\"" <> val  <> "\""

-- | Show an end tag
renderEndTag :: Builder -- ^ Tag name
             -> Builder -- ^ Whitespace to add after tag name
             -> Builder
renderEndTag name nl = "</" <> name <> nl <> ">"

isValidHtmlITag :: BSL.ByteString -> Bool
isValidHtmlITag bs = bs `Set.member` validHtmlITags

-- | The names of all elements which can be represented using the empty tag
--   short-hand.
validHtmlITags :: Set BSL.ByteString
validHtmlITags = Set.fromList [
                  "area",
                  "base",
                  "basefont",
                  "br",
                  "col",
                  "frame",
                  "hr",
                  "img",
                  "input",
                  "isindex",
                  "link",
                  "meta",
                  "param"
                 ]
