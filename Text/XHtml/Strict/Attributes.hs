{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

module Text.XHtml.Strict.Attributes where

import Text.XHtml.Internals
import qualified Data.Text.Lazy as LText

-- * Attributes in XHTML Strict

action              :: LText.Text -> HtmlAttr
align               :: LText.Text -> HtmlAttr
alt                 :: LText.Text -> HtmlAttr
altcode             :: LText.Text -> HtmlAttr
archive             :: LText.Text -> HtmlAttr
base                :: LText.Text -> HtmlAttr
border              :: Int    -> HtmlAttr
bordercolor         :: LText.Text -> HtmlAttr
cellpadding         :: Int    -> HtmlAttr
cellspacing         :: Int    -> HtmlAttr
checked             ::           HtmlAttr
codebase            :: LText.Text -> HtmlAttr
cols                :: LText.Text -> HtmlAttr
colspan             :: Int    -> HtmlAttr
content             :: LText.Text -> HtmlAttr
coords              :: LText.Text -> HtmlAttr
disabled            ::           HtmlAttr
enctype             :: LText.Text -> HtmlAttr
height              :: LText.Text -> HtmlAttr
href                :: LText.Text -> HtmlAttr
hreflang            :: LText.Text -> HtmlAttr
httpequiv           :: LText.Text -> HtmlAttr
identifier          :: LText.Text -> HtmlAttr
ismap               ::           HtmlAttr
lang                :: LText.Text -> HtmlAttr
maxlength           :: Int    -> HtmlAttr
method              :: LText.Text -> HtmlAttr
multiple            ::           HtmlAttr
name                :: LText.Text -> HtmlAttr
nohref              ::           HtmlAttr
rel                 :: LText.Text -> HtmlAttr
rev                 :: LText.Text -> HtmlAttr
rows                :: LText.Text -> HtmlAttr
rowspan             :: Int    -> HtmlAttr
rules               :: LText.Text -> HtmlAttr
selected            ::           HtmlAttr
shape               :: LText.Text -> HtmlAttr
size                :: LText.Text -> HtmlAttr
src                 :: LText.Text -> HtmlAttr
theclass            :: LText.Text -> HtmlAttr
thefor              :: LText.Text -> HtmlAttr
thestyle            :: LText.Text -> HtmlAttr
thetype             :: LText.Text -> HtmlAttr
title               :: LText.Text -> HtmlAttr
usemap              :: LText.Text -> HtmlAttr
valign              :: LText.Text -> HtmlAttr
value               :: LText.Text -> HtmlAttr
width               :: LText.Text -> HtmlAttr

action              =   strAttr "action"
align               =   strAttr "align"
alt                 =   strAttr "alt"
altcode             =   strAttr "altcode"
archive             =   strAttr "archive"
base                =   strAttr "base"
border              =   intAttr "border"
bordercolor         =   strAttr "bordercolor"
cellpadding         =   intAttr "cellpadding"
cellspacing         =   intAttr "cellspacing"
checked             = emptyAttr "checked"
codebase            =   strAttr "codebase"
cols                =   strAttr "cols"
colspan             =   intAttr "colspan"
content             =   strAttr "content"
coords              =   strAttr "coords"
disabled            = emptyAttr "disabled"
enctype             =   strAttr "enctype"
height              =   strAttr "height"
href                =   strAttr "href"
hreflang            =   strAttr "hreflang"
httpequiv           =   strAttr "http-equiv"
identifier          =   strAttr "id"
ismap               = emptyAttr "ismap"
lang                =   strAttr "lang"
maxlength           =   intAttr "maxlength"
method              =   strAttr "method"
multiple            = emptyAttr "multiple"
name                =   strAttr "name"
nohref              = emptyAttr "nohref"
rel                 =   strAttr "rel"
rev                 =   strAttr "rev"
rows                =   strAttr "rows"
rowspan             =   intAttr "rowspan"
rules               =   strAttr "rules"
selected            = emptyAttr "selected"
shape               =   strAttr "shape"
size                =   strAttr "size"
src                 =   strAttr "src"
theclass            =   strAttr "class"
thefor              =   strAttr "for"
thestyle            =   strAttr "style"
thetype             =   strAttr "type"
title               =   strAttr "title"
usemap              =   strAttr "usemap"
valign              =   strAttr "valign"
value               =   strAttr "value"
width               =   strAttr "width"

{-# INLINE action      #-}
{-# INLINE align       #-}
{-# INLINE alt         #-}
{-# INLINE altcode     #-}
{-# INLINE archive     #-}
{-# INLINE base        #-}
{-# INLINE border      #-}
{-# INLINE bordercolor #-}
{-# INLINE cellpadding #-}
{-# INLINE cellspacing #-}
{-# INLINE checked     #-}
{-# INLINE codebase    #-}
{-# INLINE cols        #-}
{-# INLINE colspan     #-}
{-# INLINE content     #-}
{-# INLINE coords      #-}
{-# INLINE disabled    #-}
{-# INLINE enctype     #-}
{-# INLINE height      #-}
{-# INLINE href        #-}
{-# INLINE hreflang    #-}
{-# INLINE httpequiv   #-}
{-# INLINE identifier  #-}
{-# INLINE ismap       #-}
{-# INLINE lang        #-}
{-# INLINE maxlength   #-}
{-# INLINE method      #-}
{-# INLINE multiple    #-}
{-# INLINE name        #-}
{-# INLINE nohref      #-}
{-# INLINE rel         #-}
{-# INLINE rev         #-}
{-# INLINE rows        #-}
{-# INLINE rowspan     #-}
{-# INLINE rules       #-}
{-# INLINE selected    #-}
{-# INLINE shape       #-}
{-# INLINE size        #-}
{-# INLINE src         #-}
{-# INLINE theclass    #-}
{-# INLINE thefor      #-}
{-# INLINE thestyle    #-}
{-# INLINE thetype     #-}
{-# INLINE title       #-}
{-# INLINE usemap      #-}
{-# INLINE valign      #-}
{-# INLINE value       #-}
{-# INLINE width       #-}

