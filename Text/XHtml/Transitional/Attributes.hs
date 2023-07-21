{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

module Text.XHtml.Transitional.Attributes where

import Text.XHtml.Internals

-- * Extra attributes in XHTML Transitional

{-# DEPRECATED alink "This attribute is deprecated in XHTML 1.0" #-}
alink               :: LText -> HtmlAttr
alink               =   strAttr "alink"

{-# DEPRECATED background "This attribute is deprecated in XHTML 1.0" #-}
background          :: LText -> HtmlAttr
background          =   strAttr "background"

{-# DEPRECATED bgcolor "This attribute is deprecated in XHTML 1.0" #-}
bgcolor             :: LText -> HtmlAttr
bgcolor             =   strAttr "bgcolor"

{-# DEPRECATED clear "This attribute is deprecated in XHTML 1.0" #-}
clear               :: LText -> HtmlAttr
clear               =   strAttr "clear"

{-# DEPRECATED code "This attribute is deprecated in XHTML 1.0" #-}
code                :: LText -> HtmlAttr
code                =   strAttr "code"

{-# DEPRECATED color "This attribute is deprecated in XHTML 1.0" #-}
color               :: LText -> HtmlAttr
color               =   strAttr "color"

{-# DEPRECATED compact "This attribute is deprecated in XHTML 1.0" #-}
compact             ::           HtmlAttr
compact             = emptyAttr "compact"

{-# DEPRECATED face "This attribute is deprecated in XHTML 1.0" #-}
face                :: LText -> HtmlAttr
face                =   strAttr "face"

{-# DEPRECATED hspace "This attribute is deprecated in XHTML 1.0" #-}
hspace              :: Int    -> HtmlAttr
hspace              =   intAttr "hspace"

{-# DEPRECATED link "This attribute is deprecated in XHTML 1.0" #-}
link                :: LText -> HtmlAttr
link                =   strAttr "link"

{-# DEPRECATED noshade "This attribute is deprecated in XHTML 1.0" #-}
noshade             ::           HtmlAttr
noshade             = emptyAttr "noshade"

{-# DEPRECATED nowrap "This attribute is deprecated in XHTML 1.0" #-}
nowrap              ::           HtmlAttr
nowrap              = emptyAttr "nowrap"

{-# DEPRECATED start "This attribute is deprecated in XHTML 1.0" #-}
start               :: Int    -> HtmlAttr
start               =   intAttr "start"

target              :: LText -> HtmlAttr
target              =   strAttr "target"

{-# DEPRECATED text "This attribute is deprecated in XHTML 1.0" #-}
text                :: LText -> HtmlAttr
text                =   strAttr "text"

{-# DEPRECATED version "This attribute is deprecated in XHTML 1.0" #-}
version             :: LText -> HtmlAttr
version             =   strAttr "version"

{-# DEPRECATED vlink "This attribute is deprecated in XHTML 1.0" #-}
vlink               :: LText -> HtmlAttr
vlink               =   strAttr "vlink"

{-# DEPRECATED vspace "This attribute is deprecated in XHTML 1.0" #-}
vspace              :: Int    -> HtmlAttr
vspace              =   intAttr "vspace"



--
-- * Html colors
--

{-# DEPRECATED aqua,black,blue,fuchsia,gray,green,lime,maroon,navy,olive,purple,red,silver,teal,yellow,white "The use of color attibutes is deprecated in XHTML 1.0" #-}
aqua          :: LText
black         :: LText
blue          :: LText
fuchsia       :: LText
gray          :: LText
green         :: LText
lime          :: LText
maroon        :: LText
navy          :: LText
olive         :: LText
purple        :: LText
red           :: LText
silver        :: LText
teal          :: LText
yellow        :: LText
white         :: LText

aqua          = "aqua"
black         = "black"
blue          = "blue"
fuchsia       = "fuchsia"
gray          = "gray"
green         = "green"
lime          = "lime"
maroon        = "maroon"
navy          = "navy"
olive         = "olive"
purple        = "purple"
red           = "red"
silver        = "silver"
teal          = "teal"
yellow        = "yellow"
white         = "white"
