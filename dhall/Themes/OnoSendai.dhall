-- Ono-Sendai Theme
-- Monochromatic blue cyberpunk aesthetic
-- Inspired by William Gibson's cyberspace deck from Neuromancer
let Theme = ../Types/Theme.dhall

let rgb = Theme.rgb

let rgba = Theme.rgba

let bg = rgb 0.09803921568627451 0.10980392156862745 0.12549019607843137

let bgAlt = rgb 0.12156862745098039 0.13725490196078433 0.16470588235294117

let bgHl = rgb 0.16470588235294117 0.18823529411764706 0.2235294117647059

let comment = rgb 0.22745098039215686 0.25882352941176473 0.30980392156862746

let fgAlt = rgb 0.4196078431372549 0.4627450980392157 0.5372549019607843

let fg = rgb 0.7725490196078432 0.8156862745098039 0.8666666666666667

let fgLight = rgb 0.8627450980392157 0.8901960784313725 0.9254901960784314

let ice = rgb 0.7137254901960784 0.8901960784313725 1.0

let sky = rgb 0.5019607843137255 0.8 1.0

let hero = rgb 0.32941176470588235 0.6823529411764706 1.0

let deep = rgb 0.12941176470588237 0.5450980392156862 1.0

let matrix = rgb 0.03529411764705882 0.4117647058823529 0.8549019607843137

let link = rgb 0.30196078431372547 0.6235294117647059 1.0

let soft = rgb 0.4235294117647059 0.7137254901960784 1.0

let corp = rgb 0.12156862745098039 0.43529411764705883 0.9215686274509803

let addedBg = rgb 0.10196078431372549 0.17647058823529413 0.2549019607843137

let removedBg = rgb 0.19215686274509805 0.22745098039215686 0.2549019607843137

in    { -- Semantic colors
        primary = hero
      , secondary = soft
      , accent = ice
      , error = ice
      , warning = sky
      , success = deep
      , info = link
      , -- Text colors
        text = fg
      , textMuted = fgAlt
      , selectedListItemText = Some fgLight
      , -- Background colors
        background = bg
      , backgroundPanel = bgAlt
      , backgroundElement = bgHl
      , backgroundMenu = Some bgAlt
      , -- Border colors
        border = bgHl
      , borderActive = hero
      , borderSubtle = bgAlt
      , -- Diff colors
        diffAdded = deep
      , diffRemoved = ice
      , diffContext = comment
      , diffHunkHeader = comment
      , diffHighlightAdded = deep
      , diffHighlightRemoved = ice
      , diffAddedBg = addedBg
      , diffRemovedBg = removedBg
      , diffContextBg = bgAlt
      , diffLineNumber = comment
      , diffAddedLineNumberBg = addedBg
      , diffRemovedLineNumberBg = removedBg
      , -- Markdown colors
        markdownText = fg
      , markdownHeading = hero
      , markdownLink = link
      , markdownLinkText = soft
      , markdownCode = sky
      , markdownBlockQuote = comment
      , markdownEmph = soft
      , markdownStrong = ice
      , markdownHorizontalRule = comment
      , markdownListItem = hero
      , markdownListEnumeration = link
      , markdownImage = link
      , markdownImageText = soft
      , markdownCodeBlock = fg
      , -- Syntax highlighting colors
        syntaxComment = comment
      , syntaxKeyword = soft
      , syntaxFunction = link
      , syntaxVariable = ice
      , syntaxString = deep
      , syntaxNumber = sky
      , syntaxType = hero
      , syntaxOperator = fg
      , syntaxPunctuation = fgAlt
      , -- Additional
        thinkingOpacity = 0.6
      }
    : Theme.Theme.Type
