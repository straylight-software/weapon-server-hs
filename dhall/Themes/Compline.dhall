-- Compline Theme
-- Based on https://complinetheme.com/palette
-- A warm, contemplative color scheme
let Theme = ../Types/Theme.dhall

let rgb = Theme.rgb

let bg = rgb 0.094 0.086 0.106

let bgPanel = rgb 0.122 0.114 0.137

let bgElement = rgb 0.161 0.153 0.180

let fg = rgb 0.906 0.878 0.859

let fgMuted = rgb 0.624 0.588 0.573

let fgSubtle = rgb 0.463 0.431 0.424

let rose = rgb 0.906 0.569 0.592

let coral = rgb 0.925 0.635 0.529

let gold = rgb 0.878 0.749 0.502

let sage = rgb 0.616 0.753 0.620

let sky = rgb 0.576 0.714 0.808

let lavender = rgb 0.706 0.620 0.788

let blush = rgb 0.824 0.553 0.620

let deepRose = rgb 0.35 0.20 0.22

let deepSage = rgb 0.18 0.25 0.18

in    { -- Semantic colors
        primary = lavender
      , secondary = sky
      , accent = coral
      , error = rose
      , warning = gold
      , success = sage
      , info = sky
      , -- Text colors
        text = fg
      , textMuted = fgMuted
      , selectedListItemText = Some fg
      , -- Background colors
        background = bg
      , backgroundPanel = bgPanel
      , backgroundElement = bgElement
      , backgroundMenu = Some bgPanel
      , -- Border colors
        border = bgElement
      , borderActive = lavender
      , borderSubtle = rgb 0.2 0.19 0.23
      , -- Diff colors
        diffAdded = sage
      , diffRemoved = rose
      , diffContext = fgMuted
      , diffHunkHeader = sky
      , diffHighlightAdded = rgb 0.45 0.60 0.45
      , diffHighlightRemoved = rgb 0.70 0.40 0.42
      , diffAddedBg = deepSage
      , diffRemovedBg = deepRose
      , diffContextBg = bgPanel
      , diffLineNumber = fgSubtle
      , diffAddedLineNumberBg = rgb 0.14 0.20 0.14
      , diffRemovedLineNumberBg = rgb 0.25 0.14 0.16
      , -- Markdown colors
        markdownText = fg
      , markdownHeading = coral
      , markdownLink = sky
      , markdownLinkText = lavender
      , markdownCode = sage
      , markdownBlockQuote = fgMuted
      , markdownEmph = gold
      , markdownStrong = coral
      , markdownHorizontalRule = bgElement
      , markdownListItem = fgMuted
      , markdownListEnumeration = lavender
      , markdownImage = blush
      , markdownImageText = blush
      , markdownCodeBlock = fg
      , -- Syntax highlighting colors
        syntaxComment = fgSubtle
      , syntaxKeyword = rose
      , syntaxFunction = lavender
      , syntaxVariable = fg
      , syntaxString = sage
      , syntaxNumber = coral
      , syntaxType = gold
      , syntaxOperator = fgMuted
      , syntaxPunctuation = fgMuted
      , -- Additional
        thinkingOpacity = 0.4
      }
    : Theme.Theme.Type
