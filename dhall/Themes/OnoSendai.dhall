-- Ono-Sendai Theme
-- Based on https://weyl.ai/brand
-- Inspired by William Gibson's cyberspace deck from Neuromancer
-- Cold, electric, cyberpunk aesthetics
let Theme = ../Types/Theme.dhall

let rgb = Theme.rgb

let rgba = Theme.rgba

let void = rgb 0.031 0.031 0.047

let matrix = rgb 0.055 0.063 0.090

let chrome = rgb 0.082 0.094 0.133

let ice = rgb 0.114 0.133 0.180

let neon = rgb 0.200 0.949 0.949

let pulse = rgb 0.482 0.996 0.831

let burn = rgb 0.996 0.325 0.431

let flux = rgb 0.925 0.686 0.216

let arc = rgb 0.486 0.537 0.992

let spike = rgb 0.894 0.412 0.851

let ghost = rgb 0.686 0.737 0.824

let bright = rgb 0.878 0.914 0.965

let dim = rgb 0.510 0.565 0.655

let shadow = rgb 0.318 0.365 0.451

in    { -- Semantic colors
        primary = neon
      , secondary = arc
      , accent = pulse
      , error = burn
      , warning = flux
      , success = pulse
      , info = arc
      , -- Text colors
        text = bright
      , textMuted = dim
      , selectedListItemText = Some bright
      , -- Background colors
        background = void
      , backgroundPanel = matrix
      , backgroundElement = chrome
      , backgroundMenu = Some matrix
      , -- Border colors
        border = ice
      , borderActive = neon
      , borderSubtle = chrome
      , -- Diff colors
        diffAdded = pulse
      , diffRemoved = burn
      , diffContext = dim
      , diffHunkHeader = arc
      , diffHighlightAdded = rgb 0.25 0.60 0.50
      , diffHighlightRemoved = rgb 0.60 0.20 0.25
      , diffAddedBg = rgb 0.06 0.15 0.12
      , diffRemovedBg = rgb 0.18 0.06 0.08
      , diffContextBg = matrix
      , diffLineNumber = shadow
      , diffAddedLineNumberBg = rgb 0.04 0.10 0.08
      , diffRemovedLineNumberBg = rgb 0.12 0.04 0.05
      , -- Markdown colors
        markdownText = bright
      , markdownHeading = neon
      , markdownLink = arc
      , markdownLinkText = pulse
      , markdownCode = pulse
      , markdownBlockQuote = dim
      , markdownEmph = flux
      , markdownStrong = neon
      , markdownHorizontalRule = ice
      , markdownListItem = dim
      , markdownListEnumeration = arc
      , markdownImage = spike
      , markdownImageText = spike
      , markdownCodeBlock = bright
      , -- Syntax highlighting colors
        syntaxComment = shadow
      , syntaxKeyword = spike
      , syntaxFunction = arc
      , syntaxVariable = bright
      , syntaxString = pulse
      , syntaxNumber = flux
      , syntaxType = neon
      , syntaxOperator = dim
      , syntaxPunctuation = dim
      , -- Additional
        thinkingOpacity = 0.6
      }
    : Theme.Theme.Type
