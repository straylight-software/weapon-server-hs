-- Enums.dhall
-- Common enumeration types used throughout the configuration
let LogLevel = < DEBUG | INFO | WARN | ERROR >

let ShareMode = < manual | auto | disabled >

let Layout = < auto | stretch >

let PermissionAction = < ask | allow | deny >

let DiffStyle = < auto | stacked >

let AgentMode = < subagent | primary | all >

let ModelStatus = < alpha | beta | deprecated | stable >

in  { LogLevel
    , ShareMode
    , Layout
    , PermissionAction
    , DiffStyle
    , AgentMode
    , ModelStatus
    }
