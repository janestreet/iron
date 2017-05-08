open! Core
open! Import

include String

let invariant (_ : t) = ()

let submissions feature_path = Feature_name.to_string (Feature_path.root feature_path)
let continuous_release feature_path = submissions feature_path ^ "-continuous-release"
