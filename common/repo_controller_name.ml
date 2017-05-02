open! Core
open! Import

include String

let invariant (_ : t) = ()

let of_feature_name = Feature_name.to_string
