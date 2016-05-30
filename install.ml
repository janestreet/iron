#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"iron"
  [ oasis_lib "iron"
  ; oasis_lib "iron_client"
  ; oasis_lib "iron_common"
  ; oasis_lib "iron_hg"
  ; oasis_lib "iron_obligations"
  ; oasis_lib "iron_protocol"
  ; oasis_lib "iron_server"
  ; oasis_lib "pdiff4"
  ; file "META" ~section:"lib"
  ; oasis_exe "fe" ~dest:"fe"
  ]
