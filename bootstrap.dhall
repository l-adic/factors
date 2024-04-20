{ name = "bootstrap"
, dependencies =
  [ "chanterelle"
  ]
, packages = ./packages.dhall
, sources = [ "./deploy/Bootstrap.purs" ]
}
