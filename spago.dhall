{ name = "rabbits"
, dependencies = [
	"effect",
	"prelude",
	"halogen",
	"foldable-traversable",
	"random",
	"nonempty",
	"aff"
	]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
